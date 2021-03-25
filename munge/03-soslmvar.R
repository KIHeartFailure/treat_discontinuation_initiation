
# Treatments from DDR --------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    select(LopNr, shf_indexdtm),
  lmsel,
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  select(LopNr, shf_indexdtm, EDATUM, diff, ATC)

lmtreats <- function(atc, treatname) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc)
    ) %>%
    filter(atcneed)

  treatnameprior <- paste0("ddr_", treatname, "_prior")
  treatnameprior8m <- paste0("ddr_", treatname, "_prior8m")
  treatnamepost <- paste0("ddr_", treatname, "_post")

  lmtmpprior <- lmtmp2 %>%
    filter(diff >= -30.5 * 6, diff <= -1) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnameprior := 1) %>%
    select(LopNr, !!sym(treatnameprior))

  ## sensitivity analysis: 2 prescriptions within 8 months prior
  lmtmppriorsens <- lmtmp2 %>%
    filter(diff >= -30.5 * 8, diff <= -1) %>%
    group_by(LopNr, EDATUM) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(LopNr) %>%
    slice(2) %>%
    ungroup() %>%
    mutate(!!treatnameprior8m := 1) %>%
    select(LopNr, !!sym(treatnameprior8m))

  lmtmppost <- lmtmp2 %>%
    filter(diff <= 30.5 * 6, diff >= 0) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost := 1) %>%
    select(LopNr, !!sym(treatnamepost))

  lmtmpall <- Reduce(
    function(...) {
      full_join(...,
        by = "LopNr"
      )
    },
    list(lmtmpprior, lmtmppriorsens, lmtmppost)
  )

  rsdata <<- left_join(
    rsdata,
    lmtmpall,
    by = c("LopNr")
  ) %>%
    mutate(
      !!treatnameprior := replace_na(!!sym(treatnameprior), 0),
      !!treatnameprior8m := replace_na(!!sym(treatnameprior8m), 0),
      !!treatnamepost := replace_na(!!sym(treatnamepost), 0),

      !!treatnameprior := factor(!!sym(treatnameprior), levels = 0:1, labels = c("No", "Yes")),
      !!treatnameprior8m := factor(!!sym(treatnameprior8m), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost := factor(!!sym(treatnamepost), levels = 0:1, labels = c("No", "Yes"))
    )

  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}


lmtreats(atc = "^(C09A|C09B|C09C|C09D(?!X04))", treatname = "rasi")

lmtreats("^(C09A|C09B)", "acei")

lmtreats("^(C09C|C09D(?!X04))", "arb")

lmtreats("^(C03CA|C03CB|C03EB)", "loopdiuretic")

lmtreats("^C07", "bbl")

lmtreats("^C03DA", "mra")

rsdata <- rsdata %>%
  mutate(across(where(is_character), as_factor))

colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry"
  )
