
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
  treatnamepost6m <- paste0("ddr_", treatname, "_post6m")
  treatnamepost1y <- paste0("ddr_", treatname, "_post1y")
  treatnamepost2y <- paste0("ddr_", treatname, "_post2y")
  treatnamepost3y <- paste0("ddr_", treatname, "_post3y")

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

  ## post treatments
  lmtmppost <- lmtmp2 %>%
    filter(diff >= 0, diff <= 30.5 * 6) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost := 1) %>%
    select(LopNr, !!sym(treatnamepost))

  lmtmppost6m <- lmtmp2 %>%
    filter(diff >= 365.25 / 2, diff <= 365.25) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost6m := 1) %>%
    select(LopNr, !!sym(treatnamepost6m))

  lmtmppost1y <- lmtmp2 %>%
    filter(diff >= 365.25, diff <= 365.25 * 1.5) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost1y := 1) %>%
    select(LopNr, !!sym(treatnamepost1y))

  lmtmppost2y <- lmtmp2 %>%
    filter(diff >= 365.25 * 2, diff <= 365.25 * 2.5) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost2y := 1) %>%
    select(LopNr, !!sym(treatnamepost2y))

  lmtmppost3y <- lmtmp2 %>%
    filter(diff >= 365.25 * 3, diff <= 365.25 * 3.5) %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatnamepost3y := 1) %>%
    select(LopNr, !!sym(treatnamepost3y))

  lmtmpall <- Reduce(
    function(...) {
      full_join(...,
        by = "LopNr"
      )
    },
    list(lmtmpprior, lmtmppriorsens, lmtmppost, lmtmppost6m, lmtmppost1y, lmtmppost2y, lmtmppost3y)
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
      !!treatnamepost6m := replace_na(!!sym(treatnamepost6m), 0),
      !!treatnamepost1y := replace_na(!!sym(treatnamepost1y), 0),
      !!treatnamepost2y := replace_na(!!sym(treatnamepost2y), 0),
      !!treatnamepost3y := replace_na(!!sym(treatnamepost3y), 0),

      !!treatnameprior := factor(!!sym(treatnameprior), levels = 0:1, labels = c("No", "Yes")),
      !!treatnameprior8m := factor(!!sym(treatnameprior8m), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost := factor(!!sym(treatnamepost), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost6m := factor(!!sym(treatnamepost6m), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost1y := factor(!!sym(treatnamepost1y), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost2y := factor(!!sym(treatnamepost2y), levels = 0:1, labels = c("No", "Yes")),
      !!treatnamepost3y := factor(!!sym(treatnamepost3y), levels = 0:1, labels = c("No", "Yes"))
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
