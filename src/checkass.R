
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 3)


# Predictors models -------------------------------------------------------

checkass <- function(efcat, medpre, medpost, valmedpre = "Yes", valmedpost = "No") {
  mod <- glm(formula(paste0(
    medpost, "=='", valmedpost, "' ~ ", paste(modvars, collapse = " + ")
  )),
  family = binomial,
  data = dataass %>% filter(sos_location == "HF in-patient" & shf_ef_cat == efcat & !!sym(medpre) == valmedpre)
  )

  # Outliers ---------------------------------------------------------------
  x11()
  plot(mod, which = 4, id.n = 3)

  # Multicollinearity -------------------------------------------------------
  print(car::vif(mod))

  # Linearity for age -------------------------------------------------------
  mod <- glm(formula(paste0(
    medpost, "=='", valmedpost, "' ~ shf_sex + ns(shf_age, 4) + shf_nyha_cat + shf_sos_com_ihd +
    shf_smoking_cat + shf_sos_com_af + shf_anemia + shf_sos_com_diabetes + shf_sos_com_hypertension + sos_com_valvular +
    sos_com_peripheralartery + sos_com_copd + sos_com_cancer3y +
    sos_com_stroketia + scb_famtype + scb_education + scb_dispincome_cat2"
  )),
  family = binomial,
  data = dataass %>% filter(sos_location == "HF in-patient" & shf_ef_cat == efcat & !!sym(medpre) == valmedpre)
  )

  probabilities <- predict(mod, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

  contdata <- dataass %>%
    filter(sos_location == "HF in-patient" & shf_ef_cat == efcat & !!sym(medpre) == valmedpre) %>%
    select(shf_age)

  # Bind the logit and tidying the data for plot
  contdata <- contdata %>%
    mutate(logit = log(probabilities / (1 - probabilities))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)

  x11()
  ggplot(contdata, aes(logit, predictor.value)) +
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    facet_wrap(~predictors, scales = "free_y")
}

checkass(efcat = "HFrEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post")
checkass(efcat = "HFpEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post")

checkass(efcat = "HFrEF", medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post")
checkass(efcat = "HFmrEF", medpre = "ddr_rasi_prior", medpost = "ddr_mra_post")

checkass(efcat = "HFrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")
checkass(efcat = "HFmrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")

checkass(
  efcat = "HFrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedpost = "Yes"
)
checkass(
  efcat = "HFmrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedpost = "Yes"
)
checkass(
  efcat = "HFpEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedpost = "Yes"
)


# check assumptions for cox models ----------------------------------------

checkasscox <- function(time, event, efcat, medpre, medpost, valmedpre = "Yes", valmedrefpost = "Yes") {
  mod <- coxph(formula(paste0(
    "Surv(", time, ",", event, " == 'Yes') ~ relevel(",
    medpost, ", ref = '", valmedrefpost, "') + ", paste(modvars, collapse = " + ")
  )), data = dataass %>% filter(sos_location == "HF in-patient" & shf_ef_cat %in% efcat & !!sym(medpre) == valmedpre))

  testpat <- cox.zph(mod)
  print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

  # check spec for sos_location
  x11()
  plot(testpat[1], resid = F, ylim = c(-4, 4))

  x11()
  survminer::ggcoxfunctional(formula(paste0(
    "Surv(", time, ",", event, " == 'Yes') ~ shf_age"
  )), data = dataass)
}

time2 <- "sos_outtime_death3y"
event2 <- "sos_out_death3y"

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")

checkasscox(
  time = time2, event = event2,
  efcat = "HFrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post"
)


checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post",
            valmedpre = "No", valmedrefpost = "No")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post",
            valmedpre = "No", valmedrefpost = "No")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post",
valmedpre = "No", valmedrefpost = "No")

checkasscox(
  time = time2, event = event2,
  efcat = "HFrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedrefpost = "No"
)


checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_mra_prior", medpost = "ddr_mra_post")

checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post")

checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")

checkasscox(
  time = time2, event = event2,
  efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedrefpost = "No"
)




time2 <- "sos_outtime_death3y"
event2 <- "sos_out_deathcv3y"

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")

checkasscox(
  time = time2, event = event2,
  efcat = "HFrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post"
)


checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_mra_prior", medpost = "ddr_mra_post",
            valmedpre = "No", valmedrefpost = "No")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post",
            valmedpre = "No", valmedrefpost = "No")

checkasscox(time = time2, event = event2, efcat = "HFrEF", medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post",
            valmedpre = "No", valmedrefpost = "No")

checkasscox(
  time = time2, event = event2,
  efcat = "HFrEF", medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedrefpost = "No"
)


checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_mra_prior", medpost = "ddr_mra_post")

checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_rasi_prior", medpost = "ddr_rasi_post")

checkasscox(time = time2, event = event2, efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_bbl_prior", medpost = "ddr_bbl_post")

checkasscox(
  time = time2, event = event2,
  efcat = c("HFmrEF", "HFrEF"), medpre = "ddr_loopdiuretic_prior", medpost = "ddr_loopdiuretic_post",
  valmedpre = "No", valmedrefpost = "No"
)

