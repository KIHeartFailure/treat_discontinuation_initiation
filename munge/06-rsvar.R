

# Additional variables from mainly SHF ------------------------------------

rsdata <- rsdata %>%
  mutate(
    shf_location = relevel(shf_location, ref = "Out-patient"),
    sos_location = relevel(sos_location, ref = "Out-patient"),

    shf_nyha_cat = case_when(
      shf_nyha == "I" ~ "I",
      shf_nyha == "II" ~ "II",
      shf_nyha %in% c("III", "IV") ~ "III-IV"
    ),

    # Anemia
    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),

    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 3,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 1
    ),
    labels = c("HFrEF", "HFmrEF", "HFpEF"),
    levels = 1:3
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Never") ~ 1,
      shf_smoking %in% c("Former", "Current") ~ 2
    ),
    labels = c("Never", "Former/Current"),
    levels = 1:2
    ),

    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),

    shf_potassium_cat = factor(
      case_when(
        is.na(shf_potassium) ~ NA_real_,
        shf_potassium < 3.5 ~ 2,
        shf_potassium <= 5 ~ 1,
        shf_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),

    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_gfrckdepi_cat = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 1,
      shf_gfrckdepi < 60 ~ 2,
    ),
    labels = c(">=60", "<60"),
    levels = 1:2
    ),

    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_valvular = case_when(
      shf_valvedisease == "Yes" |
        sos_com_valvular == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # Outcomes

    # limit to 3 yrs
    sos_out_hospany3y = if_else(sos_outtime_hospany >= 365 * 3, "No", as.character(sos_out_hospany)),
    sos_outtime_hospany3y = if_else(sos_outtime_hospany >= 365 * 3, 365, sos_outtime_hospany),

    sos_out_hospcv3y = if_else(sos_outtime_hospcv >= 365 * 3, "No", as.character(sos_out_hospcv)),
    sos_outtime_hospcv3y = if_else(sos_outtime_hospcv >= 365 * 3, 365, sos_outtime_hospcv),

    sos_out_hospnoncv3y = if_else(sos_outtime_hospnoncv >= 365 * 3, "No", as.character(sos_out_hospnoncv)),
    sos_outtime_hospnoncv3y = if_else(sos_outtime_hospnoncv >= 365 * 3, 365, sos_outtime_hospnoncv),

    sos_out_hosphf3y = if_else(sos_outtime_hosphf >= 365 * 3, "No", as.character(sos_out_hosphf)),
    sos_outtime_hosphf3y = if_else(sos_outtime_hosphf >= 365 * 3, 365, sos_outtime_hosphf),

    sos_out_deathcv3y = if_else(sos_outtime_death >= 365 * 3, "No", as.character(sos_out_deathcv)),
    sos_out_deathnoncv3y = if_else(sos_outtime_death >= 365 * 3, "No", as.character(sos_out_deathnoncv)),
    sos_out_death3y = if_else(sos_outtime_death >= 365 * 3, "No", as.character(sos_out_death)),
    sos_outtime_death3y = if_else(sos_outtime_death >= 365 * 3, 365, sos_outtime_death),

    # combined
    sos_out_deathhosphf3y = case_when(
      sos_out_death3y == "Yes" |
        sos_out_hosphf3y == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    sos_out_deathcvhosphf3y = case_when(
      sos_out_deathcv3y == "Yes" |
        sos_out_hosphf3y == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
  )


# income

inc <- rsdata %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop_last")

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-incmed)

# ntprobnp

ntprobnp <- rsdata %>%
  group_by(shf_ef_cat) %>%
  summarise(
    ntmed = quantile(shf_ntprobnp,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

rsdata <- left_join(
  rsdata,
  ntprobnp,
  by = c("shf_ef_cat")
) %>%
  mutate(
    shf_ntprobnp_cat = case_when(
      shf_ntprobnp < ntmed ~ 1,
      shf_ntprobnp >= ntmed ~ 2
    ),
    shf_ntprobnp_cat = factor(shf_ntprobnp_cat,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-ntmed)

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
