
# (HF) hospitalization ---------------------------------------------------

allhosp <- inner_join(
  rsdata323 %>% select(LopNr, shf_indexdtm),
  patreg %>% filter(sos_source == "sv"),
  by = "LopNr"
) %>%
  mutate(tmp_sosdtm = coalesce(UTDATUM, INDATUM)) %>%
  filter(tmp_sosdtm <= shf_indexdtm) %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(tmp_sosdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  select(LopNr, shf_indexdtm, tmp_sosdtm)

rsdata323 <- left_join(
  rsdata323,
  allhosp,
  by = c("LopNr", "shf_indexdtm")
) %>%
  mutate(
    sos_prevhospall = as.numeric(shf_indexdtm - tmp_sosdtm),
    sos_prevhospall = case_when(
      casecontrol == "Control" ~ NA_real_,
      is.na(sos_prevhospall) ~ NA_real_,
      TRUE ~ sos_prevhospall
    )
  ) %>%
  select(-tmp_sosdtm)


# Definition of cases and controls ----------------------------------------

rsdata323 <- rsdata323 %>%
  mutate(sos_location = case_when(
    sos_prevhosphf <= 14 ~ "HF in-patient",
    (is.na(sos_prevhospall) | sos_prevhospall > 14) & shf_location == "Out-patient" ~ "Out-patient",
    TRUE ~ "Exclude"
  ))
