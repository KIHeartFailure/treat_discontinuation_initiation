

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata324 %>%
  filter(casecontrol == "Case")

flow <- c("Number of posts (cases) in SHFDB3", nrow(rsdata))

rsdata <- rsdata %>%
  filter(
    shf_indexdtm >= ymd("2009-01-01"),
    shf_indexdtm <= ymd("2018-12-31")
  )
flow <- rbind(flow, c("Indexdate 2009-01-01 -- 2018-12-31", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef != ">=50")
flow <- rbind(flow, c("No HFpEF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_durationhf))
flow <- rbind(flow, c("No missing duration of HF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_durationhf == ">6mo")
flow <- rbind(flow, c("Duration of HF > 6 months (as registred in SwedeHF)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_location != "Other in-patient") %>%
  mutate(sos_location = droplevels(sos_location))
flow <- rbind(flow, c("Not hospitalized for anything other than HF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(is.na(sos_prevhosphf) | (sos_prevhosphf <= 14 & sos_location == "HF in-patient"))
flow <- rbind(flow, c("No prior HF hospitalization (allowing for HF hospitalization within 14 days prior to index for cases)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death >= 365 / 2)
flow <- rbind(flow, c(">= 6 months follow-up (not dead/emigrated/end follow-up during this time)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

flow <- rbind(flow, c(". whereof HF in-patients", nrow(rsdata %>% filter(sos_location == "HF in-patient"))))

colnames(flow) <- c("Criteria", "N")
