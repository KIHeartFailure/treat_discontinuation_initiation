

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata323 %>%
  filter(casecontrol == "Case")

flow <- c("Number of posts (cases) in SHFDB3", nrow(rsdata))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2009-01-01"), 
         shf_indexdtm <= ymd("2016-12-31"))
flow <- rbind(flow, c("Indexdate 2009-01-01 -- 2016-12-31", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_durationhf))
flow <- rbind(flow, c("No missing duration of HF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_durationhf == ">6mo")
flow <- rbind(flow, c("Duration of HF > 6 months", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death >= 365/2)
flow <- rbind(flow, c(">= 6 months follow-up (not dead/emigrated/end follow-up during this time)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(is.na(sos_prevhosphf) | sos_prevhosphf <= 14)
flow <- rbind(flow, c("No prior HF hospitalization (allowing for HF hospitalization within 14 days from index)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_location == "Out-patient" & sos_outtime_hosphf >= 365/2 | shf_location == "In-patient")
flow <- rbind(flow, c("Free from HF hospitalization within 6 months after index (only controls)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

flow <- rbind(flow, c("In-patients", nrow(rsdata %>% filter(shf_location == "In-patient"))))

colnames(flow) <- c("Criteria", "N")