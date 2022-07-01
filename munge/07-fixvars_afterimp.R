
# keep original imputed data (rsdataimp) just in case
imp.org <- imp

# Convert to Long
long <- mice::complete(imp, action = "long", include = TRUE)

long <- long %>%
  mutate(
    shf_gfrckdepi_cat2 = factor(case_when(
      is.na(shf_gfrckdepi_cat) ~ NA_real_,
      shf_gfrckdepi_cat %in% c("<30", "30-59") ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c(">=60", "<60"))
  )

# Convert back to mids object
imp <- as.mids(long)

rsdata <- rsdata %>%
  mutate(
    shf_gfrckdepi_cat2 = factor(case_when(
      is.na(shf_gfrckdepi_cat) ~ NA_real_,
      shf_gfrckdepi_cat %in% c("<30", "30-59") ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c(">=60", "<60"))
  )
