
# Additional variables from NPR -------------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  noof = TRUE,
  name = "nohosphf6m",
  stoptime = 365.25 / 2,
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  noof = TRUE,
  name = "nohosphf1y",
  stoptime = 365.25,
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  noof = TRUE,
  name = "nohosphf2y",
  stoptime = 2 * 365.25,
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  noof = TRUE,
  name = "nohosphf3y",
  stoptime = 3 * 365.25,
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  stoptime = 3 * 365.25,
  type = "out",
  name = "nohospany3y",
  diakod = " ",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  stoptime = 3 * 365.25,
  type = "out",
  name = "nohospcv3y",
  diakod = " I| J81| K761| G45| R57",
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  stoptime = 3 * 365.25,
  type = "out",
  name = "nohospnoncv3y",
  diakod = " I| J81| K761| G45| R57",
  diakodneg = TRUE,
  censdate = censdtm,
  warnings = TRUE,
  meta_reg = "NPR (in)"
)
