
ProjectTemplate::reload.project()

memory.limit(size = 10000000000000)

# Import LM from SoS -----------------------------------------------------

sospath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/SOS/"

load(paste0(sospath, "lev3_15875_2019 Lina Benson/RData/lm.RData"))

# Select ATC codes --------------------------------------------------------

lmsel <- lm %>%
  mutate(atcneed = stringr::str_detect(ATC, "^C0")) %>%
  filter(
    ANTAL >= 0,
    #AR >= 2013, 
    #AR <= 2018, 
    atcneed
  )

#lm2019 <- read_sasdata(paste0(sospath, "20210118/Ut2_846_2021/"), "t_r_lmed__846_2021", clean = FALSE)
                       
#lmsel2019 <- lm2019 %>%
#  mutate(atcneed = stringr::str_detect(ATC, "^C0")) %>%
#  filter(
#    ANTAL >= 0,
#    #AR >= 2013, 
#    #AR <= 2018, 
#    atcneed
#  )

# 2019 meds in first file

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lmsel.RData", list = c("lmsel"))

# Patient registry from SHFDB3 v 3.2.2, prepared in 08-prep_sosdata.R -----

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))