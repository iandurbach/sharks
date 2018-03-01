# Calculates monitoring efforts per pairs of sites (each site is a cluster of receivers)
# The active date for a site is the range of dates that AT LEAST ONE receiver is on for
# To detect a transition between a pair of sites BOTH sites must be active
# The active date for a pair of sites is the range of dates that BOTH sites are active
# Data from Alison Kock
# Ian Durbach 1 Mar 2018

library(readxl)
library(lubridate)
library(tidyverse)

# Alison's data on monitoring effort
site_info <- read_excel("data/monitoring-effort.xlsx")
site_info <- site_info %>% 
  rename("Site.Code" = `Site Code`,
         "First.Date" = `Date receiver first deployed`,
         "Last.Date" = `Date receiver last deployed`)

site_info$First.Date <- ymd(site_info$First.Date)
site_info$Last.Date <- ymd(site_info$Last.Date)

# cluster receivers into "sites"
site_info$Site.Code <- site_info$Site.Code %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
                                          KLB = c("KLBI","KLBO","MBC"), 
                                          MI = c("MO", "MI","MIA","MIB"),
                                          MSZ = c("MSB","MZV","MJB"),
                                          SFB = c("SF3","SFI4"),
                                          SFA = c("SF2","SFO1"), # Middle of the Bay
                                          SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                 "SISE","SIW"), 
                                          WR = c("WR","WRA","WRB"),
                                          HK = c("HKI", "HKO"), # East shore
                                          GB = c("GBI","GBO"),
                                          KB = c("KBI","KBO"),
                                          PB = c("PBI","PBO"),
                                          NH = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                          ST = c("STI","STO"), # West shore
                                          PP = c("PPN","PPS"), 
                                          RK = c("RKI","RKO"), 
                                          NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))

# we group receivers into sites and then treat these as a single receiver, so we want the earliest 
# and latest dates that ANY receiver in a site was active (range of detection dates at the site)
site_info <- site_info %>% 
  group_by(Site.Code) %>%
  summarize(First.Date = min(First.Date),
            Last.Date = max(Last.Date)) %>%
ungroup()

# study period
study_start <- dmy(01052005, tz = "UTC")
study_end <- dmy(31122007, tz = "UTC")
study_period <- seq(from = study_start, to = study_end, by = "day")

# make data frame with all dates in study period for each site
site_dates <- expand.grid(site_info$Site.Code, site_info$Site.Code, study_period)
colnames(site_dates) <- c("site1", "site2", "date")

# add season to each date
site_dates <- site_dates %>%
  mutate(aut_wint = (month(date) > 2 & month(date) < 9))

# add dates of first and last deployment for each receiver
site_dates <- site_dates %>% arrange(site1, site2) %>%
  left_join(site_info %>% select(Site.Code, First.Date, Last.Date), by = c("site1" = "Site.Code")) %>%
  left_join(site_info %>% select(Site.Code, First.Date, Last.Date), by = c("site2" = "Site.Code"))

# indicator of whether receiver was active on each day
site_dates <- site_dates %>% 
  mutate(active = (date >= First.Date.x) & (date <= Last.Date.x) & (date >= First.Date.y) & (date <= Last.Date.y))

# calculate number of days each receiver was active for in the study period
site_effort <- 
  site_dates %>% 
  group_by(site1, site2, aut_wint) %>% 
  summarize(effort = sum(active)) %>%
  ungroup()

# SIN = SIN_1 + SIN_2, manually adjust
site_effort <- site_effort %>% 
  mutate(site1 = recode(site1, 'SIN_1' = "SIN", 'SIN_2' = 'SIN'),
         site2 = recode(site2, 'SIN_1' = "SIN", 'SIN_2' = 'SIN'))

# need to redo calculation  to get the correct values for SIN
site_effort <- 
  site_effort %>% 
  group_by(site1, site2, aut_wint) %>% 
  summarize(effort = sum(effort)) %>%
  ungroup()

# save output
save(site_effort, file = "site_connectivity/output/site-monitoring-effort.RData")
