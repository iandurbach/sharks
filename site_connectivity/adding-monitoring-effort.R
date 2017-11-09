

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

# study period
study_start <- dmy(01052005, tz = "UTC")
study_end <- dmy(31122007, tz = "UTC")
study_period <- seq(from = study_start, to = study_end, by = "day")

# make data frame with all dates in study period for each site
site_dates <- expand.grid(site_info$Site.Code, study_period)
colnames(site_dates) <- c("Site.Code", "date")

# add season to each date
site_dates <- site_dates %>%
  mutate(aut_wint = (month(date) > 2 & month(date) < 9))

# add dates of first and last deployment for each receiver
site_dates <- site_dates %>% arrange(Site.Code) %>%
  left_join(site_info %>% select(Site.Code, First.Date, Last.Date),
            by = "Site.Code")

# indicator of whether receiver was active on each day
site_dates <- site_dates %>% 
  mutate(active = (date >= First.Date) & (date <= Last.Date))

# calculate number of days each receiver was active for in the study period
site_effort <- 
  site_dates %>% 
  group_by(Site.Code, aut_wint) %>% 
  summarize(effort = sum(active)) %>%
  ungroup()

# SIN = SIN_1 + SIN_2, manually adjust
site_effort <- site_effort %>% 
  mutate(Site.Code = recode(Site.Code, 'SIN_1' = "SIN", 'SIN_2' = 'SIN'))

# need to redo calculation  to get the correct values for SIN
site_effort <- 
  site_effort %>% 
  group_by(Site.Code, aut_wint) %>% 
  summarize(effort = sum(effort)) %>%
  ungroup()

# save output
save(site_effort, file = "site_connectivity/output/monitoring-effort.RData")
