# Making an edgelist the tidy way 
# Data from Alison Kock
# Ian Durbach 27 Feb 2018

Sys.setenv(TZ='Africa/Johannesburg')

library(tidyverse)
library(foreign)
library(lubridate)
library(geosphere)

options(dplyr.print_max = 1e9)

# parameters for user to specify
newtrack_gap <- 60 # a new track is started if the last detection was more than newtrack_gap mins ago

# read in sighting histories in Stata format
x <- read.dta("data/data_in_use.dta")

# read in site locations. A site is a cluster of receivers
load("data/cluster_locations.RData")

# include monitoring effort for spring summer (produced by site-monitoring-effort.R)
load("site_connectivity/output/site-monitoring-effort.RData")



# compute distances between all pairs of sites
sites <- receiver.location$sitecode
intersite_dst <- data.frame(startcode = rep(sites, each = length(sites)),
                        endcode = rep(sites, times = length(sites))) %>%
  left_join(receiver.location, by = c("startcode" = "sitecode")) %>%
  left_join(receiver.location, by = c("endcode" = "sitecode"))

dist_sites <- c()
for(i in 1:nrow(intersite_dst)){
  dist_sites[i] <- distm(x = c(intersite_dst$Longitude.x[i], intersite_dst$Latitude.x[i]), 
                        y = c(intersite_dst$Longitude.y[i], intersite_dst$Latitude.y[i]),
                        fun = distGeo) / 1000
}

intersite_dst <- intersite_dst %>% 
  mutate(dist = dist_sites) %>%
  select(-Longitude.x, -Longitude.y, -Latitude.x, -Latitude.y)

rm(sites, dist_sites)



## Data cleaning

# shark_id is a factor
x$shark_id <- factor(x$shark_id)

# set variables, date
x <- x %>% 
  select(shark_id, datetime, sitecode) %>%    # choose a few variables we need
  mutate(datetime = dmy_hms(x$datetime, tz = "UTC"))    # specifies date format

# add season to each date
x <- x %>%
  mutate(aut_wint = (month(datetime) > 2 & month(datetime) < 9))

# for testing, choose a small subset (remember to remove!)
#x <- x %>% filter(year(datetime) == 2007 & month(datetime) <= 1)

# remove 439 observations at KBO between 10/2006 and 11/2007 (Alison deployment typo)
x <- x %>% 
  filter(!(sitecode=="KBO" & ((year(datetime)==2006 & month(datetime)>=10)|
                                (year(datetime)==2007 & month(datetime)<=11))))

# remove 2004 and 2008, and prior to May 2005 (study period for ME paper)
x <- x %>% 
  filter(year(datetime) > 2004 & year(datetime) < 2008) %>%
  filter(!(year(datetime) == 2005 & month(datetime) < 5))

# cluster receivers into "sites"
x$sitecode <- x$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
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


# add receiver lat/long
x <- left_join(x, receiver.location, by = "sitecode")

# insert the lat/long of start/end receivers
x <- x %>%
  group_by(shark_id) %>%
  mutate(startcode = lag(sitecode), # endcode != startcode => transition, else no
         endcode = sitecode,
         Longitude.x = lag(Longitude), # long/lat for start receiver
         Latitude.x = lag(Latitude),
         Longitude.y = Longitude, # long/lat for end receiver
         Latitude.y = Latitude) %>%
  ungroup() %>% 
  select(-Longitude,-Latitude) # don't need any more

# time between adjacent detections
x <- x %>%
  arrange(shark_id, datetime) %>%
  group_by(shark_id) %>%
  mutate(time_diff = difftime(datetime, lag(datetime), units = "min")) %>%
  ungroup()

# add in distances between starting site and ending site (startcode/endcode)
x <- x %>% left_join(intersite_dst, by = c("startcode", "endcode"))

# I call a sequence of detections that form a single group a "track" (like a journey) 
# create indicator variables for each track for each shark
# a new track is started if the time between adjacent detections > T
# T = 1 hour for any sites within <2km of each other, otherwise T = d/2 where d is the 
# distance between sites
x <- x %>%
  arrange(shark_id, datetime) %>%
  group_by(shark_id) %>%
  mutate(newtrack = if_else(time_diff > (60 * pmax(2, dist) / 2), 1, 0)) %>%
  replace_na(list(newtrack = 1)) %>%   # first sighting always a new track
  mutate(track_id = cumsum(newtrack)) %>%   # creates indicator variable
  add_count(track_id) %>% 
  ungroup()

# remove any instance of a single detection at a site, this can be either where
# (a) a new site is detected only once, or (b) where the same site is detected, 
# but the track is a single observation
x <- x %>% 
  filter(sitecode == lag(sitecode) | sitecode == lead(sitecode)) %>%  # removes (a)
  filter(n > 1)   # removes (b)

# count total number of transitions between each pair of sites and sampling effort
receiver_transitions <- x %>% group_by(startcode, endcode, aut_wint) %>%
  summarize(count = n_distinct(shark_id, track_id)) %>%
  ungroup()

# for an undirected edgelist (direction not important), add up A->B and B->A transitions
receiver_transitions <- receiver_transitions %>% 
  mutate(site1 = pmin(startcode,endcode), 
         site2 = pmax(startcode, endcode)) %>%
  select(-startcode, -endcode) %>%
  group_by(site1, site2, aut_wint) %>%
  summarize(count = sum(count))
  
# the last step drops variables, here add back site lat/long, and distances between site1 and site2 
# (sure there's a way to do this in the previous step, but this is easier)
receiver_transitions <- receiver_transitions %>% 
  left_join(receiver.location, by = c("site1" = "sitecode")) %>%
  left_join(receiver.location, by = c("site2" = "sitecode")) %>% 
  left_join(intersite_dst, by = c("site1" = "startcode", "site2" = "endcode"))

# append monitoring effort 
receiver_transitions <- receiver_transitions %>% 
  left_join(site_effort, by = c("site1", "site2", "aut_wint"))

# standardise counts to transitions per 30 days
receiver_transitions <- receiver_transitions %>% 
  group_by(site1, site2, aut_wint) %>%
  mutate(stdcount = 30 * count / effort,
         stdcount_km = stdcount * dist) %>%
  ungroup()

# remove NOTFB
receiver_transitions <- receiver_transitions %>%
  filter(site1 != "NOTFB", site2 != "NOTFB") %>%
  filter(!is.na(site1), !is.na(site2))

# calculate total visits / 30 days at each site, including self-transitions
total_visits <- receiver_transitions %>% 
  group_by(site1, aut_wint) %>% 
  summarize(stdvisits = sum(stdcount)) %>%
  full_join(receiver_transitions %>% 
              group_by(site2, aut_wint) %>% 
              summarize(stdvisits = sum(stdcount)),
            by = c("site1" = "site2", "aut_wint")) %>%
  replace_na(list(stdvisits.x = 0, stdvisits.y = 0)) %>%
  mutate(stdvisits = stdvisits.x + stdvisits.y) %>%
  select(-stdvisits.x, -stdvisits.y) %>% 
  ungroup()

# total visits / 30 days at each site, excluding self-transitions
total_visits_no_self <- receiver_transitions %>% 
  filter(site1 != site2) %>%
  group_by(site1, aut_wint) %>% 
  summarize(stdvisits = sum(stdcount)) %>%
  full_join(receiver_transitions %>% 
              group_by(site2, aut_wint) %>% 
              summarize(stdvisits = sum(stdcount)),
            by = c("site1" = "site2", "aut_wint")) %>%
  replace_na(list(stdvisits.x = 0, stdvisits.y = 0)) %>%
  mutate(stdvisits_no_self = stdvisits.x + stdvisits.y) %>%
  select(-stdvisits.x, -stdvisits.y) %>%
  ungroup()

# merge and add lat/long
total_visits <- total_visits %>% 
  left_join(total_visits_no_self) %>% 
  left_join(receiver.location, by = c("site1" = "sitecode"))

rm(total_visits_no_self)
