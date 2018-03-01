# Plotting transitions between sites 
# Data from Alison Kock
# Ian Durbach 1 Mar 2018

library(tidyverse)
library(ggmap)
library(gridExtra)

source("site_connectivity/tidy-make-site-edgelist.R")

## plotting scales for all plots

# colour scale not distance standardised
cscale = scale_colour_gradientn(colors=terrain.colors(10)[10:1], 
                                breaks = c(0.15,1.5,15,150), 
                                labels = c("0.15","1.5","15","150"),
                                limits = c(0.06,200),
                                trans="log10",
                                name = "Transitions (n/mth)")

# colour scale with distance standardised
cscale_dstd = scale_colour_gradientn(colors=terrain.colors(10)[10:1], 
                                     breaks = c(2.5,25,250),
                                     labels = c("2.5","25","250"),
                                     limits = c(0.8,400),
                                     trans="log10",
                                     name = "Transitions (km/mth)")

# size scale for number of visits
sscale = scale_size_area(breaks = c(10,100,300,500), 
                         limits = c(0.3,650),
                         name = "Total visits (n/mth)")

sscale_dstd = scale_size_area(breaks = c(10,100,300,500),
                              limits = c(0.3,650),
                              name = "Total visits (n/mth)")


## adding text to the map

site_text = receiver.location[c(2,5,8,12,14),]
site_text$sitename = c("Fish Hoek","Koeel Bay","Muizenberg","Cape Point","Strandfontein")
site_text$Latitude = site_text$Latitude + c(0,-0.01,0.02,-0.01,0.025)
site_text$Longitude = site_text$Longitude + c(-0.04,0,0,0,0)

## plots

# spring/summer, transitions = counts / month 
qmplot(Longitude, Latitude, data = receiver.location, geom = "blank", maptype = "toner-hybrid") +
  geom_segment(data = receiver_transitions  %>% filter(aut_wint == FALSE), 
               aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, 
                                   colour = stdcount, size = stdcount),
               alpha = 0.7) + theme_bw(base_size = 20) +
  geom_point(data = total_visits %>% filter(aut_wint == FALSE),
             aes(x = Longitude, y = Latitude, size = stdvisits),
             colour = I("black"), shape = 1) +
  geom_text(data = site_text, 
            aes(x = Longitude, y = Latitude, label = sitename), size=3.5) +
  cscale + 
  sscale + 
  guides(size = guide_legend(override.aes = list(linetype = 0), order = 1),
                           colour = guide_colorbar(barwidth = 15, vjust = 0, order = 0)) +
  theme(legend.position = "bottom", legend.box = "vertical", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 18.41, y = -34.38, label = "(a)~italic(Spring/Summer)", parse = TRUE)

# spring/summer, transitions = counts * km / month 
qmplot(Longitude, Latitude, data = receiver.location, geom = "blank", maptype = "toner-hybrid") +
  geom_segment(data = receiver_transitions  %>% filter(aut_wint == FALSE), 
               aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, 
                   colour = stdcount_km, size = stdcount_km / 6),
               alpha = 0.7) + theme_bw(base_size = 20) +
  geom_point(data = total_visits %>% filter(aut_wint == FALSE),
             aes(x = Longitude, y = Latitude, size = stdvisits),
             colour = I("black"), shape = 1) +
  geom_text(data = site_text, 
            aes(x = Longitude, y = Latitude, label = sitename), size = 3.5) +
  cscale_dstd + 
  sscale_dstd + 
  guides(size = guide_legend(override.aes = list(linetype = 0), order = 1),
         colour = guide_colorbar(barwidth = 15, vjust = 0, order = 0)) +
  theme(legend.position = "bottom", legend.box = "vertical", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 18.41, y = -34.38, label = "(a)~italic(Spring/Summer)", parse = TRUE)

# autumn/winter, transitions = counts / month  
qmplot(Longitude, Latitude, data = receiver.location, geom = "blank", maptype = "toner-hybrid") +
  geom_segment(data = receiver_transitions  %>% filter(aut_wint == TRUE), 
               aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, 
                   colour = stdcount, size = stdcount),
               alpha = 0.7) + theme_bw(base_size = 20) +
  geom_point(data = total_visits %>% filter(aut_wint == TRUE),
             aes(x = Longitude, y = Latitude, size = stdvisits),
             colour = I("black"), shape = 1) +
  geom_text(data = site_text, 
            aes(x = Longitude, y = Latitude, label = sitename), size = 3.5) +
  cscale + 
  sscale + 
  guides(size = guide_legend(override.aes = list(linetype = 0), order = 1),
         colour = guide_colorbar(barwidth = 15, vjust = 0, order = 0)) +
  theme(legend.position = "bottom", legend.box = "vertical", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 18.41, y = -34.38, label = "(a)~italic(Autumn/Winter)", parse = TRUE)

# autumn/winter, transitions = counts * km / month 
qmplot(Longitude, Latitude, data = receiver.location, geom = "blank", maptype = "toner-hybrid") +
  geom_segment(data = receiver_transitions  %>% filter(aut_wint == TRUE), 
               aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, 
                   colour = stdcount_km, size = stdcount_km / 6),
               alpha = 0.7) + theme_bw(base_size = 20) +
  geom_point(data = total_visits %>% filter(aut_wint == TRUE),
             aes(x = Longitude, y = Latitude, size = stdvisits),
             colour = I("black"), shape = 1) +
  geom_text(data = site_text, 
            aes(x = Longitude, y = Latitude, label = sitename), size = 3.5) +
  cscale_dstd + 
  sscale_dstd + 
  guides(size = guide_legend(override.aes = list(linetype = 0), order = 1),
         colour = guide_colorbar(barwidth = 15, vjust = 0, order = 0)) +
  theme(legend.position = "bottom", legend.box = "vertical", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 18.41, y = -34.38, label = "(a)~italic(Autumn/Winter)", parse = TRUE)

# save the plots
ggsave("site_connectivity/output/fig_network.png", 
       arrangeGrob(fig_ss, fig_aw,ncol=1), width = 10, height = 12, dpi = 400)

ggsave("site_connectivity/output/fig_network_dstd.png", 
       arrangeGrob(fig_ss_d, fig_aw_d,ncol=1), width = 10, height = 12, dpi = 400)
