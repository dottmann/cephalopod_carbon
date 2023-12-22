library(ramlegacy)
library(tidyverse)

# downloads current latest version 4.44 
download_ramlegacy()

# find it 
ram_dir(vers = "4.44")

# Set wd:
setwd("C:/Users/danot/My Drive/_postdoc/projects/cephalopod_deadfalls")

# and add folder to specific location
ram <- readRDS(paste(getwd(),'/ramlegacy/4.44/RLSADB v4.44/DB Files With Assessment Data/v4.44.rds', sep=""))

# select all stocks
stock <- ram$stock

# get their average biomass between 2005 and 2009
ramdata   <- ram$timeseries_values_views
ramunits  <- ram$timeseries_units_views
rammethod <- ram$assessment
stock$SSB <- NA ;stock$SSB_units <- NA 
stock$TB  <- NA ;stock$TB_units <- NA 
stock$TC  <- NA ;stock$TC_units <- NA 
stock$TL  <- NA ;stock$TL_units <- NA 
stock$assessmethod <- NA


ramunits %>%
  filter(grepl('squid', stocklong), !is.na(TC))

# Cape Horse Squid:
# Total biomass:
p <- ramdata %>%
  filter(grepl('Cape Hope squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TB)) +
  geom_point(aes(x = year, y = TB)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/cape_hope_squid_ram.png", p, height = 20 , width = 30, units = "mm", scale = 4)

# Total catch:
p <- ramdata %>%
  filter(grepl('Cape Hope squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/cape_hope_squid_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)


# Japanese flying squid:
p <- ramdata %>%
  filter(grepl('Japanese flying squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TB)) +
  geom_point(aes(x = year, y = TB)) +
  facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/japanese flying_squid_ram.png", p, height = 20 , width = 50, units = "mm", scale = 4)

p <- ramdata %>%
  filter(grepl('Japanese flying squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/japanese flying_squid_ram_total_catch.png", p, height = 20 , width = 50, units = "mm", scale = 4)
             

# Longfin inshore:
p <- ramdata %>%
  filter(grepl('Longfin inshore squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TB)) +
  geom_point(aes(x = year, y = TB)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/longfin_inshore_squid_ram.png", p, height = 20 , width = 30, units = "mm", scale = 4)


p <- ramdata %>%
  filter(grepl('Longfin inshore squid', stocklong), !is.na(TB), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/longfin_inshore_squid_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)


# Spear squid Pacific Ocean:
p <- ramdata %>%
  filter(grepl('Spear squid Tsushima warm current', stocklong), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/spear_squid_tsushima_warm_current_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)

p <- ramdata %>%
  filter(grepl('Spear squid', stocklong), year > 1990) %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/spear_squid_Pacific_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)


# Octopus vulgaris:
p <- ramdata %>%
  filter(grepl('Octopus', stocklong), year > 1990)  %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/octopus_vulgaris_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)


# Enteroctopus dofleini:
p <- ramdata %>%
  filter(grepl('octopus Gulf', stocklong), year > 1990)  %>%
  ggplot() +
  geom_line(aes(x = year, y = TB)) +
  geom_point(aes(x = year, y = TB)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/eneroctopus_dofleini_gulf_alaska_ram.png", p, height = 20 , width = 30, units = "mm", scale = 4)


p <- ramdata %>%
  filter(grepl('octopus Gulf', stocklong), year > 1990)  %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/eneroctopus_dofleini_gulf_alaska_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)


p <- ramdata %>%
  filter(grepl('octopus Bering', stocklong), year > 1990)  %>%
  ggplot() +
  geom_line(aes(x = year, y = TC)) +
  geom_point(aes(x = year, y = TC)) +
  # facet_wrap("stocklong", scales = "free") +
  theme_bw()

ggsave("plots/eneroctopus_dofleini_Bering_ram_total_catch.png", p, height = 20 , width = 30, units = "mm", scale = 4)




