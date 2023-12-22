
## Cephalopod carbon
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: September 2023
## Last update:  December 2023
##
## ---------------------------
##
## Readme:
##
## This script estimates carbon flux of cephalopods in the paper: "Increasing high-sea fisheries impact long-term sequestration of carbon by cephalopods"
## It uses biomass and catchestmiates of 17 major stocks and of global biomass estimates of cephalopods.
## It further estimates respiration and production of fecal pellets by cephalopods, and sequestered carbon in the ocean by the 17 major stocks.
##
## ---------------------------

##################################################################
# Load libraries:
library(tidyverse)
library(ggthemes)
library(patchwork)
library(jpeg)


# Clear environment:
rm(list = ls())

# Load data:
data <- read.delim("data/summary_biomass_catch.txt", sep = '\t', header = T, stringsAsFactors = F)

#------------------------------------------------

# Set some values:
somatic_ratio <- 0.9 # Wet weight of the carcass - gonads (Wells & Clarke 1996)
wet_dry <- 0.225     # Wet weight to dry weight (Hoving et al 2017)
dry_carbon <- .44    # Dry weight to carbon weight (Hoving et al 2017)


# Edit data:
df <- data %>%
  dplyr::select(-notes) %>%
  rename(data_area = data.area,
         biomass = stock.biomass..T., # Tonnes
         deadfall_location = deadfall.location,
         m = median.mass.g) # Grams (Half of the asymptotic weight of each species. Median was taken for groups of species)


# Calculate mean biomass to landings ratio:
biomass_to_landings_ratio <-  df %>%
  filter(!is.na(biomass), taxa != "Cephalopoda") %>%
  mutate(biomass_to_landings = landings / biomass) %>%
  summarise(weighted_ratio = round(sum(biomass_to_landings * biomass) / sum(biomass), 2)) %>% # weighted mean
  as.numeric()

# Estimate biomass in stocks where it is missing:
df <- df %>%
  mutate(biomass = case_when(is.na(biomass) ~ (landings * (1/biomass_to_landings_ratio)), # This is a gross estimation
                             T ~ as.numeric(biomass)))

# Arrange table to have landings and biomass in a single t column:
df1 <- df %>%
  dplyr::select(-biomass) %>%
  mutate(landings = na_if(landings, 0)) %>%
  rename(t = landings) %>% # Tonnes
  mutate(type = "landings")

df2 <- df %>%
  dplyr::select(-landings) %>%
  rename(t = biomass) %>% # Tonnes
  mutate(type = "biomass")

df <- rbind(df1, df2)


# Physiological parameters:
# m <- 700 #  mass of cephalopod [g] (Ottmann et al in 2024) taking 1/2 of logarithmic biomass-weighted mean asymptotic size of all species ((exp(sum(log(mean_a_size_weighted_wt_cpue_cephalopoda) * mean_wt_cpue_cephalopoda, na.rm = T) / sum(mean_wt_cpue_cephalopoda, na.rm = T))) / 2)
epsilon <- 0.7 # Assimilation efficiency
f0 <- 0.6 # Average feeding
fc <- 0.2 # Critical feeding
h <- 100 # Coefficient of maximum consumption
h_low <- 22.3 # Sensitivity: 22.3 =for fish


df <- df %>%
  mutate(residence_time = case_when(deadfall_location == "mixed" ~ 100, # Years
                                    deadfall_location == "pelagic" ~ 1000,
                                    deadfall_location == "slope" ~ 100,
                                    T ~ 10),
         
         lifespan = case_when(lifespan == "mixed" ~ 2, # Years
                              T ~ as.numeric(lifespan)),
         
         # m = m/5, # Do some sensitivity to th emas (instead of 1/2 of asymptotic wight we do 1/10)
         
         carbon_deadfall = t / lifespan * somatic_ratio * wet_dry * dry_carbon, # Tonnes
         sequestration = carbon_deadfall * residence_time, # Tonnes year
         
         respiration_ind = epsilon * h * fc * m^(-1/3) * m * wet_dry * dry_carbon,  # Converted to carbon [Tonnes]
         feces_ind = (1 - epsilon) * h * (f0 - fc) * m^(2/3) * wet_dry * dry_carbon,  # Converted to carbon [Tonnes]
         
         respiration = (respiration_ind / 1e6) * (t * 1e6) / m, # Tonnes
         feces = (feces_ind / 1e6) * (t * 1e6) / m, # Tonnes
         
         respiration = case_when(type == "landings" ~ 0,
                                 T ~ respiration),
         feces = case_when(type == "landings" ~ 0,
                                   T ~ feces),  
         
         #----------------------------------------------------------
         
         respiration_ind_low = epsilon * h_low * fc * m^(-1/3) * wet_dry * dry_carbon * m,  # Converted to carbon [Tonnes]
         feces_ind_low = (1 - epsilon) * h_low * (f0 - fc) * m^(2/3) * wet_dry * dry_carbon,  # Converted to carbon [Tonnes]
         
         respiration_low = (respiration_ind_low / 1e6) * (t * 1e6) / m, # Tonnes
         feces_low = (feces_ind_low  / 1e6) * (t * 1e6) / m, # Tonnes
         
         respiration_low = case_when(type == "landings" ~ 0,
                                     T ~ respiration_low),
         feces_low = case_when(type == "landings" ~ 0,
                                       T ~ feces_low))


# Outfile as txt:
write.table(df, "data/flux_table.txt", append = F, quote = F, sep = "\t", row.names = F, col.names = T)



######################################################################################
# Time series analysis:
# Load data:
species_id <- read.delim("FAO_data/CL_FI_SPECIES_GROUPS.txt", sep = '\t', header = T, stringsAsFactors = F)
landings <- read.delim("FAO_data/Capture_Quantity.txt", sep = '\t', header = T, stringsAsFactors = F)

# Set an "average" lifespan for cephalopods:
lifespan <- 1.5 # years

# Edit cephalopod data:
species_id_ceph <- species_id %>%
  filter(Major_Group == "MOLLUSCA",  ISSCAAP_Group_En == "Squids, cuttlefishes, octopuses") %>% 
  pull(X3A_Code)

landings_ceph <- landings %>%
  filter(SPECIES.ALPHA_3_CODE %in% species_id_ceph) %>%
  rename(species = SPECIES.ALPHA_3_CODE,
         year = PERIOD) %>%
  group_by(year) %>%
  summarise(landings = sum(VALUE)) %>%
  mutate(taxa = "Cephalopods",
         carbon_deadfall = landings * somatic_ratio * wet_dry * dry_carbon)

# Edit fish data:
species_id_fish <- species_id %>%
  filter(Major_Group == "PISCES") %>% 
  pull(X3A_Code)

landings_fish <- landings %>%
  filter(SPECIES.ALPHA_3_CODE %in% species_id_fish) %>%
  rename(species = SPECIES.ALPHA_3_CODE,
         year = PERIOD) %>%
  group_by(year) %>%
  summarise(landings = sum(VALUE)) %>%
  mutate(taxa = "Fish",
         carbon_deadfall = seq(from = .1e6, to = 1.1e6, length.out = n()))


# Put them together:
landings <- rbind(landings_fish, landings_ceph)

p1 <- ggplot(data = landings_ceph) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_area(aes(x = year, y = carbon_deadfall / 1000), color = "black", alpha = .2) +
  scale_color_manual(values = my_colors4) +
  ylab(label = "Catch (Thousand t C / yr)") +
  theme_base() +
  labs(linetype = NULL) +
  theme(legend.position = "bottom")

p1


######################################################################################
# Add schematic as a panel:
p2 <- readJPEG("/plots/panel_b.jpg", native = TRUE)

p <- p3 + p4 + p1 + p2 + plot_layout(ncol = 2) & # , heights = unit(c(10, 50), c('mm', 'null'))
  theme(plot.background = element_blank()) &
  plot_annotation(tag_levels = "a")

p

ggsave("plots/Figure_1.png", p, height = 85 , width = 90, units = "mm", scale = 3)


######################################################################################
# Catch and biomass estimates.

my_colors1 <- c("bisque3", "chocolate")
my_colors2 <- c("grey50", "orange", "red")
my_colors3 <- c("black", "orange", "red")
my_colors4 <- c("black", "red")

my_labels <- as.character(17:1)


# Merge by taxa and type:
dftemp <- df %>%
  filter(taxa != "Cephalopoda") %>%
  group_by(taxa, type) %>%
  summarise(t = sum(t),
            carbon_deadfall = sum(carbon_deadfall), # Tonnes
            sequestration = sum(sequestration), # Tonnes
            feces = sum(feces), # Tonnes
            respiration = sum(respiration), # Tonnes
            feces_low = sum(feces_low), # Tonnes
            respiration_low = sum(respiration_low)) # Tonnes

# Set order by carbon deadfall:
taxa_order <- dftemp %>% 
  filter(type == "biomass") %>%
  arrange(carbon_deadfall) %>%
  dplyr::select(taxa)

taxa_order <- taxa_order$taxa

dftemp$taxa <- factor(dftemp$taxa,
                       levels = taxa_order)

p3 <- ggplot() +
  geom_bar(data = subset(dftemp,  taxa != "Cephalopoda"), aes(x = taxa, y = t / 1e6, fill = type), stat = 'identity', position = position_dodge(), width = 0.7) + # 
  scale_fill_manual(values = my_colors1, labels = c("Biomass", "Landings")) +
  scale_x_discrete(labels = my_labels) +
  labs(fill = NULL) +
  ylab("Million tonnes (/ year)") +
  xlab("Stock") +
  coord_flip() +
  theme_base() +
  theme(legend.position = c(.95,  .05),
        legend.justification = c("right", "bottom"))

p3


######################################################################################
# Plot respiration, fecal pellets and deadfall:

# Combined fluxes:
df_deadfall <- dftemp %>%
  dplyr::select(taxa, type, carbon_deadfall) %>%
  rename(flux = carbon_deadfall) %>%
  mutate(type_flux = "deadfall")

df_respiration <- dftemp %>%
  dplyr::select(taxa, type, respiration) %>%
  rename(flux = respiration) %>%
  mutate(type_flux = "respiration")

df_fecal <- dftemp %>%
  dplyr::select(taxa, type, feces) %>%
  rename(flux = feces) %>%
  mutate(type_flux = "fecal pellets")

dftemp2 <- rbind(df_deadfall, df_respiration, df_fecal)


# For sensitivity:
df_deadfall_low <- dftemp %>%
  dplyr::select(taxa, type, carbon_deadfall) %>%
  rename(flux = carbon_deadfall) %>%
  mutate(flux = NA,
    type_flux = "deadfall")

df_respiration_low <- dftemp %>%
  dplyr::select(taxa, type, respiration_low) %>%
  rename(flux = respiration_low) %>%
  mutate(type_flux = "respiration")

df_fecal_low <- dftemp %>%
  dplyr::select(taxa, type, feces_low) %>%
  rename(flux = feces_low) %>%
  mutate(type_flux = "fecal pellets")

dftemp2_low <- rbind(df_deadfall_low, df_respiration_low, df_fecal_low)


# Set order:
taxa_order <- dftemp2 %>% 
  filter(type == "biomass", type_flux == "deadfall") %>%
  arrange(flux) %>%
  dplyr::select(taxa)

taxa_order <- taxa_order$taxa

dftemp2$taxa <- factor(dftemp2$taxa,
                       levels = taxa_order)


p4 <- ggplot() +
  # geom_hline(yintercept = 1e6, alpha = .5, linetype = "dotted") +
  geom_bar(data = subset(dftemp2, type == "biomass" & taxa != "Cephalopoda"), aes(x = taxa, y = flux / 1000, fill = type_flux), stat = 'identity', position = position_dodge(), width = 0.7, alpha = .5) + # fill = "orange",
  geom_bar(data = subset(dftemp2, type == "landings" & taxa != "Cephalopoda"), aes(x = taxa, y = flux / 1000, fill = type_flux), stat = 'identity', position = position_dodge(),  width = 0.7) + # fill = "orange",
  geom_errorbar(data = subset(dftemp2_low, type == "biomass" & taxa != "Cephalopoda"), aes(x = taxa, ymin = flux / 1000, ymax = flux / 1000, color = type_flux), 
                position = position_dodge(width = .7), lwd = .3, width = .7, show.legend = FALSE) +
  scale_x_discrete(labels = my_labels) +
  scale_fill_manual(values = my_colors2, labels = c("Deadfall", "Feces", "Respiration")) +
  scale_color_manual(values = my_colors2, labels = c("Deadfall", "Feces", "Respiration")) +
  labs(fill = NULL) +
  ylab("Thousand tonnes C / year") +
  coord_flip() +
  theme_base() +
  theme(axis.title.y = element_blank(),
        legend.position = c(.95,  .05),
        legend.justification = c("right", "bottom"))

p4


######################################################################################
# Combine panels:

p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2) & # , heights = unit(c(10, 50), c('mm', 'null'))
  theme(plot.background = element_blank()) &
  plot_annotation(tag_levels = "a")

p

ggsave("plots/Figure_1.png", p, height = 85 , width = 90, units = "mm", scale = 3)


######################################################################################

# Global cephalopod deadfall based on biomasss estimates from Rodhus & Nigmatullin 1996 (Million tonnes)

lifespan <- 1.5 # years

# Lower range:
t <- 193
round(t / lifespan * somatic_ratio * wet_dry * dry_carbon, 1)

# Upper range:
t <- 375
round(t / lifespan * somatic_ratio * wet_dry * dry_carbon, 1)

t <- (193 + 375) /2
round(t / lifespan * somatic_ratio * wet_dry * dry_carbon, 1)





# Physiological parameters:
m <- 700 #  mass of cephalopod [g] (Ottmann et al in review) taking 1/2 of median asymptotic size of all species
# m <- m/5 # Try making it 1/10th of the emdian asymptotic size
epsilon <- 0.7 # Assimilation efficiency
f0 <- 0.6 # Average feeding
fc <- 0.2 # Critical feeding
h <- 100 # Coefficient of maximum consumption


respiration_individual <- epsilon * h * fc * m^(-1/3) * m
# waste_individual <- (1 - epsilon) * h * (f0 - fc) * m^(2/3)
waste_individual <- (1 - epsilon) * h * (f0 - fc) * m^(-1/3) * m  

respiration <- (respiration_individual / 1e6) / m * (t * 1e6) * wet_dry * dry_carbon
waste <- (waste_individual / 1e6) / m * (t * 1e6) * wet_dry * dry_carbon


# Fish
t <- 7000 # milion tones
h <- 22.3

respiration_individual <- epsilon * h * fc * m^(-1/3) * m
# waste_individual <- (1 - epsilon) * h * (f0 - fc) * m^(2/3)
waste_individual <- (1 - epsilon) * h * (f0 - fc) * m^(-1/3) * m  

respiration <- (respiration_individual / 1e6) / m * (t * 1e6) * wet_dry * dry_carbon
waste <- (waste_individual / 1e6) / m * (t * 1e6) * wet_dry * dry_carbon

respiration + waste
(respiration + waste) / t


