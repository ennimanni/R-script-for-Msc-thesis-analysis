#The script was made by Enni Manninen
#ChatGPT (OpenAI) was used to assist in generating R scripts. All outputs were
#reviewed, verified, and adapted by the author.
#Last edited 8.5.2026
#This script is for analyzing the data used in the Master's thesis.It includes 
#analysis of vegetation, gas fluxes and environmental variables. More 
#information about the datasets used can be found from the thesis and the from 
#author of the thesis and the script.
#-------------------------------------------------------------------------------
#1. Vegetation data analysis:
#- Spring shoulder season vegetation data gathered by Enni Manninen
#  - hits/pin across plots
#- ICOS vegetation data
#  - AGB across plots and years
#  - GAI across plots and years
#- Biomass samples of vascular plants and bryophytes gathered by López-Blanco
#   - VP to bryophyte ratios
#-------------------------------------------------------------------------------
#2. Carbon balance:
# -2.1 Long term AC data
# -2.2 Spring shoulder season Eddy data
#-------------------------------------------------------------------------------
#3. Environmental variables and gas fluxes: long-term monitoring data
#- Daily means
#- Annual means
#- PCA analysis 
#- GAMM models
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#1. Vegetation data-analysis
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Download libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)
library(patchwork)
library(scales) 
#-------------------------------------------------------------------------------
#Set working directory
setwd("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/R")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Kobbefjord pinpoint spring shoulder season analysis (hits per pin)
#-------------------------------------------------------------------------------
#Hits per pin
#------------
#Import dataset
kob_pin <- read_excel("Pinpoint_combined.xlsx", sheet = "Kobbefjord_fresh")
str(kob_pin)

#Change the species name from unidentified poales to Cyperaceae
kob_pin$Species_short <- as.character(kob_pin$Species_short)
kob_pin$Species_short[kob_pin$Species_short == "Upoa"] <- "Cype"
kob_pin$Species[kob_pin$Species == "Unidentified poales"] <- "Cyperaceae"

#Arrange systematicly
kob_pin <- kob_pin %>%
  mutate(Species_short = factor(Species_short, levels = c(
    "Peat",
    "Litter",
    "Sstr",
    "Slin",
    "Spro",
    "Lwic",
    "Owha",
    "Delo",
    "Ssp.",
    "Oelo",
    "Jung",
    "Ginf",
    "Bviv",
    "Cype",
    "Tces",
    "Eang",
    "Crar",
    "Voxy",
    "Sher",
    "Sarc"
  )))

group_colors <- c(
  "Shrubs"    = "#225E2B",  # dark green
  "Herbs"     = "#2E7D32",  # medium green
  "Sedges"    = "#4CAF50",  # light green
  "Mosses"    = "#2EBBAA",  # teal / sea-green
  "Liverwort" = "#117C8C",  # darker teal / blue-green
  "Litter"    = "#6B3E00",  # brown
  "Peat"      = "#3F2A00" )  # dark brown

#-----------------
#Functional groups
#-----------------
#Filter Peat and Litter out
kob_pin_func_group <- kob_pin %>%
  filter(Functional_group %in% c("Herbs", "Shrubs", "Sedges", "Mosses", "Liverwort"))%>%
  mutate(Functional_group = factor(Functional_group, levels = c(
    "Shrubs",
    "Sedges",
    "Herbs",
    "Liverwort",
    "Mosses")))

#Calculate mean and sd
summary_stats_func <- kob_pin_func_group %>%
  group_by(Functional_group) %>%
  summarise(
    median_hits_per_pin = median(Hits_per_pin, na.rm = TRUE),
    mean_hits_per_pin = mean(Hits_per_pin, na.rm = TRUE),
    sd_hits_per_pin = sd(Hits_per_pin, na.rm = TRUE),
    n_plots = n()
  ) %>%
  arrange(desc(mean_hits_per_pin))
print(summary_stats_func)
#Write the table to excel
write_xlsx(summary_stats_func, "summary_func_kob.xlsx")

#boxplot
ggplot(kob_pin_func_group, aes(x = reorder(Functional_group, Hits_per_pin, mean),
                               y = Hits_per_pin,
                               fill = factor(Functional_group, levels = c("Liverwort", "Mosses", "Sedges", "Herbs", "Shrubs" )))) +
  scale_fill_manual(values = group_colors)+
  geom_boxplot() +
  #geom_violin(alpha = 0.7)+
  geom_point(alpha = 0.5, shape = 23)+
  labs(title = NULL,
       x = NULL, y = "Hits/pin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

#-------------------------------------------
#Bryophyte species grouped in functional groups
#-------------------------------------------
#Filter bryophytes and species
moss_kob_pin <- kob_pin %>%
  filter(Functional_group %in% c("Mosses", "Liverwort", "Peat"))

summary_moss_spec<- moss_kob_pin %>%
  group_by(Species, Species_short, Functional_group) %>%
  summarise(
    mean_hits_per_pin = mean(Hits_per_pin, na.rm = TRUE),
    sd_hits_per_pin = sd(Hits_per_pin, na.rm = TRUE),
    n_plots = n()
  ) %>%
  arrange(desc(mean_hits_per_pin))

print(summary_moss_spec)

#boxplot
moss_pin <- ggplot(moss_kob_pin, aes(x = reorder(Species_short, Hits_per_pin, median), 
                                     y = Hits_per_pin,
                                     fill = factor(Functional_group, levels = c("Peat", "Litter","Liverwort", "Mosses", "Sedges", "Herbs", "Shrubs")))) +
  scale_fill_manual(name = NULL, values = group_colors)+
  geom_boxplot() +
  #geom_violin(alpha = 0.7) +
  geom_point(alpha = 0.5, shape = 23)+
  labs(title = "Bryophytes",
       x = NULL, y = "Hits/pin") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")+
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_number(accuracy = 0.01),  # shows 0.00, 0.25, 0.50, ...
    expand = c(0, 0)
  )
moss_pin
#--------------------------------------------------------------
#Vascular plant species and litter grouped in functional groups
#--------------------------------------------------------------
#Filter vascular plants and litter
vasc_kob_pin <- kob_pin %>%
  filter(Functional_group %in% c("Sedges", "Shrubs", "Herbs", "Litter"))%>%
  mutate(Species_short = factor(Species_short, levels = c(
           "Sarc",
           "Sher",
           "Voxy",
           "Crar",
           "Eang",
           "Tces",
           "Cype",
           "Bviv",
           "Litter")))

summary_vasc_spec<- vasc_kob_pin %>%
  group_by(Species, Species_short) %>%
  summarise(
    mean_hits_per_pin = mean(Hits_per_pin, na.rm = TRUE),
    sd_hits_per_pin = sd(Hits_per_pin, na.rm = TRUE),
    n_plots = n())

print(summary_vasc_spec)
str(vasc_kob_pin)

#boxplot
pin_vasc <- ggplot(vasc_kob_pin, aes(x = reorder(Species_short, Hits_per_pin, median), 
                                     y = Hits_per_pin,
                                     fill = factor(Functional_group, levels = c("Peat", "Litter","Liverwort", "Mosses", "Sedges", "Herbs", "Shrubs")))) +
  scale_fill_manual(name = NULL, values = group_colors)+
  geom_boxplot() +
  #geom_violin(alpha = 0.7)+
  geom_point(alpha = 0.5, shape = 23)+
  labs(title = "Vascular plants",
       x = NULL, y = "Hits/pin") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")+
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_number(accuracy = 0.01),  # shows 0.00, 0.25, 0.50, ...
    expand = c(0, 0)
  )
pin_vasc

pin_vasc+moss_pin
pin_vasc/moss_pin

#-------------------------------------------------------------------------------
#ICOS vegetation survey: AGB and GAI
#-------------------------------------------------------------------------------
#Combine the two sheets of different years
AGB_GAI_2022 <- read_excel("R_GL_NuF_GAI_AGB_20220810_20230810.xlsx", sheet = "20220810")
str(AGB_GAI_2022)
AGB_GAI_2023 <- read_excel("R_GL_NuF_GAI_AGB_20220810_20230810.xlsx", sheet = "20230810")

AGB_GAI_2022 <- AGB_GAI_2022 %>%
  mutate(Year = "2022")

AGB_GAI_2023 <- AGB_GAI_2023 %>%
  mutate(Year = "2023")

# Combine both into one
AGB_GAI_combined <- bind_rows(AGB_GAI_2022, AGB_GAI_2023)%>%
  mutate(Species_short = factor(Species_short, levels = c(
    "Bnan",
    "Sarc",
    "Sher",
    "Voxy",
    "Crar",
    "Eang",
    "Tces")))

str(AGB_GAI_combined)

AGB_GAI_combined <- AGB_GAI_combined %>%
  filter(AGB != 0, GAI != 0)

#Mean and sd
summary_AGB_GAI<- AGB_GAI_combined %>%
  group_by(Species_short) %>%
  summarise(
    median_AGB = median(AGB, na.rm = TRUE),
    mean_AGB = mean(AGB, na.rm = TRUE),
    sd_AGB = sd(AGB, na.rm = TRUE),
    median_GAI = median(GAI, na.rm = TRUE),
    mean_GAI = mean(GAI, na.rm = TRUE),
    sd_GAI = sd(GAI, na.rm = TRUE),
    n_plots = n())%>%
  arrange(desc(median_AGB))

print(summary_AGB_GAI)
write_xlsx(summary_AGB_GAI, "summary_AGB_GAI.xlsx")

#AGB boxplot
AGB_plot<- ggplot(AGB_GAI_combined, aes(x = reorder(Species_short, AGB, FUN = sum), 
                                        y = AGB,
                                        fill = factor(`Functional group`, levels = c("Sedges", "Shrubs")))) +
  scale_fill_manual(name = NULL, values = group_colors)+
  geom_boxplot() +
  #geom_violin(alpha = 0.7)+
  geom_point(alpha = 0.5, shape = 23)+
  labs(title = "(a)",
       x = NULL, y = "AGB") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
AGB_plot

#GAI boxplot
GAI_plot<- ggplot(AGB_GAI_combined, aes(x = reorder(Species_short, GAI, FUN = sum),
                                        y = GAI,
                                        fill = factor(`Functional group`, levels = c("Sedges", "Shrubs")))) +
  scale_fill_manual(name = NULL, values = group_colors)+
  geom_boxplot() +
  #geom_violin(alpha = 0.7)+
  geom_point(alpha = 0.5, shape = 23)+
  labs(title = "(b)",
       x = NULL, y = "GAI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
GAI_plot
combined_vasc <- AGB_plot + GAI_plot
combined_vasc

combined_vasc_v <-  AGB_plot/GAI_plot
combined_vasc_v

#-------------------------------------------------------------------------------
#VP to bryophyte ratio
#-------------------------------------------------------------------------------
#Kobbefjord C_N data by López-Blanco -> drymasses to vp to bryophyte ratio
#--------------------------------------------------------------------------
Drymasses_kob <- read_excel("VP_B_drymasses_Kob.xlsx", sheet = "Drymass")%>%
  filter(Part %in% c("leaf", "stem", "moss"))

str(Drymasses_kob)

#Summarise the drymasses per Sample number and type (vascular plant or bryophyte)
Drymasses_kob <- Drymasses_kob %>%
  mutate(Type = case_when(
    tolower(Part) %in% c("leaf", "stem") ~ "vascular plant",
    tolower(Part) == "moss" ~ "moss",
    TRUE ~ NA_character_
  ))

ratio_data <- Drymasses_kob %>%
  group_by(Sample_num, Type) %>%
  summarise(total_drymass = sum(Dry_mass, na.rm = TRUE))
str(ratio_data)

#Calculate the ratios
ratio_summary <- ratio_data %>%
  summarise(
    vascular_mass = total_drymass[Type == "vascular plant"],
    moss_mass = total_drymass[Type == "moss"],
    total_mass = moss_mass + vascular_mass,
    vascular_to_bryophyte_ratio = vascular_mass / moss_mass,
    vascular_to_total_ratio = vascular_mass / total_mass
  )
ratio_summary

# Add a row for averages
ratio_summary_with_avg <- ratio_summary %>%
  mutate(Sample_num = as.character(Sample_num)) %>%
  bind_rows(
    ratio_summary %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      mutate(Sample_num = "Average")
  )
ratio_summary_with_avg

#Write to excel
write_xlsx(ratio_summary_with_avg, "ratio_summary_kob.xlsx")

#Stacked bar plots
p1 <- ggplot(ratio_data, aes(x = reorder(Sample_num, total_drymass, FUN = sum), y = total_drymass, fill = factor(Type, levels = c("vascular plant", "moss")))) +
  geom_col( color = "black", size = 0.1) +
  labs(
    x = "Sample ID",
    y = "Total biomass (g)",
    title = "a)",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c("vascular plant" = "#4CAF50", "moss" = "#3FB8A7"),
    labels = c("Vascular plants", "Bryophytes")
  ) +
  theme_bw(base_size = 12)+
  theme(legend.position = "none")
p1

#relative biomass
ratio_summary <- ratio_data %>%
  group_by(Sample_num)%>%
  summarise(
    vascular_mass = total_drymass[Type == "vascular plant"],
    moss_mass = total_drymass[Type == "moss"],
    total_mass = moss_mass + vascular_mass,
    vascular_to_bryophyte_ratio = vascular_mass / moss_mass,
    vascular_to_total_ratio = vascular_mass / total_mass
  )
ratio_summary

fen_summary2 <- ratio_data %>%
  group_by(Sample_num, Type) %>%
  summarise(total_biomass = sum(total_drymass, na.rm = TRUE)) %>%
  mutate(Fen = "Kobbefjord",
         proportion = total_biomass / sum(total_biomass) * 100)

p2 <- ggplot(fen_summary2, aes(x = reorder(Sample_num, total_biomass, FUN=sum), y = proportion, fill = factor(Type, levels = c("vascular plant", "moss")))) +
  geom_col( color = "black", size = 0.1) +
  labs(
    x = "Sample ID",
    y = "Relative biomass (%)",
    title = "b)",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c("vascular plant" = "#4CAF50", "moss" = "#3FB8A7"),
    labels = c("Vascular plants", "Bryophytes")
  ) +
  theme_bw(base_size = 12)+
  theme(legend.position = "right")
p2
p1+p2
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#2. Carbon balance
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# -2.1 Long term AC data
# -2.2 Spring shoulder season Eddy data
#-------------------------------------------------------------------------------
#download libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(patchwork)
library(writexl)
#-------------------------------------------------------------------------------
Sys.setlocale("LC_TIME", "C")
setwd("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/R")
#-------------------------------------------------------------------------------
#2.1 Long term AC data
#-------------------------------------------------------------------------------
#import dataset (Downloaded from GEM database)
AC <- read.table("AutochambersFen_10.17897_7ZME-8H97_data.txt", header = TRUE, sep = "\t")
#Check the structure
str(AC)

# --- Clean data ---
AC[AC == -9999] <- NA #Change -9999 to NA

#Remove the outlier of –3331.932 in CH4
AC$CH4FluxMean..mg..m2h..[AC$CH4FluxMean..mg..m2h.. < -3000] <- NA

AC2 <- AC %>%
  mutate(
    Date = ymd(Date),
    DOY = yday(Date),
    CH4Flux_mean = CH4FluxMean..mg..m2h..,
    CH4Flux_sd = CH4FluxStd..mg..m2h..,
    CO2Flux_mean = CO2Flux..mg..m2h..,
    CO2Flux_sd = CO2FluxStd..mg..m2h..)

#----------------------------------------------------------------------
#Carbon balance of snow-free season and growing season across 2008-2020
#----------------------------------------------------------------------
str(AC2)

#The IPCC GWP100 of natural CH₄ is: 1 CH₄ = 27 CO₂-eq 
GWP_CH4 <- 27

#Snow-free season (sf)
AC_snowfree <- AC2 %>%
  filter(month(Date) %in% 6:10)%>%
  mutate(
    CH4Flux_CO2eq = CH4Flux_mean * GWP_CH4,
    net_c_flux = CO2Flux_mean + CH4Flux_CO2eq   # positive = source, negative = sink
  )

summary_AC_snowfree <- AC_snowfree %>%
  pivot_longer(
    cols = c(CO2Flux_mean,
             CH4Flux_CO2eq,
             net_c_flux),
    names_to = "variable",
    values_to = "value"
  ) %>%
  summarise(
    stats = list(boxplot.stats(value)$stats),
    .by = variable
  ) %>%
  unnest(stats) %>%                    # <-- makes 5 rows per variable
  group_by(variable) %>%
  mutate(
    stat = c("Lower whisker", "Q1", "Median", "Q3", "Upper whisker")
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = variable,
    values_from = stats
  )

write_xlsx(summary_AC_snowfree, "summary_AC_snowfree_kob.xlsx")

AC_long_sf <- AC_snowfree %>%
  select(CO2Flux_mean, CH4Flux_CO2eq, net_c_flux) %>%
  pivot_longer(
    cols = everything(),
    names_to = "flux_type",
    values_to = "flux"
  ) %>%
  mutate(flux_type = case_when(
    flux_type == "net_c_flux" ~ "Net Carbon Flux",
    flux_type == "CO2Flux_mean" ~ "CO2 Flux",
    flux_type == "CH4Flux_CO2eq" ~ "CH4 Flux"
  ))

sf <- ggplot(AC_long_sf, aes(x = flux_type, y = flux, fill = flux_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Net Carbon Flux" = "#1f78b4",
    "CO2 Flux" = "#33a02c",
    "CH4 Flux" = "#e31a1c"
  )) +
  labs(
    title = "(a)",
    x = "",
    y = "Flux (mg CO₂-eq m⁻² h⁻¹)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

#Growing season (gs)
AC_gs <- AC2 %>%
  filter(month(Date) %in% 7:8)%>%
  mutate(
    CH4Flux_CO2eq = CH4Flux_mean * GWP_CH4,
    net_c_flux = CO2Flux_mean + CH4Flux_CO2eq   # positive = source, negative = sink
  )

summary_AC_flux <- AC_gs %>%
  pivot_longer(
    cols = c(CO2Flux_mean,
             CH4Flux_CO2eq,
             net_c_flux),
    names_to = "variable",
    values_to = "value"
  ) %>%
  summarise(
    stats = list(boxplot.stats(value)$stats),
    .by = variable
  ) %>%
  unnest(stats) %>%                    # <-- makes 5 rows per variable
  group_by(variable) %>%
  mutate(
    stat = c("Lower whisker", "Q1", "Median", "Q3", "Upper whisker")
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = variable,
    values_from = stats
  )

write_xlsx(summary_AC_flux, "summary_gs_AC_flux_kob.xlsx")

AC_long <- AC_gs %>%
  select(CO2Flux_mean, CH4Flux_CO2eq, net_c_flux) %>%
  pivot_longer(
    cols = everything(),
    names_to = "flux_type",
    values_to = "flux"
  ) %>%
  mutate(flux_type = case_when(
    flux_type == "net_c_flux" ~ "Net Carbon Flux",
    flux_type == "CO2Flux_mean" ~ "CO2 Flux",
    flux_type == "CH4Flux_CO2eq" ~ "CH4 Flux"
  ))

gs <- ggplot(AC_long, aes(x = flux_type, y = flux, fill = flux_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Net Carbon Flux" = "#1f78b4",
    "CO2 Flux" = "#33a02c",
    "CH4 Flux" = "#e31a1c"
  )) +
  labs(
    title = "(b)",
    x = "",
    y = "Flux (mg CO₂-eq m⁻² h⁻¹)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
sf+gs

#-------------------------------------------------------------------------------
#2.2 Spring shoulder season Eddy data
#-------------------------------------------------------------------------------
#Import dataset
EC_kob <- read_csv("Kobbefjord_CH4_CO2_EC.csv") #Eddy covariance data
str(EC_kob)

#Clean the data based on qc number; 2 = no data and filter the early growing season
EC_kob_clean <- EC_kob %>% 
  mutate(
    co2_flux = ifelse(qc_co2_flux == 2, NA, co2_flux),
    ch4_flux = ifelse(qc_ch4_flux == 2, NA, ch4_flux))%>%
  filter(date >= as.Date("2025-06-16"),
         date <= as.Date("2025-06-26")
  )
str(EC_kob_clean)

#Change the unit to fit with the AC data:
#CO₂ (mg m⁻² h⁻¹)=CO₂ (µmol m⁻² s⁻¹)×44.01×3.6
#CH₄ (mg m⁻² h⁻¹)=CH₄ (µmol m⁻² s⁻¹)×16.04×3.6

EC_kob_clean <- EC_kob_clean %>%
  mutate(
    co2_flux_mg_m2_h = co2_flux * 44.01 * 3.6,
    ch4_flux_mg_m2_h = ch4_flux * 16.04 * 3.6)

#Erase ouliers
#negative values of methane
EC_kob_clean$ch4_flux_mg_m2_h[EC_kob_clean$ch4_flux_mg_m2_h < 0.0] <- NA
#CO2 more than 1500
EC_kob_clean$co2_flux_mg_m2_h[EC_kob_clean$co2_flux_mg_m2_h > 1500] <- NA
str(EC_kob_clean)

#Convert CH₄ to CO₂-equivalent
#calculate the CO2-equivalent and net carbon flux
EC_C_balance <- EC_kob_clean %>%
  mutate(
    ch4_flux_co2eq = ch4_flux_mg_m2_h * GWP_CH4, #CH4 as CO2-eq
    net_c_flux = co2_flux_mg_m2_h + ch4_flux_co2eq #net carbon flux: positive = source, negative = sink
  )

#Boxplot
EC_long <- EC_C_balance %>%
  select(co2_flux_mg_m2_h, ch4_flux_co2eq, net_c_flux) %>%
  pivot_longer(
    cols = everything(),
    names_to = "flux_type",
    values_to = "flux"
  ) %>%
  mutate(flux_type = case_when(
    flux_type == "net_c_flux" ~ "Net Carbon Flux",
    flux_type == "co2_flux_mg_m2_h" ~ "CO2 Flux",
    flux_type == "ch4_flux_co2eq" ~ "CH4 Flux"
  ))

summary_stats_2 <- EC_C_balance %>%
  pivot_longer(
    cols = c(co2_flux_mg_m2_h,
             ch4_flux_co2eq,
             net_c_flux),
    names_to = "variable",
    values_to = "value"
  ) %>%
  summarise(
    stats = list(boxplot.stats(value)$stats),
    .by = variable
  ) %>%
  unnest(stats) %>%                    # <-- makes 5 rows per variable
  group_by(variable) %>%
  mutate(
    stat = c("Lower whisker", "Q1", "Median", "Q3", "Upper whisker")
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = variable,
    values_from = stats
  )

write_xlsx(summary_stats_2, "summary_boxplotstats_flux_kob.xlsx")

ggplot(EC_long, aes(x = flux_type, y = flux, fill = flux_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Net Carbon Flux" = "#1f78b4",
    "CO2 Flux" = "#33a02c",
    "CH4 Flux" = "#e31a1c"
  )) +
  labs(
    title = NULL,
    x = "",
    y = "Flux (mg CO₂-eq m⁻² h⁻¹)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#3. Environmental variables and gas fluxes: long-term monitoring data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# -3.1 Data treatment
# -3.2 Annual means 
# -3.3 Daily means - combine datasets by date and plot with time
# -3.4 PCA-analysis
# -3.5 GAMM models
#-------------------------------------------------------------------------------
library(readr)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(lubridate)
library(tidyverse)
library(mgcv)
library(missMDA)
library(car)
library(factoextra)
library(broom)
library(nlme)
library(mgcv)
library(MuMIn)
library(visreg)
library(patchwork)
#-------------------------------------------------------------------------------
setwd("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/R")
#-------------------------------------------------------------------------------
#3.1 Data treatment
#-------------------------------------------------------------------------------
#Import datasets
#-------------------------------------------------------------------------------
#Soil Water chemistry; DON, DOC, pH
#-------------------------------------------------------------------------------
Soilchem <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/GEM_soilchemistry/SoilFen SoilWaterChemistry_10.17897_JV0K-4P72_data.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)
str(Soilchem)

#Change -9999 to NA
Soilchem[Soilchem == -9999] <- NA 
Soilchem$pH[Soilchem$pH == 6233.000] <- NA #erase outliers in pH
Soilchem$pH[Soilchem$pH == 6053.000] <- NA
str(Soilchem)

#use 10 cm depth data for analysis
Soilchem_10cm <- Soilchem %>%
  filter(`Depth (cm)`== 10)%>%
  mutate(DOC_ug_L = `DisolvedOrganicCarbon (DOC) (ppm)` * 1000) #convert to µg/l

str(Soilchem_10cm)
#-------------------------------------------------------------------------------
#Soil temperature
#-------------------------------------------------------------------------------
Soiltemp <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/GEM_Soiltemp/SoilFen SoilProperties 5min_10.17897_N33Q-M118_data.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

Soiltemp[Soiltemp== -9999] <- NA 
str(Soiltemp)
#-------------------------------------------------------------------------------
#Water table depth
#-------------------------------------------------------------------------------
WTD <- read_excel("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/soil.xlsx", 
                  sheet = "WTD")
#WTD data is from 6 automatic chambers that measure fluxes  
#Aggregate WTD across chambers
#Reshape WTD to long
WTD_long <- WTD %>%
  pivot_longer(
    cols = starts_with("Ch"),
    names_to = "Chamber",
    values_to = "WTD"
  ) %>%
  mutate(WTD = as.numeric(WTD))
str(WTD_long)
#-------------------------------------------------------------------------------
#Automatic chamber data
#-------------------------------------------------------------------------------
#import Kobbefjord dataset (Downloaded from GEM database)
AC <- read.table("AutochambersFen_10.17897_7ZME-8H97_data.txt", header = TRUE, sep = "\t")
str(AC)

#Clean data
AC[AC == -9999] <- NA #Change -9999 to NA
#Remove the outlier of –3331.932 in CH4
AC$CH4FluxMean..mg..m2h..[AC$CH4FluxMean..mg..m2h.. < -3000] <- NA
AC$CH4FluxMean..mg..m2h..[AC$CH4FluxMean..mg..m2h.. < -0.00] <- NA
#Fix date-time formats
AC <- AC %>%
  mutate(
    Date = as.Date(Date),
    Time = hm(Time),
    datetime = as.POSIXct(Date) + seconds(Time),
    DOY = yday(Date))
#-------------------------------------------------------------------------------
#PAR and air temperature
#-------------------------------------------------------------------------------
Meteorology <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/GEM_meteorology/SoilFen Meteorology 30min_10.17897_JSYF-KZ36_data.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

Meteorology[Meteorology == -9999] <- NA #Change -9999 to NA

#-------------------------------------------------------------------------------
#NDVI datasets
#-------------------------------------------------------------------------------
#Import NERO line NDVI
#The data was first filtered in QGIS including data only measured inside the fen
NERO_line_NDVI_Fen <- read_csv("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/NDVI/NERO line NDVI/NERO_line_NDVI_Fen.csv")

#Metadata of the original dataset
NDVI_NERO_meta <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/NDVI/NERO line NDVI/NERO line NDVI (discontinued)_10.17897_RE9T-H355_columns_metadata.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)
#Select NDVI and Date columns
NDVI_NERO <- NERO_line_NDVI_Fen%>%
  select(Date, SF4)%>%
  rename(NDVI = SF4)

#Import Plot NDVI (discontinued) dataset (2008-2016)
Plot_NDVI_discontinued <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/NDVI/Plot NDVI (discontinued)_10.17897_R522-CA15/Plot NDVI (discontinued)_10.17897_R522-CA15_data.txt", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

#Filter ERI2 plots that are situated on the fen and select NDVI and date columns
Plot_NDVI_ERI2 <- Plot_NDVI_discontinued%>%
  filter(Plot == "ERI2")%>%
  select(Date, `SF 4`)%>%
  rename(NDVI = `SF 4`)

#Import Plot NDVI (2018-2024)
Plot_NDVI <- read_delim("C:/Users/OMISTAJA/OneDrive - University of Oulu and Oamk/Koulu/Maisteri/Gradu/Master's thesis - Enni/Kobbefjord/GEM_database/NDVI/Plot NDVI_10.17897_TQTT-EV69/Plot NDVI_10.17897_TQTT-EV69_data.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
str(Plot_NDVI)
#Filter ERI2 plots and select NDVI and Date columns
Plot_NDVI_ERI2_2 <- Plot_NDVI%>%
  filter(Species == "ERI",
         Plot == "2")%>%
  select(Date, NDVI)

str(NDVI_NERO)
str(Plot_NDVI_ERI2)
str(Plot_NDVI_ERI2_2)

#Combine all NDVI datasets
combined_ndvi <- bind_rows(
  NDVI_NERO %>% mutate(dataset = "NDVI_NERO"),
  Plot_NDVI_ERI2 %>% mutate(dataset = "Plot_NDVI_ERI2"),
  Plot_NDVI_ERI2_2%>% mutate(dataset = "Plot_NDVI_ERI2_2")
)
#Change -9999 to NA
combined_ndvi[combined_ndvi == -9999] <- NA 
str(combined_ndvi)

#-------------------------------------------------------------------------------
#3.2 Annual means of soil chemistry and air temperature
#-------------------------------------------------------------------------------
#Soil chemistry: Mean per year
mean_annual_soichem1 <- Soilchem_10cm %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    mean_DON =mean(`DisolvedOrganicNitrogen (DON) (µg/l)`, na.rm = TRUE),
    sd_DON = sd(`DisolvedOrganicNitrogen (DON) (µg/l)`, na.rm = TRUE),
    mean_DOC = mean(DOC_ug_L, na.rm = TRUE),
    sd_DOC = sd(`DisolvedOrganicCarbon (DOC) (ppm)`, na.rm = TRUE),
    mean_DTN = mean(`DisolvedTotalNitrogen (DTN) (µg/l)`, na.rm = TRUE),
    sd_DTN = sd(`DisolvedTotalNitrogen (DTN) (µg/l)`, na.rm = TRUE),
    mean_NH4 = mean(`NH4-N (µg/l)`, na.rm = TRUE),
    sd_NH4 = sd(`NH4-N (µg/l)`, na.rm = TRUE),
    mean_NO3 = mean(`NO3-N (µg/l)`, na.rm = TRUE),
    sd_NO3 = sd(`NO3-N (µg/l)`, na.rm = TRUE),
    mean_alkalinity = mean(`Alkalinity (mmol/l)`, na.rm = TRUE),
    mean_Ca = mean(`Ca (ppm)`, na.rm = TRUE),
    mean_Na = mean(`Na (ppm)`, na.rm = TRUE),
    mean_Mg = mean(`Mg (ppm)`, na.rm = TRUE),
    mean_K = mean(`K (ppm)`, na.rm = TRUE),
    mean_Fe = mean(`Fe (ppm)`, na.rm = TRUE),
    mean_Mn = mean(`Mn (ppm)`, na.rm = TRUE),
    mean_Al = mean(`Al (ppm)`, na.rm = TRUE),
    mean_conductivity = mean(`SpecificConductivity (µS/cm)`, na.rm = TRUE)
  )

mean_annual_soichem1

#Mean across yearly means
mean_annual_soichem2 <- mean_annual_soichem1%>%
  summarise(
    mean_pH = mean(mean_pH, na.rm = TRUE),
    mean_DON =mean(mean_DON, na.rm = TRUE),
    mean_DOC = mean(mean_DOC, na.rm = TRUE),
    mean_DTN = mean(mean_DTN, na.rm = TRUE),
    mean_NH4 = mean(mean_NH4, na.rm = TRUE),
    mean_NO3 = mean(mean_NO3, na.rm = TRUE),
    mean_alkalinity = mean(mean_alkalinity, na.rm = TRUE),
    mean_Ca = mean(mean_Ca, na.rm = TRUE),
    mean_Na = mean(mean_Na, na.rm = TRUE),
    mean_Mg = mean(mean_Mg, na.rm = TRUE),
    mean_K= mean(mean_K, na.rm = TRUE),
    mean_Fe = mean(mean_Fe, na.rm = TRUE),
    mean_Mn = mean(mean_Mn, na.rm = TRUE),
    mean_Al = mean(mean_Al, na.rm = TRUE),
    mean_cond = mean(mean_conductivity, na.rm = TRUE)
    
  )
#Annual mean
mean_annual_soichem2

#Mean annual soil temperature (10 cm depth)
str(Soiltemp)
#mean per year
mean_annual_soiltemp1 <- Soiltemp %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_temp10 = mean(`SoilTemperature 10cm (°C)`, na.rm = TRUE),
    sd_temp10 = sd(`SoilTemperature 10cm (°C)`, na.rm = TRUE))

mean_annual_soiltemp1
#mean across yearly means
mean_annual_soiltemp2 <- mean_annual_soiltemp1%>%
  summarise(
    mean_temp = mean(mean_temp10, na.rm = TRUE),
    sd_temp = sd(mean_temp10, na.rm = TRUE)
  )
#annual mean
mean_annual_soiltemp2

#Air temperature: mean per year
mean_annual_airtemp1 <- Meteorology %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_airtemp = mean(`AirTemperature (°C)`, na.rm = TRUE),
    sd_temp10 = sd(`AirTemperature (°C)`, na.rm = TRUE))

mean_annual_airtemp1

#Mean across yearly means
mean_annual_airtemp2 <- mean_annual_airtemp1%>%
  summarise(
    mean_temp = mean(mean_airtemp, na.rm = TRUE),
    sd_temp = sd(mean_airtemp, na.rm = TRUE)
  )
#Annual mean
mean_annual_airtemp2

#-------------------------------------------------------------------------------
#3.3. Daily means - combine all datasets by date
#-------------------------------------------------------------------------------
#Calculate daily means for every dataset

AC_daily<- AC %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(
    CH4_mean = mean(`CH4FluxMean..mg..m2h..`, na.rm = TRUE),
    CH4_sd = sd(`CH4FluxMean..mg..m2h..`, na.rm = TRUE),
    CO2_mean = mean(`CO2Flux..mg..m2h..`, na.rm = TRUE),
    CO2_sd = sd(`CO2Flux..mg..m2h..`, na.rm = TRUE),
    .groups = 'drop'
  )

Soilchem_daily <- Soilchem_10cm %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(
    DON_mean = mean(`DisolvedOrganicNitrogen (DON) (µg/l)`, na.rm = TRUE),
    DON_sd = sd(`DisolvedOrganicNitrogen (DON) (µg/l)`, na.rm = TRUE),
    DOC_mean = mean(`DisolvedOrganicCarbon (DOC) (ppm)`, na.rm = TRUE),
    DOC_sd = sd(`DisolvedOrganicCarbon (DOC) (ppm)`, na.rm = TRUE),
    pH_mean = mean(pH, na.rm = TRUE),
    pH_sd = sd(pH, na.rm = TRUE),
    .groups = 'drop'
  )

wtd_daily <- WTD_long %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(
    WTD_mean = mean(WTD, na.rm = TRUE),
    WTD_sd = sd(WTD, na.rm = TRUE),
    .groups = 'drop'
  )

soiltemp_daily <- Soiltemp %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(
    soiltemp_mean = mean(`SoilTemperature 10cm (°C)`, na.rm = TRUE),
    soiltemp_sd = sd(`SoilTemperature 10cm (°C)`, na.rm = TRUE),
    .groups = 'drop'
  )

Met_daily <- Meteorology %>%
  mutate(Date= as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(
    PARTotal_mean = mean(`PARTotal (µmol(s*m2))`, na.rm = TRUE),
    PARTotal_sd = sd(`PARTotal (µmol(s*m2))`, na.rm = TRUE),
    Airtemp_mean = mean(`AirTemperature (°C)`, na.rm = TRUE),
    Airtemp_sd = sd(`AirTemperature (°C)`, na.rm = TRUE),
    .groups = 'drop'
  )

NDVI_daily <- combined_ndvi%>%
  group_by(Date)%>%
  summarise(
    NDVI_mean = mean(NDVI, na.rm = TRUE),
    NDVI_sd = sd(NDVI, na.rm = TRUE),
    .groups = 'drop'
  )

str(AC_daily)
str(Soilchem_daily)
str(wtd_daily)
str(soiltemp_daily)
str(Met_daily)
str(NDVI_daily)

combined_data <- AC_daily %>%
  full_join(Soilchem_daily, by = "Date") %>%
  full_join(wtd_daily, by = "Date") %>%
  full_join(soiltemp_daily, by = "Date") %>%
  full_join(Met_daily, by = "Date") %>%
  full_join(NDVI_daily, by = "Date")%>%
  arrange(Date)

str(combined_data)

#-------------------------------------------------------------------------------
#Plot daily means across years
#-------------------------------------------------------------------------------
#Filter years
combined_data_fil_plot <- combined_data %>%
  filter(year(Date) >= 2008,
         year(Date) <= 2020)

#Select variables for plotting
combined_data_plot <- combined_data_fil_plot %>%
  select(
    Date,
    CH4_mean, CH4_sd,
    CO2_mean, CO2_sd,
    soiltemp_mean, soiltemp_sd,
    Airtemp_mean, Airtemp_sd,
    PARTotal_mean, PARTotal_sd,
    WTD_mean, WTD_sd,
    NDVI_mean, NDVI_sd
    # Uncomment if needed:
    # DOC_mean, DOC_sd,
    # DON_mean, DON_sd,
    # pH_mean, pH_sd
  )

# Detect mean variables automatically
mean_vars <- names(combined_data_plot) %>%
  stringr::str_subset("_mean$")

# Axis labels
var_labels <- c(
  CH4 = "CH4\n(mg m⁻² h⁻¹)",
  CO2 = "CO2\n(mg m⁻² h⁻¹)",
  soiltemp = "Soil temperature\n(°C)",
  PARTotal = "PAR\n(µmol m⁻² s⁻¹)",
  Airtemp = "Air temperature\n(°C)",
  WTD = "Water table depth\n(cm)",
  NDVI = "NDVI")
  #DON = "DON\n(µg L⁻¹)",
  #DOC = "DOC\n(ppm)",
  #pH = "pH"

# Panel function
make_panel <- function(var_mean, show_x = FALSE){
  
  var_sd <- stringr::str_replace(var_mean, "_mean", "_sd")
  var_name <- stringr::str_replace(var_mean, "_mean", "")
  
  p <- ggplot(
    combined_data_plot,
    aes(x = Date, y = .data[[var_mean]])
  ) +
    
    # SD ribbon
    geom_ribbon(
      aes(
        ymin = .data[[var_mean]] - .data[[var_sd]],
        ymax = .data[[var_mean]] + .data[[var_sd]]
      ),
      fill = "darkblue",
      #alpha = 1,
      na.rm = TRUE
    ) +
    
    # mean line
    geom_point(
      colour = "#2c7fb8",
      size = 0.5,
      na.rm = TRUE
    ) +
    
    labs(
      y = var_labels[[var_name]],
      x = NULL
    ) +
    
    scale_x_date(
      date_breaks = "1 years",
      date_labels = "%Y",
      expand = c(0.01, 0.01)
    ) +
    
    theme_bw(base_size = 11) +
    
    theme(
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.line = element_line(linewidth = 0.3),
      
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(3, 6, 3, 6)
    )
  
  # remove x axis except last panel
  # if(!show_x){
  #    p <- p +
  #      theme(
  #        axis.text.x = element_blank(),
  #        axis.ticks.x = element_blank()
  #      )
  #  }
  
  return(p)
}

#Generate all panels automatically
plots <- purrr::map(mean_vars, make_panel)

#Combine plots vertically
final_plot <-
  wrap_plots(plots, ncol = 1) +
  plot_annotation(tag_levels = "a")

#Show plot
print(final_plot)

#Save figure
#pdf
ggsave(
  filename = "timeseries_panels_2008_2020.pdf",
  plot = final_plot,
  width = 7,
  height = 10,
  dpi = 300
)
#png
ggsave(
  filename = "timeseries_panels_2008_2020.png",
  plot = final_plot,
  width = 7,
  height = 10,
  dpi = 300
)

#-------------------------------------------------------------------------------
#3.4. PCA-analysis
#-------------------------------------------------------------------------------
#Data treatment
#--------------
#Filter snow-free season (June = 6, October =10)
combined_data_fil <- combined_data %>%
  filter(month(Date) %in% 6:10)
summary(combined_data_fil)
colSums(is.na(combined_data_fil))
str(combined_data_fil)

#Erase rows with NA values
df <- combined_data_fil |>
  dplyr::filter(!is.na(CH4_mean),
                !is.na(CO2_mean))

#Make sure date format is correct
df <- df%>%
  mutate(
    Date = ymd(gsub("_", "-", Date)),  # convert to proper Date
    Month = month(Date),               # numeric month (1–12)
    Year = year(Date))

#Select variables used in the PCA analysis
pca_df <- df %>%
  select(Year,Month,CH4_mean, CO2_mean,
         soiltemp_mean, Airtemp_mean, NDVI_mean, WTD_mean, PARTotal_mean) %>%
  rename(
    Methane = CH4_mean,
    `Carbon dioxide` = CO2_mean,
    `Soil temperature` = soiltemp_mean,
    `Air temperature` = Airtemp_mean,
    PAR = PARTotal_mean,
    NDVI = NDVI_mean,
    `Water level` = WTD_mean,
  ) %>%
  na.omit()
#------------
#PCA-analysis
#------------
pca_scaled <- scale(pca_df[,-c(1,2)])
pca_model <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

#Results
#Importance of components
summary(pca_model)
#contribution of each variable to the components
pca_model$rotation

#----------
#PCA biplot
#----------
#PCA scores (datapoints)
scores_a <- as.data.frame(pca_model$x)
scores_a$Year <- pca_df$Year
scores_a$Month <- pca_df$Month

#PCA loadings (arrows)
loadings_a <- as.data.frame(pca_model$rotation)
loadings_a$Variable <- rownames(loadings_a)

#Optional: scale arrows so they fit nicely with points
arrow_scale <- 3
loadings_a$PC1 <- loadings_a$PC1 * arrow_scale
loadings_a$PC2 <- loadings_a$PC2 * arrow_scale

ggplot(scores_a, aes(x = PC1, y = PC2, color = factor(Month), shape = factor(Year))) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(aes(group = factor(Month), fill = factor(Month)),
               geom = "polygon",
               type = "t",
               linewidth = 0.5,
               alpha = 0.05) +
  geom_segment(data = loadings_a,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "black",
               inherit.aes = FALSE) +
  geom_text_repel(data = loadings_a,
                  aes(x = PC1, y = PC2, label = Variable),
                  inherit.aes = FALSE) +
  scale_shape_manual(values = c(16,17,15,18,1,2,5, 6, 7, 8)) +
  theme_bw() +
  labs(
    x = paste0("PC1 (", round(summary(pca_model)$importance[2,1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(pca_model)$importance[2,2]*100,1), "%)"),
    color = "Month",
    fill = "Month",
    shape = "Year"
  )

#-------------------------------------------------------------------------------
#3.5 GAMM models
#-------------------------------------------------------------------------------
combined_data_fil <- combined_data %>%
  filter(month(Date) %in% 6:10)
#-----------------------------------
#3.4.1. CH4 flux models
#-----------------------------------
#Erase rows with NA-values
ch4_daily <- combined_data_fil |>
  dplyr::filter(!is.na(CH4_mean))
#-------------------------------------
#Model selection for core model
#-------------------------------------
ch4_core <- ch4_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year, DOY, Month, Date, CH4_mean, soiltemp_mean, Airtemp_mean) %>%
  na.omit()  #1093
ch4_core$time <- as.numeric(ch4_core$Date)

#check correlations
predictors <- ch4_core %>% select(soiltemp_mean, Airtemp_mean)
cor(predictors, use = "complete.obs", method = "pearson")

acf(ch4_core$CH4_mean) #Very autocorrelated
diff(ch4_core$time) #-> irregular therefore corCAR

soiltemp_gamm_ch4 <- gamm(
  log(CH4_mean) ~ s(DOY, bs="cc", k=15) + s(soiltemp_mean),
  random = list(Year = ~1),
  correlation = corCAR1(form = ~time | Year),
  data = ch4_core)

airtemp_gamm_ch4 <- gamm(
  log(CH4_mean) ~ s(DOY, bs="cc", k=15) + s(Airtemp_mean),
  random = list(Year = ~1),
  correlation = corCAR1(form = ~time | Year),
  data = ch4_core)

n <- nrow(ch4_core)
k <- length(coef(airtemp_gamm_ch4$gam))
n / k #>40 -> AIC is fine

AIC(soiltemp_gamm_ch4$lme, airtemp_gamm_ch4$lme)
-1463.811-(-1494.781) #With smooth difference is 30.97
-1465.811-(-1471.895) #Without smooths difference is 6.084
summary(soiltemp_gamm_ch4$gam)
summary(airtemp_gamm_ch4$gam)

#-----------------------------
#Vegetation model
#-----------------------------
ch4_NDVI <- ch4_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year,Month, DOY, Date,soiltemp_mean, CH4_mean, NDVI_mean)%>%
  na.omit()  #N = 129
ch4_NDVI$time <- as.numeric(ch4_NDVI$Date)

table(ch4_NDVI$Year, ch4_NDVI$Month)

#Diagnostics
acf(ch4_NDVI$CH4_mean)
hist(ch4_NDVI$CH4_mean)
plot(density(ch4_NDVI$CH4_mean)) #right skewed -> log transformation needed
#Correlations
predictors <-  ch4_NDVI%>% select(NDVI_mean, soiltemp_mean, CH4_mean)
cor(predictors, use = "complete.obs", method = "pearson")

#Model selection within vegetation model subset
NDVI_gamm_ch4 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc")+ soiltemp_mean+ NDVI_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_NDVI #ΔAICc = 1.8 -> substantial support
)

NDVI_gamm_ch42 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc")+ NDVI_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_NDVI #ΔAICc = 20 -> Essentially no support
)

NDVI_gamm_ch43 <- gamm( 
  log(CH4_mean) ~ s(DOY, bs = "cc")+ soiltemp_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_NDVI #Best model - substantial support
)

n <- nrow(ch4_NDVI)
k <- length(coef(NDVI_gamm_ch4$gam))
n/k # = 11.72 -> AICc

AICc(NDVI_gamm_ch4$lme, NDVI_gamm_ch42$lme, NDVI_gamm_ch43$lme)
-122.2083-(-123.9618) 
-101.8972-(-122.2083)
#AICc difference between two best models:1.7535 -> substantial support 
#Model with NDVI is chosen since AIC difference is <2 and we want to know results
#regarding CH4 flux ~ NDVI

#vegetation model output
summary(NDVI_gamm_ch4$gam)

#Visualization
#Residual check
resids <- resid(NDVI_gamm_ch4$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
res2 <- plot(NDVI_gamm_ch4$lme, pages = 1)
res2
gam.check(NDVI_gamm_ch4$gam)
concurvity(NDVI_gamm_ch4$gam)

#Plots response vs. predictor
p1 <- visreg(NDVI_gamm_ch4$gam, "DOY", 
             data = ch4_NDVI,
             gg = TRUE) +
  labs(title = "Vegetation model",
       x = "DOY", 
       y = "Effect on log(CH4)") +
  theme_classic()

p2 <- visreg(NDVI_gamm_ch4$gam, "soiltemp_mean", 
             data = ch4_NDVI,
             gg = TRUE) +
  labs(x = "Soil temperature (°C)", 
       y = "Effect on log(CH4)") +
  theme_classic()

p3 <- visreg(NDVI_gamm_ch4$gam, "NDVI_mean", 
             data = ch4_NDVI,
             gg = TRUE) +
  labs(x = "NDVI", 
       y = "Effect on log(CH4)") +
  theme_classic()

p1+p2+p3

#----------------------------
#Hydrology model
#----------------------------
ch4_WTD <- ch4_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year,Month, DOY, Date,CH4_mean, soiltemp_mean, WTD_mean)%>%
  na.omit()  #N = 156 # N=206 if 7-9
ch4_WTD$time <- as.numeric(ch4_WTD$Date)

table(ch4_WTD$Year, ch4_WTD$Month)

acf(ch4_WTD$CH4_mean)
hist(ch4_WTD$CH4_mean)
plot(density(ch4_WTD$CH4_mean)) #right skewed -> log trans

predictors <-  ch4_WTD%>% select(WTD_mean, soiltemp_mean)
cor(predictors, use = "complete.obs", method = "pearson")

#Model selection
WTD_gamm_ch4 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc")+ soiltemp_mean+ WTD_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_WTD #Selected model
)

WTD_gamm_ch42 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc") + WTD_mean, 
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_WTD
)

WTD_gamm_ch43 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc") + soiltemp_mean, 
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = ch4_WTD
)

n <- nrow(ch4_WTD)
k <- length(coef(WTD_gamm_ch4$gam))
n / k #23

AICc(WTD_gamm_ch4$lme, WTD_gamm_ch42$lme, WTD_gamm_ch43$lme)
-316.9184-(-317.2386) #substantial support for both models
summary(WTD_gamm_ch4$gam) 

#Residual check
resids <- resid(WTD_gamm_ch4$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
gam.check(WTD_gamm_ch4$gam)
res3 <- plot(WTD_gamm_ch4$lme, pages = 1)
res3
concurvity(WTD_gamm_ch4$gam)

#model result plots
p4 <- visreg(WTD_gamm_ch4$gam, "DOY", 
             data = ch4_WTD,
             gg = TRUE) +
  labs(title = "Hydrology model",
       x = "DOY", 
       y = "Effect on log(CH4)") +
  theme_classic()

p5 <- visreg(WTD_gamm_ch4$gam, "soiltemp_mean", 
             data = ch4_WTD,
             gg = TRUE) +
  labs(x = "Soil temperature (°C)", 
       y = "Effect on log(CH4)") +
  theme_classic()

p6 <- visreg(WTD_gamm_ch4$gam, "WTD_mean", 
             data = ch4_WTD,
             gg = TRUE) +
  labs(x = "WL (cm)", 
       y = "Effect on log(CH4)") +
  theme_classic()

p4+p5+p6+p1+p2+p3
#----------------------------------
#Vegetation-hydrology model
#----------------------------------
ch4_WN <- ch4_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year, Month, DOY, Date,CH4_mean, soiltemp_mean, WTD_mean, NDVI_mean)%>%
  na.omit()  #N = 73 #N = 53 if 7-9
ch4_WN$time <- as.numeric(ch4_WN$Date)

table(ch4_WN$Year, ch4_WN$Month)

acf(ch4_WN$CH4_mean)
hist(ch4_WN$CH4_mean)
plot(density(ch4_WN$CH4_mean)) #right skewed -> log trans

predictors <-  ch4_WN%>% select(CH4_mean, WTD_mean, soiltemp_mean, NDVI_mean)
cor(predictors, use = "complete.obs", method = "pearson")

WN_gamm_ch4 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc")+ soiltemp_mean+ WTD_mean + NDVI_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year), #not necessary 
  data = ch4_WN #Best model
)

WN_gamm_ch42 <- gamm(
  log(CH4_mean) ~ s(DOY, bs = "cc") +soiltemp_mean+ WTD_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year), #not necessary 
  data = ch4_WN
) 

n <- nrow(ch4_WN)
k <- length(coef(WN_gamm_ch4$gam))
n/k # 5.1

AICc(WN_gamm_ch4$lme, WN_gamm_ch42$lme)
-39.60739-(-39.76320) # 0.15581 -> substantial support for both models

#Model results output
summary(WN_gamm_ch4$gam)

#residual check
resids <- resid(WN_gamm_ch4$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
gam.check(WN_gamm_ch4$gam)
res4 <- plot(WN_gamm_ch4$lme, pages = 1)
res4
concurvity(WN_gamm_ch4$gam)

#plots
p7 <- visreg(WN_gamm_ch4$gam, "DOY", 
             data = ch4_WN,
             gg = TRUE) +
  labs(x = "DOY", 
       y = "Effect on log(CH4)") +
  theme_classic()

p8 <- visreg(WN_gamm_ch4$gam, "soiltemp_mean", 
             data = ch4_WN,
             gg = TRUE) +
  labs(x = "Soil temperature (°C)", 
       y = "Effect on log(CH4)") +
  theme_classic()

p9 <- visreg(WN_gamm_ch4$gam, "WTD_mean", 
             data = ch4_WN,
             gg = TRUE) +
  labs(x = "WL (cm)", 
       y = "Effect on log(CH4)") +
  theme_classic()

p10 <- visreg(WN_gamm_ch4$gam, "NDVI_mean", 
              data = ch4_WN,
              gg = TRUE) +
  labs(x = "NDVI", 
       y = "Effect on log(CH4)") +
  theme_classic()

p7+p8+p9+p10
#--------------------------------------------------------------
#CO2 flux models
#--------------------------------------------------------------
co2_daily <- combined_data_fil |>
  dplyr::filter(!is.na(CO2_mean))

#-----------------------------------------
#CO2 core model: air or soil temperature?
#-----------------------------------------
co2_core <- co2_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date))%>%
  select(Year,DOY, Date, CO2_mean, Airtemp_mean,soiltemp_mean) %>%
  na.omit()  #879 #N=624 if 7-9
co2_core$time <- as.numeric(co2_core$Date)

acf(co2_core$CO2_mean) #-> strong autocorrelation
hist(co2_core$CO2_mean) #-> left skewness no log trans
plot(density(co2_core$CO2_mean))

#check correlations
predictors <- co2_core %>% select(Airtemp_mean, soiltemp_mean)
cor(predictors, use = "complete.obs", method = "pearson")

#Model selection: Which is better soil temperature or air temperature?
m_soil <- gamm(
  CO2_mean ~ s(DOY, bs="cc") + soiltemp_mean,
  random = list(Year=~1),
  correlation = corCAR1(form = ~ time | Year),
  #weights = varExp(form = ~ fitted(.)),
  data = co2_core #Best model
)

m_air <- gamm(
  CO2_mean ~ s(DOY, bs="cc") + Airtemp_mean,
  random = list(Year=~1),
  correlation = corCAR1(form = ~ time | Year),
  #weights = varExp(form = ~ fitted(.)),
  data = co2_core
)

n <- nrow(co2_core)
k <- length(coef(m_air$gam))
n/k #<40 -> AIC fine

AIC(m_soil$lme, m_air$lme)
8976.967-8966.925 #10.042 -> essentially no support for air temperature
summary(m_soil$gam)
#Soil temperature explains CO₂ flux better than air temperature in this dataset.

#Residual check
gam.check(m_soil$gam)
resids <- resid(m_soil$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
res8 <- plot(m_soil$lme, pages = 1)
res8
#high heteroscedasticity-> weights = varExp(form = ~ fitted(.)) = expects that 
#variance increases or decreases exponentially with fitted value -> 16 warnings
#->numerical instability -> not going to use weights = varExp(form = ~ fitted(.))

#----------------------------------------
#CO2 core model: PAR or soil temperature
#----------------------------------------
co2_rad <- co2_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year, Month, DOY, Date, CO2_mean, PARTotal_mean, soiltemp_mean) %>%
  na.omit()  #521 #N = 390 if 7-9
co2_rad$time <- as.numeric(co2_rad$Date)

table(co2_rad$Year, co2_rad$Month)

acf(co2_rad$CO2_mean)
hist(co2_rad$CO2_mean) #-> left skewness no log trans
plot(density(co2_rad$CO2_mean))

#check correlations
predictors <- co2_rad %>% select(PARTotal_mean, soiltemp_mean)
cor(predictors, use = "complete.obs", method = "pearson")

rad_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + s(PARTotal_mean),  #PAR is nonlinear
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year), #residuals autocorrelated
  data = co2_rad
)

rad_gamm_co22 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + s(soiltemp_mean),  #PAR is nonlinear
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year), #residuals autocorrelated
  data = co2_rad
)

n <- nrow(co2_rad)
k <- length(coef(rad_gamm_co2$gam))
n/k #<40

AICc(rad_gamm_co2$lme, rad_gamm_co22$lme)
5530.628-5346.563 #essentially no support for soil temp

summary(rad_gamm_co2$gam)
summary(rad_gamm_co22$gam)

#Model check
plot(rad_gamm_co2$gam, pages = 1)
gam.check(rad_gamm_co2$gam)
resids <- resid(rad_gamm_co2$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
plot(residuals(rad_gamm_co2$lme))
res9 <- plot(rad_gamm_co22$lme, pages = 1)
res9

concurvity(rad_gamm_co2$gam)
#-----------------------------
#Radiation-vegetation model
#------------------------------
co2_NP <- co2_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year,Month, DOY, Date, CO2_mean, NDVI_mean, PARTotal_mean, soiltemp_mean) %>%
  na.omit()  #59
co2_NP$time <- as.numeric(co2_NP$Date)

table(co2_NP$Year, co2_NP$Month)

acf(co2_NP$CO2_mean) #not super strong but autocorrelation is detected
hist(co2_NP$CO2_mean) #-> left skewness no log trans
plot(density(co2_NP$CO2_mean))

#check correlations
predictors <- co2_NP %>% select(PARTotal_mean, NDVI_mean, soiltemp_mean)
cor(predictors, use = "complete.obs", method = "pearson")

NPT_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + NDVI_mean  + soiltemp_mean + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year), #residuals not correlated
  data = co2_NP
)

NP_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + NDVI_mean + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_NP
) #best model

N_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + NDVI_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_NP
)

P_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_NP
)

n <- nrow(co2_NP)
k <- length(coef(NP_gamm_co2$gam))
n/k #= 5.363636

AICc(NP_gamm_co2$lme, NPT_gamm_co2$lme, N_gamm_co2$lme, P_gamm_co2$lme)
648.0222-645.4604 #<2
652.4423-645.4604 #<2

#NP model best
summary(NP_gamm_co2$gam)

#Check
gam.check(NP_gamm_co2$gam)
resids <- resid(NP_gamm_co2$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
res11 <- plot(NP_gamm_co2$lme, pages = 1)
res11

#Model result plots
p11 <- visreg(NP_gamm_co2$gam, "DOY", 
              data = co2_NP,
              gg = TRUE) +
  labs(title = "Vegetation-radiation model",
       x = "DOY", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p12 <- visreg(NP_gamm_co2$gam, "PARTotal_mean", 
              data = co2_NP,
              gg = TRUE) +
  labs(x = "PAR (µmol m⁻² s⁻¹)", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p13 <- visreg(NP_gamm_co2$gam, "NDVI_mean", 
              data = co2_NP,
              gg = TRUE) +
  labs(x = "NDVI", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p11+p12+p13

#------------------------------
#Hydrology-radiation model
#------------------------------
co2_WP <- co2_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year, Month, DOY, Date, CO2_mean, WTD_mean, PARTotal_mean, soiltemp_mean) %>%
  na.omit()  #129 # N = 107 if 7-9
co2_WP$time <- as.numeric(co2_WP$Date)

table(co2_WP$Year, co2_WP$Month)

acf(co2_WP$CO2_mean) # moderate autocorrelation
hist(co2_WP$CO2_mean) #-> left skewness no log trans
plot(density(co2_WP$CO2_mean))
#check correlations
predictors <- co2_WP %>% select(WTD_mean, soiltemp_mean, PARTotal_mean)
cor(predictors, use = "complete.obs", method = "pearson")

WP_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + WTD_mean + soiltemp_mean + PARTotal_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = co2_WP
)

WP_gamm_co22 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + WTD_mean  + PARTotal_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = co2_WP
) #Best model

WP_gamm_co23 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + PARTotal_mean,
  random = list(Year = ~1),
  correlation = corCAR1(form = ~ time|Year),
  data = co2_WP #substantial support
)

n <- nrow(co2_WP)
k <- length(coef(WP_gamm_co22$gam))
n/k

AICc(WP_gamm_co2$lme,WP_gamm_co22$lme, WP_gamm_co23$lme)
1283.960-1283.363 #substantial support for both models

#Model output results
summary(WP_gamm_co22$gam)

#Check
concurvity(WP_gamm_co22$gam)
gam.check(WP_gamm_co22$gam)
resids <- resid(WP_gamm_co23$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
res13 <- plot(WP_gamm_co23$lme, pages = 1)
res13

#Model result plots
p14 <- visreg(WP_gamm_co22$gam, "DOY", 
              data = co2_WP,
              gg = TRUE) +
  labs(title = "Hydrology-radiation model",
       x = "DOY", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p15 <- visreg(WP_gamm_co22$gam, "PARTotal_mean", 
              data = co2_WP,
              gg = TRUE) +
  labs(x = "PAR (µmol m⁻² s⁻¹)", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p16 <- visreg(WP_gamm_co22$gam, "WTD_mean", 
              data = co2_WP,
              gg = TRUE) +
  labs(x = "WL (cm)", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p14+p15+p16+p11+p12+p13

#----------------------------------------
#CO2 flux Full model
#---------------------------------------
co2_WPN <- co2_daily %>%
  mutate(Year = year(Date),
         DOY = yday(Date),
         Month = month(Date))%>%
  select(Year,Month,DOY, Date, CO2_mean, WTD_mean, soiltemp_mean, PARTotal_mean, NDVI_mean) %>%
  na.omit()  #35 # N = 31 if 7-9
co2_WPN$time <- as.numeric(co2_WPN$Date)

table(co2_WPN$Year, co2_WPN$Month)

acf(co2_WPN$CO2_mean) # very small autocorrelation
hist(co2_WPN$CO2_mean) #-> left skewness no log trans
plot(density(co2_WPN$CO2_mean))
#check correlations
predictors <- co2_WPN %>% select(WTD_mean, soiltemp_mean, PARTotal_mean, NDVI_mean)
cor(predictors, use = "complete.obs", method = "pearson")

WPN_gamm_co2 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + WTD_mean + soiltemp_mean + NDVI_mean + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_WPN
)

WPN_gamm_co22 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + WTD_mean  + NDVI_mean + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_WPN #second best, substantial support
)

WPN_gamm_co23 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + WTD_mean + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_WPN
) #Best model

WPN_gamm_co24 <- gamm(
  CO2_mean ~ s(DOY, bs = "cc") + PARTotal_mean,
  random = list(Year = ~1),
  #correlation = corCAR1(form = ~ time|Year),
  data = co2_WPN
) 

n <- nrow(co2_WPN)
k <- length(coef(WPN_gamm_co22$gam))
n / k #<40

AICc(WPN_gamm_co2$lme, WPN_gamm_co22$lme,WPN_gamm_co23$lme, WPN_gamm_co24$lme)
376.1631-374.1986 #1.96 -> substantial support for both models
379.4623- 376.1631
380.5820- 374.1986
#Second best model with substantial support was chosen because we want to see 

#NDVI results
summary(WPN_gamm_co22$gam)

#Check
concurvity(WPN_gamm_co22$gam)
gam.check(WPN_gamm_co22$gam)
resids <- resid(WPN_gamm_co22$lme, type="normalized")
acf(resids, main="ACF of normalized residuals")
res14 <- plot(WPN_gamm_co22$lme, pages = 1)
res14

#Model result plots
p17 <- visreg(WPN_gamm_co22$gam, "DOY", 
              data = co2_WPN,
              gg = TRUE) +
  labs(x = "DOY", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p18 <- visreg(WPN_gamm_co22$gam, "PARTotal_mean", 
              data = co2_WPN,
              gg = TRUE) +
  labs(x = "PAR (µmol m⁻² s⁻¹)", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p19 <- visreg(WPN_gamm_co22$gam, "WTD_mean", 
              data = co2_WPN,
              gg = TRUE) +
  labs(x = "WL (cm)", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p20 <- visreg(WPN_gamm_co22$gam, "NDVI_mean", 
              data = co2_WPN,
              gg = TRUE) +
  labs(x = "NDVI", 
       y = "Effect on CO2 flux \n (mg m⁻² h⁻¹)") +
  theme_classic()

p17+p18+p19+p20
#-------------------------------------------------------------------------------