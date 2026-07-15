#Sub question 1: temporal - before versus after installment of LB
#How do fish communities change over time following the installment of LB, compared to control revetments and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)

#Importing data from google sheets:
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

#Filter data for only the relevant dates: 18/06/2025 and 20/04/2026
sp_data <- MaxnData |>
  filter(Date %in% c("18/06/2025", "20/04/2026"))

#First look at species richness, abundance, community composition:

#SPECIES RICHNESS:
richness <- sp_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "Richness")) |>
  mutate(`Sampling period` = factor(`Sampling period`, levels = c("Baseline 2", "6 months")))


ggplot(richness, aes(x = `Sampling period`, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Species Richness before versus after installment of LB",
     x = "Sampling Period",
     y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggplot(richness, aes(x = Treatment, y = Richness)) +
  geom_boxplot() +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species Richness before versus after installment of LB",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_minimal()

ggplot(richness, aes(x = Site, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species Richness before versus after installment of LB",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#SPECIES ABUNDANCE:
abundance <- sp_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "MaxN"))|>
  mutate(`Sampling period` = factor(`Sampling period`, levels = c("Baseline 2", "6 months")))

ggplot(abundance, aes(x = Site, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species abundance before versus after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#due to very very high count of ambassis species, the data is a bit right-skewed so log transformation for the y-axis:
ggplot(abundance, aes(x = Site, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_y_log10() +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species abundance before versus after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#lets try removing ambassis species:????





#COMMUNITY COMPOSITION:
#NMDS / PCA



