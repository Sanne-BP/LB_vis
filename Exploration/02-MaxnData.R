###################################################################################################Script for exploring the MaxnData dataframe

rm(list=ls())
library(tidyverse)

#FactMaxnData
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

#2 videos at Rocky shore 1, Baseline 2, 18/06/2025, contained no fish. EventMeasure produces no rows for videos with no tags, so these are added manually to ensure they're represented in abundance/richness


ggplot(MaxnData, aes(x=Treatment, y=Richness))+
  geom_boxplot(alpha=0.6)+
  facet_wrap(~MaxnData$`Sampling period`, ncol=1)

ggplot(MaxnData, aes(x=Treatment, y=MaxN))+
  geom_boxplot(alpha=0.6)+
  facet_wrap(~MaxnData$`Sampling period`, ncol=1)
