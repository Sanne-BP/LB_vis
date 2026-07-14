###################################################################################################Script for exploring the LongData dataframe

rm(list=ls())
library(tidyverse)

#FactLongData
LongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=302691208&single=true&output=csv")

#making some exploratory plots
ggplot(LongData, aes(x=Date, y=spp, color=Treatment)) +
  geom_boxplot()+
  facet_wrap(~Treatment)

ggplot(LongData, aes(x="", y="", fill=spp)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_wrap(~Treatment)

