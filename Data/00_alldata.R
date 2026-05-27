#Script for all the raw cleaned data sheets
rm(list=ls())

library(readr)

#the raw data from EventMeasure:
all_point_measurements <- read_csv("Data/All Point measurements.csv", skip = 4)
view(all_point_measurements)

#long data frame
long.data <- read_csv("Data/raw-data-long-20260527.csv")

#MaxN data frame
maxn.data <- read_csv("Data/proj-fish-maxn-20260527.csv")

#Observation data frame
obs.data <- read_csv("Data/proj-fish-obs-20260527.csv")

#Behaviour data frame - long
behav.long.data <- read_csv("Data/behaviour-20260527.csv")

#Feeding data frame
feed.data <- read_csv("Data/bites-20260527.csv")

#Feeding long data frame
feed.long.data <- read_csv("Data/bites-long-20260527.csv")


############################################################
#AFTER QA QC
#the raw data from EventMeasure:
all_point_measurements <- read_csv("Data/All Point measurements_1.csv", skip = 4)
view(all_point_measurements)

#long data frame
long.data <- read_csv("Data/2raw-data-long-20260527.csv")

#MaxN data frame
maxn.data <- read_csv("Data/2proj-fish-maxn-20260527.csv")

#Observation data frame
obs.data <- read_csv("Data/2proj-fish-obs-20260527.csv")

#Behaviour data frame - long
behav.long.data <- read_csv("Data/2behaviour-20260527.csv")

#Feeding data frame
feed.data <- read_csv("Data/2bites-20260527.csv")

#Feeding long data frame
feed.long.data <- read_csv("Data/2bites-long-20260527.csv")



