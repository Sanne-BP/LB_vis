#Script for all the raw cleaned data sheets without errors
rm(list=ls())

library(readr)

#THIS IS DATA WITH ERRORS, BELOW IS CORRECT DATA:
# #the raw data from EventMeasure:
# all_point_measurements <- read_csv("Data/All Point measurements.csv", skip = 4)
# view(all_point_measurements)
#
# #long data frame
# long.data <- read_csv("Data/raw-data-long-20260527.csv")
#
# #MaxN data frame
# maxn.data <- read_csv("Data/proj-fish-maxn-20260527.csv")
#
# #Observation data frame
# obs.data <- read_csv("Data/proj-fish-obs-20260527.csv")
#
# #Behaviour data frame - long
# behav.long.data <- read_csv("Data/behaviour-20260527.csv")
#
# #Feeding data frame
# feed.data <- read_csv("Data/bites-20260527.csv")
#
# #Feeding long data frame
# feed.long.data <- read_csv("Data/bites-long-20260527.csv")


############################################################
#AFTER QA QC, so this is the data you want:
#the raw data from EventMeasure:
all_point_measurements <- read_csv("Data/QC/All Point measurements_1.csv", skip = 4)
unique(all_point_measurements$Species)

#long data frame
long.data <- read_csv("Data/QC/2raw-data-long-20260527.csv")
unique(long.data$spp)
sheet_write(long.data, ss, sheet = "long_data")

#MaxN data frame
maxn.data <- read_csv("Data/QC/2proj-fish-maxn-20260527.csv")
sheet_write(maxn.data, ss, sheet = "maxn_data")

#Observation data frame
obs.data <- read_csv("Data/QC/2proj-fish-obs-20260527.csv")
sheet_write(obs.data, ss, sheet = "obs_data")

#Behaviour data frame - long
behav.long.data <- read_csv("Data/QC/2behaviour-20260527.csv")
sheet_write(behav.long.data, ss, sheet = "behav_long_data")

#Feeding data frame
feed.data <- read_csv("Data/QC/2bites-20260527.csv")
sheet_write(feed.data, ss, sheet = "feed_data")

#Feeding long data frame
feed.long.data <- read_csv("Data/QC/2bites-long-20260527.csv")
sheet_write(feed.long.data, ss, sheet = "feed_long_data")

#MIGHT BE NICE TO ADD FAMILY COLUMN

#Transferring everything to google sheets:
library(googlesheets4)
gs4_auth()

ss = "https://docs.google.com/spreadsheets/d/1EjCvvDv8wWy2WELs9FNIWJEpbxizLsl_Nmny4ud9euY/edit?gid=2075341656#gid=2075341656"


