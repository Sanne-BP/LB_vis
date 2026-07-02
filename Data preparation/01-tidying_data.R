#Tidying data from EventMeasure

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lme4)

all_point_measurements <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=2075341656&single=true&output=csv")

#check which colums are empty
empty_cols <- names(all_point_measurements)[
  sapply(all_point_measurements, function(x)
    all(is.na(x) | trimws(as.character(x)) == ""))]

empty_cols

#MaxN
fish_maxn.df <- fish_all_points.df %>%
  rename_all(snakecase::to_snake_case) %>%
  dplyr::select(filename, frame, time_mins,
                op_code, tape_reader,
                sampling_period, date, site, treatment, camera_no,
                genus, species,
                number, stage, activity) %>%# selects all the columns that aren't empty or useful
  unite(spp, genus, species, sep = ".") %>%# creates a new column that combine the Genus and Species columns
  add_column(count = 1) %>% # adds a column of all 1's, this is how we get MaxN per frame
  separate(filename, into = c("code", "scrap"), sep = "_DJI|_GH|_GO|_GP") %>% # Creates unique video identifiers for each video
  group_by(code, frame, time_mins, sampling_period, date, site, treatment, camera_no, tape_reader, spp) %>%
  summarise(maxn = sum(count)) %>% # this line and the above sum the 1s per frame, to get us the MaxN for each species per video, still in long data format though
  ungroup() %>%
  dplyr::select(-frame, -time_mins) %>% # Selects the columns that we need to continue, remove ones no longer needed.
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis", # corrects issues with identification spelling etc
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus", # this Genus was renamed recently
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "Unidentified.sp1" = "Unidentified")) %>%
  pivot_wider(names_from = spp, values_from = maxn,
              values_fn = list(maxn = max), values_fill = list(maxn = 0)) %>% # Turns the long data frame into a wide data frame
  mutate(tape_reader = dplyr::recode(tape_reader, BSL = "Brendan", BL = "Brendan")) %>%
  mutate(richness = rowSums(across(8:ncol(.), ~ . > 0)),
         maxn = rowSums(across(8:ncol(.))))
