##############################################
#### Data Frames from EventMeasure Output ####
#### Brendan S. Lanham                    ####
#### MARCH 2022                           ####
##############################################

#### COPY THIS FILE TO YOUR PROJECT FOLDER RATHER THAN EDITING THIS ONE.
#### KEEP THIS FILE AS IS, SO THERE'S ALWAYS A REFERENCE DOCUMENT.

#### Libraries ####
library(tidyverse)

#### Import and data wrangling ####

### One step method to get a long data frame. Each row will be a tagged fish.
#    This will still be quite raw data and errors in the factors and species names will still need to be dealt with.
long.datQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,                         # read in the EM csv
                     col_types = cols(`Camera no.` = col_character())) %>%                # this column has letters and numbers, this part of the code makes sure it's a character, not an integer.
  dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28) %>%                                      # selects all the columns that aren't empty or useful
  unite(spp, Genus, Species, sep = ".") %>%                                               # creates a new column that combine the Genus and Species columns
  add_column(count = 1) %>%                                                               # adds a column of all 1's, this is how we get MaxN per frame
  separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP|_GX") %>%             # Creates unique video identifiers for each video
  dplyr::select(1, 3:4, 6:12, 14:16)  %>%                                                 # selects only the columns we need
  mutate(Activity = dplyr::recode(Activity, Scavenging = "Transient"))                    # Transiet is listed as Scavenging in early videos, this changes those to Transient
View(long.dat)

write.csv(long.datQAQC, "Data/2raw-data-long-20260527.csv", row.names = F)
# write.csv(long.dat, "Data/raw-data-long-20220324.csv", row.names = F)

# The following code uses the process of "piping" to import the data and wrangle it to produce the required data frame.
## Below, in the "Line by Line" section, it is done in individual steps, which may be easier to follow at first ...
##   and to better understand what each line is doing.
# There's also code in the "without correction" section that will import and wrangle the data but not fix any issues ...
## with the data. E.g., typos in factor names, or the same species spelt differently which creates two species etc.

#### One step method including data import, wrangling, and quality control
#### MaxN ####
maxn.datQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,      # read in the EM csv
                     col_types = cols(`Camera no.` = col_character())) %>%                # this column has letters and numbers, this part of the code makes sure it's a character, not an integer.
  dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28) %>%                                      # selects all the columns that aren't empty or useful
  unite(spp, Genus, Species, sep = ".") %>%                                               # creates a new column that combine the Genus and Species columns
  add_column(count = 1) %>%                                                               # adds a column of all 1's, this is how we get MaxN per frame
  separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP|_GX") %>%             # Creates unique video identifiers for each video
  group_by(Code, Frame, `Time (mins)`, `Sampling period`, Date, Site, Treatment, `Camera no.`, TapeReader,  spp) %>%
  summarise(MaxN = sum(count)) %>%                                                        # this line and the above sum the 1s per frame, to get us the MaxN for each species per video, still in long data format though
  ungroup() %>%
  dplyr::select(1, 4:11) %>%                                                              # Selects the columns that we need to continue, remove ones no longer needed.
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis",        # corrects issues with identification spelling etc
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus",                   # this Genus was renamed recently
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "NA.lineolata" = "Pseudocaranx.georgianus",
                             "Ambassis.NA" =  "Ambassis.jackoniensis",

                             "Unidentified.sp1" = "Unidentified"))  %>%
  pivot_wider(names_from = spp, values_from = MaxN,
              values_fn = list(MaxN = max), values_fill = list(MaxN = 0)) %>%              # Turns the long data frame into a wide data frame
  mutate(Richness = rowSums(.[8:(ncol(.))] > 0)) %>%
  mutate(Richness = rowSums(.[8:(ncol(.))] > 0)) %>%                                       # creates a species richness column
  mutate(MaxN = rowSums(.[8:(ncol(.)-1)]))                                                          # creates a total MaxN column, this will need to be adjusted depending on the number of species columns

write.csv(maxn.datQAQC, "Data/2proj-fish-maxn-20260527.csv", row.names = FALSE)                 # saves the data frame as a .csv file, add project and date to filename


#### Observations ####
## this is much the same as above but species don't need to be summed by frame like with MaxN.
## instead, the "distinct" function is used to make a single frame with many individuals as single observation,
obs.dataQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,
                     col_types = cols(`Camera no.` = col_character())) %>%
  distinct() %>%   # removes duplicate rows. aka, multiple individuals from one species in the same frame. without this step, we just have MaxN
  separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP|_GX") %>%
  dplyr::select(1, 12, 15:19, 24:25) %>%
  unite(spp, Genus, Species, sep = ".") %>%
  add_column(count = 1) %>%
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis",
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus",
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "Unidentified.sp1" = "Unidentified")) %>%
  pivot_wider(names_from = spp, values_from = count,
              values_fn = list(count = sum), values_fill = list(count = 0)) %>%
  mutate(`Sampling period` = dplyr::recode(`Sampling period`, Clontarf = "12 months")) %>%
  mutate(Treatment = dplyr::recode(Treatment, Clontarf = "Rocky shore 1", RS1 = "Rocky shore 1", RS2 = "Rocky shore 2")) %>%
  mutate(Site = dplyr::recode(Site, Clontarf2 = "Clontarf", `Rocky shore 1` = "Clontarf", Before = "Clontarf", `Fairlight Pool` = "Fairlight", `Fairlight  Pool` = "Fairlight")) %>%
  mutate(TapeReader = dplyr::recode(TapeReader, BSL = "Brendan", BL = "Brendan")) %>%
  mutate(Richness = rowSums(.[8:(ncol(.))] > 0)) %>%
  mutate(Observations = rowSums(.[8:(ncol(.)-1)]))

write.csv(obs.dataQAQC, "Data/2proj-fish-obs-20260527.csv", row.names = FALSE)

#### Import without fixing data mistakes ####
## This section is important as the issues with the data will differ between projects.
#    So this will show you how to just import the data, clean up empty rows, and create a wide data frame.

#### MaxN ####
# data import
raw.dat <- read_csv("Data/All Point measurements.csv", skip = 4)
# selects all the columns that aren't empty or useful
dat.2 <- raw.dat %>% dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28)
# creates a new column that combine the Genus and Species columns
dat.3 <- dat.2 %>% unite(spp, Genus, Species, sep = ".")
# adds a column of all 1's, this is how we get MaxN per frame
dat.4 <- dat.3 %>% add_column(count = 1)
# Creates unique video identifiers for each video
dat.5 <- dat.4 %>% separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP")
# this line and the above sum the 1s per frame, to get us the MaxN for each species per video, still in long data format though
dat.6 <- dat.5 %>% group_by(Code, Frame, `Time (HMS)`, `Sampling period`, Date, Site, Treatment, `Camera no.`, TapeReader,  spp) %>% summarise(MaxN = sum(count)) %>% ungroup()
# Selects the columns that we need to continue, remove ones no longer needed.
dat.7 <- dat.6 %>% dplyr::select(1, 4:11)
# make long data wide
maxn.dat1 <- dat.7 %>% pivot_wider(names_from = spp, values_from = MaxN, values_fn = list(MaxN = max), values_fill = list(MaxN = 0))
# adding species richness column
maxn.dat1 <- maxn.dat %>% mutate(Richness = rowSums(.[8:(ncol(.))] > 0))
# adding total MaxN column this will need to be adjusted depending on the number of species columns
maxn.dat1 <- maxn.dat %>% mutate(MaxN = rowSums(.[8:79]))

# write.csv(maxn.dat1, "data/filename.csv", row.names = FALSE)

#### Behaviour ####

## behaviour long - long data, assigning a behaviour to every time a fish was tagged.
behav.longQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,
                       col_types = cols(`Camera no.` = col_character())) %>%
  dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28) %>%
  unite(spp, Genus, Species, sep = ".") %>%
  add_column(count = 1) %>%
  dplyr::select(-Number, -Stage) %>%
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis",
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus",
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "Unidentified.sp1" = "Unidentified"))


View(behav.long)

write.csv(behav.longQAQC, "Data/2behaviour-20260527.csv", row.names = F)

## Feeding, this creates a data frame similar to the MaxN and Obs frame but for observed bites.
feed.datQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,                         # read in the EM csv
                     col_types = cols(`Camera no.` = col_character())) %>%                # this column has letters and numbers, this part of the code makes sure it's a character, not an integer.
  dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28) %>%                                      # selects all the columns that aren't empty or useful
  unite(spp, Genus, Species, sep = ".") %>%
  filter(Activity == "Feeding") %>%                                                       # selects on the Feeding data points
  separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP|_GX") %>%             # Creates unique video identifiers for each video
  group_by(Code, `Sampling period`, Date, Site, Treatment, `Camera no.`, TapeReader,  spp) %>%
  summarise(Bites = sum(as.numeric(Number))) %>%
  ungroup() %>%
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis",        # corrects issues with identification spelling etc
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus",                  # this Genus was renamed recently
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "Unidentified.sp1" = "Unidentified")) %>%
  pivot_wider(names_from = spp, values_from = Bites,
              values_fn = list(Bites = sum), values_fill = list(Bites = 0)) %>%           # Turns the long data frame into a wide data frame
  mutate(Bites = rowSums(.[8:(ncol(.))]))
View(feed.dat)

write.csv(feed.datQAQC, "Data/2bites-20260527.csv", row.names = F)

### Feeding - long data frame.
feed.longQAQC <- read_csv("Data/All Point measurements_1.csv", skip = 4,                         # read in the EM csv
                      col_types = cols(`Camera no.` = col_character())) %>%                # this column has letters and numbers, this part of the code makes sure it's a character, not an integer.
  dplyr::select(1:3, 10:11, 14:18, 23:24, 26:28) %>%                                      # selects all the columns that aren't empty or useful
  unite(spp, Genus, Species, sep = ".") %>%
  filter(Activity == "Feeding") %>%                                                       # selects on the Feeding data points
  separate(Filename, into = c("Code", "scrap"), sep = "_DJI|_GH|_GO|_GP|_GX") %>%             # Creates unique video identifiers for each video
  group_by(Code, `Sampling period`, Date, Site, Treatment, `Camera no.`, TapeReader,  spp) %>%
  summarise(Bites = sum(as.numeric(Number))) %>%
  ungroup() %>%
  mutate(spp = dplyr::recode(spp, "Acanthopagrus.sp1" = "Acanthopagrus.australis",        # corrects issues with identification spelling etc
                             "Acanthopagrus.sp" = "Acanthopagrus.australis",
                             "Ambassis.jacksoniensis" = "Ambassis.spp",
                             "Ambassis.jackoniensis" = "Ambassis.spp",
                             "Ambassis.marianus" = "Ambassis.spp",
                             "Trachiops.taeniatus" = "Trachinops.taeniatus",
                             "Cheilodactylus.fuscus" = "Morwong.fuscus",                  # this Genus was renamed recently
                             "Unidentified.Unidentified" = "Unidentified",
                             "NA.Unidentified" = "Unidentified",
                             "Unidentified.sp2" = "Unidentified",
                             "Unidentified.sp1" = "Unidentified"))

View(feed.long)

write.csv(feed.longQAQC, "Data/2bites-long-20260527.csv", row.names = F)











