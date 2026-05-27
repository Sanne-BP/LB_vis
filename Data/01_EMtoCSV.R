#DID NOT USE THIS ONE
#Getting MaxN, Observations, and behaviour data frames

library(tidyverse)
library(janitor)

#### Import and data wrangling ####

all_point_measurements <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=2075341656&single=true&output=csv")%>%
  clean_names()

colnames(all_point_measurements)

### One step method to get a long data frame. Each row will be a tagged fish.
#This will still be quite raw data and errors in the factors and species names will still need to be dealt with.
long.dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=2075341656&single=true&output=csv", col_types = cols(`Camera no.` = col_character()))%>%
  clean_names() %>%
  # convert column names to snake_case
  # this column has letters and numbers, this part of the code makes sure it's a character, not an integer.
  dplyr::select("filename", "frame", "time_mins", "op_code", "tape_reader", "comment_13", "sampling_period", "date", "site", "treatment", "camera_no", "family", "genus", "species", "code", "number", "stage", "activity", "comment_29") %>%
  # selects all the columns that aren't empty or useful
  unite(spp, genus, species, sep = ".") %>%
  # creates a new column that combine the Genus and Species columns
  add_column(count = 1) %>%
  # adds a column of all 1's, this is how we get MaxN per frame
  separate(filename,
    into = c("code", "scrap"),
    sep = "_DJI|_GH|_GO|_GP|_GX") %>%
  # Creates unique video identifiers for each video
  dplyr::select(1, 3:4, 7:19)
  # selects only the columns we need
View(long.dat)

write.csv(long.dat, "Data/raw-data-long-20260527.csv", row.names = F)
# write.csv(long.dat, "Data/raw-data-long-20220324.csv", row.names = F)






