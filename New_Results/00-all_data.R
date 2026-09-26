rm(list=ls())
library(tidyverse)

#All dataframes are extracted from the following google sheets, which contains all the data for this master research project: https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pubhtml


#FactLongData
LongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=302691208&single=true&output=csv")


#FactMaxnData
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")


#FactObsData
ObsData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")


#FactBehavLongData
BehavLongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=855991795&single=true&output=csv")


#FactFeedData
FeedData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=733876966&single=true&output=csv")


#FactFeedLongData
FeedLongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=336325502&single=true&output=csv")








# Species lookup table + fixed viridis colours for species composition plots
# Source this at the top of each SQ script:  source("Results/00-species_colours.R")
# Every species keeps the SAME colour in every plot, even if some species are absent.

library(tidyverse)
library(viridis)

# 1. Species lookup table ------------------------------------------------------
# Names match the ObsData column names (read.csv turns spaces into dots).
species_info <- tribble(
  ~Species,                    ~Habitat_guild,  ~Feeding_mode,
  "Abudefduf.sexfasciatus",    NA,              "Planktivore",
  "Ambassis.spp",              NA,              "Planktivore",
  "Trachinops.taeniatus",      NA,              "Planktivore",
  "Istiblennius.edentulus",    "Cryptobenthic", "Herbivore",
  "Mugil.cephalus",            NA,              "Herbivore",
  "Girella.elevata",           NA,              "Omnivore",
  "Girella.tricuspidata",      NA,              "Omnivore",
  "Meuschenia.trachylepis",    NA,              "Omnivore",
  "Microcanthus.strigatus",    NA,              "Omnivore",
  "Monacanthus.chinensis",     NA,              "Omnivore",
  "Monodactylus.argenteus",    NA,              "Omnivore",
  "Omobranchus.anolius",       "Cryptobenthic", "Omnivore",
  "Omobranchus.rotundiceps",   "Cryptobenthic", "Omnivore",
  "Scobinichthys.granulatus",  NA,              "Omnivore",   # TO CHECK: predator or omnivore?
  "Acanthopagrus.australis",   NA,              "Predator",
  "Acanthopagrus.butcheri",    NA,              "Predator",
  "Achoerodus.viridis",        NA,              "Predator",
  "Favonigobius.exquisitus",   "Cryptobenthic", "Predator",
  "Favonigobius.lentiginosus", "Cryptobenthic", "Predator",
  "Gerres.subfasciatus",       NA,              "Predator",
  "Parupeneus.spilurus",       NA,              "Predator",
  "Pseudocaranx.georgianus",   NA,              "Predator",
  "Tetractenos.glaber",        NA,              "Predator",
  "Tetractenos.hamiltoni",     NA,              "Predator"
) |>
  mutate(
    Feeding_mode = factor(Feeding_mode,
                          levels = c("Planktivore", "Herbivore", "Omnivore", "Predator")),
    # Readable legend label: "Ambassis.spp" -> "Ambassis spp."
    Label = str_replace(Species, "\\.", " ") |> str_replace(" spp$", " spp.")
  ) |>
  arrange(Feeding_mode, Species)

# 2. Fixed colours -------------------------------------------------------------
# Species are ordered by feeding mode, so each guild occupies its own band of the
# viridis gradient: planktivores = dark purple ... predators = yellow.
species_colours <- setNames(viridis(nrow(species_info), option = "viridis"),
                            species_info$Species)
species_labels  <- setNames(species_info$Label, species_info$Species)

# 3. Plot function -------------------------------------------------------------
# data      : a wide data frame with one column per species (e.g. ObsData or sq1_data)
# x_var     : column for the x-axis, e.g. "Treatment_pooled" or "Treatment"
# facet_var : optional column to facet by, e.g. "Sampling.period"
# relative  : TRUE = proportions (bars sum to 1), FALSE = summed counts
plot_species_composition <- function(data, x_var, facet_var = NULL, relative = TRUE,
                                     x_labels = waiver()) {

  # Warn if the data has a species column that is missing from the lookup table
  non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site",
                        "Treatment", "Treatment_pooled", "Camera.no.",
                        "Richness", "Observations")
  unknown <- setdiff(names(data), c(non_species_cols, species_info$Species))
  if (length(unknown) > 0) {
    warning("Columns not in species_info (not plotted): ", paste(unknown, collapse = ", "))
  }

  group_vars <- c(x_var, facet_var)

  comp <- data |>
    pivot_longer(any_of(species_info$Species),
                 names_to = "Species", values_to = "Count") |>
    group_by(across(all_of(group_vars)), Species) |>
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
    group_by(across(all_of(group_vars))) |>
    mutate(Value = if (relative) Count / sum(Count) else Count) |>
    ungroup() |>
    filter(Count > 0) |>
    # Stack order in the bars follows the lookup-table order (feeding mode)
    mutate(Species = factor(Species, levels = species_info$Species))

  p <- ggplot(comp, aes(x = .data[[x_var]], y = Value, fill = Species)) +
    geom_col(colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = species_colours, labels = species_labels) +
    scale_x_discrete(labels = x_labels) +
    labs(x = "Treatment",
         y = if (relative) "Relative abundance" else "Total abundance",
         fill = "Species") +
    theme_bw() +
    theme(text = element_text(size = 11),
          legend.text = element_text(face = "italic"))

  if (relative) p <- p + scale_y_continuous(labels = scales::percent)
  if (!is.null(facet_var)) p <- p + facet_wrap(vars(.data[[facet_var]]))
  p
}

# Example use (SQ1) -------------------------------------------------------------
# plot_species_composition(sq1_data, x_var = "Treatment_pooled",
#                          facet_var = "Sampling.period",
#                          x_labels = treatment_labels) +
#   labs(title = "Species composition: before versus after instalment of LB")
# ggsave("plots/SQ1_species_composition.png", width = 11, height = 6, dpi = 300, bg = "white")
