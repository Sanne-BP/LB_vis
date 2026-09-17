#Sub Question 2: between site comparison after installment
#After instalment, how do fish communities differ between modified revetments, control revetments, and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)
library(patchwork)

#Importing data from google sheets:
#MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")


#SQ2 focuses on after installation only, therefore we can filter out the baseline 2, which is the 2025 data: 18/06/2025. However, we can also filter out 30/04/2026 and 01/05/2026, because on these dates only videos for the modified sites were watched which would enlarge the sample size for those sites very much. So that leaves us with 20/04/2026! + pool together data from modified site as it is between site selection

#after_data <- MaxnData |>
#  filter(Date == "20/04/2026")

sp_data_Obs <- ObsData |>
  filter(Date == "20/04/2026") |>
  mutate(Sampling.period = factor(Sampling.period, levels = c("Baseline 2", "6 months")),
         Treatment_pooled = case_when(Treatment %in% c("Modified", "Modified Existing Revetment",
                                                       "Modified Living Boulder") ~ "Modified",
                                      TRUE ~ Treatment),
         Treatment_pooled = factor(Treatment_pooled, levels = c("Control 1", "Control 2",
                                                                "Rocky shore 1",
                                                                "Rocky shore 2", "Modified")))




##################################################################
#Sub sampling step, reused for everything for SQ2

# At 6 months, "Modified" is split into MOD-ER and MOD-LB (two habitats sampled at the
# modified site), so pooling them gives 6 rows for Modified x 6 months, vs. only 3 at
# baseline (pre-split, one "Modified" category).SO, we randomly keep 3 of those 6, so Modified is balanced at n=3 per Sampling.period like every other treatment.

# This subsample is done ONCE here (not separately per analysis), so richness, abundance,
# sample size, and community composition below all describe the exact same set of replicate
# videos, instead of four independently-drawn random subsets.

set.seed(123)

modified_6mo <- sp_data_Obs |>
  filter(Treatment_pooled == "Modified", Sampling.period == "6 months")

other_rows <- sp_data_Obs |>
  filter(!(Treatment_pooled == "Modified" & Sampling.period == "6 months"))

modified_6mo_sampled <- modified_6mo |> slice_sample(n = 3)

sq2_data <- bind_rows(other_rows, modified_6mo_sampled)

# Sanity check: Modified should now show 3/3 at both sampling periods, every other
# treatment should show its original (unbalanced) count, unchanged.
sq2_data |> count(Treatment_pooled, Sampling.period)

treatment_labels <- c("Control 1" = "C1", "Control 2" = "C2", "Modified" = "MOD",
                      "Rocky shore 1" = "RS1", "Rocky shore 2" = "RS2")


# Controls share a blue family (dark = C1, light = C2), rocky shores share a green
# family (dark = RS1, light = RS2), and Modified gets its own distinct colour since
# it's the treatment of primary interest. Reused across every plot below that fills
# or colours by Treatment_pooled, so colour-coding stays consistent across figures.
treatment_colors <- c("Control 1" = "#08519C", "Control 2" = "#3182BD",
                      "Rocky shore 1" = "#238B45", "Rocky shore 2" = "#74C476",
                      "Modified" = "#E6550D")








##################################################################

#SPECIES RICHNESS:
ggplot(sq2_data, aes(x = Treatment_pooled, y = Richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species Richness after installment of LB",
       x = "Treatment", y = "Species richness") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ2_richness.png", width = 7, height = 4, dpi = 300, bg = "white")







##################################################################

#SPECIES ABUNDANCE:
ggplot(sq2_data, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species abundance after installment of LB",
       x = "Treatment", y = "Species abundance") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ2_abundance.png", width = 7, height = 4, dpi = 300, bg = "white")

#Pattern with the schooling event of Ambassis species, and now the pattern without them:

sq2_data <- sq2_data |>
  mutate(Observations_no_Ambassis = Observations - Ambassis.spp)

ggplot(sq2_data, aes(x = Treatment_pooled, y = Observations_no_Ambassis, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species abundance after installment of LB excluding Ambassis sp.",
       x = "Treatment", y = "Species abundance") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ2_abundanceEXCLambassis.png", width = 7, height = 4, dpi = 300, bg = "white")




## Does overall fish activity/detectability change after installment, and does that change differ by treatment? A rising total at Modified but not at the Control/Rocky shore treatments would be an early signal that Living Boulders are attracting fish. Summed (not averaged) Observations per Treatment x Sampling.period group, using the same Observations column as the abundance plot above - this is a different SUMMARY of the same underlying data, not a different variable.

total_obs <- sq2_data |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(total_observations = sum(Observations_no_Ambassis), .groups = "drop")

total_obs

ggplot(sq2_data, aes(x = Treatment_pooled, y = Observations_no_Ambassis, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  geom_text(data = total_obs,
            aes(x = Treatment_pooled, y = Inf, label = paste0("N = ", total_observations)),
            inherit.aes = FALSE, vjust = 1.5, size = 3) +
  facet_wrap(~Sampling.period) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species abundance: before versus after installment of LB",
       x = "Treatment", y = "Species abundance") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")









##################################################################

#CRYPTOBENTHIC SPECIES:

cryptobenthic_species <- c("Omobranchus.rotundiceps", "Omobranchus.anolius",
                           "Istiblennius.edentulus", "Favonigobius.exquisitus",
                           "Favonigobius.lentiginosus")

sq2_data <- sq2_data |>
  mutate(Cryptobenthic_abundance = rowSums(across(all_of(cryptobenthic_species))),
         Cryptobenthic_richness = rowSums(across(all_of(cryptobenthic_species), ~ .x > 0)))

p_abund <- ggplot(sq2_data, aes(x = Treatment_pooled, y = Cryptobenthic_abundance, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Cryptobenthic abundance", x = "Treatment", y = "Abundance") +
  theme_bw(base_size = 11)

p_rich <- ggplot(sq2_data, aes(x = Treatment_pooled, y = Cryptobenthic_richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Cryptobenthic richness", x = "Treatment", y = "Richness") +
  theme_bw(base_size = 11)

p_abund + p_rich + plot_layout(guides = "collect") +
  plot_annotation(title = "Cryptobenthic fish after installment of LB")

ggsave("plots/SQ2_cryptobenthic.png", width = 11, height = 5, dpi = 300, bg = "white")












##################################################################

#COMMUNITY COMPOSITION:
#NMDS + Bray-Curtis dissimilarity matrix

non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Treatment_pooled", "Camera.no.", "Richness", "Observations",
                      "Observations_no_Ambassis", "Cryptobenthic_abundance",
                      "Cryptobenthic_richness")
sp_cols <- setdiff(names(sq2_data), non_species_cols)

sp_matrix <- sq2_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sq2_data$Code

#Sanity check: every remaining column should be numeric species counts.
stopifnot(all(sapply(sp_matrix, is.numeric)))

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sq2_data[!zero_rows, ]

sum(zero_rows)  # how many all-zero-catch videos get dropped

set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
sum(is.na(sp_matrix_nz))

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment_pooled <- meta_nz$Treatment_pooled

ggplot(nmds_scores,
       aes(x = NMDS1, y = NMDS2, colour = Treatment_pooled, shape = Treatment_pooled)) +
  geom_point(size = 3) +
  scale_colour_manual(values = treatment_colors, labels = treatment_labels) +
  scale_shape_discrete(labels = treatment_labels) +
  labs(title = "Community composition after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2",
       colour = "Treatment", shape = "Treatment") +
  theme_bw(base_size = 11)

ggsave("plots/SQ2_CC.png", width = 8, height = 5, dpi = 300, bg = "white")


bray_dist <- vegdist(sp_matrix_nz, method = "bray")

permanova <- adonis2(bray_dist ~ Treatment_pooled, data = meta_nz, permutations = 999)
permanova

disp_treatment <- betadisper(bray_dist, meta_nz$Treatment_pooled)
permutest(disp_treatment)

TukeyHSD(disp_treatment)
plot(disp_treatment)









##################################################################

#INDICATOR SPECIES:
library(indicspecies)

set.seed(123)
indval <- multipatt(sp_matrix_nz, meta_nz$Treatment_pooled,
                    func = "IndVal.g", control = how(nperm = 999))

summary(indval)




##################################################################
#SPECIES COMPOSITION (which species make up each treatment's community)
species_by_treatment <- sp_matrix_nz |>
  as.data.frame() |>
  mutate(Treatment_pooled = meta_nz$Treatment_pooled) |>
  group_by(Treatment_pooled) |>
  summarise(across(where(is.numeric), sum), .groups = "drop") |>
  pivot_longer(-Treatment_pooled, names_to = "Species", values_to = "Count") |>
  group_by(Treatment_pooled) |>
  mutate(RelAbundance = Count / sum(Count)) |>
  ungroup()

#With 25 species, a stacked bar with one colour each is unreadable -- keep the top N by
#total count, collapse the rest into "Other" (full breakdown still available in the
#underlying table for an appendix/supplementary if you want it).
top_species <- species_by_treatment |>
  group_by(Species) |>
  summarise(total = sum(Count), .groups = "drop") |>
  slice_max(total, n = 10) |>
  pull(Species)

species_by_treatment <- species_by_treatment |>
  mutate(Species_grouped = if_else(Species %in% top_species, Species, "Other"))

ggplot(species_by_treatment, aes(x = Treatment_pooled, y = RelAbundance, fill = Species_grouped)) +
  geom_col() +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Relative species composition by treatment after installment of LB",
       x = "Treatment", y = "Relative abundance", fill = "Species") +
  theme_bw(base_size = 11)

ggsave("plots/SQ2_speciescomp.png", width = 9, height = 5, dpi = 300, bg = "white")


#lets do the same without Ambassis:
species_by_treatment_noAmb <- sp_matrix_nz |>
  as.data.frame() |>
  select(-Ambassis.spp) |>
  mutate(Treatment_pooled = meta_nz$Treatment_pooled) |>
  group_by(Treatment_pooled) |>
  summarise(across(where(is.numeric), sum), .groups = "drop") |>
  pivot_longer(-Treatment_pooled, names_to = "Species", values_to = "Count") |>
  group_by(Treatment_pooled) |>
  mutate(RelAbundance = Count / sum(Count)) |>
  ungroup()

#Recompute top species from the Ambassis-excluded totals, not reusing the earlier top_species
#list, since removing the dominant species changes which species rank highest overall.
top_species_noAmb <- species_by_treatment_noAmb |>
  group_by(Species) |>
  summarise(total = sum(Count), .groups = "drop") |>
  slice_max(total, n = 8) |>
  pull(Species)

species_by_treatment_noAmb <- species_by_treatment_noAmb |>
  mutate(Species_grouped = if_else(Species %in% top_species_noAmb, Species, "Other"))

ggplot(species_by_treatment_noAmb, aes(x = Treatment_pooled, y = RelAbundance, fill = Species_grouped)) +
  geom_col() +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Relative species composition by treatment (excluding Ambassis sp.)",
       x = "Treatment", y = "Relative abundance", fill = "Species") +
  theme_bw(base_size = 11)

ggsave("plots/SQ2_speciescomp_noAmbassis.png", width = 9, height = 5, dpi = 300, bg = "white")


















# 1. Identify species columns (everything that isn't metadata)
non_species_cols <- c("Code", "Sampling period", "Date", "Site", "Treatment",
                      "Camera no.", "TapeReader", "Richness", "MaxN")
sp_cols <- setdiff(names(after_data), non_species_cols)

# 2. Build species matrix, remove all-zero rows
sp_matrix <- after_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- after_data$Code

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- after_data[!zero_rows, ]

# 3. Run NMDS
set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
#[1] 0.1830469 --> Stress value is <0.2, which is an acceptable fit. Usable for interpretation,  but still have to be cautious.

# Check for NA values in the species matrix
sum(is.na(sp_matrix_nz))    #[1] 0

# Extract site scores and attach metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment <- meta_nz$Treatment

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        colour = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  labs(title = "Community composition: after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

#Too few points to calculate an ellipse


# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: does Site, Treatment, or their interaction explain composition?
permanova <- adonis2(bray_dist ~ Site * Treatment,
                     data = meta_nz,
                     permutations = 999)

permanova

#R2 = 0.618, p = 0.001, so the whole model explains a significant 61.8% of the variation in       community composition.

permanova_terms <- adonis2(bray_dist ~ Site * Treatment,
                           data = meta_nz,
                           permutations = 999,
                           by = "terms")
permanova_terms

#           Df SumOfSqs      R2      F Pr(>F)
#Site       2   1.2766 0.23681 3.4079  0.003 **
#Treatment  3   2.0538 0.38100 3.6552  0.002 **
#Residual  11   2.0603 0.38219
#Total     16   5.3907 1.00000

#Site R2 = 0.23681, p = 0.003, so the effect of Site alone is significant. So, fish communities differ per site. Pearl Bay, Ellery Punt, and Spit West have different fish communities from each other overall.

permanova_treatment <- adonis2(bray_dist ~ Treatment,
                                    data = meta_nz, permutations = 999)
permanova_treatment

#Df SumOfSqs      R2      F Pr(>F)
#Model     5   3.3304 0.61781 3.5562  0.001 ***
#Residual 11   2.0603 0.38219
#Total    16   5.3907 1.00000

#Treatment R2 = 0.61781, p = 0.001. So treatment significantly explains 61.8% of the variation in fish community composition. Fish communities differ substantially depending on which treatment type you're looking at.

