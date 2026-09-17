#Sub Question 1: temporal - before versus after installment of LB
#How do fish communities change over time following the installment of LB, compared to control revetments and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)

ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

#Filter data for only the relevant dates: 18/06/2025 and 20/04/2026 + pool together data from modified site as it is between site selection

sp_data_Obs <- ObsData |>
  filter(Date %in% c("18/06/2025", "20/04/2026")) |>
  mutate(Sampling.period = factor(Sampling.period, levels = c("Baseline 2", "6 months")),
         Treatment_pooled = case_when(Treatment %in% c("Modified", "Modified Existing Revetment",
                                                       "Modified Living Boulder") ~ "Modified",
                                      TRUE ~ Treatment),
         Treatment_pooled = factor(Treatment_pooled, levels = c("Control 1", "Control 2",
                                                                "Rocky shore 1",
                                                                "Rocky shore 2", "Modified")))


##################################################################
#Sub sampling step, reused for everything for SQ1

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

sq1_data <- bind_rows(other_rows, modified_6mo_sampled)

# Sanity check: Modified should now show 3/3 at both sampling periods, every other
# treatment should show its original (unbalanced) count, unchanged.
sq1_data |> count(Treatment_pooled, Sampling.period)

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

ggplot(sq1_data, aes(x = Treatment_pooled, y = Richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species richness: before versus after installment of LB",
       x = "Treatment", y = "Species richness") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ1_richness.png", width = 7, height = 4, dpi = 300, bg = "white")





##################################################################

#SPECIES ABUNDANCE:
ggplot(sq1_data, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species abundance: before versus after installment of LB",
       x = "Treatment", y = "Species abundance") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ1_abundance.png", width = 7, height = 4, dpi = 300, bg = "white")

#Pattern with the schooling event of Ambassis species, and now the pattern without them:

sq1_data <- sq1_data |>
  mutate(Observations_no_Ambassis = Observations - Ambassis.spp)

ggplot(sq1_data, aes(x = Treatment_pooled, y = Observations_no_Ambassis, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period) +
  scale_x_discrete(labels = treatment_labels) +
  labs(title = "Species abundance: before versus after installment of LB excluding Ambassis sp.",
       x = "Treatment", y = "Species abundance") +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ1_abundanceEXCLambassis.png", width = 7, height = 4, dpi = 300, bg = "white")

## Does overall fish activity/detectability change after installment, and does that change differ by treatment? A rising total at Modified but not at the Control/Rocky shore treatments would be an early signal that Living Boulders are attracting fish. Summed (not averaged) Observations per Treatment x Sampling.period group, using the same Observations column as the abundance plot above - this is a different SUMMARY of the same underlying data, not a different variable.

total_obs <- sq1_data |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(total_observations = sum(Observations), .groups = "drop")

total_obs

ggplot(sq1_data, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
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

#COMMUNITY COMPOSITION:

non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Treatment_pooled", "Camera.no.", "Richness", "Observations")
sp_cols <- setdiff(names(sq1_data), non_species_cols)

sp_matrix <- sq1_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sq1_data$Code

# Bray-Curtis is undefined for two all-zero samples, so true zero-catch videos are dropped
# here ONLY (not from the richness/abundance/sample-size steps above). This means the
# community composition sample size is smaller than n in the other three results above -
# state that explicitly when reporting, don't let the n's look like they should match.
zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sq1_data[!zero_rows, ]

sum(is.na(sp_matrix_nz))  # should be 0

set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)
nmds$stress #[1] 0.2077891

# Report this value when you write this up. Stress < 0.1 = great, < 0.2 = usable with
# caution, > 0.2 = interpret the ORDINATION PLOT cautiously. Either way, PERMANOVA below
# (not the NMDS plot) is the actual hypothesis test - the plot is just a visual aid.

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment_pooled <- meta_nz$Treatment_pooled
nmds_scores$Sampling.period <- meta_nz$Sampling.period

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, colour = Treatment_pooled, shape = Sampling.period)) +
  geom_point(size = 3, position = position_jitter(width = 0.04, height = 0.04, seed = 123)) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw(base_size = 11) +
  scale_colour_manual(values = treatment_colors, name = "Treatment")

ggsave("plots/SQ1_CC.png", width = 8, height = 5, dpi = 300, bg = "white")

#####

bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: does Sampling period, Treatment, or their interaction explain composition?
# The interaction term is the one that actually speaks to SQ1: it tests whether the
# CHANGE over time differs by treatment (i.e. Modified sites changing differently from
# Control/Rocky shore references), not just whether communities differ or time passed.
permanova <- adonis2(bray_dist ~ Sampling.period * Treatment_pooled,
                     data = meta_nz, permutations = 999)
permanova

permanova_terms <- adonis2(bray_dist ~ Sampling.period * Treatment_pooled,
                           data = meta_nz, permutations = 999, by = "terms")
permanova_terms

##################################################################
# Dispersion check (PERMANOVA assumption)
##################################################################
# PERMANOVA assumes similar within-group spread (dispersion) across groups. A significant
# PERMANOVA can reflect different dispersion rather than (or as well as) truly different
# community centroids - check both factors before you interpret the result above as a
# pure "location" effect.

disp_treatment <- betadisper(bray_dist, meta_nz$Treatment_pooled)
permutest(disp_treatment, permutations = 999)

disp_period <- betadisper(bray_dist, meta_nz$Sampling.period)
permutest(disp_period, permutations = 999)

# If either permutest comes back significant (p < .05), that factor's PERMANOVA result
# may partly reflect dispersion differences - report this alongside the PERMANOVA p-value
# rather than citing the p-value alone.






##################################################################
#SPECIES COMPOSITION (which species make up each treatment's community)

# Sum each species' count within every Treatment_pooled x Sampling.period group
species_sums <- sq1_data |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(across(all_of(sp_cols), sum), .groups = "drop") |>
  pivot_longer(cols = all_of(sp_cols), names_to = "Species", values_to = "Count")

# Keep the top N most abundant species (summed across the whole dataset) as their
# own category, and fold everything else into "Other" - otherwise the legend has
# one entry per species in your dataset, which usually isn't readable. Adjust
# top_n_species if you want to show more/fewer species individually.
top_n_species <- 10

top_species <- species_sums |>
  group_by(Species) |>
  summarise(total = sum(Count), .groups = "drop") |>
  slice_max(total, n = top_n_species) |>
  pull(Species)

species_sums <- species_sums |>
  mutate(Species = if_else(Species %in% top_species, Species, "Other")) |>
  group_by(Treatment_pooled, Sampling.period, Species) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  group_by(Treatment_pooled, Sampling.period) |>
  mutate(RelAbundance = Count / sum(Count)) |>
  ungroup()

ggplot(species_sums, aes(x = Treatment_pooled, y = RelAbundance, fill = Species)) +
  geom_col(position = "stack") +
  facet_wrap(~Sampling.period) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Species composition (relative abundance) by treatment",
       x = "Treatment", y = "Relative abundance") +
  theme_bw(base_size = 11)
  #scale_fill_viridis_d(option = "viridis", name = "Species")

ggsave("plots/SQ1_speciescomp.png", width = 10, height = 6, dpi = 300, bg = "white")

















##################################################################
#DRAFT, not using this anymore

#NMDS + Bray-Curtis dissimilarity matrix
library(vegan)

# 1. Identify species columns (everything that isn't metadata)
non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Camera.no.", "Richness", "Observations")
sp_cols <- setdiff(names(sp_data_Obs), non_species_cols)

# 2. Build species matrix, remove all-zero rows
sp_matrix <- sp_data_Obs |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sp_data_Obs$Code

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sp_data_Obs[!zero_rows, ]

# 3. Run NMDS
set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
#[1] 0.2208074 --> Stress value is above 0.2, which indicates that the NMDS may not be a good representation of the data. Could be usable, but interpretation should be done with caution. Lean on PERMANOVA for statistical confidence

# Check for NA values in the species matrix
sum(is.na(sp_matrix_nz)) #[1] 0

# See which rows/columns have NAs, if any
#which(rowSums(is.na(sp_matrix_nz)) > 0)
#which(colSums(is.na(sp_matrix_nz)) > 0)

# Extract site scores and attach metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment <- meta_nz$Treatment
nmds_scores$`Sampling.period` <- factor(meta_nz$`Sampling.period`,
                                        levels = c("Baseline 2", "6 months"))

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        colour = Treatment, shape = `Sampling.period`)) +
  geom_point(size = 3) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

ggsave("plots/SQ1_CC.png", width = 8, height = 5, dpi = 300, bg = "white")

# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: does Group, Sampling period, or their interaction explain composition?
permanova <- adonis2(bray_dist ~ `Sampling.period` * Treatment,
                     data = meta_nz,
                     permutations = 999)

permanova

#R2 = 0.62931, p = 0.001, so the whole model explains a significant 62,9% of the variation in       community composition.

permanova_terms <- adonis2(bray_dist ~ `Sampling.period` * Treatment,
                           data = meta_nz,
                           permutations = 999,
                           by = "terms")
permanova_terms

#Df SumOfSqs      R2      F Pr(>F)
#Sampling.period            1   0.6443 0.06711 3.2586  0.006 **
#Treatment                  6   3.5226 0.36691 2.9694  0.001 ***
#Sampling.period:Treatment  3   1.8749 0.19529 3.1609  0.001 ***
#Residual                  18   3.5590 0.37069
#Total                     28   9.6008 1.00000






#SAME AGAIN: As this is a site scale comparison, I will let R randomly select 3 datapoints for the MOD site of 6 months so it can be compared to the before MOD site.
set.seed(123)

# Step 1: recode Modified categories consistently across both time points
meta_recode <- meta_nz %>%
  mutate(Treatment_pooled = case_when(
    Treatment %in% c("Modified", "Modified Existing Revetment", "Modified Living Boulder") ~ "Modified",
    TRUE ~ Treatment))

# Step 2: randomly keep at most 3 rows per Treatment_pooled x Sampling.period group
meta_final <- meta_recode %>%
  group_by(Treatment_pooled, `Sampling.period`) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Step 3: subset the species matrix to match, using Code to keep rows aligned
sp_matrix_final <- sp_matrix_nz[rownames(sp_matrix_nz) %in% meta_final$Code, ]

# Step 4: re-order sp_matrix_final to exactly match meta_final's row order
sp_matrix_final <- sp_matrix_final[match(meta_final$Code, rownames(sp_matrix_final)), ]

# sanity check - must return TRUE before proceeding
identical(rownames(sp_matrix_final), meta_final$Code)

set.seed(123)
nmds_final <- metaMDS(sp_matrix_final, distance = "bray", k = 2, trymax = 100)
nmds_final$stress
#[1] 0.203709 better!!

nmds_scores_final <- as.data.frame(scores(nmds_final, display = "sites"))
nmds_scores_final$Treatment_pooled <- meta_final$Treatment_pooled
nmds_scores_final$`Sampling.period` <- factor(meta_final$`Sampling.period`,
                                              levels = c("Baseline 2", "6 months"))

ggplot(nmds_scores_final, aes(x = NMDS1, y = NMDS2,
                              colour = Treatment_pooled, shape = `Sampling.period`)) +
  geom_point(size = 3) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds_final$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

ggsave("plots/SQ1_CC2.png", width = 8, height = 5, dpi = 300, bg = "white")

# PERMANOVA
bray_dist_final <- vegdist(sp_matrix_final, method = "bray")

permanova2 <- adonis2(bray_dist_final ~ Treatment_pooled * `Sampling.period`,
                     data = meta_final,
                     permutations = 999)

permanova2

#Df SumOfSqs      R2      F Pr(>F)
#Model     9   5.6867 0.64848 3.2797  0.001 ***
#Residual 16   3.0825 0.35152
#Total    25   8.7692 1.00000

permanova_final <- adonis2(bray_dist_final ~ Treatment_pooled * `Sampling.period`,
                           data = meta_final,
                           permutations = 999,
                           by = "terms")
permanova_final

#Df SumOfSqs      R2      F Pr(>F)
#Treatment_pooled                  4   2.5490 0.29067 3.3077  0.001 ***
#Sampling.period                   1   0.5509 0.06283 2.8597  0.007 **
#Treatment_pooled:Sampling.period  4   2.5868 0.29498 3.3567  0.001 ***
#Residual                         16   3.0825 0.35152
#Total                            25   8.7692 1.00000



