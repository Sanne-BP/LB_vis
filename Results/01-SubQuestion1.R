#Sub Question 1: temporal - before versus after installment of LB
#How do fish communities change over time following the installment of LB, compared to control revetments and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)

#Importing data from google sheets:
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

#Filter data for only the relevant dates: 18/06/2025 and 20/04/2026
sp_data_Maxn <- MaxnData |>
  filter(Date %in% c("18/06/2025", "20/04/2026"))

sp_data_Obs <- ObsData |>
  filter(Date %in% c("18/06/2025", "20/04/2026"))





##################################################################
#First look at species richness, abundance, community composition:

#SPECIES RICHNESS:
richness <- sp_data_Obs |>
  select(c("Sampling.period", "Date", "Site", "Treatment", "Richness")) |>
  mutate(`Sampling.period` = factor(`Sampling.period`, levels = c("Baseline 2", "6 months")))

ggplot(richness, aes(x = Treatment, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period)+
  scale_x_discrete(labels = c("Control 1" = "C1",
                              "Control 2" = "C2",
                              "Modified" = "MOD",
                              "Modified Existing Revetment" = "MOD-ER",
                              "Modified Living Boulder" = "MOD-LB",
                              "Rocky shore 1" = "RS1",
                              "Rocky shore 2" = "RS2")) +
  labs(title = "Species richness: before versus after instalment of LB",
       x = "Treatment", y = "Species richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggsave("plots/SQ1_richness.png", width = 11, height = 6, dpi = 300, bg = "white")


#As this is a site scale comparison, I will let R randomly select 3 datapoints for the MOD site of 6 months so it can be compared to the before MOD site.
set.seed(123)  # For reproducibility

richness_pooled_mod <- richness %>%
  mutate(Treatment_pooled = case_when(
    Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder") ~ "Modified",
    TRUE ~ Treatment),
  `Sampling.period` = factor(`Sampling.period`, levels = c("Baseline 2", "6 months")))

# Separate the pooled "Modified, 6 months" rows from everything else
modified_6mo <- richness_pooled_mod %>%
  filter(Treatment_pooled == "Modified", `Sampling.period` == "6 months")

other_rows <- richness_pooled_mod %>%
  filter(!(Treatment_pooled == "Modified" & `Sampling.period` == "6 months"))

# Randomly sample 3 of the 6 pooled Modified rows
modified_6mo_sampled <- modified_6mo %>% slice_sample(n = 3)

# Recombine
richness_final <- bind_rows(other_rows, modified_6mo_sampled)

richness_final %>% count(Treatment_pooled, `Sampling.period`)  # check: Modified should now show 3/3

#plot again:
ggplot(richness_final, aes(x = Treatment_pooled, y = Richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period)+
  scale_x_discrete(labels = c("Control 1" = "C1",
                              "Control 2" = "C2",
                              "Modified" = "MOD",
                              "Modified Existing Revetment" = "MOD-ER",
                              "Modified Living Boulder" = "MOD-LB",
                              "Rocky shore 1" = "RS1",
                              "Rocky shore 2" = "RS2")) +
  labs(title = "Species richness: before versus after instalment of LB",
       x = "Treatment", y = "Species richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggsave("plots/SQ1_richness2.png", width = 11, height = 6, dpi = 300, bg = "white")





#SPECIES ABUNDANCE:
abundance <- sp_data_Obs |>
  select(c("Sampling.period", "Date", "Site", "Treatment", "Observations")) |>
  mutate(`Sampling.period` = factor(`Sampling.period`, levels = c("Baseline 2", "6 months")))

ggplot(abundance, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~`Sampling.period`)+
  labs(title = "Species abundance before versus after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggplot(abundance, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period)+
  scale_x_discrete(labels = c("Control 1" = "C1",
                              "Control 2" = "C2",
                              "Modified" = "MOD",
                              "Modified Existing Revetment" = "MOD-ER",
                              "Modified Living Boulder" = "MOD-LB",
                              "Rocky shore 1" = "RS1",
                              "Rocky shore 2" = "RS2")) +
  labs(title = "Species abundance: before versus after instalment of LB",
       x = "Treatment", y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggsave("plots/SQ1_abundance.png", width = 11, height = 6, dpi = 300, bg = "white")


#As this is a site scale comparison, I will let R randomly select 3 datapoints for the MOD site of 6 months so it can be compared to the before MOD site.
set.seed(123)  # For reproducibility

abundance_pooled_mod <- abundance %>%
  mutate(Treatment_pooled = case_when(
    Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder") ~ "Modified",
    TRUE ~ Treatment),
    `Sampling.period` = factor(`Sampling.period`, levels = c("Baseline 2", "6 months")))

# Separate the pooled "Modified, 6 months" rows from everything else
modifiedA_6mo <- abundance_pooled_mod %>%
  filter(Treatment_pooled == "Modified", `Sampling.period` == "6 months")

other_rowsA <- abundance_pooled_mod %>%
  filter(!(Treatment_pooled == "Modified" & `Sampling.period` == "6 months"))

# Randomly sample 3 of the 6 pooled Modified rows
modified_6mo_sampledA <- modifiedA_6mo %>% slice_sample(n = 3)

# Recombine
abundance_final <- bind_rows(other_rowsA, modified_6mo_sampledA)

abundance_final %>% count(Treatment_pooled, `Sampling.period`)  # check: Modified should now show 3/3

#plot again:
ggplot(abundance_final, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Sampling.period)+
  scale_x_discrete(labels = c("Control 1" = "C1",
                              "Control 2" = "C2",
                              "Modified" = "MOD",
                              "Modified Existing Revetment" = "MOD-ER",
                              "Modified Living Boulder" = "MOD-LB",
                              "Rocky shore 1" = "RS1",
                              "Rocky shore 2" = "RS2")) +
  labs(title = "Species richness: before versus after instalment of LB",
       x = "Treatment", y = "Species richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggsave("plots/SQ1_abundance2.png", width = 11, height = 6, dpi = 300, bg = "white")






#COMMUNITY COMPOSITION:
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



