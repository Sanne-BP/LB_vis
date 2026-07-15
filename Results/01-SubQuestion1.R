#Sub question 1: temporal - before versus after installment of LB
#How do fish communities change over time following the installment of LB, compared to control revetments and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)

#Importing data from google sheets:
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

#Filter data for only the relevant dates: 18/06/2025 and 20/04/2026
sp_data <- MaxnData |>
  filter(Date %in% c("18/06/2025", "20/04/2026"))



##################################################################
#First look at species richness, abundance, community composition:

#SPECIES RICHNESS:
richness <- sp_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "Richness")) |>
  mutate(`Sampling period` = factor(`Sampling period`, levels = c("Baseline 2", "6 months")))


ggplot(richness, aes(x = `Sampling period`, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Species Richness before versus after installment of LB",
     x = "Sampling Period",
     y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

ggplot(richness, aes(x = Treatment, y = Richness)) +
  geom_boxplot() +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species Richness before versus after installment of LB",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_minimal()

ggplot(richness, aes(x = Site, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species Richness before versus after installment of LB",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")





#SPECIES ABUNDANCE:
abundance <- sp_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "MaxN"))|>
  mutate(`Sampling period` = factor(`Sampling period`, levels = c("Baseline 2", "6 months")))

ggplot(abundance, aes(x = Site, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species abundance before versus after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#due to very very high count of ambassis species, the data is a bit right-skewed so log transformation for the y-axis:
ggplot(abundance, aes(x = Site, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_y_log10() +
  facet_wrap(~`Sampling period`)+
  labs(title = "Species abundance before versus after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#lets try removing ambassis species:????





#COMMUNITY COMPOSITION:
#NMDS + Bray-Curtis dissimilarity matrix
library(vegan)

# 1. Identify species columns (everything that isn't metadata)
non_species_cols <- c("Code", "Sampling period", "Date", "Site", "Treatment",
                      "Camera no.", "TapeReader", "Richness", "MaxN")
sp_cols <- setdiff(names(sp_data), non_species_cols)

# 2. Build species matrix, remove all-zero rows
sp_matrix <- sp_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sp_data$Code

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sp_data[!zero_rows, ]

# 3. Run NMDS
set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
#[1] 0.2344393 --> Stress value is above 0.2, which indicates that the NMDS may not be a good representation of the data. Could be usable, but interpretation should be done with caution. Lean on PERMANOVA for statistical confidence

# Check for NA values in the species matrix
sum(is.na(sp_matrix_nz))

# See which rows/columns have NAs, if any
which(rowSums(is.na(sp_matrix_nz)) > 0)
which(colSums(is.na(sp_matrix_nz)) > 0)

# Extract site scores and attach metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment <- meta_nz$Treatment
nmds_scores$`Sampling period` <- factor(meta_nz$`Sampling period`,
                                        levels = c("Baseline 2", "6 months"))

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        colour = Treatment, shape = `Sampling period`)) +
  geom_point(size = 3) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

#It also has a lot of categories right now, so perhaps it’s a good idea to collapse treatment into 3 groups only? Control, rocky shore and modified?

meta_nz2 <- meta_nz |>
  mutate(Group = case_when(
    Treatment %in% c("Rocky shore 1", "Rocky shore 2") ~ "Natural rocky shore",
    Treatment %in% c("Control 1", "Control 2") ~ "Control site",
    Treatment %in% c("Modified", "Modified Existing Revetment", "Modified Living Boulder") ~ "Modified (Living Boulder site)",
    TRUE ~ NA_character_))

# sanity check - make sure nothing fell through as NA
table(meta_nz2$Group, useNA = "always")

nmds_scores2 <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores2$Group <- meta_nz2$Group
nmds_scores2$`Sampling period` <- factor(meta_nz2$`Sampling period`,
                                        levels = c("Baseline 2", "6 months"))

ggplot(nmds_scores2, aes(x = NMDS1, y = NMDS2,
                        colour = Group, shape = `Sampling period`)) +
  geom_point(size = 3) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

#Ellipses added to the NMDS plot:
ggplot(nmds_scores2, aes(x = NMDS1, y = NMDS2,
                        colour = Group, shape = `Sampling period`)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Group), type = "norm", level = 0.95, linetype = "solid") +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

ggplot(nmds_scores2, aes(x = NMDS1, y = NMDS2,
                         colour = Group, shape = `Sampling period`)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Group, colour = Group),
               type = "norm", level = 0.95, linetype = "solid", linewidth = 0.7) +
  stat_ellipse(aes(group = `Sampling period`, linetype = `Sampling period`),
               type = "norm", level = 0.95, colour = "grey40", linewidth = 0.5) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()

#ellipse per group and sampling period:
ggplot(nmds_scores2, aes(x = NMDS1, y = NMDS2,
                         colour = Group, shape = `Sampling period`)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Group), type = "norm", level = 0.95, linetype = "solid") +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()


# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: does Group, Sampling period, or their interaction explain composition?
permanova <- adonis2(bray_dist ~ Group * `Sampling period`,
                     data = meta_nz2,
                     permutations = 999)

permanova

#R2 = 0.289, p = 0.01, so the whole model explains a significant 28.9% of the variation in       community composition.

permanova_terms <- adonis2(bray_dist ~ Group * `Sampling period`,
                           data = meta_nz2,
                           permutations = 999,
                           by = "terms")
permanova_terms

#                         Df SumOfSqs      R2       F Pr(>F)
#Group                     2   0.9691 0.09861 1.5952  0.091 .
#`Sampling period`         1   0.6720 0.06837 2.2122  0.047 *
# Group:`Sampling period`  2   1.2004 0.12214 1.9759  0.034 *
# Residual                23   6.9865 0.71088
#Total                    28   9.8280 1.00000

#Group R2 = 0.09861, p = 0.091, so the effect of Group alone is not significant.
#Sampling period R2 = 0.06837, p = 0.047, so the effect of Sampling period alone is significant. So, fish communities did change from the baseline period to 6 months OVERALL, across all sites combined
#Group:Sampling period R2 = 0.12214, p = 0.034, so the interaction effect is significant. This means that the change in fish communities from baseline to 6 months depends on the Group (Control, Natural rocky shore, Modified). In other words, the effect of time on community composition is different for different groups.



