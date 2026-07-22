#Sub Question 2: between site comparison after installment
#After instalment, how do fish communities differ between modified revetments, control revetments, and natural rocky shores?

rm(list=ls())
library(tidyverse)
library(viridis)

#Importing data from google sheets:
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

#SQ2 focuses on after installation only, therefore we can filter out the baseline 2, which is the 2025 data: 18/06/2025. However, we can also filter out 30/04/2026 and 01/05/2026, because on these dates only videos for the modified sites were watched which would enlarge the sample size for those sites very much. So that leaves us with 20/04/2026!

after_data <- MaxnData |>
  filter(Date == "20/04/2026")


##################################################################
#First look at species richness, abundance, community composition:

#SPECIES RICHNESS:
richness <- after_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "Richness"))

ggplot(richness, aes(x = Treatment, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Species Richness after installment of LB",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")





#SPECIES ABUNDANCE:
abundance <- after_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "MaxN"))

ggplot(abundance, aes(x = Treatment, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Species abundance after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

#Again there is a  very high count of ambassis species, the data is a bit right-skewed so log transformation for the y-axis:
ggplot(abundance, aes(x = Treatment, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_y_log10() +
  labs(title = "Species abundance after installment of LB",
       x = "Sampling Period",
       y = "Species abundance") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")





#COMMUNITY COMPOSITION:
#NMDS + Bray-Curtis dissimilarity matrix
library(vegan)

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

