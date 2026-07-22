#Sub Question 3: within site comparison after installment
#Within the modified site, do fish preferentially use Living Boulder rockpools as habitat, or is there a spill-over effect onto the surrounding revetment?

rm(list=ls())
library(tidyverse)
library(viridis)

#Importing data from google sheets:
MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

#SQ3 focuses on the within site comparison only, specifically the modified site after the installation of the LBs. Therefore, we can filter out the 2025 data, which leaves us only with the 2026 data. However for this SQ we only focus on the modified site, so the other sites/treatments can be filtered out.

mod_data <- MaxnData |>
  filter(Date %in% c("20/04/2026", "30/04/2026", "01/05/2026")) |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))




##################################################################
#First look at species richness, abundance, community composition:

#SPECIES RICHNESS:
richness <- mod_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "Richness"))

ggplot(richness, aes(x = Treatment, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Species Richness within the modified site",
       x = "Sampling Period",
       y = "Species Richness") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")





#SPECIES ABUNDANCE:
abundance <- mod_data |>
  select(c("Sampling period", "Date", "Site", "Treatment", "MaxN"))

ggplot(abundance, aes(x = Treatment, y = MaxN, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Species abundance within the modified site",
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
sp_cols <- setdiff(names(mod_data), non_species_cols)

# 2. Build species matrix, remove all-zero rows
sp_matrix <- mod_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- mod_data$Code

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- mod_data[!zero_rows, ]

# 3. Run NMDS
set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
#[1] 0.1687984 --> Stress value is <0.2, which is an acceptable fit. Usable for interpretation,  but still have to be cautious.

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

#Ellipses added to the NMDS plot:
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                         colour = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Treatment), type = "norm", level = 0.95, linetype = "solid") +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_bw()


# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: do these two groups (rockpool vs revetment) occupy different positions in community space?
permanova_within <- adonis2(bray_dist ~ Treatment,
                            data = meta_nz, permutations = 999)
permanova_within

#Df SumOfSqs      R2      F Pr(>F)
#Model     1  0.16303 0.06475 1.1077  0.334
#Residual 16  2.35477 0.93525
#Total    17  2.51780 1.00000

#Treatment explains only 6.5% of the variation in community composition (R² = 0.065), and this is not statistically significant (p = 0.334). So fish community composition on the rockpools doesn't look  different from the community on the surrounding revetment.

#Feeding rates --> are fish actively feeding more around LB? More bites = more foraging activity = LB functioning as foraging habitat
FeedData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=733876966&single=true&output=csv")

bites <- FeedData |>
  filter(Date %in% c("20/04/2026", "30/04/2026", "01/05/2026")) |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))

ggplot(bites, aes(x = Treatment, y = Bites, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Feeding behaviour within the modified site",
       x = "Sampling Period",
       y = "Number of bites") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")




#Number of observations --> are fish observed more frequently at Living Boulders than on the surrounding revetment?
ObsData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

observations <- ObsData |>
  filter(Date %in% c("20/04/2026", "30/04/2026", "01/05/2026")) |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))

ggplot(observations, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6)+
  labs(title = "Habitat use within the modified site",
       x = "Sampling Period",
       y = "Number of observations") +
  theme_bw()+
  scale_fill_viridis_d(option = "viridis")

