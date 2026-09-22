#Sub Question 1: within site comparison after installment
#Do fish interact differently with Living Boulders than with the surrounding existing revetment?

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)
library(patchwork)

#Importing data from google sheets:
#MaxnData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=484656251&single=true&output=csv")

ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

#SQ3 focuses on the within site comparison only, specifically the modified site after the installation of the LBs. Therefore, we can filter out the 2025 data, which leaves us only with the 2026 data. However for this SQ we only focus on the modified site, so the other sites/treatments can be filtered out.

#For the boxplots below we also pull in the pre-installation baseline for the (undifferentiated) "Modified" treatment, purely as a visual reference point - it is NOT included in the statistical tests further down (those stay post-installation-only, since the before/after question is what SQ1 already covers).

#mod_data <- MaxnData |>
#  filter(Date %in% c("20/04/2026", "30/04/2026", "01/05/2026")) |>
#  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))

mod_data <- ObsData |>
  filter((Date == "18/06/2025" & Treatment == "Modified") |
           (Date %in% c("20/04/2026", "30/04/2026", "01/05/2026") &
              Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))) |>
  mutate(Treatment = factor(Treatment,
                            levels = c("Modified", "Modified Existing Revetment",
                                       "Modified Living Boulder")))

#Post-installation-only subset: used for community composition (NMDS/PERMANOVA) and feeding rates,
#which are testing the within-site spillover question specifically.
mod_data_post <- mod_data |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder")) |>
  mutate(Treatment = droplevels(Treatment))


###########
treatment_labels <- c("Modified" = "MOD (baseline)",
                      "Modified Existing Revetment" = "MOD-ER",
                      "Modified Living Boulder" = "MOD-LB")

#Full 3-class ColorBrewer Oranges ramp: baseline Modified = lightest, MOD-ER = medium,
#MOD-LB = darkest and exactly matches the orange SQ1 used for pooled "Modified" (#E6550D),
#since LB is the focal habitat type. Flip MOD-ER/MOD-LB if you'd rather MOD-ER kept that shade.
treatment_colors <- c("Modified" = "#FEE6CE",
                      "Modified Existing Revetment" = "#FDAE6B",
                      "Modified Living Boulder" = "#E6550D")

text_theme <- theme(text = element_text(size = 11))





##################################################################

#SPECIES RICHNESS:

ggplot(mod_data, aes(x = Treatment, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Species richness within the modified site",
       subtitle = "Pre-installation baseline shown for reference",
       x = "Treatment", y = "Species richness") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_richness.png", width = 7, height = 4, dpi = 300, bg = "white")










##################################################################

#SPECIES ABUNDANCE:

ggplot(mod_data, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Number of observations within the modified site",
       subtitle = "Pre-installation baseline shown for reference",
       x = "Treatment", y = "Species abundance") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_abundance.png", width = 7, height = 4, dpi = 300, bg = "white")


#number of observations:

#Total observations per treatment, for "N = X" labels (same combined-plot approach as SQ1's abundance plot):

abundance <- mod_data |>
  select(c("Sampling.period", "Date", "Site", "Treatment", "Observations"))

total_obs <- abundance |>
  group_by(Treatment) |>
  summarise(N = sum(Observations), .groups = "drop")

ggplot(abundance, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_text(data = total_obs,
            aes(x = Treatment, y = max(abundance$Observations) * 1.05, label = paste0("N = ", N)),
            inherit.aes = FALSE, size = 3.5) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Species abundance within the modified site",
       subtitle = "Pre-installation baseline shown for reference",
       x = "Treatment", y = "Species abundance") +
  theme_bw() + text_theme





##################################################################

#COMMUNITY COMPOSITION (post-installation only):

# 1. Identify species columns (everything that isn't metadata)
non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Camera.no.", "Richness", "Observations")
sp_cols <- setdiff(names(mod_data_post), non_species_cols)

# 2. Build species matrix, remove all-zero rows
sp_matrix <- mod_data_post |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- mod_data_post$Code

zero_rows <- rowSums(sp_matrix) == 0
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- mod_data_post[!zero_rows, ]

# 3. Run NMDS
set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
#[1] 0.1655758

# Check for NA values in the species matrix
sum(is.na(sp_matrix_nz))    #[1] 0

# Extract site scores and attach metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment <- meta_nz$Treatment

#Small reproducible jitter to separate overlapping points - cosmetic only, same fix used in SQ1,
#doesn't touch the underlying Bray-Curtis/PERMANOVA calculations (those run on sp_matrix_nz/bray_dist directly):
set.seed(123)
nmds_scores_jit <- nmds_scores |>
  mutate(NMDS1 = jitter(NMDS1, amount = 0.02),
         NMDS2 = jitter(NMDS2, amount = 0.02))

ggplot(nmds_scores_jit, aes(x = NMDS1, y = NMDS2, colour = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Treatment), type = "norm", level = 0.95, linetype = "solid") +
  scale_colour_manual(values = treatment_colors, labels = treatment_labels) +
  scale_shape_discrete(labels = treatment_labels) +
  labs(title = "Community composition within the modified site",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2", colour = "Treatment", shape = "Treatment") +
  theme_bw() + text_theme

ggsave("plots/SQ3_CC.png", width = 8, height = 5, dpi = 300, bg = "white")


# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: do these two groups (rockpool vs revetment) occupy different positions in community space?
permanova_within <- adonis2(bray_dist ~ Treatment,
                            data = meta_nz, permutations = 999)
permanova_within

#Df SumOfSqs      R2      F Pr(>F)
#Model     1  0.12059 0.05847 0.9936  0.384
#Residual 16  1.94175 0.94153
#Total    17  2.06234 1.00000

#Treatment explains only 5.8% of the variation in community composition (R2 = 0.058), and this is not statistically significant (p = 0.384). So fish community composition on the rockpools doesn't look different from the community on the surrounding revetment.

#Dispersion check - added for consistency with SQ1's betadisper approach. This confirms whether the
#non-significant PERMANOVA above reflects genuine similarity in composition, rather than being masked by unequal within-group spread:

disp_within <- betadisper(bray_dist, meta_nz$Treatment)

anova(disp_within)
#Response: Distances
#Df  Sum Sq  Mean Sq F value Pr(>F)
#Groups     1 0.04811 0.048110  1.5364  0.233
#Residuals 16 0.50102 0.031314

#not significant, so dispersion (within-group spread) doesn't differ between MOD-ER and MOD-LB. The non-significant PERMANOVA above is a genuine "no difference in composition" result, not an artifact of unequal spread.

permutest(disp_within, permutations = 999)

#permutest(disp_within, 999): Df=1, F=1.5364, p=0.232 --> confirms the anova() result above.









##################################################################

#FEEDING RATES --> are fish actively feeding more around LB? More bites = more foraging activity = LB functioning as foraging habitat

#when looking at behaviour activities, it is important to keep in mind that videos where nothing happened will not appear as rows

video_ids_baseline <- mod_data |> distinct(Code, Treatment, Date)

bites <- video_ids |>
  left_join(FeedData |> select(Code, Bites), by = "Code") |>
  left_join(mod_data_post |> distinct(Code, Date), by = "Code") |>
  mutate(Bites = replace_na(Bites, 0))

#Per-day average, same reasoning as richness/abundance above.
bites_daily <- bites |>
  group_by(Treatment, Date) |>
  summarise(Bites = mean(Bites), .groups = "drop")

ggplot(bites_daily, aes(x = Treatment, y = Bites, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Feeding behaviour (intensity) within the modified site",
       subtitle = "Daily average bites, zero-filled for videos with no feeding observed",
       x = "Treatment", y = "Mean number of bites per day") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_feedbehav.png", width = 7, height = 4, dpi = 300, bg = "white")






##################################################################

#FEEDING FREQUENCY:

#BehavLongData tags the OCCURRENCE of a feeding event, which is a genuinely different metric from Bites above: this is how OFTEN fish are seen feeding per video (frequency), while Bites is how MUCH they eat when they do (intensity).

BehavLongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=855991795&single=true&output=csv")

behav_post <- BehavLongData |>
  filter(Date %in% c("20/04/2026", "30/04/2026", "01/05/2026")) |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder")) |>
  mutate(Treatment = factor(Treatment, levels = c("Modified Existing Revetment", "Modified Living Boulder")))

behav_sums <- behav_post |>
  group_by(OpCode, Activity) |>
  summarise(count = sum(count), .groups = "drop")

#Sanity check before trusting the zero-fill below: Code (ObsData/FeedData) and OpCode (BehavLongData) need to refer to the same videos for this join to be meaningful. Run this - it should return 0 rows, or only videos you know genuinely had zero behavioural events recorded:
anti_join(video_ids, behav_sums, by = c("Code" = "OpCode")) |> distinct(Code)

#Covers Passing/Transient too (used in the "OTHER BEHAVIOURS" section further down) - computed once
#here so it isn't rebuilt twice.
behav_per_video <- video_ids |>
  crossing(Activity = c("Passing", "Transient", "Feeding")) |>
  left_join(behav_sums, by = c("Code" = "OpCode", "Activity")) |>
  mutate(count = replace_na(count, 0))

feeding_events <- behav_per_video |> filter(Activity == "Feeding")

ggplot(feeding_events, aes(x = Treatment, y = count, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Feeding frequency within the modified site",
       subtitle = "Number of feeding events per video (frequency, not bite count)",
       x = "Treatment", y = "Number of feeding events") +
  theme_bw() + text_theme

ggsave("plots/SQ3_feedfreq.png", width = 7, height = 4, dpi = 300, bg = "white")



##################################################################

#BEHAVIOUR ACTIVITIES:

ggplot(behav_per_video, aes(x = Treatment, y = count, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Activity, scales = "free_y") +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Behavioural activity within the modified site",
       subtitle = "Passing, Transient, and Feeding events per video",
       x = "Treatment", y = "Number of events") +
  theme_bw() + text_theme

#ggsave("plots/SQ3_behav_overview.png", width = 9, height = 4, dpi = 300, bg = "white")
behav_composition <- behav_per_video |>
  group_by(Treatment, Activity) |>
  summarise(total = sum(count), .groups = "drop")

ggplot(behav_composition, aes(x = Treatment, y = total, fill = Activity)) +
  geom_col(position = "fill", alpha = 0.85) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Behavioural composition within the modified site",
       subtitle = "Share of tagged events by activity type, per treatment",
       x = "Treatment", y = "Proportion of events") +
  theme_bw() + text_theme

#Not interesting

#ggsave("plots/SQ3_behav_composition.png", width = 7, height = 4, dpi = 300, bg = "white"


