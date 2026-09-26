#Sub Question 1: within site comparison after installment
#Do fish interact differently with Living Boulders than with the surrounding existing revetment?

##Response variables: species richness, number of observations, community composition, feeding rates

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)
library(patchwork)

#Importing data from google sheets:
ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

FeedData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=733876966&single=true&output=csv")



#SQ1 focuses on the within site comparison only, specifically the modified site after the installation of the LBs. Therefore, we can filter out the 2025 data, which leaves us only with the 2026 data. However for this SQ we only focus on the modified site, so the other sites/treatments can be filtered out.

#For the boxplots below we also pull in the pre-installation baseline for the (undifferentiated) "Modified" treatment, purely as a visual reference point - it is NOT included in the statistical tests further down (those stay post-installation-only, since the before/after question is what SQ1 already covers).


mod_data <- ObsData |>
  filter((Date == "18/06/2025" & Treatment == "Modified") |
           (Date %in% c("20/04/2026", "30/04/2026", "01/05/2026") &
              Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder"))) |>
  mutate(Treatment = factor(Treatment,
                            levels = c("Modified", "Modified Existing Revetment",
                                       "Modified Living Boulder")))

#EXCLUDE AMBASSIS: Ambassis spp. are removed from all analyses (large transient schools dominate the counts).
#Richness and Observations in ObsData include Ambassis, so they are recalculated from the remaining species.

non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Camera.no.", "Richness", "Observations")

#CHECK first: does recalculating from the species columns reproduce the original columns?
#Both should be TRUE - otherwise Richness/Observations are defined differently and we need to look at it.
all_sp_cols <- setdiff(names(mod_data), non_species_cols)
mod_data |>
  summarise(richness_matches     = all(rowSums(across(all_of(all_sp_cols)) > 0) == Richness),
            observations_matches = all(rowSums(across(all_of(all_sp_cols))) == Observations))

ambassis_cols <- grep("^Ambassis", names(mod_data), value = TRUE)
ambassis_cols                                    # CHECK: should only list the Ambassis column(s)

mod_data <- mod_data |>
  select(-all_of(ambassis_cols))

sp_cols <- setdiff(names(mod_data), non_species_cols)

mod_data <- mod_data |>
  mutate(Richness     = rowSums(across(all_of(sp_cols)) > 0),   # number of species present
         Observations = rowSums(across(all_of(sp_cols))))       # total observations

#Post-installation-only subset: used for community composition (NMDS/PERMANOVA) and feeding rates,
#which are testing the within-site spillover question specifically.
mod_data_post <- mod_data |>
  filter(Treatment %in% c("Modified Existing Revetment", "Modified Living Boulder")) |>
  mutate(Treatment = droplevels(Treatment))



#PER-DAY AVERAGES: videos from the same day are not independent (same tide, weather, visibility, fish present that day), so richness and number of observations are averaged per treatment per day. Each box is therefore based on the daily means (one value per day).
mod_daily <- mod_data_post |>
  group_by(Treatment, Date) |>
  summarise(n_videos = n(),
            Richness = mean(Richness),
            Observations = mean(Observations),
            .groups = "drop")
mod_daily

#Baseline reference: the baseline is a single day, so its daily mean = the mean of the baseline videos. Shown as a dotted line in the plots.
baseline_ref <- mod_data |>
  filter(Treatment == "Modified") |>
  summarise(Richness = mean(Richness),
            Observations = mean(Observations))
baseline_ref



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

#Dotted baseline line, reused in every plot (gives its own legend entry)
baseline_linetype <- scale_linetype_manual(name = NULL, values = c("MOD (baseline)" = "dotted"))













##################################################################

#SPECIES RICHNESS:

p_rich <- ggplot(mod_daily, aes(x = Treatment, y = Richness, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_hline(data = baseline_ref, aes(yintercept = Richness, linetype = "MOD (baseline)"),
             colour = "grey30", linewidth = 0.7) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  baseline_linetype +
  labs(title = "Species richness within the modified site",
       subtitle = "Pre-installation baseline shown as dotted line",
       x = "Treatment", y = "Mean species richness per day") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_richness.png", width = 7, height = 4, dpi = 300, bg = "white")














##################################################################

#NUMBER OF OBSERVATIONS:

p_obs <- ggplot(mod_daily, aes(x = Treatment, y = Observations, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_hline(data = baseline_ref, aes(yintercept = Observations, linetype = "MOD (baseline)"),
             colour = "grey30", linewidth = 0.7) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  baseline_linetype +
  labs(title = "Number of observations within the modified site",
       subtitle = "Pre-installation baseline shown as dotted line",
       x = "Treatment", y = "Mean number of observations per day") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_observations.png", width = 7, height = 4, dpi = 300, bg = "white")












#Put together the 2 graphs for report:
p_rich + p_obs +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("New_Plots/SQ1_rich_obs_combi.png", width = 9, height = 6, dpi = 300, bg = "white")













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
#[1] 0.1757859

# Check for NA values in the species matrix
sum(is.na(sp_matrix_nz))    #[1] 0

# Extract site scores and attach metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment <- meta_nz$Treatment

#Small reproducible jitter to separate overlapping points - cosmetic only,
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

ggsave("New_Plots/SQ1_CC.png", width = 8, height = 5, dpi = 300, bg = "white")



# Bray-Curtis distance matrix on the same non-zero species matrix used for NMDS
bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: do these two groups (rockpool vs revetment) occupy different positions in community space?
permanova_within <- adonis2(bray_dist ~ Treatment,
                            data = meta_nz, permutations = 999)
permanova_within

#Df SumOfSqs      R2      F Pr(>F)
#Model     1   0.0296 0.01766 0.2877  0.893
#Residual 16   1.6461 0.98234
#Total    17   1.6757 1.00000

#Treatment explains only 1.8% of the variation in community composition (R2 = 0.01766), and this is not statistically significant (p = 0.893). So fish community composition on the rockpools doesn't look different from the community on the surrounding revetment.

#Dispersion check - added for consistency with SQ1's betadisper approach. This confirms whether the
#non-significant PERMANOVA above reflects genuine similarity in composition, rather than being masked by unequal within-group spread:

disp_within <- betadisper(bray_dist, meta_nz$Treatment)

anova(disp_within)
#Response: Distances
#Df  Sum Sq   Mean Sq F value Pr(>F)
#Groups     1 0.00008 0.0000764  0.0033 0.9552
#Residuals 16 0.37537 0.0234609

#So, not significant, so dispersion (within-group spread) doesn't differ between MOD-ER and MOD-LB. The non-significant PERMANOVA above is a genuine "no difference in composition" result, not an artifact of unequal spread.

permutest(disp_within, permutations = 999)

#permutest(disp_within, 999): Df=1, F=0.0033, p=0.962 --> confirms the anova() result above.






















##################################################################

#FEEDING RATES --> are fish actively feeding more around LB? More bites = more foraging activity = LB functioning as foraging habitat

#when looking at behaviour activities, it is important to keep in mind that videos where nothing happened will not appear as rows


video_ids <- mod_data_post |> distinct(Code, Treatment, Date)
video_ids_baseline <- mod_data |> filter(Treatment == "Modified") |> distinct(Code, Treatment, Date)

bites <- video_ids |>
  left_join(FeedData |> select(Code, Bites), by = "Code") |>
  mutate(Bites = replace_na(Bites, 0))

#Per-day average, same reasoning as richness/abundance above.
bites_daily <- bites |>
  group_by(Treatment, Date) |>
  summarise(Bites = mean(Bites), .groups = "drop")

#Baseline reference (dotted line). NB: if the baseline videos were never scored for bites, this will be 0 - check that the baseline is in FeedData:
FeedData |> filter(Date == "18/06/2025") |> nrow()

baseline_bites <- video_ids_baseline |>
  left_join(FeedData |> select(Code, Bites), by = "Code") |>
  mutate(Bites = replace_na(Bites, 0)) |>
  summarise(Bites = mean(Bites))
baseline_bites

ggplot(bites_daily, aes(x = Treatment, y = Bites, fill = Treatment)) +
  geom_boxplot(alpha = 0.6) +
  #geom_hline(data = baseline_bites, aes(yintercept = Bites, linetype = "MOD (baseline)"),
             #colour = "grey30", linewidth = 0.7) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Feeding behaviour within the modified site",
       x = "Treatment", y = "Mean number of bites per day") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ1_feedbehav.png", width = 7, height = 4, dpi = 300, bg = "white")












##################################################################

#SPECIES COMPOSITION (post-installation only):
#Which species make up the observations in each treatment? Per-day averaged like the other plots:
#mean count per video per day -> mean over the days -> share of the total per treatment.
#The 8 most observed species are shown separately, the rest are grouped as "Other".

# Species lookup table + fixed viridis colours for species composition plots
# Source this at the top of each SQ script:  source("Results/00-species_colours.R")
# Every species keeps the SAME colour in every plot, even if some species are absent.

species_info <- tribble(
  ~Species,                    ~Feeding_mode,
  "Abudefduf.sexfasciatus",    "Planktivore",
  "Ambassis.spp",              "Planktivore",
  "Trachinops.taeniatus",      "Planktivore",
  "Istiblennius.edentulus",    "Herbivore",
  "Mugil.cephalus",            "Herbivore",
  "Girella.elevata",           "Omnivore",
  "Girella.tricuspidata",      "Omnivore",
  "Meuschenia.trachylepis",    "Omnivore",
  "Microcanthus.strigatus",    "Omnivore",
  "Monacanthus.chinensis",     "Omnivore",
  "Monodactylus.argenteus",    "Omnivore",
  "Omobranchus.anolius",       "Omnivore",
  "Omobranchus.rotundiceps",   "Omnivore",
  "Scobinichthys.granulatus",  "Omnivore",   # TO CHECK: predator or omnivore?
  "Acanthopagrus.australis",   "Predator",
  "Acanthopagrus.butcheri",    "Predator",
  "Achoerodus.viridis",        "Predator",
  "Favonigobius.exquisitus",   "Predator",
  "Favonigobius.lentiginosus", "Predator",
  "Gerres.subfasciatus",       "Predator",
  "Parupeneus.spilurus",       "Predator",
  "Pseudocaranx.georgianus",   "Predator",
  "Tetractenos.glaber",        "Predator",
  "Tetractenos.hamiltoni",     "Predator"
) |>
  mutate(Feeding_mode = factor(Feeding_mode,
                               levels = c("Planktivore", "Herbivore", "Omnivore", "Predator")),
         Label = str_replace(Species, "\\.", " ") |> str_replace(" spp$", " spp.")) |>
  arrange(Feeding_mode, Species)

#Fixed viridis colour per species, ordered by feeding guild (planktivores = purple ... predators = yellow)
species_colours <- setNames(viridis(nrow(species_info)), species_info$Species)
species_labels  <- setNames(species_info$Label, species_info$Species)

#CHECK: every species column in mod_data_post should be in the lookup table (should return character(0))
setdiff(sp_cols, species_info$Species)

comp_data <- mod_data_post |>
  pivot_longer(any_of(species_info$Species),
               names_to = "Species", values_to = "Count") |>
  # 1. mean count per video, per day
  group_by(Treatment, Date, Species) |>
  summarise(day_mean = mean(Count, na.rm = TRUE), .groups = "drop") |>
  # 2. mean over the days
  group_by(Treatment, Species) |>
  summarise(mean_count = mean(day_mean), .groups = "drop") |>
  # 3. share of the total per treatment
  group_by(Treatment) |>
  mutate(share = mean_count / sum(mean_count)) |>
  ungroup()

#The 8 most observed species at the modified site (summed over MOD-ER and MOD-LB)
top10 <- comp_data |>
  group_by(Species) |>
  summarise(total = sum(mean_count), .groups = "drop") |>
  slice_max(total, n = 10, with_ties = FALSE) |>
  pull(Species)
top10

#Group the rest as "Other"; stack order follows the lookup table (grouped by feeding mode)
comp_plot <- comp_data |>
  mutate(Species_group = if_else(Species %in% top10, Species, "Other")) |>
  group_by(Treatment, Species_group) |>
  summarise(share = sum(share), .groups = "drop") |>
  filter(share > 0) |>
  mutate(Species_group = factor(Species_group,
                                levels = c(intersect(species_info$Species, top10), "Other")))

#Fixed colours for the top 8, grey for "Other"
comp_colours <- c(species_colours[top10], Other = "grey70")
comp_labels  <- c(species_labels[top10],  Other = "Other")

ggplot(comp_plot, aes(x = Treatment, y = share, fill = Species_group)) +
  #geom_col(colour = "white", linewidth = 0.2, width = 0.6) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = comp_colours, labels = comp_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Species composition within the modified site",
       x = "Treatment", y = "Share of observations", fill = "Species") +
  theme_bw() + text_theme +
  theme(legend.text = element_text(face = "italic"))

ggsave("New_Plots/SQ1_species_composition.png", width = 7, height = 5, dpi = 300, bg = "white")



#Percentages exactly as shown in the plot (top 8 + "Other")
comp_plot |>
  arrange(Treatment, desc(share)) |>
  mutate(Percent = round(share * 100, 1)) |>
  print(n = 21)

#Percentages per feeding guild
comp_data |>
  left_join(species_info, by = "Species") |>
  group_by(Treatment, Feeding_mode) |>
  summarise(Percent = round(sum(share) * 100, 1), .groups = "drop") |>
  arrange(Treatment, desc(Percent))















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


