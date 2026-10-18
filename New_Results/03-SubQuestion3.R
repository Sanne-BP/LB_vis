#Sub Question 3: temporal - before versus after installment of LB
#How do fish communities change over time following the installment of LB, compared to control revetments and natural rocky shores?

##Response variables: species richness, number of observations, community composition (incl. feeding modes +
##cryptobenthic fish, as in SQ2), species composition, indicator species, feeding rates (bites)

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)
library(patchwork)
library(indicspecies)

#Importing data from google sheets:
ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

FeedData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=733876966&single=true&output=csv")



#Filter data for only the relevant dates: 18/06/2025 (Baseline 2) and 20/04/2026 (6 months) + pool together data from modified site as it is between site selection.
#30/04/2026 and 01/05/2026 are left out, because on these dates only the modified site was watched (same as SQ2).

#Both sampling periods are ONE day each, so there is no per-day averaging here (same as SQ2): each video is one replicate.

site_levels <- c("Control 1", "Control 2", "Rocky shore 1", "Rocky shore 2", "Modified")

sp_data_Obs <- ObsData |>
  filter(Date %in% c("18/06/2025", "20/04/2026")) |>
  mutate(Sampling.period = factor(Sampling.period, levels = c("Baseline 2", "6 months")),
         Treatment_pooled = case_when(Treatment %in% c("Modified", "Modified Existing Revetment",
                                                       "Modified Living Boulder") ~ "Modified",
                                      TRUE ~ Treatment),
         Treatment_pooled = factor(Treatment_pooled, levels = site_levels))

#CHECK: Modified at Baseline 2 should be 3. In the May 2026 CSV one baseline MOD video (SW_MOD_B2_18June2025)
#had the date typed as 18/06/2026 - if you see 2 here, fix that date in the Google Sheet.
sp_data_Obs |> count(Treatment_pooled, Sampling.period)



#EXCLUDE AMBASSIS: Ambassis spp. are removed from all analyses (large transient schools dominate the counts).
#Richness and Observations in ObsData include Ambassis, so they are recalculated from the remaining species.

non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Treatment_pooled", "Camera.no.", "Richness", "Observations")

#CHECK first: does recalculating from the species columns reproduce the original columns?
#Both should be TRUE - otherwise Richness/Observations are defined differently and we need to look at it.
all_sp_cols <- setdiff(names(sp_data_Obs), non_species_cols)
sp_data_Obs |>
  summarise(richness_matches     = all(rowSums(across(all_of(all_sp_cols)) > 0) == Richness),
            observations_matches = all(rowSums(across(all_of(all_sp_cols))) == Observations))

ambassis_cols <- grep("^Ambassis", names(sp_data_Obs), value = TRUE)
ambassis_cols                                    # CHECK: should only list the Ambassis column(s)

#NB: "Unidentified" stays in: it is one (unknown) species, so it counts for richness and observations.
sp_data_Obs <- sp_data_Obs |>
  select(-all_of(ambassis_cols))

sp_cols <- setdiff(names(sp_data_Obs), non_species_cols)

sp_data_Obs <- sp_data_Obs |>
  mutate(Richness     = rowSums(across(all_of(sp_cols)) > 0),   # number of species present
         Observations = rowSums(across(all_of(sp_cols))))       # total observations



##################################################################
#Sub sampling step, reused for everything for SQ3

# At 6 months, "Modified" is split into MOD-ER and MOD-LB (two habitats sampled at the
# modified site), so pooling them gives 6 rows for Modified x 6 months, vs. only 3 at
# baseline (pre-split, one "Modified" category).SO, we randomly keep 3 of those 6, so Modified is balanced at n=3 per Sampling.period like every other treatment.

# This subsample is done ONCE here (not separately per analysis), so every analysis below
# describes the exact same set of replicate videos, instead of independently-drawn random subsets.

set.seed(123)

modified_6mo <- sp_data_Obs |>
  filter(Treatment_pooled == "Modified", Sampling.period == "6 months")

other_rows <- sp_data_Obs |>
  filter(!(Treatment_pooled == "Modified" & Sampling.period == "6 months"))

modified_6mo_sampled <- modified_6mo |> slice_sample(n = 3)

sq3_data <- bind_rows(other_rows, modified_6mo_sampled)

# Sanity check: Modified should now show 3/3 at both sampling periods, every other
# treatment should show its original (unbalanced) count, unchanged (C1 baseline = 2, RS1 6 months = 2).
sq3_data |> count(Treatment_pooled, Sampling.period)
modified_6mo_sampled |> count(Treatment)         # which MOD habitats ended up in the subsample



##################################################################
#Shared plot settings

treatment_labels <- c("Control 1" = "C1", "Control 2" = "C2", "Modified" = "MOD",
                      "Rocky shore 1" = "RS1", "Rocky shore 2" = "RS2")

# Controls share a blue family (dark = C1, light = C2), rocky shores share a green
# family (dark = RS1, light = RS2), and Modified gets its own distinct colour since
# it's the treatment of primary interest. Reused across every plot below that fills
# or colours by Treatment_pooled, so colour-coding stays consistent across figures.
treatment_colors <- c("Control 1" = "#08519C", "Control 2" = "#3182BD",
                      "Rocky shore 1" = "#238B45", "Rocky shore 2" = "#74C476",
                      "Modified" = "#E6550D")

text_theme <- theme(text = element_text(size = 11))


# Species lookup table + fixed viridis colours for species composition plots (identical to SQ1 + SQ2)
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

#Feeding mode colours: 4 viridis colours in the same order as the species colours
guild_colours <- setNames(viridis(4), c("Planktivore", "Herbivore", "Omnivore", "Predator"))

#CHECK: every species column in sq3_data should be in the lookup table (should return "Unidentified" only)
setdiff(sp_cols, species_info$Species)

#Long format (one row per video x species), reused by the species-level plots below
sq3_long <- sq3_data |>
  pivot_longer(all_of(sp_cols), names_to = "Species", values_to = "Count") |>
  left_join(species_info, by = "Species") |>
  filter(!is.na(Feeding_mode))      # removes Unidentified (and any other unknown column) from the species plots














##################################################################

#SPECIES RICHNESS:

#Baseline and 6 months side by side per treatment: Baseline C1, 6 months C1, Baseline C2, ...
#Each treatment gets its own small panel (treatment name underneath), with "Baseline" and "6 months" written on the x-axis,
#so the sampling period can be read directly from the axis instead of from a legend.
#Same treatment colour for both boxes (baseline lighter), and the individual videos are shown as points,
#because with only 2-3 videos per box a boxplot alone can look like a flat line.

period_labels <- c("Baseline 2" = "Baseline", "6 months" = "6 months")

period_boxplot <- list(
  geom_boxplot(width = 0.6, outlier.shape = NA),
  #geom_point(position = position_jitter(width = 0.08, height = 0, seed = 123),
  #           size = 1.6, alpha = 1, colour = "grey15", show.legend = FALSE),
  facet_grid(~ Treatment_pooled, switch = "x",
             labeller = as_labeller(treatment_labels)),
  scale_x_discrete(labels = period_labels),
  scale_fill_manual(values = treatment_colors, guide = "none"),        # treatment is already named under each panel
  scale_alpha_manual(values = c("Baseline 2" = 0.3, "6 months" = 0.85), guide = "none"),
  expand_limits(y = 0),
  theme_bw(),
  text_theme,
  theme(strip.placement = "outside",                                   # treatment labels below the Baseline/6 months labels
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold"),
        panel.spacing = unit(0.3, "lines"),
        axis.text.x = element_text(size = 9),
        panel.grid.major.x = element_blank())
)

p_rich <- ggplot(sq3_data, aes(x = Sampling.period, y = Richness,
                               fill = Treatment_pooled, alpha = Sampling.period)) +
  period_boxplot +
  labs(title = "Species richness: before versus after installment of LB",
       x = NULL, y = "Species richness")

ggsave("New_Plots/SQ3_richness.png", width = 8, height = 4, dpi = 300, bg = "white")















##################################################################

#NUMBER OF OBSERVATIONS:

## Does overall fish activity/detectability change after installment, and does that change differ by treatment? A rising total at Modified but not at the Control/Rocky shore treatments would be an early signal that Living Boulders are attracting fish. Summed (not averaged) Observations per Treatment x Sampling.period group.

total_obs <- sq3_data |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(n_videos = n(),
            total_observations = sum(Observations), .groups = "drop")

total_obs

p_obs <- ggplot(sq3_data, aes(x = Sampling.period, y = Observations,
                              fill = Treatment_pooled, alpha = Sampling.period)) +
  period_boxplot +
  #geom_text(data = total_obs,
            #aes(x = Sampling.period, y = Inf, label = paste0("N = ", total_observations)),
            #inherit.aes = FALSE, vjust = 1.5, size = 3) +
  labs(title = "Number of observations: before versus after installment of LB",
       x = NULL, y = "Number of observations")

ggsave("New_Plots/SQ3_observations.png", width = 8, height = 4, dpi = 300, bg = "white")



#Put together the 2 graphs for report (same as SQ1 + SQ2):
p_rich / p_obs

ggsave("New_Plots/SQ3_rich_obs_combi.png", width = 8, height = 8, dpi = 300, bg = "white")












##################################################################

#COMMUNITY COMPOSITION:

sp_matrix <- sq3_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sq3_data$Code

#Sanity check: every column should be numeric species counts.
stopifnot(all(sapply(sp_matrix, is.numeric)))

# Bray-Curtis is undefined for two all-zero samples, so true zero-catch videos are dropped
# here ONLY (not from the richness/observations steps above). This means the
# community composition sample size is smaller than n in the results above -
# state that explicitly when reporting, don't let the n's look like they should match.
zero_rows <- rowSums(sp_matrix) == 0
sum(zero_rows)                                   # how many zero-catch videos get dropped
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sq3_data[!zero_rows, ]

sum(is.na(sp_matrix_nz))  # should be 0

set.seed(123)
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)
nmds$stress

# Report this value when you write this up. Stress < 0.1 = great, < 0.2 = usable with
# caution, > 0.2 = interpret the ORDINATION PLOT cautiously. Either way, PERMANOVA below
# (not the NMDS plot) is the actual hypothesis test - the plot is just a visual aid.

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment_pooled <- meta_nz$Treatment_pooled
nmds_scores$Sampling.period <- meta_nz$Sampling.period

#Small reproducible jitter to separate overlapping points - cosmetic only, doesn't touch the Bray-Curtis/PERMANOVA calculations.
set.seed(123)
nmds_scores_jit <- nmds_scores |>
  mutate(NMDS1 = jitter(NMDS1, amount = 0.02),
         NMDS2 = jitter(NMDS2, amount = 0.02))

ggplot(nmds_scores_jit, aes(x = NMDS1, y = NMDS2, colour = Treatment_pooled, shape = Sampling.period)) +
  geom_point(size = 3) +
  scale_colour_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Community composition: before vs after Living Boulder installation",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2", colour = "Treatment", shape = "Sampling period") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ3_CC.png", width = 8, height = 5, dpi = 300, bg = "white")

#####

bray_dist <- vegdist(sp_matrix_nz, method = "bray")

# PERMANOVA: does Sampling period, Treatment, or their interaction explain composition?
# The interaction term is the one that actually speaks to SQ3: it tests whether the
# CHANGE over time differs by treatment (i.e. Modified sites changing differently from
# Control/Rocky shore references), not just whether communities differ or time passed.
set.seed(123)
permanova <- adonis2(bray_dist ~ Sampling.period * Treatment_pooled,
                     data = meta_nz, permutations = 999)
permanova

set.seed(123)
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
set.seed(123)
permutest(disp_treatment, permutations = 999)

disp_period <- betadisper(bray_dist, meta_nz$Sampling.period)
set.seed(123)
permutest(disp_period, permutations = 999)

# If either permutest comes back significant (p < .05), that factor's PERMANOVA result
# may partly reflect dispersion differences - report this alongside the PERMANOVA p-value
# rather than citing the p-value alone.


##################################################################
# Which treatment(s) drive the interaction? Before vs after, per treatment
##################################################################
#The interaction says the change over time differs between treatments, but not WHERE. So: PERMANOVA Baseline 2 vs 6 months within each treatment separately.
#NB: same power problem as the SQ2 pairwise tests: with 2-3 videos per period the smallest possible p-value is ~0.1 (3 vs 3) or higher (2 vs 3). These tests can therefore never be significant - report R2 as the effect size (how much of the variation is explained by time, per treatment) and treat the p-values as descriptive.

period_change <- map_dfr(site_levels, function(site) {
  keep <- meta_nz$Treatment_pooled == site
  set.seed(123)
  res <- adonis2(vegdist(sp_matrix_nz[keep, ], method = "bray") ~ Sampling.period,
                 data = droplevels(meta_nz[keep, ]), permutations = 999)
  tibble(Treatment = treatment_labels[site],
         n_baseline = sum(meta_nz$Sampling.period[keep] == "Baseline 2"),
         n_6months  = sum(meta_nz$Sampling.period[keep] == "6 months"),
         R2 = round(res$R2[1], 3), F = round(res$F[1], 2), p = res$`Pr(>F)`[1])
})
period_change

#Mean Bray-Curtis dissimilarity between the baseline and 6-month videos of each treatment (0 = same community, 1 = no species in common).
#Descriptive: how much did the community of each treatment change over time? Uses the same bray_dist as the PERMANOVA above.
bray_mat <- as.matrix(bray_dist)

dissim_time <- map_dfr(site_levels, function(site) {
  base <- meta_nz$Code[meta_nz$Treatment_pooled == site & meta_nz$Sampling.period == "Baseline 2"]
  six  <- meta_nz$Code[meta_nz$Treatment_pooled == site & meta_nz$Sampling.period == "6 months"]
  tibble(Treatment = treatment_labels[site],
         mean_dissim_before_after = round(mean(bray_mat[base, six]), 2))
})
dissim_time












##################################################################

#INDICATOR SPECIES: which species characterise the baseline and which the 6-month communities?
#IndVal.g combines how specific a species is to a group (A) and how consistently it occurs in that group (B).
#Uses the same videos as the NMDS/PERMANOVA (sp_matrix_nz).

#1. Before vs after across all sites. Permutations are restricted WITHIN treatments (blocks), so the test compares Baseline 2 vs 6 months at the same treatment and doesn't pick up differences between treatments.
set.seed(123)
indval_period <- multipatt(sp_matrix_nz, meta_nz$Sampling.period,
                           func = "IndVal.g",
                           control = how(nperm = 999, blocks = meta_nz$Treatment_pooled))

summary(indval_period, indvalcomp = TRUE)            # significant species only (p <= 0.05)
summary(indval_period, indvalcomp = TRUE, alpha = 1) # all species, for the supplementary table

#2. Before vs after at the modified site only (the treatment of interest).
#NB: 3 vs 3 videos gives only 20 possible ways to split the videos, so the smallest possible p-value is 0.05: read this one as descriptive (A, B and IndVal values), not as a test.
keep_mod <- meta_nz$Treatment_pooled == "Modified"
sp_matrix_mod <- sp_matrix_nz[keep_mod, colSums(sp_matrix_nz[keep_mod, ]) > 0]

set.seed(123)
indval_mod <- multipatt(sp_matrix_mod, droplevels(meta_nz$Sampling.period[keep_mod]),
                        func = "IndVal.g", control = how(nperm = 999))

summary(indval_mod, indvalcomp = TRUE, alpha = 1)

#Clean table of the modified site results (for the text / supplementary)
indval_mod$sign |>
  rownames_to_column("Species") |>
  filter(!is.na(p.value)) |>                    # species seen in BOTH periods can't be an indicator of one (no p-value)
  mutate(Group = levels(droplevels(meta_nz$Sampling.period[keep_mod]))[index],
         Species = str_replace(Species, "\\.", " ")) |>
  select(Species, Group, IndVal = stat, p = p.value) |>
  mutate(IndVal = round(IndVal, 2)) |>
  arrange(Group, desc(IndVal))

#Supplementary table: both indicator analyses in one table (all sites + modified site only)
indval_table <- function(indval, groups, analysis) {
  sign <- indval$sign
  sign$A <- indval$A[cbind(seq_len(nrow(sign)), sign$index)]   # specificity for the group the species is assigned to
  sign$B <- indval$B[cbind(seq_len(nrow(sign)), sign$index)]   # fidelity for that group
  sign |>
    rownames_to_column("Species") |>
    filter(!is.na(p.value)) |>                  # species seen in BOTH periods can't be an indicator of one
    mutate(Analysis = analysis,
           Group = levels(droplevels(groups))[index],
           Species = str_replace(Species, "\\.", " ")) |>
    select(Analysis, Species, Group, A, B, IndVal = stat, p = p.value) |>
    mutate(across(c(A, B, IndVal), ~ round(.x, 2)), p = round(p, 3)) |>
    arrange(Group, desc(IndVal))
}

indval_supp <- bind_rows(
  indval_table(indval_period, meta_nz$Sampling.period, "All sites"),
  indval_table(indval_mod, meta_nz$Sampling.period[keep_mod], "Modified site")
)
indval_supp








##################################################################

#SPECIES COMPOSITION: which species make up the observations at each treatment, before and after?
#Mean count per video -> share of the total per treatment per period (same approach as SQ2, one day per period so no day averaging).
#The 10 most observed species are shown separately, the rest are grouped as "Other".

comp_data <- sq3_long |>
  group_by(Treatment_pooled, Sampling.period, Species) |>
  summarise(mean_count = mean(Count), .groups = "drop") |>
  group_by(Treatment_pooled, Sampling.period) |>
  mutate(share = mean_count / sum(mean_count)) |>
  ungroup()

top10 <- comp_data |>
  group_by(Species) |>
  summarise(total = sum(mean_count), .groups = "drop") |>
  slice_max(total, n = 10, with_ties = FALSE) |>
  pull(Species)
top10

#Group the rest as "Other"; stack order follows the lookup table (grouped by feeding mode)
comp_plot <- comp_data |>
  mutate(Species_group = if_else(Species %in% top10, Species, "Other")) |>
  group_by(Treatment_pooled, Sampling.period, Species_group) |>
  summarise(share = sum(share), .groups = "drop") |>
  filter(share > 0) |>
  mutate(Species_group = factor(Species_group,
                                levels = c(intersect(species_info$Species, top10), "Other")))

comp_colours <- c(species_colours[top10], Other = "grey70")
comp_labels  <- c(species_labels[top10],  Other = "Other")

p_comp <- ggplot(comp_plot, aes(x = Treatment_pooled, y = share, fill = Species_group)) +
  geom_col(width = 0.9) +
  facet_wrap(~Sampling.period) +
  scale_fill_manual(values = comp_colours, labels = comp_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Species composition: before versus after installment of LB",
       x = "Treatment", y = "Share of observations", fill = "Species") +
  theme_bw() + text_theme +
  theme(legend.text = element_text(face = "italic"))

ggsave("New_Plots/SQ3_species_composition.png", width = 12, height = 6, dpi = 300, bg = "white")

#Percentages exactly as shown in the plot
comp_plot |>
  arrange(Treatment_pooled, Sampling.period, desc(share)) |>
  mutate(Percent = round(share * 100, 1)) |>
  print(n = Inf)











##################################################################

#FEEDING MODES (as in SQ2): how are the observations divided over the feeding guilds, before and after?

guild_data <- sq3_long |>
  group_by(Code, Treatment_pooled, Sampling.period, Feeding_mode) |>
  summarise(Observations = sum(Count), .groups = "drop")      # observations per guild per video

guild_share <- comp_data |>
  left_join(species_info, by = "Species") |>
  group_by(Treatment_pooled, Sampling.period, Feeding_mode) |>
  summarise(share = sum(share), .groups = "drop")

#Percentages per feeding guild
guild_share |>
  mutate(Percent = round(share * 100, 1)) |>
  select(-share) |>
  pivot_wider(names_from = Feeding_mode, values_from = Percent)

guild_share <- guild_share |>
  filter(share > 0) |>                  # drop feeding modes with 0%
  mutate(Feeding_mode = droplevels(Feeding_mode))

p_guild_share <- ggplot(guild_share, aes(x = Treatment_pooled, y = share, fill = Feeding_mode)) +
  geom_col(width = 0.9) +
  facet_wrap(~Sampling.period) +
  scale_fill_manual(values = guild_colours) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Feeding modes: before versus after installment of LB",
       x = "Treatment", y = "Share of observations", fill = "Feeding mode") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ3_guildshare.png", width = 9, height = 5, dpi = 300, bg = "white")

#Species composition + feeding modes together (same as SQ2)
p_comp / p_guild_share +
  plot_annotation(title = "Species composition and feeding modes: before versus after installment of LB")

ggsave("New_Plots/SQ3_feeding_modes.png", width = 10, height = 10, dpi = 300, bg = "white")


#Does the feeding-mode composition change over time, and differently per treatment? PERMANOVA on the guild x video matrix
guild_matrix <- guild_data |>
  pivot_wider(names_from = Feeding_mode, values_from = Observations) |>
  filter(Code %in% meta_nz$Code)                              # same videos as the species PERMANOVA

set.seed(123)
permanova_guild <- adonis2(vegdist(select(guild_matrix, all_of(levels(species_info$Feeding_mode))),
                                   method = "bray") ~ Sampling.period * Treatment_pooled,
                           data = guild_matrix, permutations = 999, by = "terms")
permanova_guild
















##################################################################

#FEEDING RATES (bites) --> do fish feed more at the modified site after the installment?
#Videos where no feeding was seen are not in FeedData, so they are filled in as 0 bites.
#Ambassis were never seen feeding, so FeedData doesn't need an Ambassis filter.

#CHECK: the baseline videos should be in FeedData (otherwise all baseline bites become 0)
FeedData |> filter(Date == "18/06/2025") |> nrow()

video_ids <- sq3_data |> distinct(Code, Treatment_pooled, Sampling.period)

bites <- video_ids |>
  left_join(FeedData |> select(Code, Bites), by = "Code") |>
  mutate(Bites = replace_na(Bites, 0))

bites |> count(Treatment_pooled, Sampling.period, Bites > 0)   # how many videos per group had any feeding

bites |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(n_videos = n(), total_bites = sum(Bites),

                       mean_bites = round(mean(Bites), 1), .groups = "drop")
#Same layout as richness and observations: Baseline and 6 months side by side per treatment, individual videos as points
ggplot(bites, aes(x = Sampling.period, y = Bites,
                  fill = Treatment_pooled, alpha = Sampling.period)) +
  period_boxplot +
  labs(title = "Feeding intensity: before versus after installment of LB",
       subtitle = "Number of bites per video",
       x = NULL, y = "Number of bites")


ggsave("New_Plots/SQ3_feeding.png", width = 7, height = 4, dpi = 300, bg = "white")

