#Sub Question 2: between site comparison after installment
#Do fish populations and assemblages differ between sites with Living Boulders and sites without?

##Response variables: number of observations, species richness (supplementary), species habitat use,
##community composition (incl. feeding modes + cryptobenthic fish), behaviour (feeding), Ambassis schooling

rm(list=ls())
library(tidyverse)
library(viridis)
library(vegan)
library(patchwork)

#Importing data from google sheets:
ObsData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=667551629&single=true&output=csv")

FeedData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=733876966&single=true&output=csv")

BehavLongData <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=855991795&single=true&output=csv")



#SQ2 focuses on after installation only, so the baseline (18/06/2025) is filtered out. 30/04/2026 and 01/05/2026 are also filtered out, because on these dates only the modified site was watched, which would enlarge the sample size for that site only. So that leaves us with 20/04/2026, when all five sites were sampled on the same day.

#Because everything is from ONE day, there is no per-day averaging here (unlike SQ1): each video is one replicate.

#The modified site is pooled (MOD-ER + MOD-LB = "Modified"), as this is a between site comparison. "Habitat" keeps MOD-ER and MOD-LB apart, which is only used in the Ambassis section at the end.

site_levels <- c("Control 1", "Control 2", "Rocky shore 1", "Rocky shore 2", "Modified")

after_data <- ObsData |>
  filter(Date == "20/04/2026") |>
  mutate(Treatment_pooled = case_when(Treatment %in% c("Modified Existing Revetment",
                                                       "Modified Living Boulder") ~ "Modified",
                                      TRUE ~ Treatment),
         Treatment_pooled = factor(Treatment_pooled, levels = site_levels),
         Habitat = factor(Treatment, levels = c("Control 1", "Control 2", "Rocky shore 1",
                                                "Rocky shore 2", "Modified Existing Revetment",
                                                "Modified Living Boulder")))

after_data |> count(Treatment_pooled, Habitat)   # CHECK: MOD-ER and MOD-LB should both be here (3 + 3)



#EXCLUDE AMBASSIS: Ambassis spp. are removed from all analyses (large transient schools dominate the counts).
#Richness and Observations in ObsData include Ambassis, so they are recalculated from the remaining species.
#(Ambassis gets its own section at the end of this script, using ObsData directly.)

non_species_cols <- c("Code", "TapeReader", "Sampling.period", "Date", "Site", "Treatment",
                      "Camera.no.", "Richness", "Observations", "Treatment_pooled", "Habitat")

#CHECK first: does recalculating from the species columns reproduce the original columns?
#Both should be TRUE - otherwise Richness/Observations are defined differently and we need to look at it.
all_sp_cols <- setdiff(names(after_data), non_species_cols)
after_data |>
  summarise(richness_matches     = all(rowSums(across(all_of(all_sp_cols)) > 0) == Richness),
            observations_matches = all(rowSums(across(all_of(all_sp_cols))) == Observations))

ambassis_cols <- grep("^Ambassis", names(after_data), value = TRUE)
ambassis_cols                                    # CHECK: should only list the Ambassis column(s)

#NB: "Unidentified" stays in: it is one (unknown) species, so it counts for richness and observations.
after_data <- after_data |>
  select(-all_of(ambassis_cols))

sp_cols <- setdiff(names(after_data), non_species_cols)

after_data <- after_data |>
  mutate(Richness     = rowSums(across(all_of(sp_cols)) > 0),   # number of species present
         Observations = rowSums(across(all_of(sp_cols))))       # total observations



##################################################################
#Sub sampling step, reused for everything for SQ2

#On 20/04/2026 the modified site has 6 videos (3 MOD-ER + 3 MOD-LB), vs. 3 at every other site. So, we randomly keep 3 of those 6, so Modified is balanced at n=3 like the other sites.

#This subsample is done ONCE here (not separately per analysis), so every analysis below describes the exact same set of replicate videos.

set.seed(123)

modified_sampled <- after_data |>
  filter(Treatment_pooled == "Modified") |>
  slice_sample(n = 3)

sq2_data <- after_data |>
  filter(Treatment_pooled != "Modified") |>
  bind_rows(modified_sampled)

#Sanity check: Modified should now show n=3, every other site its original count (RS1 = 2, camera malfunction).
sq2_data |> count(Treatment_pooled)
modified_sampled |> count(Habitat)               # which MOD habitats ended up in the subsample



##################################################################
#Shared plot settings

treatment_labels <- c("Control 1" = "C1", "Control 2" = "C2",
                      "Rocky shore 1" = "RS1", "Rocky shore 2" = "RS2", "Modified" = "MOD")

#Controls share a blue family (dark = C1, light = C2), rocky shores a green family (dark = RS1, light = RS2), and Modified gets its own orange since it's the treatment of primary interest (same colours as before).
treatment_colors <- c("Control 1" = "#08519C", "Control 2" = "#3182BD",
                      "Rocky shore 1" = "#238B45", "Rocky shore 2" = "#74C476",
                      "Modified" = "#E6550D")

text_theme <- theme(text = element_text(size = 11))


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

#Feeding mode colours: 4 viridis colours in the same order as the species colours, grey for Unknown
guild_colours <- c(setNames(viridis(4), c("Planktivore", "Herbivore", "Omnivore", "Predator")))

#CHECK: every species column in sq2_data should be in the lookup table (should return character(0))
setdiff(sp_cols, species_info$Species)



#Long format (one row per video x species), reused by the species-level plots below
sq2_long <- sq2_data |>
  pivot_longer(all_of(sp_cols), names_to = "Species", values_to = "Count") |>
  left_join(species_info, by = "Species") |>
  filter(!is.na(Feeding_mode))      # removes Unidentified (and any other unknown column) from the species plots










##################################################################

#SPECIES RICHNESS:

p_rich <- ggplot(sq2_data, aes(x = Treatment_pooled, y = Richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Species richness after installment of LB",
       x = "Treatment", y = "Species richness", fill = "Treatment") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_richness.png", width = 7, height = 4, dpi = 300, bg = "white")















##################################################################

#NUMBER OF OBSERVATIONS:

#Total (summed) number of observations per site, shown as "N = X" on top of the boxes
total_obs <- sq2_data |>
  group_by(Treatment_pooled) |>
  summarise(n_videos = n(),
            total_observations = sum(Observations), .groups = "drop")
total_obs

p_obs <- ggplot(sq2_data, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  #geom_text(data = total_obs,
            #aes(x = Treatment_pooled, y = Inf, label = paste0("N = ", total_observations)),
            #inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +   # room for the N labels
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Number of observations after installment of LB",
       x = "Treatment", y = "Number of observations", fill = "Treatment") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_observations.png", width = 7, height = 4, dpi = 300, bg = "white")






#Put together the 2 graphs for report (same as SQ1):
p_rich + p_obs +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("New_Plots/SQ2_rich_obs_combi.png", width = 9, height = 6, dpi = 300, bg = "white")




















##################################################################

#COMMUNITY COMPOSITION:
#NMDS + Bray-Curtis dissimilarity matrix

sp_matrix <- sq2_data |>
  select(all_of(sp_cols)) |>
  as.data.frame()

rownames(sp_matrix) <- sq2_data$Code

#Sanity check: every column should be numeric species counts.
stopifnot(all(sapply(sp_matrix, is.numeric)))

zero_rows <- rowSums(sp_matrix) == 0
sum(zero_rows)                                   # how many zero-catch videos get dropped
sp_matrix_nz <- sp_matrix[!zero_rows, ]
meta_nz <- sq2_data[!zero_rows, ]

set.seed(123) #[1] 0.1396111
nmds <- metaMDS(sp_matrix_nz, distance = "bray", k = 2, trymax = 100)

nmds$stress
sum(is.na(sp_matrix_nz))    # should be 0

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Treatment_pooled <- meta_nz$Treatment_pooled

#Small reproducible jitter to separate overlapping points - cosmetic only, doesn't touch the Bray-Curtis/PERMANOVA calculations.
#No ellipses: with 2-3 videos per site there are too few points to draw one.
set.seed(123)
nmds_scores_jit <- nmds_scores |>
  mutate(NMDS1 = jitter(NMDS1, amount = 0.02),
         NMDS2 = jitter(NMDS2, amount = 0.02))

ggplot(nmds_scores_jit, aes(x = NMDS1, y = NMDS2, colour = Treatment_pooled, shape = Treatment_pooled)) +
  geom_point(size = 3) +
  scale_colour_manual(values = treatment_colors, labels = treatment_labels) +
  scale_shape_discrete(labels = treatment_labels) +
  labs(title = "Community composition after installment of LB",
       subtitle = paste0("NMDS, stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2", colour = "Treatment", shape = "Treatment") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_CC.png", width = 8, height = 5, dpi = 300, bg = "white")



bray_dist <- vegdist(sp_matrix_nz, method = "bray")

#PERMANOVA: do the sites differ in community composition?
set.seed(123)
permanova <- adonis2(bray_dist ~ Treatment_pooled, data = meta_nz, permutations = 999)
permanova

#Dispersion check: is the within-site spread equal? (if not, a significant PERMANOVA could be caused by spread instead of composition)
disp_treatment <- betadisper(bray_dist, meta_nz$Treatment_pooled)
set.seed(123)
permutest(disp_treatment, permutations = 999)


#Pairwise: Modified vs each other site (the comparisons that matter for SQ2)
#NB: with only 2-3 videos per site there are very few possible ways to shuffle the videos, so the smallest possible p-value is ~0.1 (3 vs 3) or ~0.1-0.3 (2 vs 3). These pairwise tests can therefore never be significant - report R2 as the effect size and treat the p-values as descriptive.
pairwise_mod <- map_dfr(setdiff(site_levels, "Modified"), function(site) {
  keep <- meta_nz$Treatment_pooled %in% c("Modified", site)
  set.seed(123)
  res <- adonis2(vegdist(sp_matrix_nz[keep, ], method = "bray") ~ Treatment_pooled,
                 data = droplevels(meta_nz[keep, ]), permutations = 999)
  tibble(Comparison = paste("MOD vs", treatment_labels[site]),
         R2 = round(res$R2[1], 3), F = round(res$F[1], 2), p = res$`Pr(>F)`[1])
}) |>
  mutate(p_holm = p.adjust(p, method = "holm"))
pairwise_mod

#Is the modified site more similar to the control revetments or to the natural rocky shores?
#Mean Bray-Curtis dissimilarity between every MOD video and every video of the other sites (0 = identical communities, 1 = no species in common).
#Uses the same bray_dist as the PERMANOVA above.

bray_mat <- as.matrix(bray_dist)
mod_codes <- meta_nz$Code[meta_nz$Treatment_pooled == "Modified"]

mod_dissim <- meta_nz |>
  filter(Treatment_pooled != "Modified") |>
  select(Code, Treatment_pooled) |>
  mutate(Site_type = if_else(str_detect(Treatment_pooled, "Control"), "Control revetments", "Rocky shores"),
         dissim = map_dbl(Code, ~ mean(bray_mat[mod_codes, .x])))   # mean dissimilarity to the MOD videos

#Per site
mod_dissim |>
  group_by(Treatment_pooled) |>
  summarise(mean_dissim_to_MOD = round(mean(dissim), 2), .groups = "drop")

#Controls vs rocky shores (the number for the text)
mod_dissim |>
  group_by(Site_type) |>
  summarise(mean_dissim_to_MOD = round(mean(dissim), 2),
            sd = round(sd(dissim), 2), .groups = "drop")

#For comparison: how different are the controls and rocky shores from EACH OTHER?
ctrl_codes <- meta_nz$Code[str_detect(meta_nz$Treatment_pooled, "Control")]
rs_codes   <- meta_nz$Code[str_detect(meta_nz$Treatment_pooled, "Rocky")]
round(mean(bray_mat[ctrl_codes, rs_codes]), 2)

#Descriptive only (no test): with 2-3 videos per site a test here would have the same power problem as the pairwise PERMANOVA.












##################################################################

#SPECIES COMPOSITION: which species make up the observations at each site?
#Mean count per video -> share of the total per site (same approach as SQ1, but no day averaging as it is one day).
#The 10 most observed species are shown separately, the rest are grouped as "Other".

comp_data <- sq2_long |>
  group_by(Treatment_pooled, Species) |>
  summarise(mean_count = mean(Count), .groups = "drop") |>
  group_by(Treatment_pooled) |>
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
  group_by(Treatment_pooled, Species_group) |>
  summarise(share = sum(share), .groups = "drop") |>
  filter(share > 0) |>
  mutate(Species_group = factor(Species_group,
                                levels = c(intersect(species_info$Species, top10), "Other")))

comp_colours <- c(species_colours[top10], Other = "grey70")
comp_labels  <- c(species_labels[top10],  Other = "Other")

p_comp <- ggplot(comp_plot, aes(x = Treatment_pooled, y = share, fill = Species_group)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = comp_colours, labels = comp_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Species composition after installment of LB",
       x = "Treatment", y = "Share of observations", fill = "Species") +
  theme_bw() + text_theme +
  theme(legend.text = element_text(face = "italic"))

ggsave("New_Plots/SQ2_species_composition.png", width = 7, height = 5, dpi = 300, bg = "white")

#Percentages exactly as shown in the plot
comp_plot |>
  arrange(Treatment_pooled, desc(share)) |>
  mutate(Percent = round(share * 100, 1)) |>
  print(n = Inf)










##################################################################

#FEEDING MODES: how are the observations divided over the feeding guilds at each site?

guild_data <- sq2_long |>
  group_by(Code, Treatment_pooled, Feeding_mode) |>
  summarise(Observations = sum(Count), .groups = "drop")      # observations per guild per video

guild_share <- comp_data |>
  left_join(species_info, by = "Species") |>
  group_by(Treatment_pooled, Feeding_mode) |>
  summarise(share = sum(share), .groups = "drop")

#Percentages per feeding guild
guild_share |>
  mutate(Percent = round(share * 100, 1)) |>
  select(-share) |>
  pivot_wider(names_from = Feeding_mode, values_from = Percent)

guild_share <- comp_data |>
  left_join(species_info, by = "Species") |>
  group_by(Treatment_pooled, Feeding_mode) |>
  summarise(share = sum(share), .groups = "drop") |>
  filter(share > 0) |>                  # drop feeding modes with 0% (e.g. Unknown)
  mutate(Feeding_mode = droplevels(Feeding_mode))

p_guild_share <- ggplot(guild_share, aes(x = Treatment_pooled, y = share, fill = Feeding_mode)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = guild_colours) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Different feeding modes after installment of LB"
       , x = "Treatment", y = "Share of observations",
       fill = "Feeding mode") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_guidlshare.png", width = 7, height = 5, dpi = 300, bg = "white")

p_guild_box <- ggplot(guild_data, aes(x = Treatment_pooled, y = Observations, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~Feeding_mode, scales = "free_y", nrow = 1) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Number of observations per feeding mode", x = "Treatment",
       y = "Number of observations per video", fill = "Treatment") +
  theme_bw() + text_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_guild_share / p_guild_box +
  plot_annotation(title = "Feeding modes after installment of LB")

p_comp  + p_guild_share +
  plot_annotation(title = "Feeding modes and species composition after installment of LB")

ggsave("New_Plots/SQ2_feeding_modes.png", width = 12, height = 6, dpi = 300, bg = "white")


#Does the feeding-mode composition differ between sites? PERMANOVA on the guild x video matrix
guild_matrix <- guild_data |>
  pivot_wider(names_from = Feeding_mode, values_from = Observations) |>
  filter(Code %in% meta_nz$Code)                              # same videos as the species PERMANOVA

set.seed(123)
permanova_guild <- adonis2(vegdist(select(guild_matrix, all_of(levels(species_info$Feeding_mode))),
                                   method = "bray") ~ Treatment_pooled,
                           data = guild_matrix, permutations = 999)
permanova_guild











##################################################################

#CRYPTOBENTHIC SPECIES:

cryptobenthic_species <- c("Omobranchus.rotundiceps", "Omobranchus.anolius",
                           "Istiblennius.edentulus", "Favonigobius.exquisitus",
                           "Favonigobius.lentiginosus")

sq2_data <- sq2_data |>
  mutate(Cryptobenthic_obs      = rowSums(across(any_of(cryptobenthic_species))),
         Cryptobenthic_richness = rowSums(across(any_of(cryptobenthic_species), ~ .x > 0)))

#Number of observations of cryptobenthic fish (main text):
ggplot(sq2_data, aes(x = Treatment_pooled, y = Cryptobenthic_obs, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Cryptobenthic fish after installment of LB", x = "Treatment",
       y = "Number of observations per video", fill = "Treatment") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_cryptobenthic.png", width = 7, height = 4, dpi = 300, bg = "white")


#Species richness of cryptobenthic fish (supplementary):
ggplot(sq2_data, aes(x = Treatment_pooled, y = Cryptobenthic_richness, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Cryptobenthic species richness after installment of LB", x = "Treatment",
       y = "Species richness per video", fill = "Treatment") +
  theme_bw() + text_theme

ggsave("New_Plots/SQ2_cryptobenthic_richness_SUPPL.png", width = 7, height = 4, dpi = 300, bg = "white")











##################################################################

#FEEDING RATES (bites) --> are fish actively feeding more at the modified site?
#Videos where no feeding was seen are not in FeedData, so they are filled in as 0 bites.
#Ambassis were never seen feeding, so FeedData doesn't need an Ambassis filter.

video_ids <- sq2_data |> distinct(Code, Treatment_pooled)

bites <- video_ids |>
  left_join(FeedData |> select(Code, Bites), by = "Code") |>
  mutate(Bites = replace_na(Bites, 0))

bites |> count(Treatment_pooled, Bites > 0)   # how many videos per site had any feeding

p_bites <- ggplot(bites, aes(x = Treatment_pooled, y = Bites, fill = Treatment_pooled)) +
  geom_boxplot(alpha = 0.6) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = treatment_labels) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels) +
  labs(title = "Feeding intensity", subtitle = "Number of bites per video",
       x = "Treatment", y = "Number of bites", fill = "Treatment") +
  theme_bw() + text_theme


ggsave("New_Plots/SQ2_feeding.png", width = 7, height = 4, dpi = 300, bg = "white")










##################################################################

#AMBASSIS SCHOOLING: do Ambassis interact more with the Living Boulders?
#Ambassis are excluded everywhere above, so here they are looked at on their own.
#MOD-ER and MOD-LB are kept apart here (Habitat), and ALL six modified videos of 20/04/2026 are used (no subsampling), because the question is about the LB specifically.

habitat_labels <- c("Control 1" = "C1", "Control 2" = "C2", "Rocky shore 1" = "RS1",
                    "Rocky shore 2" = "RS2", "Modified Existing Revetment" = "MOD-ER",
                    "Modified Living Boulder" = "MOD-LB")

habitat_colors <- c(treatment_colors[1:4],
                    "Modified Existing Revetment" = "#FDAE6B",   # same MOD-ER/MOD-LB oranges as SQ1
                    "Modified Living Boulder"     = "#E6550D")

ambassis_data <- ObsData |>
  filter(Date == "20/04/2026") |>
  mutate(Habitat = factor(Treatment, levels = names(habitat_labels))) |>
  select(Code, Date, Habitat, Ambassis = Ambassis.spp)

ambassis_data |>
  group_by(Habitat) |>
  summarise(n_videos = n(),
            videos_with_Ambassis = sum(Ambassis > 0),
            total = sum(Ambassis),
            mean = round(mean(Ambassis), 1))

p_amb_obs <- ggplot(ambassis_data, aes(x = Habitat, y = Ambassis, fill = Habitat)) +
  geom_boxplot(alpha = 0.6) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 123), size = 1.5) +
  scale_x_discrete(labels = habitat_labels) +
  scale_fill_manual(values = habitat_colors, labels = habitat_labels) +
  labs(title = "Number of observations", x = "Treatment",
       y = "Ambassis observations per video", fill = "Treatment") +
  theme_bw() + text_theme


#Ambassis behaviour: are they just passing, or do they stay (transient) around the LB?
amb_behav <- BehavLongData |>
  filter(OpCode %in% ambassis_data$Code, spp == "Ambassis.spp") |>
  left_join(ambassis_data |> select(Code, Habitat), by = c("OpCode" = "Code")) |>
  group_by(Habitat, Activity) |>
  summarise(total = sum(count), .groups = "drop") |>
  group_by(Habitat) |>
  mutate(share = total / sum(total)) |>
  ungroup() |>
  complete(Habitat, Activity, fill = list(total = 0, share = 0))

amb_behav |>
  select(-share) |>
  pivot_wider(names_from = Activity, values_from = total)     # number of events per activity

p_amb_behav <- ggplot(amb_behav, aes(x = Habitat, y = share, fill = Activity)) +
  geom_col(width = 0.9) +
  scale_x_discrete(labels = habitat_labels, drop = FALSE) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  scale_fill_viridis_d() +
  labs(title = "Behaviour", x = "Treatment", y = "Share of Ambassis events") +
  theme_bw() + text_theme

p_amb_obs + p_amb_behav +
  plot_annotation(title = "Ambassis sp. after installment of LB")

ggsave("New_Plots/SQ2_ambassis.png", width = 12, height = 4.5, dpi = 300, bg = "white")
