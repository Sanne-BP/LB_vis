#Script for the research assignment: Rubbish

#Q1 (within MOD): How does litter accumulation inside the Living Boulders compare to the surrounding revetment?
#Q2 (between sites): How does litter accumulation at the LB site compare to natural rocky shore (and control) sites?

rm(list = ls())
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(vegan)
library(viridis)
library(vegan)
library(patchwork)


# ---------------------------------------------------------------
# 1. Import and clean
# ---------------------------------------------------------------
raw <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQb66D4c8m-XdTybthjskUdl-eITzveZioAnkONlgf1eVb515iZXQweaDOZ9cljvJKoh1DjV6cyxYme/pub?gid=198299633&single=true&output=csv",
                col_types = cols(.default = col_character()))

names(raw) <- str_trim(names(raw))          # "Colour " has a trailing space

survey_dates <- c("14-04-2026", "21-04-2026", "28-04-2026", "05-05-2026", "12-05-2026")
site_levels  <- c("CON1", "CON2", "RS1", "RS2", "MOD")

litter <- raw |>
  mutate(across(everything(), str_trim),     # removes trailing spaces ("Metal ", "100+ ", ...)
         Weight_kg    = as.numeric(Weight_kg),
         Boulder_type = if_else(is.na(Boulder_type) | Boulder_type == "", "Site", Boulder_type),
         Is_item      = Rubbish_category != "None",
         Location     = factor(Location, levels = site_levels))

# Size ranges -> fixed size classes (midpoint of each recorded range)
size_breaks <- c(0, 5, 10, 20, 50, 100, Inf)
size_labels <- c("<5", "5-10", "10-20", "20-50", "50-100", ">100")

litter <- litter |>
  mutate(size_lo  = as.numeric(str_extract(Size_class, "^[0-9.]+")),
         size_hi  = as.numeric(str_extract(Size_class, "(?<=-)[0-9.]+")),
         size_mid = case_when(str_detect(Size_class, "\\+") ~ 101,
                              is.na(size_hi) ~ size_lo,
                              TRUE ~ (size_lo + size_hi) / 2),
         Size_group = cut(size_mid, breaks = size_breaks, labels = size_labels, right = FALSE))

# Transect-level data: number of items and weight per transect per survey
transects <- litter |>
  group_by(Date, Location, Boulder_type, Tidal_height) |>
  summarise(n_items   = sum(Is_item),
            weight_kg = if (all(is.na(Weight_kg))) 0 else max(Weight_kg, na.rm = TRUE),
            .groups = "drop") |>
  mutate(weight_g     = weight_kg * 1000,
         Tidal_height = factor(Tidal_height, levels = c("L", "M", "H")),
         Boulder_type = factor(Boulder_type, levels = c("Site", "REV-B", "LB", "CON-B")),
         Date         = factor(Date, levels = survey_dates))

nrow(transects)   # should be 80

# Individual items only (no "None" rows)
items <- litter |> filter(Is_item)






# ---------------------------------------------------------------
# 2. Shared plot settings (same colours and labels as the fish chapters)
# ---------------------------------------------------------------
site_colors <- c("CON1" = "#08519C", "CON2" = "#3182BD",
                 "RS1"  = "#238B45", "RS2"  = "#74C476",
                 "MOD"  = "#E6550D")
site_labels <- c("CON1" = "C1", "CON2" = "C2", "RS1" = "RS1", "RS2" = "RS2", "MOD" = "MOD")
tide_labels <- c("L" = "Low", "M" = "Mid", "H" = "High")
structure_labels <- c("REV-B" = "Existing revetment", "LB" = "Living Boulder",
                      "CON-B" = "Control boulder")
date_labels <- c("14-04-2026" = "14 Apr", "21-04-2026" = "21 Apr", "28-04-2026" = "28 Apr",
                 "05-05-2026" = "5 May", "12-05-2026" = "12 May")

structure_colors <- c("REV-B" = "#FDAE6B",   # = "Modified Existing Revetment"
                      "LB"    = "#E6550D",   # = "Modified Living Boulder"
                      "CON-B" = "#FEE6CE")   # control boulder: lightest shade (no fish equivalent)

# Every text element at 11 pt (theme_bw otherwise scales titles, strips and legends up or down)
theme_set(theme_bw(base_size = 11) +
            theme(text         = element_text(size = 11),
                  axis.text    = element_text(size = 11),
                  axis.title   = element_text(size = 11),
                  legend.text  = element_text(size = 11),
                  legend.title = element_text(size = 11),
                  strip.text   = element_text(size = 11),
                  plot.title   = element_text(size = 11)))

# Plots are saved at the size they will have in Word (16 cm = text width of an A4 page with 2.5 cm margins).
# Insert them in Word at 100% (don't resize), so 11 pt in R stays 11 pt in Word.
plot_width  <- 16   # cm
plot_height <- 9    # cm






# ---------------------------------------------------------------
# 3. Between sites
# ---------------------------------------------------------------
# 3a. Number of items per transect, per site (black diamond = mean)
ggplot(transects, aes(x = Location, y = n_items, fill = Location)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 1.5, alpha = 0.6) +
  scale_fill_manual(values = site_colors) +
  scale_x_discrete(labels = site_labels) +
  labs(x = "Site", y = "Number of litter items per transect") +
  theme(legend.position = "none")
ggsave("Rubbish_Plots/P1_items_per_site.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")

# 3b. Number of items per transect, per tidal height and site
# Same style as the fish SQ figures: one panel per site, site name in bold below,
# shades of the site colour from light (Low) to dark (High)
tide_shades <- function(col) {
  ramp <- colorRampPalette(c("white", col))(101)
  c(L = ramp[36], M = ramp[66], H = ramp[101])
}
tide_fill <- unlist(lapply(names(site_colors), function(s) {
  shades <- tide_shades(site_colors[[s]])
  setNames(shades, paste(s, names(shades), sep = "."))
}))

transects_tide <- transects |>
  mutate(Location = factor(Location, levels = site_levels),
         fill_key = paste(Location, Tidal_height, sep = "."))

ggplot(transects_tide, aes(x = Tidal_height, y = n_items, fill = fill_key)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 1.5, alpha = 0.6) +
  facet_grid(~Location, scales = "free_x", space = "free_x", switch = "x",
             labeller = as_labeller(site_labels)) +
  scale_fill_manual(values = tide_fill) +
  scale_x_discrete(labels = tide_labels) +
  labs(x = NULL, y = "Number of litter items per transect") +
  theme(legend.position = "none",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold"))
ggsave("Rubbish_Plots/P2_items_per_tidalheight.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")

# 3c. Wet weight per transect, per site (log scale because of the construction-wood outlier)
ggplot(transects, aes(x = Location, y = weight_g, fill = Location)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 1.5, alpha = 0.6) +
  scale_fill_manual(values = site_colors) +
  scale_x_discrete(labels = site_labels) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 10, 100, 1000)) +
  labs(x = "Site", y = "Wet weight per transect (g, log scale)") +
  theme(legend.position = "none")
ggsave("Rubbish_Plots/P3_weight_per_site.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")

# 3d. Number of items per week (mean per transect), per site
weekly <- transects |>
  group_by(Date, Location) |>
  summarise(mean_items = mean(n_items),
            se_items   = sd(n_items) / sqrt(n()),
            .groups = "drop")

ggplot(weekly, aes(x = Date, y = mean_items, colour = Location, group = Location)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_items - se_items, ymax = mean_items + se_items), width = 0.15) +
  scale_colour_manual(values = site_colors, labels = site_labels) +
  scale_x_discrete(labels = date_labels) +
  labs(x = "Survey date", y = "Mean number of items per transect (± SE)", colour = "Site")
ggsave("Rubbish_Plots/P4_items_per_week.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")










# ---------------------------------------------------------------
# 4. Within the Modified site
# ---------------------------------------------------------------
within_mod <- transects |> filter(Location == "MOD")

ggplot(within_mod, aes(x = Boulder_type, y = n_items, fill = Boulder_type)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 1.5, alpha = 0.6) +
  scale_x_discrete(labels = structure_labels) +
  scale_fill_manual(values = structure_colors) +
  labs(x = "Structure", y = "Number of litter items per transect") +
  theme(legend.position = "none")
ggsave("Rubbish_Plots/P5_items_within_MOD.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")

# Per tidal height (control boulders only at mid)
ggplot(within_mod, aes(x = Tidal_height, y = n_items, fill = Boulder_type)) +
  geom_boxplot(alpha = 1, outlier.shape = NA,
               position = position_dodge(preserve = "single")) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, seed = 123),
             size = 1.2, alpha = 0.6) +
  scale_x_discrete(labels = tide_labels) +
  scale_fill_manual(values = structure_colors, labels = structure_labels) +
  labs(x = "Tidal height", y = "Number of litter items per transect", fill = "Structure")
ggsave("Rubbish_Plots/P6_items_within_MOD_tidalheight.png", width = plot_width, height = plot_height, units = "cm", dpi = 300, bg = "white")





