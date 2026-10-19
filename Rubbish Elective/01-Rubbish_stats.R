#Continue from script 00-RubbishData, this script is for stats

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)


# ---------------------------------------------------------------
# 1. Data from rubbish_plots.R
# ---------------------------------------------------------------
stopifnot(exists("transects"), nrow(transects) == 80)   # error = run rubbish_plots.R first

# Transect ID for the random effect (repeated surveys of the same transect)
transects <- transects |>
  mutate(Transect = paste(Location, Boulder_type, Tidal_height, sep = "_"))

transects |> count(Location, Boulder_type, Tidal_height)   # surveys per transect (RS1 L = 1, RS2 L = 4, rest = 5)






# ---------------------------------------------------------------
# 2. Descriptive summaries
# ---------------------------------------------------------------
# Per site
transects |>
  group_by(Location) |>
  summarise(surveys   = n(),
            total_items = sum(n_items),
            mean_items  = mean(n_items),
            se_items    = sd(n_items) / sqrt(n()),
            total_weight_g = sum(weight_g),
            mean_weight_g  = mean(weight_g))

#Location     surveys total_items   mean_items se_items   total_weight_g    mean_weight_g
#1 CON1          10         203       20.3      7.89           4454           445.
#2 CON2          10          54       5.4       1.65            698.          69.8
#3 RS1           11          33       3         0.405           159           14.5
#4 RS2           14          65       4.64      1.01           1118.          79.8
#5 MOD           35         100       2.86      0.710           706.          20.2


# Per tidal height
transects |>
  group_by(Tidal_height) |>
  summarise(surveys = n(), total_items = sum(n_items), mean_items = mean(n_items),
            se_items = sd(n_items) / sqrt(n()))

#Tidal_height     surveys   total_items   mean_items  se_items
#1 L                 15          41       2.73        0.765
#2 M                 35         135       3.86        0.727
#3 H                 30         279       9.3         2.99


# Within MOD, per boulder type
transects |>
  filter(Location == "MOD") |>
  group_by(Boulder_type, Tidal_height) |>
  summarise(surveys = n(), total_items = sum(n_items), mean_items = mean(n_items),
            .groups = "drop")

#Boulder_type Tidal_height surveys total_items mean_items
#1 REV-B        L                  5          27        5.4
#2 REV-B        M                  5          27        5.4
#3 REV-B        H                  5          44        8.8
#4 LB           L                  5           0        0
#5 LB           M                  5           0        0
#6 LB           H                  5           2        0.4
#7 CON-B        M                  5           0        0






---------------------------------------------------------------
  # 3. Q2: between sites
  # ---------------------------------------------------------------
# Only the existing revetment at MOD is compared with the other sites.
# The Living Boulder and control boulder transects are analysed separately within MOD (section 4).
# Reason: with all MOD transects, MOD mixes structures with ~0 items (LB, CON-B) and 5-9 items (revetment),
# which caused unequal residual variance between sites (DHARMa Levene test significant, also with
# dispformula = ~ Location, AIC 386.1 vs 383.1). With revetment only, all DHARMa checks were n.s.

between <- transects |> dplyr::filter(Location != "MOD" | Boulder_type == "REV-B")
nrow(between)

# Descriptive summary for this comparison (MOD = revetment only)
between |>
  group_by(Location) |>
  summarise(surveys = n(), total_items = sum(n_items), mean_items = mean(n_items),
            se_items = sd(n_items) / sqrt(n()),
            median_weight_g = median(weight_g))

#Location   surveys total_items   mean_items  se_items   median_weight_g
#1 CON1          10         203      20.3     7.89            128
#2 CON2          10          54       5.4     1.65             59.5
#3 RS1           11          33       3       0.405            10.5
#4 RS2           14          65       4.64    1.01             22
#5 MOD           15          98       6.53    1.08             34.5





# 3a. Number of items: negative binomial GLMM
# Week and transect as random effects (repeated surveys of the same transects).
# Additive model: the controls have no low transect, so a Site x Tidal height interaction is not fully estimable.
m_items_pois <- glmmTMB(n_items ~ Location + Tidal_height + (1 | Date) + (1 | Transect),
                        family = poisson, data = between)
m_items_nb   <- glmmTMB(n_items ~ Location + Tidal_height + (1 | Date) + (1 | Transect),
                        family = nbinom2, data = between)
AIC(m_items_pois, m_items_nb)   # pick the lower AIC > negative binomial


res_items <- simulateResiduals(m_items_nb)
plot(res_items)
testDispersion(res_items)
testZeroInflation(res_items)
plotResiduals(res_items, between$Location)
plotResiduals(res_items, between$Tidal_height)

#model assumptions were checked with simulated residuals (DHARMa), and no deviations in distribution, dispersion, zero inflation or homogeneity of variance were detected

summary(m_items_nb)
drop1(m_items_nb, test = "Chisq")
emmeans(m_items_nb, pairwise ~ Location, type = "response")    # pairwise site comparisons (Tukey)
emmeans(m_items_nb, pairwise ~ Tidal_height, type = "response")











# 3b. Weight: Tweedie GLMM (continuous, right-skewed, with exact zeros)
m_weight <- glmmTMB(weight_g ~ Location + Tidal_height + (1 | Date) + (1 | Transect),
                    family = tweedie, data = between)

m_weight_log <- glmmTMB(log1p(weight_g) ~ Location + Tidal_height + (1 | Date) + (1 | Transect),
                        family = gaussian, data = between)
res_weight_log <- simulateResiduals(m_weight_log)
plot(res_weight_log)
plotResiduals(res_weight_log, between$Location)

m_weight_log2 <- update(m_weight_log, dispformula = ~ Location)
AIC(m_weight_log, m_weight_log2)
res_weight_log2 <- simulateResiduals(m_weight_log2)
plot(res_weight_log2)
plotResiduals(res_weight_log2, between$Location)

summary(m_weight_log2)
drop1(m_weight_log2, test = "Chisq")
emmeans(m_weight_log2, pairwise ~ Location, type = "response")
emmeans(m_weight_log2, pairwise ~ Tidal_height, type = "response")












# ---------------------------------------------------------------
# 4. Q1: within MOD (Living Boulders vs surrounding revetment vs control boulders)
# ---------------------------------------------------------------
within_mod <- transects |> dplyr::filter(Location == "MOD")

# LB and CON-B are (almost) all zeros, so a GLMM would not fit (complete separation).
# Paired comparison instead: LB vs revetment at the same tidal height in the same week.
lb_pairs <- within_mod |>
  filter(Boulder_type %in% c("LB", "REV-B")) |>
  select(Date, Tidal_height, Boulder_type, n_items) |>
  pivot_wider(names_from = Boulder_type, values_from = n_items)

lb_pairs
wilcox.test(lb_pairs$LB, lb_pairs$`REV-B`, paired = TRUE, exact = FALSE)

# Control boulders (mid only, 5 surveys): too few pairs for a test (minimum p = 0.0625) -> report descriptively
within_mod |> filter(Tidal_height == "M") |>
  select(Date, Boulder_type, n_items) |>
  pivot_wider(names_from = Boulder_type, values_from = n_items)


