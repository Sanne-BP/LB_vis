#continue from 03-SubQuestion3.R for the statistics

##################################################################

#STATISTICS

library(glmmTMB)   # GLMs for counts
library(DHARMa)    # assumption checks
library(emmeans)   # estimated means + change over time per treatment

#Design: 5 treatments x 2 sampling periods (Baseline 2 = 18/06/2025, 6 months = 20/04/2026), n = 2-3 videos per group.
#Each sampling period is ONE day, so Date = Sampling.period: no random effect for Date (it would be the same as the fixed effect). Each video is one replicate (same as SQ2).

#Model: Response ~ Sampling.period * Treatment_pooled
#The INTERACTION is the term that answers SQ3: does the change from baseline to 6 months differ between treatments? (= did MOD change differently from the controls and rocky shores?)

#For every response: fit Poisson first -> check dispersion with DHARMa -> switch to negative binomial (over-dispersion) or generalised Poisson (under-dispersion) if needed -> check again -> test.

#Tests:
#1. drop1() = likelihood ratio test for the interaction (does the change over time differ between treatments?)
#2. emmeans: change over time within each treatment (ratio 6 months / Baseline; > 1 = increase)
#3. emmeans interaction contrast: the change at MOD vs the change at each other treatment (ratio of ratios; > 1 = that treatment increased MORE than MOD)
#   Modified is set as the reference level, so trt.vs.ctrl compares every treatment with MOD (same as SQ2).

#NB: a group where the response is 0 in ALL videos gives an estimate of ~0 with a huge standard error. That comparison can't be tested with a GLM, which is normal - report it as "never observed" and check summary()/diagnose() for it.

stats_data <- sq3_data |>
  select(Code, Treatment_pooled, Sampling.period, Richness, Observations) |>
  mutate(Treatment_pooled = relevel(Treatment_pooled, ref = "Modified"))

stats_data |> count(Treatment_pooled, Sampling.period)   # CHECK: 2-3 videos per group (C1 baseline = 2, RS1 6 months = 2)

#Means per group (for the text)
stats_data |>
  group_by(Treatment_pooled, Sampling.period) |>
  summarise(n = n(),
            mean_richness = round(mean(Richness), 1),
            mean_observations = round(mean(Observations), 1), .groups = "drop")




















#--- SPECIES RICHNESS ---

#Step 1: Poisson
m_rich <- glmmTMB(Richness ~ Sampling.period * Treatment_pooled, family = poisson, data = stats_data)

res_rich <- simulateResiduals(m_rich, n = 1000)
plot(res_rich)
testDispersion(res_rich)          # ratio < 1 and p < 0.05 -> under-dispersed -> use genpois (step 2)
# ratio > 1 and p < 0.05 -> over-dispersed  -> use nbinom2
plotResiduals(res_rich, form = stats_data$Treatment_pooled)
plotResiduals(res_rich, form = stats_data$Sampling.period)


#Model choice: Poisson under-dispersed (DHARMa dispersion = 0.44, p = 0.034).
#Tried: genpois (AIC 103.9) and genpois with dispersion per sampling period (AIC 99.1). Both passed the DHARMa checks, but biased the estimated group means (e.g. MOD baseline 4.14 vs observed 3.33, RS1 baseline 0.09 vs 0.33) and inflated the interaction test (p = 0.0001 vs 0.055) -> Poisson retained. Under-dispersion makes Poisson tests conservative.

summary(m_rich)
diagnose(m_rich)

#1. Does the change over time differ between treatments? (the SQ3 test)
drop1(m_rich, test = "Chisq")

    #Between treatments, the species richness increased over time, but the change differed only         marginally (Poisson GLM, period × treatment: χ² = 9.24, df = 4, p = 0.055).


#Did richness change over time across all sites together? (model without interaction)
m_rich_add <- update(m_rich, . ~ Sampling.period + Treatment_pooled)
drop1(m_rich_add, test = "Chisq")

    #Sampling.period: χ² = 5.73, df = 1, p = 0.017. Richness was significantly higher at 6 months       than at baseline, taking all sites together.


#2. Change over time within each treatment (ratio 6 months / Baseline) + 95% CI
emm_rich <- emmeans(m_rich, ~ Sampling.period | Treatment_pooled, type = "response")
emm_rich
contrast(emm_rich, "revpairwise")
confint(contrast(emm_rich, "revpairwise"))

    #MOD (p = 0.14), increase not significant
    #C1 (p = 1.00), C2 (p = 0.67), no change
    #RS1 (p = 0.019), significant increase
    #RS2 (p = 0.090), marginal increase


#3. (not reported) Change at MOD vs change at every other treatment (ratio of ratios, Dunnett-adjusted)
contrast(emmeans(m_rich, ~ Sampling.period * Treatment_pooled),
         interaction = c("revpairwise", "trt.vs.ctrl"), type = "response", adjust = "dunnettx")






















#--- NUMBER OF OBSERVATIONS ---

#Step 1: Poisson
m_obs_pois <- glmmTMB(Observations ~ Sampling.period * Treatment_pooled, family = poisson, data = stats_data)
testDispersion(simulateResiduals(m_obs_pois, n = 1000))    # ratio > 1 and p < 0.05 -> over-dispersed -> use NB

#Step 2: negative binomial
m_obs <- glmmTMB(Observations ~ Sampling.period * Treatment_pooled, family = nbinom2, data = stats_data)
AIC(m_obs_pois, m_obs)                                     # lower AIC = better model

res_obs <- simulateResiduals(m_obs, n = 1000)
plot(res_obs)
testDispersion(res_obs)
plotResiduals(res_obs, form = stats_data$Treatment_pooled)
plotResiduals(res_obs, form = stats_data$Sampling.period)

summary(m_obs)
diagnose(m_obs)

#SO, negative binomial is good fit!

#1. Does the change over time differ between treatments? (the SQ3 test)
drop1(m_obs, test = "Chisq")

    #Yes, the change over time differed between treatments (p < 0.001)

#If the interaction is NOT significant: main effects
#m_obs_add <- update(m_obs, . ~ Sampling.period + Treatment_pooled)
#drop1(m_obs_add, test = "Chisq")

#2. Change over time within each treatment (ratio 6 months / Baseline)
emm_obs <- emmeans(m_obs, ~ Sampling.period | Treatment_pooled, type = "response")
emm_obs
contrast(emm_obs, "revpairwise")


stats_noRS1 <- stats_data |> filter(Treatment_pooled != "Rocky shore 1") |> droplevels()
m_obs_noRS1 <- glmmTMB(Observations ~ Sampling.period * Treatment_pooled,
                       family = nbinom2, data = stats_noRS1)
drop1(m_obs_noRS1, test = "Chisq")

confint(contrast(emm_obs, "revpairwise"))             # you skipped this one: 95% CI per treatment



#3. Change at MOD vs change at every other treatment (ratio of ratios, Dunnett-adjusted)
contrast(emmeans(m_obs, ~ Sampling.period * Treatment_pooled),
         interaction = c("revpairwise", "trt.vs.ctrl"), type = "response", adjust = "dunnettx")

#RESULT:


#Interaction: χ² = 28.4, df = 4, p < 0.001 -> main-effects model NOT reported
#Sensitivity without RS1: χ² = 7.22, df = 3, p = 0.065 -> interaction driven by RS1's near-empty baseline
#Change per treatment (6 months / baseline, mean observations per video, 95% CI):
#  MOD 29.0 -> 44.3 (×1.5, 0.70-3.35, p = 0.29); C1 32.0 -> 41.7 (×1.3, 0.54-3.12, p = 0.55);
#  C2 57.0 -> 30.7 (×0.54, 0.25-1.17, p = 0.12); RS1 0.3 -> 34.0 (×102, 12-866, p < 0.001);
#  RS2 14.7 -> 44.7 (×3.0, 1.35-6.85, p = 0.007)
#-> observations increased at the rocky shores; small, non-significant changes at MOD and the controls













#COMMUNITY COMPOSITION

sum(zero_rows)            # how many zero-catch videos were dropped
nmds$stress

set.seed(123)
permanova_terms <- adonis2(bray_dist ~ Sampling.period * Treatment_pooled,
                           data = meta_nz, permutations = 999, by = "terms")
permanova_terms

set.seed(123)
permutest(betadisper(bray_dist, meta_nz$Treatment_pooled), permutations = 999)
set.seed(123)
permutest(betadisper(bray_dist, meta_nz$Sampling.period), permutations = 999)

period_change
dissim_time

permanova_terms
period_change
dissim_time












#Species composition: percentages per treatment and period (the numbers in the plot)
comp_plot |>
  arrange(Treatment_pooled, Sampling.period, desc(share)) |>
  mutate(Percent = round(share * 100, 1)) |>
  print(n = Inf)

#Feeding modes: percentages per treatment and period
comp_data |>
  left_join(species_info, by = "Species") |>
  group_by(Treatment_pooled, Sampling.period, Feeding_mode) |>
  summarise(Percent = round(sum(share) * 100, 1), .groups = "drop") |>
  pivot_wider(names_from = Feeding_mode, values_from = Percent)

#Feeding modes: PERMANOVA (does the feeding-mode composition change differently over time per treatment?)
permanova_guild

#Dispersion check for the feeding-mode PERMANOVA (not in the script yet)
guild_dist <- vegdist(select(guild_matrix, all_of(levels(species_info$Feeding_mode))), method = "bray")
set.seed(123)
permutest(betadisper(guild_dist, guild_matrix$Treatment_pooled), permutations = 999)
set.seed(123)
permutest(betadisper(guild_dist, guild_matrix$Sampling.period), permutations = 999)


set.seed(123)
permutest(betadisper(sqrt(guild_dist), guild_matrix$Treatment_pooled), permutations = 999)
set.seed(123)
permutest(betadisper(sqrt(guild_dist), guild_matrix$Sampling.period), permutations = 999)
