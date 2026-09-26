#continue from 01-SubQuestion1.R for the statistics

##################################################################

#STATISTICS

library(glmmTMB)   # GLMMs (counts + random effect for Date)
library(DHARMa)    # assumption checks for GLMMs
library(emmeans)   # estimated means + MOD-ER vs MOD-LB comparison

stats_data <- mod_data_post |> mutate(Date = factor(Date))


#--- SPECIES RICHNESS ---
m_rich <- glmmTMB(Richness ~ Treatment + (1 | Date), family = poisson, data = stats_data)

res_rich <- simulateResiduals(m_rich, n = 1000)
plot(res_rich)                 # all tests n.s. + flat lines = assumptions OK
plotResiduals(res_rich, form = stats_data$Treatment)
testDispersion(res_rich)


#Model does not meet assumptions yet because of under-dispersion
#Dispersion test: ratio 0.26, p = 0.002

m_rich <- glmmTMB(Richness ~ Treatment + (1 | Date), family = genpois, data = stats_data)

res_rich <- simulateResiduals(m_rich, n = 1000)
plot(res_rich)                                   # QQ plot: points should now follow the line
testDispersion(res_rich)                         # hoping for p > 0.05, ratio closer to 1
plotResiduals(res_rich, form = stats_data$Treatment)

#Model assumptions were checked with simulated residuals (DHARMa); no significant deviations were detected (KS test p = 0.83, dispersion test p = 0.76, outlier test p = 1).

summary(m_rich)
diagnose(m_rich)


drop1(m_rich, test = "Chisq")                              # p-value for Treatment
emmeans(m_rich, pairwise ~ Treatment, type = "response")   # means + ratio MOD-ER / MOD-LB

#p=0.3929, so the treatment has no significant effect on species richness

#Species richness did not differ significantly between the Living Boulder rockpools and the surrounding existing revetment (GLMM, χ² = 0.73, df = 1, p = 0.39).










#--- NUMBER OF OBSERVATIONS ---

#Step 1: try Poisson first
m_obs_pois <- glmmTMB(Observations ~ Treatment + (1 | Date), family = poisson, data = stats_data)
testDispersion(simulateResiduals(m_obs_pois, n = 1000))   # ratio > 1 and p < 0.05 -> over-dispersed -> use NB

#dispersion = 2.9365, p-value = 0.094, over-dispersion, so will use negative binomial instead

#Step 2: negative binomial
m_obs <- glmmTMB(Observations ~ Treatment + (1 | Date), family = nbinom2, data = stats_data)
AIC(m_obs_pois, m_obs)                                     # lower AIC = better model
#AIC: 420.4 vs 156.3, so the negative binomial is better by about 264 (ΔAIC = 264)

res_obs <- simulateResiduals(m_obs, n = 1000)
plot(res_obs)
testDispersion(res_obs)
plotResiduals(res_obs, form = stats_data$Treatment)

summary(m_obs)
diagnose(m_obs)
drop1(m_obs, test = "Chisq")
#χ² = 0.13, df = 1, p = 0.72. treatment has no significant effect on the number of observations.

emmeans(m_obs, pairwise ~ Treatment, type = "response")








#--- FEEDING RATES (bites) ---
bites_stats <- bites |> mutate(Date = factor(Date))

bites_stats |> count(Treatment)          # CHECK: 9 videos per habitat?
sum(bites_stats$Bites == 0)              # how many videos had no bites at all

#Step 1: Poisson
m_bites_pois <- glmmTMB(Bites ~ Treatment + (1 | Date), family = poisson, data = bites_stats)
testDispersion(simulateResiduals(m_bites_pois, n = 1000))

#Step 2: negative binomial
m_bites <- glmmTMB(Bites ~ Treatment + (1 | Date), family = nbinom2, data = bites_stats)
AIC(m_bites_pois, m_bites)

#             df      AIC
#m_bites_pois  3 420.3550
#m_bites       4 156.3361

res_bites <- simulateResiduals(m_bites, n = 1000)
plot(res_bites)
testDispersion(res_bites)
testZeroInflation(res_bites)             # significant? -> more zeros than the model expects
plotResiduals(res_bites, form = bites_stats$Treatment)

summary(m_bites)
diagnose(m_bites)
drop1(m_bites, test = "Chisq")
emmeans(m_bites, pairwise ~ Treatment, type = "response")

#Feeding intensity did not differ significantly between the Living Boulder rockpools and the existing revetment (GLMM, χ² = 0.05, df = 1, p = 0.83)
#Feeding varied considerably between sampling days (random-effect SD = 0.67).












#--- COMMUNITY COMPOSITION ---
nrow(meta_nz)                  # CHECK: how many videos are left (zero-catch videos are dropped)
meta_nz |> count(Treatment)

#PERMANOVA, with videos only shuffled within the same day
set.seed(123)
permanova_day <- adonis2(bray_dist ~ Treatment, data = meta_nz,
                         permutations = how(nperm = 999, blocks = meta_nz$Date))
permanova_day

#         Df SumOfSqs      R2      F Pr(>F)
#Model     1   0.0296 0.01766 0.2877  0.829
#Residual 16   1.6461 0.98234
#Total    17   1.6757 1.00000

#PERMANOVA: R² = 0.018, F = 0.29, p = 0.829. Habitat explains only 1.8% of the variation in species composition. The blocked p-value (0.829) differs slightly from your free one (0.893), as expected, and the conclusion stays the same. So fish community composition on the rockpools doesn't look different from the community on the surrounding revetment.

#Assumption check: equal spread (dispersion) in both habitats
disp_within <- betadisper(bray_dist, meta_nz$Treatment)
set.seed(123)
permutest(disp_within, permutations = how(nperm = 999, blocks = meta_nz$Date))

#         Df  Sum Sq   Mean Sq      F N.Perm Pr(>F)
#Groups     1 0.00008 0.0000764 0.0033    999  0.925
#Residuals 16 0.37537 0.0234609

#Dispersion check: F = 0.0033, p = 0.925. Both habitats have practically the same spread, so the assumption is met. The non-significant PERMANOVA reflects genuinely similar communities.



#OVERVIEW:
#Response	              Method	                    Test statistic	      p	      LB vs ER
#Species richness	      Generalised Poisson         GLMM	χ² = 0.73	      0.39	  +9%
#Number of observations	Negative binomial           GLMM	χ² = 0.13	      0.72	  +7%
#Feeding (bites)	      Negative binomial           GLMM	χ² = 0.05	      0.83	  +13%
#Community composition	PERMANOVA (blocked by day)	R² = 0.02, F = 0.29	  0.83	  —

