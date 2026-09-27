#continue from 02-SubQuestion2.R for the statistics

##################################################################

#STATISTICS

library(glmmTMB)   # GLMs / GLMMs for counts
library(DHARMa)    # assumption checks
library(emmeans)   # estimated means + MOD vs each other site

#All five sites were sampled on the same day (20/04/2026), so there is no random effect for Date here (unlike SQ1): each video is one replicate, n = 2-3 per site.

#For every response: fit Poisson first -> check dispersion with DHARMa -> switch to negative binomial (over-dispersion) or generalised Poisson (under-dispersion) if needed -> check again -> test.

#Test for site: drop1() = likelihood ratio test (is there ANY difference between the 5 sites?)
#Then emmeans "trt.vs.ctrl" with Modified as reference: MOD vs C1, C2, RS1 and RS2 (Dunnett-adjusted), which are the comparisons SQ2 is about. Ratios > 1 mean more at that site than at MOD.

#NB: a site where the response is 0 in ALL videos (e.g. no bites at RS1) gives an estimate of ~0 with a huge standard error. That comparison can't be tested with a GLM, which is normal - report it as "never observed" and check summary()/diagnose() for it.

stats_data <- sq2_data |> select(Code, Treatment_pooled, Richness, Observations,
                                 Cryptobenthic_obs)
stats_data |> count(Treatment_pooled)       # CHECK: 3 per site, RS1 = 2



















#--- SPECIES RICHNESS ---

m_rich <- glmmTMB(Richness ~ Treatment_pooled, family = poisson, data = stats_data)

res_rich <- simulateResiduals(m_rich, n = 1000)
plot(res_rich)
testDispersion(res_rich)          # ratio < 1 and p < 0.05 -> under-dispersed -> use genpois below
plotResiduals(res_rich, form = stats_data$Treatment_pooled)

#checks all look good, so Poisson is fine for richness
#Model assumptions were checked with simulated residuals (DHARMa); no significant deviations were detected (KS test p = 0.79, dispersion test p = 0.28, outlier test p = 1). Within-site residuals uniform and homogeneous (Levene n.s.).

summary(m_rich)
diagnose(m_rich)
drop1(m_rich, test = "Chisq")
emmeans(m_rich, trt.vs.ctrl ~ Treatment_pooled, ref = "Modified", type = "response")

#RESULT:
#Species richness did not differ significantly between sites
#(Poisson GLM, χ² = 5.53, df = 4, p = 0.24).

#Mean richness per video: C1 = 3.0, C2 = 3.3, RS1 = 4.0, RS2 = 6.0, MOD = 6.0.
#Compared with MOD, richness was 50% lower at C1 (p = 0.27), 44% lower at C2 (p = 0.38), 33% lower at RS1 (p = 0.71) and equal at RS2 (p = 1.00) (Dunnett-adjusted).

#"Species richness did not differ significantly between sites (χ² = 5.53, df = 4, p = 0.24). Mean richness was highest at the modified site and Rocky shore 2 (6.0 species per video) and lowest at the control revetments (3.0–3.3)."

#Reason for lack of significance is small sample size (n = 2-3 per site)
#SO, the direction is consistent with a positive effect, but because of low replication it means that this can't be confirmed












#--- NUMBER OF OBSERVATIONS ---

#Step 1: Poisson
m_obs_pois <- glmmTMB(Observations ~ Treatment_pooled, family = poisson, data = stats_data)
testDispersion(simulateResiduals(m_obs_pois, n = 1000))
  #strongly over-dispersed (dispersion = 5.3967, p-value < 2.2e-16)

#Step 2: negative binomial
m_obs <- glmmTMB(Observations ~ Treatment_pooled, family = nbinom2, data = stats_data)
AIC(m_obs_pois, m_obs)

  #df      AIC
  #m_obs_pois  5 159.6693
  #m_obs       6 126.2918
  #SO, negative binomial fits much better


res_obs <- simulateResiduals(m_obs, n = 1000)
plot(res_obs)
testDispersion(res_obs)
plotResiduals(res_obs, form = stats_data$Treatment_pooled)

#Poisson model strongly over-dispersed (DHARMa dispersion = 5.40, p < 0.001) -> negative binomial (nbinom2), which fitted much better (AIC 126.3 vs 159.7).
#NB model assumptions met (DHARMa: KS test p = 0.36, dispersion test p = 0.97, outlier test p = 1; within-site residuals uniform, Levene n.s.).

summary(m_obs)
diagnose(m_obs)
drop1(m_obs, test = "Chisq")
emmeans(m_obs, trt.vs.ctrl ~ Treatment_pooled, ref = "Modified", type = "response")

#RESULT:
#Number of observations did not differ between sites
#(negative binomial GLM, χ² = 2.03, df = 4, p = 0.73).

#Mean observations per video: C1 = 41.7, C2 = 30.7, RS1 = 34.0, RS2 = 44.7, MOD = 44.3.
#Compared with MOD: C1 ×0.94 (p = 0.99), C2 ×0.69 (p = 0.57), RS1 ×0.77 (p = 0.82), RS2 ×1.01 (p = 1.00) (Dunnett-adjusted).


















#--- COMMUNITY COMPOSITION ---
#Uses bray_dist + meta_nz from the NMDS section in 02-SubQuestion2.R (zero-catch videos already dropped)

nrow(meta_nz)                  # CHECK: how many videos are left
meta_nz |> count(Treatment_pooled)

#PERMANOVA: do the sites differ in community composition?
#Free permutations (all videos are from the same day, so no blocking by Date like in SQ1)
set.seed(123)
permanova <- adonis2(bray_dist ~ Treatment_pooled, data = meta_nz, permutations = 999)
permanova

#RESULT:

#Df SumOfSqs      R2      F Pr(>F)
#Model     4   1.8288 0.56966 2.9785  0.002 **
#Residual  9   1.3815 0.43034
#Total    13   3.2103 1.00000

#Assumption check: equal spread (dispersion) at all sites.
#If significant, a significant PERMANOVA could (partly) be caused by differences in spread instead of composition.
disp_treatment <- betadisper(bray_dist, meta_nz$Treatment_pooled)
set.seed(123)
permutest(disp_treatment, permutations = 999)

#RESULT:
#Response: Distances
#Df   Sum Sq  Mean Sq      F N.Perm Pr(>F)
#Groups     4 0.095245 0.023811 1.7207    999  0.237
#Residuals  9 0.124546 0.013838

#Community composition differed significantly between sites
#(PERMANOVA, R² = 0.57, F(4,9) = 2.98, p = 0.002).
#Dispersion check: homogeneous multivariate dispersion (betadisper, F = 1.72, p = 0.24) -> PERMANOVA result reflects differences in composition, not in spread.



#Pairwise: Modified vs each other site
#NB: with only 2-3 videos per site the smallest possible p-value is ~0.1, so these can never be significant -> report R2 as effect size, p-values descriptive only.
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

#RESULT:
#Pairwise PERMANOVA (MOD vs each site): R² = 0.26 (C1), 0.26 (C2), 0.39 (RS1), 0.61 (RS2).
#No comparison significant (p = 0.1-0.3; Holm-adjusted p = 0.4), but with n = 2-3 per site the minimum attainable p = 0.1, which was reached for RS1 and RS2.
#-> MOD community most similar to control revetments, most different from rocky shores (esp. RS2).



#Is the modified site more similar to the control revetments or to the natural rocky shores?
#Mean Bray-Curtis dissimilarity between every MOD video and every video of the other sites (0 = identical communities, 1 = no species in common). Descriptive only, no test.
bray_mat  <- as.matrix(bray_dist)
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

#RESULT:
#Mean Bray-Curtis dissimilarity to MOD: C1 = 0.43, C2 = 0.59, RS1 = 0.73, RS2 = 0.82.
#Control revetments 0.51 ± 0.12 vs rocky shores 0.78 ± 0.15; controls vs rocky shores = 0.80.
#-> Six months after installation, the MOD community is more similar to the control revetments than to the natural rocky shores.





















#--- FEEDING-MODE COMPOSITION ---
#Does the division of observations over the feeding guilds differ between sites?
#Same approach as the species PERMANOVA, but on a video x feeding-mode matrix (from guild_data in 02-SubQuestion2.R)

guild_matrix <- guild_data |>
  pivot_wider(names_from = Feeding_mode, values_from = Observations) |>
  filter(Code %in% meta_nz$Code)                              # same videos as the species PERMANOVA

guild_cols <- setdiff(names(guild_matrix), c("Code", "Treatment_pooled"))
guild_cols                                                    # CHECK: only feeding modes

guild_dist <- vegdist(guild_matrix[, guild_cols], method = "bray")

set.seed(123)
permanova_guild <- adonis2(guild_dist ~ Treatment_pooled, data = guild_matrix, permutations = 999)
permanova_guild

disp_guild <- betadisper(guild_dist, guild_matrix$Treatment_pooled)
set.seed(123)
permutest(disp_guild, permutations = 999)

#RESULT:


















#--- CRYPTOBENTHIC FISH (number of observations) ---

m_crypto_pois <- glmmTMB(Cryptobenthic_obs ~ Treatment_pooled, family = poisson, data = stats_data)
testDispersion(simulateResiduals(m_crypto_pois, n = 1000))

m_crypto <- glmmTMB(Cryptobenthic_obs ~ Treatment_pooled, family = nbinom2, data = stats_data)
AIC(m_crypto_pois, m_crypto)
#Poisson is strongly over-dispersed, negative binomial fits better


res_crypto <- simulateResiduals(m_crypto, n = 1000)
plot(res_crypto)
testDispersion(res_crypto)
testZeroInflation(res_crypto)              # significant? -> more zeros than the model expects
plotResiduals(res_crypto, form = stats_data$Treatment_pooled)


#Poisson strongly over-dispersed (DHARMa dispersion = 5.75, p < 0.001) -> negative binomial (AIC 91.2 vs 117.3).
#NB model: KS p = 0.66, dispersion p = 0.72, outliers p = 1, no zero-inflation (p = 0.79); residuals per site uniform, Levene n.s.
#The quantile test in residuals vs predicted was significant, but with a categorical predictor (5 predicted values, n = 14) this test is unstable; the per-site residual checks (the appropriate check for a factor) showed no problems.

summary(m_crypto)
diagnose(m_crypto)
drop1(m_crypto, test = "Chisq")
emmeans(m_crypto, trt.vs.ctrl ~ Treatment_pooled, ref = "Modified", type = "response")

#RESULT:
#Number of cryptobenthic observations did not differ between sites (negative binomial GLM, χ² = 3.80, df = 4, p = 0.43).
#Mean per video: C1 = 3.0, C2 = 8.7, RS1 = 13.5, RS2 = 5.7, MOD = 4.7.
#Compared with MOD: C1 ×0.64 (p = 0.90), C2 ×1.86 (p = 0.77), RS1 ×2.89 (p = 0.46), RS2 ×1.21 (p = 0.98) (Dunnett-adjusted).










#--- FEEDING RATES (bites) ---

bites |> count(Treatment_pooled)          # CHECK: same videos as above
sum(bites$Bites == 0)                     # how many videos had no bites at all

m_bites_pois <- glmmTMB(Bites ~ Treatment_pooled, family = poisson, data = bites)
testDispersion(simulateResiduals(m_bites_pois, n = 1000))

m_bites <- glmmTMB(Bites ~ Treatment_pooled, family = nbinom2, data = bites)
AIC(m_bites_pois, m_bites)

#df       AIC
#m_bites_pois  5 105.67906
#m_bites       6  63.80939
#So, negative binomial it is

res_bites <- simulateResiduals(m_bites, n = 1000)
plot(res_bites)
testDispersion(res_bites)
testZeroInflation(res_bites)
plotResiduals(res_bites, form = bites$Treatment_pooled)

#Poisson strongly over-dispersed (DHARMa dispersion = 8.49, p < 0.001) -> negative binomial (AIC 63.8 vs 105.7).
#NB model: KS p = 0.42, dispersion p = 0.91, outliers p = 1, no zero-inflation (8 zeros observed, ratio 1.07, p = 1); residuals per site uniform, Levene n.s.

summary(m_bites)
diagnose(m_bites)
drop1(m_bites, test = "Chisq")
emmeans(m_bites, trt.vs.ctrl ~ Treatment_pooled, ref = "Modified", type = "response")

#RESULT:
#Number of bites did not differ significantly between sites (negative binomial GLM, χ² = 6.29, df = 4, p = 0.18).
#Mean bites per video: C1 = 7.0, C2 = 2.3, RS1 = 0 (no feeding observed), RS2 = 0.3, MOD = 8.3.
#Compared with MOD: C1 ×0.84 (p = 1.00), C2 ×0.28 (p = 0.71), RS2 ×0.04 (p = 0.15) (Dunnett-adjusted).
#Feeding observed in 3/3 MOD videos vs 1/3 at C1, C2 and RS2, and 0/2 at RS1.



















#OVERVIEW (fill in):
#Response	                       Method	                          Test statistic	   p	   MOD vs other sites
#Number of observations
#Species richness
#Cryptobenthic observations
#Feeding (bites)
#Community composition           PERMANOVA
#Feeding-mode composition        PERMANOVA
