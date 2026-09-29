# ==============================================================================
# OCCUPATIONAL STRESS DATASET — WORKSHOP SCRIPT (RoleConflict worked example)
# Rajiv Gandhi University, Department of Commerce
#
# THE THREE QUESTIONS WE ASK AT EVERY STEP:
#   1. WHY do it?
#   2. WHY this method over the alternatives?
#   3. WHAT does the reported number mean?
# A good analysis is one you can defend at every single step.
#
# HOW THIS SCRIPT WORKS:
# Run STEP 0 and STEP 1 first (they set everything up). After that, each step is
# self-contained: running it prints the result in the console, pops an image
# (result-card or plot) into the Plots pane, AND saves that image as a .jpg in
# the "outputs" folder. Copy-paste one step, run it, the matching picture
# appears immediately.
# ==============================================================================


# ------------------------------------------------------------------------------
# STEP 0: Install and load packages, set up the image helpers (RUN FIRST)
# ------------------------------------------------------------------------------
install.packages(c("haven", "dplyr", "ggplot2", "car", "rstatix",
                   "FSA", "broom", "caret", "boot", "gridExtra", "rankFD"))

library(haven)      # read SPSS .sav files
library(dplyr)      # data wrangling
library(ggplot2)    # plotting
library(car)        # Levene's test
library(rstatix)    # tidy stats helpers
library(FSA)        # Dunn's post-hoc test
library(broom)      # tidy model output
library(caret)      # cross-validation
library(boot)       # bootstrap resampling
library(gridExtra)  # render result tables as images
library(grid)       # low-level drawing (comes with R)
library(rankFD)     # Brunner-Munzel / rank-based test (assumes neither normality nor equal variance)

dir.create("outputs", showWarnings = FALSE)

# --- Lecture colour theme for the result-card tables --------------------------
card_theme <- ttheme_default(
  core    = list(fg_params = list(fontface = "plain", fontsize = 12),
                 bg_params = list(fill = c("#FDF3E7", "#FFFFFF"))),
  colhead = list(fg_params = list(col = "white", fontface = "bold", fontsize = 13),
                 bg_params = list(fill = "#E8622C")),
  rowhead = list(fg_params = list(fontface = "italic"))
)

# --- Helper 1: show a result-card on screen AND save it as a jpg --------------
show_card <- function(df, title, file, height = NULL) {
  df_rounded <- as.data.frame(df)
  num_cols <- sapply(df_rounded, is.numeric)
  df_rounded[num_cols] <- lapply(df_rounded[num_cols], function(x) round(x, 4))
  tbl  <- tableGrob(df_rounded, rows = NULL, theme = card_theme)
  ttl  <- textGrob(title, gp = gpar(fontsize = 16, fontface = "bold", col = "#1F3A5F"), vjust = 1)
  card <- arrangeGrob(ttl, tbl, ncol = 1,
                      heights = unit.c(unit(1.2, "lines"), unit(1, "null")))
  grid.newpage(); grid.draw(card)
  h <- if (is.null(height)) 1 + 0.35 * nrow(df_rounded) + 1 else height
  ggsave(file, card, width = 8, height = h, dpi = 300, limitsize = FALSE)
}

# --- Helper 2: show a ggplot on screen AND save it ---------------------------
show_plot <- function(p, file, width = 8, height = 6) {
  print(p)
  ggsave(file, p, width = width, height = height, dpi = 300)
}


# ------------------------------------------------------------------------------
# STEP 1: Load and inspect the data (RUN SECOND)
# ------------------------------------------------------------------------------
# WHY: you cannot analyse what you have not looked at. First job is to load the
#      data and confirm it is what we expect (right number of rows, right groups).
# NOTE: the file name contains spaces, so it must be quoted exactly as below.
data <- read_sav("Total data- Rajashree.sav")

str(data)
dim(data)                       # should be 400 rows, 146 columns

data$OrgType <- factor(data$Organisationtype,
                       levels = c(1, 2, 3, 4),
                       labels = c("Administrative", "Banking",
                                  "Educational", "Industrial"))
data$GenderLabel <- factor(data$Gender, levels = c(1, 2),
                           labels = c("Male", "Female"))

constructs <- c("RoleAmbiguity", "RoleConflict", "RoleOverload",
                "WorkLifeConflict", "SocialSupport", "PerfncRelatedStress",
                "JobSatisfaction", "SelfConcept", "OrganisationalCommitment",
                "SupervisorsEvaluation", "IntentionalToLeave",
                "FulfilmentExpectations", "Lvl_Sress")

group_counts <- as.data.frame(table(data$OrgType))
colnames(group_counts) <- c("Organisation Type", "N")
show_card(group_counts, "Sample Size by Organisation Type",
          "outputs/step1_group_counts.jpg")


# ------------------------------------------------------------------------------
# STEP 2: Check for missing values
# ------------------------------------------------------------------------------
# WHY: missing data can bias or break an analysis. We never assume it is clean.
# WHY THIS WAY: colSums(is.na()) is the simplest complete count, variable by variable.
# WHAT IT MEANS: every number here should be 0 -> nothing missing, nothing to fix.
#   "We checked and found nothing" is itself a real, documented step.
missing_tbl <- data.frame(Variable = constructs,
                          Missing  = as.integer(colSums(is.na(data[constructs]))))
print(missing_tbl)
cat("Total missing values in whole dataset:", sum(is.na(data)), "\n")
show_card(missing_tbl, "Missing Values Check (expect all 0)",
          "outputs/step2_missing_values.jpg")


# ------------------------------------------------------------------------------
# STEP 3: Check for duplicate records
# ------------------------------------------------------------------------------
# WHY: a duplicated respondent would be counted twice and inflate our confidence.
# WHAT IT MEANS: 0 = every row is a unique employee.
dup_count <- sum(duplicated(data))
print(dup_count)
show_card(data.frame(Check = "Duplicate rows", Count = dup_count),
          "Duplicate Records Check (expect 0)", "outputs/step3_duplicates.jpg")


# ------------------------------------------------------------------------------
# STEP 4: Check for outliers (RoleConflict)
# ------------------------------------------------------------------------------
# WHY: extreme values can distort averages and mislead tests, so we look first.
# WHY IQR/boxplot: it is simple and non-parametric -- it does not itself assume
#      the data is normal, which suits data we suspect is not normal.
# WHAT IT MEANS: flagged points are "unusual", NOT automatically "wrong". We
#      trace them to the raw answers before deciding. Here they are genuine
#      responses (real people at the extremes), so we KEEP them -- removing real
#      data to tidy a result is a form of p-hacking.
box_plot <- ggplot(data, aes(x = OrgType, y = RoleConflict, fill = OrgType)) +
  geom_boxplot() +
  labs(title = "Role Conflict by Organisation Type",
       x = "Organisation Type", y = "Role Conflict Score") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")
show_plot(box_plot, "outputs/step4_boxplot.jpg")

Q1 <- quantile(data$RoleConflict, 0.25); Q3 <- quantile(data$RoleConflict, 0.75)
IQR_val <- Q3 - Q1
outliers <- data %>% filter(RoleConflict < Q1 - 1.5*IQR_val |
                            RoleConflict > Q3 + 1.5*IQR_val)
cat("Number of IQR-flagged outliers (whole-sample):", nrow(outliers), "\n")


# ------------------------------------------------------------------------------
# STEP 5: Descriptive statistics by organisation type
# ------------------------------------------------------------------------------
# WHY: before any test, understand the basic shape -- centre and spread per group.
# WHAT IT MEANS: mean/median = typical value; sd = spread; comparing medians
#      across groups previews what the formal test will confirm.
desc_table <- data %>% group_by(OrgType) %>%
  summarise(n = n(), mean = mean(RoleConflict), sd = sd(RoleConflict),
            median = median(RoleConflict), min = min(RoleConflict),
            max = max(RoleConflict))
print(as.data.frame(desc_table))
show_card(desc_table, "Descriptive Statistics: Role Conflict",
          "outputs/step5_descriptives.jpg")


# ------------------------------------------------------------------------------
# STEP 6: Normality check (Shapiro-Wilk + Q-Q plot)
# ------------------------------------------------------------------------------
# WHY: the choice between parametric and non-parametric tests hinges on normality.
# WHY SHAPIRO-WILK: it has the best statistical power for detecting non-normality
#      across most situations (see Xu & Goodacre 2025).
# WHAT IT MEANS: W is a 0-1 score of how bell-shaped the data is (1 = perfect);
#      p < 0.05 means "not normal". Expect all four groups to fail (p < 0.05).
shapiro_show <- as.data.frame(
  data %>% group_by(OrgType) %>%
    summarise(shapiro_W = shapiro.test(RoleConflict)$statistic,
              shapiro_p = shapiro.test(RoleConflict)$p.value))
shapiro_show$Verdict <- ifelse(shapiro_show$shapiro_p < 0.05, "NOT normal", "Looks normal")
print(shapiro_show)
show_card(shapiro_show, "Normality Check: Shapiro-Wilk (p < .05 = not normal)",
          "outputs/step6_shapiro.jpg")

qqnorm(data$RoleConflict[data$OrgType == "Administrative"],
       main = "Q-Q Plot: Role Conflict (Administrative)")
qqline(data$RoleConflict[data$OrgType == "Administrative"])
jpeg("outputs/step6_qqplot.jpg", width = 8, height = 6, units = "in", res = 300)
qqnorm(data$RoleConflict[data$OrgType == "Administrative"],
       main = "Q-Q Plot: Role Conflict (Administrative)")
qqline(data$RoleConflict[data$OrgType == "Administrative"]); dev.off()


# ------------------------------------------------------------------------------
# STEP 7: Homogeneity of variance (Levene's test)
# ------------------------------------------------------------------------------
# WHY: many tests assume groups have similar spread; this is the second gatekeeper.
# WHY LEVENE'S (median-based): the median version is robust to non-normality,
#      unlike Bartlett's test which itself assumes normality (which we lack).
# WHAT IT MEANS: df1 = groups - 1 (=3), df2 = N - groups (=396), F = how unequal
#      the spreads are; p < 0.05 means variances are NOT equal. Expect p < 0.05.
levene_result <- leveneTest(RoleConflict ~ OrgType, data = data)
print(levene_result)
levene_tbl <- data.frame(
  Statistic = round(levene_result$`F value`[1], 4),
  df1 = levene_result$Df[1], df2 = levene_result$Df[2],
  p_value = signif(levene_result$`Pr(>F)`[1], 4),
  Verdict = ifelse(levene_result$`Pr(>F)`[1] < 0.05,
                   "Variances NOT equal", "Variances roughly equal"))
show_card(levene_tbl, "Homogeneity of Variance: Levene's Test",
          "outputs/step7_levene.jpg")


# ------------------------------------------------------------------------------
# STEP 8: Choose and run the appropriate test
# ------------------------------------------------------------------------------
# WHERE WE ARE: normality FAILS (Step 6) and equal variance FAILS (Step 7).
# WHY NOT one-way ANOVA: it assumes normality -> ruled out.
# WHY NOT Welch's ANOVA: it relaxes equal variance BUT still assumes normality
#      -> also ruled out.
# WHY NOT (only) Kruskal-Wallis: it handles non-normality, but it still assumes
#      the groups have a similar shape/spread -- which Levene's says they do not.
# WHY BRUNNER-MUNZEL / rankFD: this rank-based test assumes NEITHER normality NOR
#      equal variance NOR equal shape. It is the one option that survives both of
#      our failed checks. TEACHING POINT: it is always worth looking beyond the
#      classical tests for a method that fits your data -- and if none exists, you
#      fall back on judgement (which violation is worse, and choose accordingly).
# WHAT IT MEANS: rankFD reports an ANOVA-Type Statistic (ATS) and a p-value; it
#      works with "relative effects" (the chance a value from one group exceeds a
#      value from another). p < 0.05 -> the groups genuinely differ.
bm_result <- rankFD(RoleConflict ~ OrgType, data = data)
print(bm_result)   # <-- the console output shows all three tests (ATS, WTS, Kruskal-Wallis)

# The ANOVA-Type Statistic (ATS) is the Brunner-Munzel result we want. It lives in
# bm_result$ANOVA.Type.Statistic as a 1-row matrix: Statistic, df1, df2, p-value.
ats <- bm_result$ANOVA.Type.Statistic
stat_val <- as.numeric(ats[1, 1])
p_val    <- as.numeric(ats[1, ncol(ats)])   # p-value is the last column

ats_tbl <- data.frame(
  Test = "Brunner-Munzel (ATS, rankFD)",
  Statistic = round(stat_val, 3),
  df1 = round(as.numeric(ats[1, 2]), 2),
  df2 = round(as.numeric(ats[1, 3]), 2),
  p_value = signif(p_val, 4),
  Verdict = ifelse(p_val < 0.05, "Groups DIFFER significantly",
                   "No significant difference"))
print(ats_tbl)
show_card(ats_tbl,
          "Group Comparison: Brunner-Munzel Test (neither assumption needed)",
          "outputs/step8_brunner_munzel.jpg")


# ------------------------------------------------------------------------------
# STEP 9: Post-hoc comparisons + effect size
# ------------------------------------------------------------------------------
# WHY: a significant overall test only says "at least one group differs" -- the
#      post-hoc tells us WHICH pairs differ.
# WHY DUNN'S: it is the rank-based post-hoc that matches a rank-based omnibus test
#      (same ranking logic). WHY BH correction: many pairwise tests inflate false
#      positives, so we control the false discovery rate.
dunn_result <- dunnTest(RoleConflict ~ OrgType, data = data, method = "bh")
print(dunn_result)
show_card(dunn_result$res, "Post-hoc: Dunn's Test (Benjamini-Hochberg corrected)",
          "outputs/step9_dunn.jpg")

# WHY EFFECT SIZE: a p-value says IF there is a difference, not HOW BIG. With
#      N=400 even tiny differences can be "significant", so effect size tells us
#      whether it actually matters. epsilon-squared is the KW-family effect size.
eff_result <- data %>% kruskal_effsize(RoleConflict ~ OrgType, method = "epsilon2")
print(as.data.frame(eff_result))
show_card(as.data.frame(eff_result), "Effect Size (epsilon-squared)",
          "outputs/step9_effectsize.jpg")


# ------------------------------------------------------------------------------
# STEP 10: PCA across all 12 constructs
# ------------------------------------------------------------------------------
# WHY: the individual tests ask "does each construct differ?"; PCA asks the
#      different question "taken all together, is there an overall pattern that
#      separates the organisation types?" -- the bird's-eye multivariate view.
# WHY STANDARDISE FIRST: the constructs sit on different scales, so without
#      scaling the widest-scale one would dominate (same principle as Pareto
#      scaling before a variance-based method).
# WHAT IT MEANS: PC1/PC2 are new combined axes; the % variance is how much of the
#      overall spread each captures.
pca_scaled <- scale(data[constructs[1:12]])
pca_result <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)
print(summary(pca_result))
data$PC1 <- pca_result$x[, 1]; data$PC2 <- pca_result$x[, 2]

pca_plot <- ggplot(data, aes(x = PC1, y = PC2, colour = OrgType)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "PCA of the 12 Occupational Stress Constructs",
       x = "PC1 (~28% of variance)", y = "PC2 (~13% of variance)") +
  theme_minimal(base_size = 14)
show_plot(pca_plot, "outputs/step10_pca_scatter.jpg")

pca_var <- as.data.frame(round(summary(pca_result)$importance[, 1:5], 4))
pca_var <- cbind(Measure = rownames(pca_var), pca_var)
show_card(pca_var, "PCA: Variance Explained (first 5 components)",
          "outputs/step10_pca_variance.jpg")


# ------------------------------------------------------------------------------
# STEP 11: Multiple linear regression
# ------------------------------------------------------------------------------
# WHY: group tests look at one factor at a time. Regression asks "does
#      organisation type still matter AFTER accounting for gender and age?" -- it
#      weighs several predictors together.
# WHY LINEAR regression: our outcome (a composite score) is approximately
#      continuous, and linear regression is the simplest, most interpretable
#      model for a continuous outcome. ALTERNATIVES and why not: logistic (binary
#      outcome), ordinal logistic (a single ordered item like Lvl_Sress), Poisson
#      (counts) -- none match a continuous composite as cleanly.
# WHAT IT MEANS: R-squared = proportion of variation in RoleConflict explained by
#      the predictors together (0-1). ~0.29 here = ~29% explained; the other ~71%
#      is unmeasured factors. In social science 0.29 is respectable -- there is no
#      universal "good" threshold, judge it against the field. Adjusted R-squared
#      is the more honest number as it penalises adding useless predictors.
reg_model <- lm(RoleConflict ~ OrgType + GenderLabel + Age, data = data)
print(summary(reg_model))

reg_tbl <- as.data.frame(tidy(reg_model))
reg_tbl[ , c("estimate","std.error","statistic","p.value")] <-
  round(reg_tbl[ , c("estimate","std.error","statistic","p.value")], 4)
show_card(reg_tbl,
          paste0("Regression: Role Conflict ~ Org + Gender + Age  (R2 = ",
                 round(summary(reg_model)$r.squared, 3),
                 ", adj R2 = ", round(summary(reg_model)$adj.r.squared, 3), ")"),
          "outputs/step11_regression.jpg")

# WHY check residuals (not raw data) for normality: regression's normality
#      assumption is about the model's leftover errors, not the raw scores.
shapiro.test(residuals(reg_model))
jpeg("outputs/step11_residuals.jpg", width = 8, height = 6, units = "in", res = 300)
hist(residuals(reg_model), main = "Residuals of the Regression Model",
     xlab = "Residual", col = "grey80"); dev.off()


# ------------------------------------------------------------------------------
# STEP 12: 5-fold cross-validation of the regression
# ------------------------------------------------------------------------------
# WHY: a model always fits its own training data well; CV checks whether it holds
#      up on data it has NOT seen -- the guard against overfitting.
# WHY 5-FOLD (not 3, 10, or leave-one-out): 5 is the standard bias-variance
#      compromise. Too few folds (3) makes each training set small (high bias);
#      too many (10 / LOO) is heavier and gives higher-variance estimates. 5 and
#      10 are the conventional choices; 5 is lighter and ample here.
# WHAT IT MEANS: cross-validated R-squared (~0.24) a little below the full-sample
#      0.29 is normal and healthy; a big drop would signal overfitting.
set.seed(42)
cv_model <- train(RoleConflict ~ OrgType + GenderLabel + Age,
                  data = data, method = "lm",
                  trControl = trainControl(method = "cv", number = 5))
print(cv_model)
cv_tbl <- data.frame(RMSE = round(cv_model$results$RMSE, 4),
                     Rsquared = round(cv_model$results$Rsquared, 4),
                     MAE = round(cv_model$results$MAE, 4))
show_card(cv_tbl, "5-Fold Cross-Validation of the Regression Model",
          "outputs/step12_crossvalidation.jpg")


# ------------------------------------------------------------------------------
# STEP 13: Permutation test
# ------------------------------------------------------------------------------
# WHY: this is the most assumption-light check available -- it makes almost no
#      distributional assumptions, so it confirms our group difference WITHOUT
#      relying on the equal-spread assumption we could not fully meet in Step 7.
# WHY 2000 shuffles (not 200 or 20000): the count sets the RESOLUTION of the
#      p-value. 2000 resolves down to ~1/2000 = 0.0005, which is ample. 200 is
#      too coarse to see very small p-values; 20000 is finer but unnecessary when
#      the result is already clearly significant. 1000-5000 is the usual range.
# WHAT IT MEANS: p = fraction of shuffled datasets that produced a difference as
#      large as the real one. Tiny p -> the real result is very unlikely by chance.
set.seed(42)
observed_stat <- kruskal.test(RoleConflict ~ OrgType, data = data)$statistic
n_permutations <- 2000
perm_stats <- numeric(n_permutations)
for (i in 1:n_permutations) {
  perm_stats[i] <- kruskal.test(data$RoleConflict ~ sample(data$OrgType))$statistic
}
perm_p_value <- (sum(perm_stats >= observed_stat) + 1) / (n_permutations + 1)
cat("Observed:", round(observed_stat, 3), "| Permutation p:", perm_p_value, "\n")

perm_label <- c(paste0("Observed = ", round(observed_stat, 1)),
                paste0("Permutation p = ", signif(perm_p_value, 3)),
                paste0("Rounds = ", n_permutations))
plot_perm <- function() {
  hist(perm_stats, breaks = 40, main = "Permutation Test: Shuffled vs Observed",
       xlab = "Test statistic", col = "grey80")
  abline(v = observed_stat, col = "red", lwd = 3)
  legend("topright", legend = perm_label, bty = "n", text.col = "red", cex = 1.1)
}
plot_perm()
jpeg("outputs/step13_permutation.jpg", width = 8, height = 6, units = "in", res = 300)
plot_perm(); dev.off()


# ------------------------------------------------------------------------------
# STEP 14: Bootstrapping
# ------------------------------------------------------------------------------
# WHY: the permutation test asks "is it significant?"; the bootstrap asks the
#      DIFFERENT question "how stable / precise is our estimate?" -- it builds a
#      confidence interval by resampling the data with replacement.
# WHY 2000 resamples: enough for a smooth, stable confidence interval (1000 is
#      the common floor); going much higher adds precision we do not need here.
# WHAT IT MEANS: 14a -> a 95% CI for the Banking-Industrial gap that does not
#      cross zero means the difference is stable, not a one-sample fluke.
#      14b -> a 95% CI for R-squared shows how much it would wobble in a new sample.

# --- 14a: bootstrap the Banking - Industrial median gap ---
median_gap <- function(d, indices) {
  dd <- d[indices, ]
  median(dd$RoleConflict[dd$OrgType == "Banking"]) -
    median(dd$RoleConflict[dd$OrgType == "Industrial"])
}
two_groups <- data %>% filter(OrgType %in% c("Banking", "Industrial")) %>%
  mutate(OrgType = droplevels(OrgType))
set.seed(42)
boot_gap <- boot(two_groups, median_gap, R = 2000, strata = two_groups$OrgType)
boot_ci_gap <- boot.ci(boot_gap, type = "perc"); print(boot_ci_gap)

gap_label <- c(paste0("Observed gap = ", round(boot_gap$t0, 2)),
               paste0("95% CI = [", round(boot_ci_gap$percent[4], 2), ", ",
                      round(boot_ci_gap$percent[5], 2), "]"),
               paste0("Resamples = ", boot_gap$R))
plot_gap <- function() {
  hist(boot_gap$t, breaks = 40, main = "Bootstrap: Banking - Industrial Median Gap",
       xlab = "Median difference", col = "grey80")
  abline(v = boot_gap$t0, col = "red", lwd = 3)
  legend("topright", legend = gap_label, bty = "n", text.col = "red", cex = 1.1)
}
plot_gap()
jpeg("outputs/step14_bootstrap_gap.jpg", width = 8, height = 6, units = "in", res = 300)
plot_gap(); dev.off()

# --- 14b: bootstrap the regression R-squared ---
boot_r2 <- function(d, indices) {
  summary(lm(RoleConflict ~ OrgType + GenderLabel + Age, data = d[indices, ]))$r.squared
}
set.seed(42)
boot_r2_result <- boot(data, boot_r2, R = 1000)
boot_ci_r2 <- boot.ci(boot_r2_result, type = "perc"); print(boot_ci_r2)

r2_label <- c(paste0("Observed R2 = ", round(boot_r2_result$t0, 3)),
              paste0("95% CI = [", round(boot_ci_r2$percent[4], 3), ", ",
                     round(boot_ci_r2$percent[5], 3), "]"),
              paste0("Resamples = ", boot_r2_result$R))
plot_r2 <- function() {
  hist(boot_r2_result$t, breaks = 40, main = "Bootstrap: Regression R-squared",
       xlab = "R-squared", col = "grey80")
  abline(v = boot_r2_result$t0, col = "red", lwd = 3)
  legend("topleft", legend = r2_label, bty = "n", text.col = "red", cex = 1.1)
}
plot_r2()
jpeg("outputs/step14_bootstrap_r2.jpg", width = 8, height = 6, units = "in", res = 300)
plot_r2(); dev.off()


# ------------------------------------------------------------------------------
# STEP 15: Multiple testing correction across all 12 constructs
# ------------------------------------------------------------------------------
# WHY: if we test all 12 constructs, the chance of at least one false positive
#      climbs well above 5%. We correct for that.
# WHY BH: the Benjamini-Hochberg procedure controls the false discovery rate,
#      the right balance when testing many related outcomes (stricter methods
#      like Bonferroni over-correct and cause false negatives).
# WHAT IT MEANS: constructs still significant after correction are trustworthy
#      differences. Expect all 12 to survive here -- strong, real effects.
raw_p_values <- sapply(constructs[1:12], function(v) {
  kruskal.test(data[[v]] ~ data$OrgType)$p.value
})
adjusted_p_values <- p.adjust(raw_p_values, method = "BH")
results_table <- data.frame(
  Construct = constructs[1:12],
  Raw_p = signif(raw_p_values, 3),
  Adjusted_p = signif(adjusted_p_values, 3),
  Significant = adjusted_p_values < 0.05)
results_table <- results_table[order(results_table$Raw_p), ]
print(results_table, row.names = FALSE)
show_card(results_table, "Multiple Testing: All 12 Constructs (BH corrected)",
          "outputs/step15_multiple_testing.jpg", height = 6)


# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat("\n\nAll images saved to the 'outputs' folder:\n")
print(list.files("outputs"))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
