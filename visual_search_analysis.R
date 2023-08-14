# load "rstatix" and "tidyverse" packages---------------------------------------
library("tidyverse")
library("rstatix")


# load data---------------------------------------------------------------------
results_df <- read.csv("visual_search_results_cleaned.csv")
results_df$distractorCount <- as.factor(results_df$distractorCount)
results_df$featureContrast <- as.factor(results_df$featureContrast)
results_df


# aggregate data----------------------------------------------------------------
aggregated_results <- aggregate(x = filtered_zLogRT ~ subjectNumber + 
                                distractorCount + featureContrast + condition, 
                                data = results_df, mean, na.rm = TRUE)
aggregated_results


# summary statistics------------------------------------------------------------

# experimental conditions
summ_stats <- aggregated_results %>%
  group_by(distractorCount, featureContrast) %>% 
  summarize(n = n(), mean = mean(filtered_zLogRT), sd = sd(filtered_zLogRT), 
            se = sd / sqrt(n))
summ_stats

# distractors
summ_distract <- aggregated_results %>% 
  group_by(distractorCount) %>% 
  summarize(n = n(), mean = mean(filtered_zLogRT), sd = sd(filtered_zLogRT), 
            se = sd / sqrt(n))
summ_distract

# feature contrast
summ_fc <- aggregated_results %>% 
  group_by(featureContrast) %>%  
  summarize(n = n(), mean = mean(filtered_zLogRT), sd = sd(filtered_zLogRT), 
            se = sd / sqrt(n))
summ_fc


# assumption checks ------------------------------------------------------------

# assumption check 1 - outliers
aggregated_results %>% 
  group_by(distractorCount, featureContrast) %>% 
  identify_outliers(filtered_zLogRT)
# result: no extreme outliers

# assumption check 2 - normality assumption
aggregated_results %>% 
  group_by(distractorCount, featureContrast) %>% 
  shapiro_test(filtered_zLogRT)
# result:filtered_zLogRT is normally distributed for each experimental condition


# anova ------------------------------------------------------------------------

# two-way repeated measures anova
results_anova <- aggregated_results %>% 
  anova_test(dv = filtered_zLogRT, wid = subjectNumber,
             within = c(distractorCount, featureContrast), detailed = TRUE)
get_anova_table(results_anova)


# note: anovas only confirm that means are different between groups; do post-hoc
# tests to confirm the direction of the observed effects


# post-hoc tests for main effects----------------------------------------------- 

# pairwise comparisons - number of distractors
distract_pwc <- aggregated_results %>% 
  pairwise_t_test(filtered_zLogRT ~ distractorCount, paired = TRUE, 
                  p.adjust.method = "bonferroni", detailed = TRUE)
distract_pwc

# pairwise comparisons - feature contrast
fc_pwc <- aggregated_results %>% 
  pairwise_t_test(filtered_zLogRT ~ featureContrast, paired = TRUE,
                  p.adjust.method = "bonferroni", detailed = TRUE)
fc_pwc


# post-hoc tests for significant two-way interaction----------------------------

# since experiment is 2x2 within subjects, simple effect anova gives the same
# results as paired t-test; both are included

# simple effect anova - distractors
distract_se_tw <- aggregated_results %>% 
  group_by(featureContrast) %>% 
  anova_test(dv = filtered_zLogRT, wid = subjectNumber,
             within = distractorCount, detailed = TRUE) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
distract_se_tw

# pairwise comparisons between 5 and 10 distractors
distract_pwc_tw <- aggregated_results %>% 
  group_by(featureContrast) %>% 
  pairwise_t_test(filtered_zLogRT ~ distractorCount, paired = TRUE,
                  p.adjust.method = "bonferroni", detailed = TRUE)
distract_pwc_tw

# simple effect anova - feature contrast
fc_se_tw <- aggregated_results %>% 
  group_by(distractorCount) %>% 
  anova_test(dv = filtered_zLogRT, wid = subjectNumber,
             within = featureContrast, detailed = TRUE) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
fc_se_tw

# pairwise comparisons between contrast and no contrast
fc_pwc_tw <- aggregated_results %>% 
  group_by(distractorCount) %>% 
  pairwise_t_test(filtered_zLogRT ~ featureContrast, paired = TRUE,
                  p.adjust.method = "bonferroni", detailed = TRUE)
fc_pwc_tw

# visualization-----------------------------------------------------------------

# box plot - experimental conditions
ggplot(data = aggregated_results, aes(x = distractorCount, y = filtered_zLogRT,
                                      fill = featureContrast)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_discrete(name = "feature\ncontrast") +
  ylim(-.75,.75) +
  xlab("number of distractors") +
  ylab("mean normalized response times")

# line plot - experimental conditions
ggplot(data = summ_stats, aes(x = distractorCount, y = mean, 
                              group = featureContrast)) + 
  geom_line(aes(linetype = featureContrast)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = .1) +
  scale_linetype_discrete(name = "feature\ncontrast") +
  ylim(-.75,.75) +
  xlab("number of distractors") +
  ylab("mean normalized response times")


# box plot - distractors
ggplot(data = aggregated_results, aes(x = distractorCount, 
                                      y = filtered_zLogRT)) + 
  geom_boxplot(outlier.shape = NA) +
  ylim(-.75,.75) +
  xlab("number of distractors") +
  ylab("mean normalized response times")

# line plot - distractors
ggplot(data = summ_distract, aes(x = distractorCount, y = mean, group = 1)) + 
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = .1) +
  ylim(-.75,.75) +
  xlab("number of distractors") +
  ylab("mean normalized response times")
  
# box plot - feature contrast
ggplot(data = aggregated_results, aes(x = featureContrast, 
                                      y = filtered_zLogRT)) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(-.75,.75) +
  xlab("feature contrast") +
  ylab("mean normalized response times")

# line plot - feature contrast
ggplot(data = summ_fc, aes(x = featureContrast, y = mean, group = 1)) + 
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), width = .1) +
  ylim(-.75,.75) +
  xlab("feature contrast") +
  ylab("mean normalized response times")

# ------------------------------------------------------------------------------
# reference: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#two-way-repeated-measures-anova