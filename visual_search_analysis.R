# load packages-----------------------------------------------------------------
library("tidyverse")
library("rstatix")
library("ggpubr")


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

# assumption check 1 - identify extreme outliers

aggregated_results %>% 
  group_by(distractorCount, featureContrast) %>% 
  identify_outliers(filtered_zLogRT)
# interpretation: no extreme outliers

# assumption check 2 - normality assumption

# shapiro-wilk test
aggregated_results %>% 
  group_by(distractorCount, featureContrast) %>% 
  shapiro_test(filtered_zLogRT)
# interpretation: filtered_zLogRT is normally distributed for 
# each experimental condition

# qqplot inspection
ggqqplot(data = aggregated_results, x = "filtered_zLogRT", 
         ggtheme = theme_bw()) + 
  facet_grid(distractorCount ~ featureContrast, labeller = "label_both")
# interpretation: filtered_zLogRT appears to be normally distributed for
# each experimental condition

# assumption check 3 - homogeneity of variances

# levene's test
aggregated_results %>% 
  levene_test(filtered_zLogRT ~ distractorCount*featureContrast)
# interpretation: no evidence to suggest that variances of RTs
# are significantly different

# bartlett's test
bartlett.test(filtered_zLogRT ~ interaction(distractorCount, featureContrast), 
              data = aggregated_results)
# interpretation: no evidence to suggest that variances of RTs
# are significantly different  


# anova ------------------------------------------------------------------------

# two-way repeated measures anova
results_anova <- aggregated_results %>% 
  anova_test(dv = filtered_zLogRT, wid = subjectNumber,
             within = c(distractorCount, featureContrast), detailed = TRUE)
results_anova
get_anova_table(results_anova)
# result: effect of number of distractors, feature contrast, and interaction
# are all statistically significant 

# note: anovas only confirm that means are different between groups; do post-hoc
# tests to confirm the direction of the observed effects


# post-hoc tests for main effects----------------------------------------------- 

# pairwise comparisons - number of distractors
distract_pwc <- aggregated_results %>% 
  pairwise_t_test(filtered_zLogRT ~ distractorCount, paired = TRUE, 
                  p.adjust.method = "bonferroni", detailed = TRUE)
distract_pwc
# result: participants had statistically significant faster RTs when there were 
# 5 distractors compared to when there were 10 distractors

# pairwise comparisons - feature contrast
fc_pwc <- aggregated_results %>% 
  pairwise_t_test(filtered_zLogRT ~ featureContrast, paired = TRUE,
                  p.adjust.method = "bonferroni", detailed = TRUE)
fc_pwc
# result: participants had statistically significant faster RTs when there was 
# contrast compared to when there was no contrast


# post-hoc tests for significant two-way interaction----------------------------

# pairwise comparisons - experimental conditions
ec_pwc_tw <- aggregated_results %>% 
  pairwise_t_test(filtered_zLogRT ~ condition, paired = TRUE,
                  p.adjust.method = "bonferroni", detailed = TRUE)
ec_pwc_tw
# result: 
# participants had statistically significant faster RTs for: 
#   5 distractors + contrast trials than 10 distractors + contrast trials
#   5 distractors + no contrast trials than 10 distractors + no contrast trials
#   5 distractors + contrast trials than 10 distractors + no contrast trials
#   5 distractors + no contrast trials than 10 distractors + contrast trials
#   10 distractors + contrast trials than 10 distractors + no contrast trials
#   5 distractors + contrast trials than 5 distractors + no contrast trials *
# * -> p-value for this comparison is just about equal to 0.05. Therefore, 
# while it can be considered statistically significant, this should be noted
# when interpreting the magnitude of the results. 


# visualization-----------------------------------------------------------------

# plots saved as 600x380

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
  ylab("mean normalized response times") +
  ggtitle("a") +
  theme(plot.title = element_text(hjust = -0.075, face = "bold", size = 30))
  
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
  ylab("mean normalized response times") +
  ggtitle("b") +
  theme(plot.title = element_text(hjust = -0.075, face = "bold", size = 30))

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
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  scale_linetype_discrete(name = "feature\ncontrast") +
  ylim(-.75,.75) +
  xlab("number of distractors") +
  ylab("mean normalized response times") +
  ggtitle("c") +
  theme(plot.title = element_text(hjust = -0.075, face = "bold", size = 30))


# ------------------------------------------------------------------------------
# References:
# https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#two-way-repeated-measures-anova
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/#:~:text=Bartlett's%20test%3A%20Compare%20the%20variances,robust%20against%20departures%20from%20normality.