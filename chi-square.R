### EDA ###

library(dplyr)
library(gmodels)
library(corrgram)
library(corrplot)
library(ggplot2)
library(PerformanceAnalytics)
library(readr)

df <- read_csv("C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 1 - Payment Prediction/df_train_r.csv")
df <- subset(df, select = -...1)

attach(df)
dim(df)
str(df)
summary(df)


############ Target Variable - target ############

table(target)
prop.table(table(target))


###Features



##########################################################################################


##payment_method_new [categorical]

df_payment_method_new <- df[, c('payment_method_new', 'target')]

CrossTable(df_payment_method_new$payment_method_new, df_payment_method_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_payment_method_new$payment_method_new, df_payment_method_new$target, chisq = T)$chisq[3]$p.value
df_stats <- data.frame('payment_method_new', p_chisq_test)

colnames(df_stats) <- c('feature', 'p-value')

df_stats


##########################################################################################


##custom_meal_selection_new [categorical]

df_custom_meal_selection_new <- df[, c('custom_meal_selection_new', 'target')]

CrossTable(df_custom_meal_selection_new$custom_meal_selection_new, df_custom_meal_selection_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_custom_meal_selection_new$custom_meal_selection_new, df_custom_meal_selection_new$target, chisq = T)$chisq[3]$p.value
df_stats3 <- data.frame('custom_meal_selection_new', p_chisq_test)

df_stats3

colnames(df_stats3) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats3)

df_stats


##########################################################################################


##state_new [categorical]

df_state_new <- df[, c('state_new', 'target')]

CrossTable(df_state_new$state_new, df_state_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_state_new$state_new, df_state_new$target, chisq = T)$chisq[3]$p.value
df_stats4 <- data.frame('state_new', p_chisq_test)

df_stats4

colnames(df_stats4) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats4)

df_stats


##########################################################################################


##channel_new [categorical]

df_channel_new <- df[, c('channel_new', 'target')]

CrossTable(df_channel_new$channel_new, df_channel_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_channel_new$channel_new, df_channel_new$target, chisq = T)$chisq[3]$p.value
df_stats5 <- data.frame('channel_new', p_chisq_test)

df_stats5

colnames(df_stats5) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats5)

df_stats


##########################################################################################


##engagement_score [numeric]

##Transforming into bins to run chi-square test

q_engagement_score <- quantile(engagement_score, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                 0.6, 0.7, 0.8, 0.9, 1))
q_engagement_score

df_engagement_score <- df[, c('engagement_score', 'target')]

df_engagement_score$fx_engagement_score <- cut(df_engagement_score$engagement_score,
                                     breaks= c(-10000000,
                                               q_engagement_score[1],
                                               q_engagement_score[2],
                                               q_engagement_score[3],
                                               q_engagement_score[4],
                                               q_engagement_score[6],
                                               q_engagement_score[7],
                                               q_engagement_score[8],
                                               q_engagement_score[9],
                                               q_engagement_score[10]),
                                     labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%',
                                              '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                     right=T)

CrossTable(df_engagement_score$fx_engagement_score, df_engagement_score$target, chisq = T)


feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_engagement_score$fx_engagement_score, df_engagement_score$target, chisq = T)$chisq[3]$p.value

df_stats6 <- data.frame('engagement_score', p_chisq_test)

df_stats6

colnames(df_stats6) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats6)

df_stats


##########################################################################################


##num_prior_orders_failed [numeric]

##Transforming into bins to run chi-square test

q_num_prior_orders_failed <- quantile(num_prior_orders_failed, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                           0.6, 0.7, 0.8, 0.9, 1))
q_num_prior_orders_failed

df_num_prior_orders_failed <- df[, c('num_prior_orders_failed', 'target')]

df_num_prior_orders_failed$fx_num_prior_orders_failed <- cut(df_num_prior_orders_failed$num_prior_orders_failed,
                                               breaks= c(-10000000,
                                                         q_num_prior_orders_failed[1],
                                                         q_num_prior_orders_failed[3],
                                                         q_num_prior_orders_failed[6],
                                                         q_num_prior_orders_failed[7],
                                                         q_num_prior_orders_failed[8],
                                                         q_num_prior_orders_failed[9],
                                                         q_num_prior_orders_failed[10]),
                                               labels=c('<= 10%', '<= 30%',
                                                        '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                               right=T)

CrossTable(df_num_prior_orders_failed$fx_num_prior_orders_failed, df_num_prior_orders_failed$target, chisq = T)


feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_num_prior_orders_failed$fx_num_prior_orders_failed, df_num_prior_orders_failed$target, chisq = T)$chisq[3]$p.value

df_stats7 <- data.frame('num_prior_orders_failed', p_chisq_test)

df_stats7

colnames(df_stats7) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats7)

df_stats


##########################################################################################


##num_prior_orders_unpaid [numeric]

##Transforming into bins to run chi-square test

q_num_prior_orders_unpaid <- quantile(num_prior_orders_unpaid, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                         0.6, 0.7, 0.8, 0.9, 1))
q_num_prior_orders_unpaid

df_num_prior_orders_unpaid <- df[, c('num_prior_orders_unpaid', 'target')]

df_num_prior_orders_unpaid$fx_num_prior_orders_unpaid <- cut(df_num_prior_orders_unpaid$num_prior_orders_unpaid,
                                                             breaks= c(-10000000,
                                                                       q_num_prior_orders_unpaid[9],
                                                                       q_num_prior_orders_unpaid[10]),
                                                             labels=c('<= 90%', '<= 100%'),
                                                             right=T)

CrossTable(df_num_prior_orders_unpaid$fx_num_prior_orders_unpaid, df_num_prior_orders_unpaid$target, chisq = T)


feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_num_prior_orders_unpaid$fx_num_prior_orders_unpaid, df_num_prior_orders_unpaid$target, chisq = T)$chisq[3]$p.value

df_stats8 <- data.frame('num_prior_orders_unpaid', p_chisq_test)

df_stats8

colnames(df_stats8) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats8)

df_stats


##########################################################################################


##engagement_cat_new [categorical]

df_engagement_cat_new <- df[, c('engagement_cat_new', 'target')]

CrossTable(df_engagement_cat_new$engagement_cat_new, df_engagement_cat_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_engagement_cat_new$engagement_cat_new, df_engagement_cat_new$target, chisq = T)$chisq[3]$p.value
df_stats10 <- data.frame('engagement_cat_new', p_chisq_test)

df_stats10

colnames(df_stats10) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats10)

df_stats


##########################################################################################


##ever_rated_recipe_new [categorical]

df_ever_rated_recipe_new <- df[, c('ever_rated_recipe_new', 'target')]

CrossTable(df_ever_rated_recipe_new$ever_rated_recipe_new, df_ever_rated_recipe_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_ever_rated_recipe_new$ever_rated_recipe_new, df_ever_rated_recipe_new$target, chisq = T)$chisq[3]$p.value
df_stats11 <- data.frame('ever_rated_recipe_new', p_chisq_test)

df_stats11

colnames(df_stats11) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats11)

df_stats


##########################################################################################


##is_f_new [categorical]

df_is_f_new <- df[, c('is_f_new', 'target')]

CrossTable(df_is_f_new$is_f_new, df_is_f_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_is_f_new$is_f_new, df_is_f_new$target, chisq = T)$chisq[3]$p.value
df_stats12 <- data.frame('is_f_new', p_chisq_test)

df_stats12

colnames(df_stats12) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats12)

df_stats


##########################################################################################


##########################################################################################


##delivery_week_new [categorical]

df_delivery_week_new <- df[, c('delivery_week_new', 'target')]

CrossTable(df_delivery_week_new$delivery_week_new, df_delivery_week_new$target, chisq = T)

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_delivery_week_new$delivery_week_new, df_delivery_week_new$target, chisq = T)$chisq[3]$p.value
df_stats13 <- data.frame('delivery_week_new', p_chisq_test)

df_stats13

colnames(df_stats13) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats13)

df_stats


##########################################################################################

test <- df_stats[-c(14), ]

write.csv(test, 'C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 1 - Payment Prediction/df_train_chisquare.csv')


##########################################################################################

