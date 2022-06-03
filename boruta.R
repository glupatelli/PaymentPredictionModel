### Boruta ###


library(dplyr)
library(Boruta)
library(readr)

## Dataframe ##

df <- read_csv("C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 1 - Payment Prediction/df_train_r.csv")
df <- subset(df, select = -...1)


### Database summary ###

attach(df)
dim(df)
str(df)
summary(df)


##################### BORUTA #####################


par(mfrow = c(1,1))
set.seed(222)
df_boruta_2 <- df[, c('delivery_week_new', 'payment_method_new',
                      'custom_meal_selection_new', 'state_new', 'channel_new', 'engagement_score',
                      'num_prior_orders_failed', 'num_prior_orders_unpaid',
                      'engagement_cat_new', 'ever_rated_recipe_new', 'is_f_new',
                      'target')]
Boruta(target ~ ., data = df_boruta_2, doTrace=2, maxRuns = 20, pValue = 0.01) -> Boruta_df
#Shadows attributes should be rejected
print(Boruta_df)
plot(Boruta_df)
stats <- attStats(Boruta_df)
stats$feature <- row.names(stats)
stats <- stats %>% arrange(desc(meanImp))
stats
plotImpHistory(Boruta_df)

write.csv(stats, 'C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 1 - Payment Prediction/df_train_boruta.csv')

