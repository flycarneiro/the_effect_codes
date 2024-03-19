#### Coding exercises from Nick Huntington-Klein's "The Effect"
#### Flavio Lyrio Carneiro
#### mar-2024

### Chapter 4 - Describing Relationships


## Steps:
# 1. load in the data
# 2. draw a scatterplot between log women's earnings and log other earnings in the household, among women who work
# 3. Get the conditional mean of women's earnings by whether they attended college
# 4. Get the confitional mean of women's earnings by different bins of other household earnings
# 5. Draw the loess and linear regression curves of the meano fo log womens earnings conditional on log other earnings
# 6. run linear regression of log womens earnings on log other earnings, by itself and including controls for 
#    college attendance and n of children under five in household


## R code
install.packages(c("tidyverse", "modelsummary", "causaldata")) 
library(tidyverse); library(modelsummary); library(causaldata)

# 1: Load in the data
df <- causaldata::Mroz |> 
  # Keep just working women
  filter(lfp == TRUE) |> 
  # Get unlogged earnings
  mutate(earn = exp(lwg))

# 2. Draw a scatterplot
ggplot(df, aes(x = inc, y = earn)) + 
  geom_point() + 
  # Use a log scale for both axes;
  scale_x_log10() + scale_y_log10()

# 3. Get the conditional mean by college attendance
df |> 
  # wc is the college variable
  group_by(wc) |> 
  summarize(earn = mean(earn))

# 4. Get the conditional mean by bins
df |> 
  # use cut() to cut the variable into 10 bins
  mutate(inc_cut = cut(inc, 10)) |> 
  group_by(inc_cut) |> 
  summarize(earn = mean(earn))

# 5. Draw the loess and linear regression curves
ggplot(df , aes(x = inc, y = earn)) +
  geom_point() +
  # geom_smooth() by default draws a loess; we don't want standard errors
  geom_smooth(se = FALSE) +
  scale_x_log10() + scale_y_log10()
  # Linear regression needs a 'lm' method
  ggplot(df, aes(x = inc, y = earn)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_log10() + scale_y_log10()

# 6. Run a linear regression
# By itself:
model1 <- lm(lwg ~ log(inc), data = df)
# Controlling for wife attended college and number of kids under 5 in household
model2 <- lm(lwg ~ log(inc) + wc + k5, data = df)
# make a nice table
msummary(list(model1, model2))













