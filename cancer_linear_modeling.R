library(tidyverse)
library(tidyselect)
library(ggplot2)
library(GGally)
library(janitor)
library(kableExtra)
library(stringr)
library(olsrr)
library(lmtest)


# Breaking Geography Variable into Region
cncr_2[c("County", "State")] <- str_split_fixed(cncr_2$Geography, ", ", 2)

cncr_2 <- cncr_2 %>% 
  left_join(state_region, by = "State")

cncr_2 <- cncr_2 %>% 
  select(-c(Geography, County, State, "State Code", Division))


# Creating variable for race majority counties
cncr_2 <- cncr_2 %>% 
  mutate(countyMaj = case_when(
    (PctBlack > PctWhite & PctBlack > PctAsian & PctBlack > PctOtherRace) ~ "blackPlurality",
    (PctWhite > PctBlack & PctWhite > PctAsian & PctWhite > PctOtherRace) ~ "whitePlurality",
    (PctAsian > PctBlack & PctAsian > PctWhite & PctAsian > PctOtherRace) ~ "asianPlurality",
    (PctOtherRace > PctBlack & PctOtherRace > PctAsian & PctOtherRace > PctWhite) ~ "Other"
  )
    )
  
# Looking to see if there is significant linear relationship between variables in simple regression
region.lm <- lm(deathRate ~ Region, data = cncr_2)
summary(region.lm)

Inc.lm <- lm(deathRate ~ medIncome, data = cncr_2)
summary(Inc.lm)

Black.lm <- lm(deathRate ~ PctBlack, data = cncr_2)
summary(Black.lm)

Bach.lm <- lm(deathRate ~ PctBachDeg25_Over, data = cncr_2)
summary(Bach.lm)

HS.lm <- lm(deathRate ~ PctHS25_Over, data = cncr_2)
summary(HS.lm)

Public.lm <- lm(deathRate ~ PctPublicCoverageAlone, data = cncr_2)
summary(Public.lm)

# Multiple Regression Model 
lm.1 <- lm(deathRate ~ Region + medIncome + PctBlack + PctBachDeg25_Over + PctHS25_Over + PctPublicCoverageAlone, cncr_2)

summary(lm.1)

# Checking for Multicollinearity
mr.num <- cncr_2 %>% 
  select(medIncome, PctBlack, PctBachDeg25_Over, PctHS25_Over, PctPublicCoverageAlone)
ggpairs(mr.num)

ols_vif_tol(lm.1)

mr.num2 <- cncr_2 %>% 
  select(medIncome, PctBlack, PctHS25_Over, PctPublicCoverageAlone)

ggpairs(mr.num2)

lm.2 <- lm(deathRate ~ Region + medIncome + PctBlack + PctHS25_Over + PctPublicCoverageAlone, cncr_2)

ols_vif_tol(lm.2)

summary(lm.2)

summary(lm.1)

# Checking Residual Assumptions - Normality
ggplot(data = cncr_2, aes(x = lm.2$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  geom_density(col = "red") +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

ols_plot_resid_qq(lm.2)

ols_test_normality(lm.2)

ols_plot_resid_hist(lm.2)


# Checking Residual Assumptions - Homoscedascity 
gqtest(lm.2, order.by = ~Region + medIncome + PctBlack + PctHS25_Over + PctPublicCoverageAlone, data = cncr_2, fraction = 579)

plot(lm.2, las = 1)

ggplot(data = cncr_2, aes(x = log(deathRate))) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  geom_density(col = "red")
  labs(title = 'Histogram of deathRate')

shapiro.test(log(cncr_2$deathRate))
