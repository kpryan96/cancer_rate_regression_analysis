library(tidyverse)
library(tidyselect)
library(ggplot2)
library(GGally)
library(janitor)
library(kableExtra)
library(stringr)


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
  

# Model V1 
lm.1 <- lm(deathRate ~ ., cncr_2)

p.fvr <- ggplot(lm.1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted Plot Created using ggplot2") +
  theme_bw()

p.fvr + geom_smooth(se = FALSE)

summary(lm.1)


lm.1 <- lm(deathRate ~ Region + medIncome + PctBlack + PctBachDeg25_Over + PctHS25_Over + PctPublicCoverageAlone, cncr_2)

p.fvr <- ggplot(lm.1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted Plot Created using ggplot2") +
  theme_bw()

p.fvr + geom_smooth(se = FALSE)


summary(lm.1)

