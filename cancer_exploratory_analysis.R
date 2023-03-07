library(tidyverse)
library(tidyselect)
library(ggplot2)
library(GGally)
library(janitor)
library(kableExtra)
library(stringr)

# Reading in Data 
cncr <- read_csv("cancer_reg.csv")

cncr_2 <- cncr %>% 
  select(-c(avgAnnCount, avgDeathsPerYear, incidenceRate, binnedInc, studyPerCap))


# Exploratory Data Analysis - Race/Ethnicity 

race <- cncr_2 %>% 
  select(deathRate, PctWhite, PctBlack, PctAsian, PctOtherRace)

cor(race$deathRate, race$PctBlack)

ggpairs(race)

ggplot(race, aes(x=PctWhite, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)


ggplot(race, aes(x=PctBlack, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

ggplot(race, aes(x=PctAsian, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

ggplot(race, aes(x=PctOtherRace, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

race <- race %>% 
  mutate(raceMaj = case_when(
    PctWhite >= 50 ~ "White",
    PctBlack >= 50 ~ "Black",
    PctAsian >= 50 ~ "Asian",
    PctOtherRace >= 50 ~ "OtherRace"
  ))

black_white <- race %>% 
  filter(raceMaj %in% c("White", "Black"))

whiteDR <- race %>% 
  filter(raceMaj == "White") %>% 
  select(deathRate)

blackDR <- race %>% 
  filter(raceMaj == "Black") %>% 
  select(deathRate)


ggplot(whiteDR, aes(x=deathRate)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

t.test(x= blackDR$deathRate, y = whiteDR$deathRate, u = 0, alternative = "two.sided")

# Exploratory Data Analysis - Geography 
state_region <- read_csv("state_region.csv")

geo <- cncr_2 %>% 
  select(Geography, deathRate)

geo[c("County", "State")] <- str_split_fixed(geo$Geography, ", ", 2)

geo <- geo %>% 
  left_join(state_region, by = "State")

unique(geo$Region)

ggplot(geo, aes(x=Region, y=deathRate)) + 
  geom_boxplot()

summary(aov(deathRate ~ Region, data = geo))

# Exploratory Data Analysis - Income/Poverty Levels 

ggplot(cncr_2, aes(x=PctWhite, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)









