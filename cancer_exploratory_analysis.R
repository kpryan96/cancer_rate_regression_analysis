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

ggplot(cncr_2, aes(x=medIncome, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

cor(cncr_2$medIncome, cncr_2$deathRate)


ggplot(cncr_2, aes(x=povertyPercent, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

cor(cncr_2$povertyPercent, cncr_2$deathRate)

# Exploratory Data Analysis - Education Levels
Edu <- cncr_2 %>% 
  select(PctNoHS18_24, PctHS18_24, PctSomeCol18_24, PctBachDeg18_24, PctHS25_Over, PctBachDeg25_Over, deathRate)

summary(Edu)

Edu$PctSomeCol18_24 <- NULL

Edu_hs <- Edu %>% 
  select(deathRate, PctNoHS18_24, PctHS18_24, PctHS25_Over)

ggpairs(Edu_hs)

ggplot(Edu_hs, aes(x=PctHS25_Over, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

cor(Edu_hs$deathRate, Edu_hs$PctHS25_Over)

Edu_Cg <- Edu %>% 
  select(deathRate, PctBachDeg18_24, PctBachDeg25_Over)

ggpairs(Edu_Cg)

cor(Edu_Cg$deathRate, Edu_Cg$PctBachDeg18_24)


# Exploratory Data Analysis - #Marriage/Family 
mar <- cncr_2 %>% 
  select(AvgHouseholdSize, PercentMarried, PctMarriedHouseholds, deathRate)


ggpairs(mar)

# Exploratory Data Analysis - #Health Insurance Coverage 
ins <- cncr_2 %>% 
  select(PctPrivateCoverage, PctPrivateCoverageAlone, PctEmpPrivCoverage, PctPublicCoverage, PctPublicCoverageAlone, deathRate)

ggpairs(ins)

ggplot(ins, aes(x=PctPublicCoverage, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

ggplot(ins, aes(x=PctPublicCoverage, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

ggplot(ins, aes(x=PctPublicCoverageAlone, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

# Exploratory Data Analysis - #Birth Rate

ggplot(cncr_2, aes(x=BirthRate, y=deathRate)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm)

cor(cncr_2$BirthRate, cncr_2$deathRate)

# Features with Strongest Correlation to Death Rate

cncr_2$PctSomeCol18_24 <- NULL

cncr_nums <- cncr_2 %>% 
  select_if(is.numeric)

feat_corr <- data.frame(matrix(ncol = 2, nrow = 27))
colnames(feat_corr) <- c("Feature", "corrDeathRate")

feat_corr$Feature <- colnames(cncr_nums)

col_names <- colnames(cncr_nums)
for(i in 1:27) {       
  if(col_names[[i]] == feat_corr$Feature[i]){
    feat_corr$corrDeathRate[[i]] <- cor(cncr_nums[,i], cncr_nums$deathRate)
  }
}

feat_corr$absCorrDeathRate <- abs(feat_corr$corrDeathRate)

