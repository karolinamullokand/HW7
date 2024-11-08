# HW7 Lab8

```
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)

setwd("/Users/karolinamullokand/Desktop/ECO_B2000")
load("ACS_2021_couples.RData")

acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")

acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0",
                                       "Mexican" = "1",
                                       "Puerto Rican" = "2",
                                       "Cuban" = "3",
                                       "Other" = "4")


names(acs2021_couples)
summary(acs2021_couples$REGION)

trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") & (REGION == "South Atlantic Division"))

trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)

table(trad_data$he_more_than_5yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))

ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)

names(trad_data)
summary(trad_data$RACE)


ols_out2 <- lm(he_more_than_5yrs_than_her ~ AGE + RACE, data = trad_data)
summary(ols_out2)

summary(trad_data$POVERTY)

ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + POVERTY, data = trad_data)
summary(ols_out3)
```


# 1
```
trad_data_summary <- trad_data %>%
  group_by(he_more_than_5yrs_than_her, FAMSIZE) %>%
  summarise(count = n(), .groups = "drop")

trad_data_summary$FAMSIZE <- as.factor(trad_data_summary$FAMSIZE)

nrow(trad_data_summary)
ncol(trad_data_summary)
summary(trad_data_summary)

a2 <- ggplot(trad_data_summary, aes(x = as.factor(he_more_than_5yrs_than_her), y = count, fill = as.factor(FAMSIZE))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "He More Than 5 Years Than Her", 
       y = "Count", 
       fill = "Family Size") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()

print(a2)
#WHAT DOES IT MEAN?

#Focusing on factors affecting whether a male partner is more than 5 years older than a female partner within couples from the __South Atlantic Division__
#The distribution of family sizes based on the age gap helps to see that if couple has more than 5 years age gap tend to have less children than those with less age gap.
```

# 2

```
load(tidyv)
trad_data2 <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") & !is.na(HHINCOME))

trad_data2$he_more_than_5yrs_than_her <- as.numeric(trad_data2$age_diff < -5)

table(trad_data2$he_more_than_5yrs_than_her,cut(trad_data2$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))
summary(trad_data2)
trad_data2$HHINCOME <- as.factor(trad_data2$HHINCOME)

ols_out4 <- lm(he_more_than_5yrs_than_her ~ HHINCOME, data = trad_data2) #Usig too much memore, narrowing down
trad_data2_small <- trad_data2 %>% sample_n(10000)
ols_out4 <- lm(he_more_than_5yrs_than_her ~ HHINCOME, data = trad_data2_small)
summary(ols_out4)
names(trad_data2_summary)
summary(trad_data2_summary)
levels(trad_data2_summary$HHINCOME)
trad_data2_summary$HHINCOME <- as.numeric(as.character(trad_data2_summary$HHINCOME))

trad_data2_summary$income_midpoint <- cut(trad_data2_summary$HHINCOME,
                                          breaks = c(0, 25000, 35000, 50000, 75000, 100000, 150000, 200000, Inf),
                                          labels = c("12500", "30000", "40000", "62500", "82500", "125000", "175000", "225000"),
                                          right = FALSE)
summary(trad_data2_summary$income_midpoint)

trad_data2_summary <- trad_data2_small %>%
  group_by(he_more_than_5yrs_than_her, HHINCOME) %>%
  summarise(count = n(), .groups = "drop")


a3 <- ggplot(trad_data2_summary, aes(x = as.factor(he_more_than_5yrs_than_her), y = count, fill = as.factor(income_midpoint))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(x = "He More Than 5 Years Than Her", 
       y = "Count", 
       fill = "Income") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()
print(a3)
#Result: the graph helped us see how is income distributed in families with age gap more
or less than 5 years. I can observe that the most income is concentrated in couples where head of the household(male) 
is not more than 5 years older and income is in range either 100k-150k or 200k-250k. Even though I can see also the same
income trend is common to a couples where head of the household is older than 5 years
```

# Hypothesis testing:

H0:Household income has no effect on the likelihood of the male partner being older by more than 5 years.

HA:Household income does have an effect on the age gap.

```
ols_out <- lm(he_more_than_5yrs_than_her ~ income_midpoint, data = trad_data2_summary)
summary(ols_out)
```

#Results:

Significance (p = 0.0115): This result is statistically significant, suggesting that households in the highest income 
group are indeed less likely to have a male partner significantly older than the female partner.
