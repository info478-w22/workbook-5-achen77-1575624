# Workbook 6: analyze NHANES data

## Set up
library(survey)
library(Hmisc)

demographic <- sasxport.get("./DEMO_I.XPT")
alcohol <- sasxport.get("./ALQ_I.XPT")
nhanes <- merge(x = demographic, y = alcohol, by = "seqn", all = TRUE)


wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)
## wt_sum combines all of the survey weights together. This should be the total number of the population,
## or the total number of people in the United States in this case.


# Analysis

## In ALQ151, we want 2 to be 0 and want to ignore 7 and 9
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA


## Create analysis, survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

## Calculates means of survey weights and means by gender.
nhanes_mean <- svymean(~alq151, nhanes_survey, na.rm = TRUE)
mean_by_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean, na.rm = TRUE)
