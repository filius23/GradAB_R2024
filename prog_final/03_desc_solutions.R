# R Intro GradAB Sept 2024
# Chapter 3 - Solutions
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")

# Exercise 1 -------------------------------------------------------------------

# We are interested in the variable `famstand`, which contains the marital status of the respondents:
# + Display a table with absolute frequencies of `famstand` using both `table()` and `count()` (Remember to load `{tidyverse}` for `count()`). 
library(tidyverse)

pend %>% count(famstand)
# + Overwrite missing codes with `NA`.
pend$famstand[pend$famstand<0] <- NA
# + Did the replacement of missing values with NA work? Create the table again.
pend %>% count(famstand)

# + Display the relative frequencies (proportions). Use `prop.table()` 
tab_ue1 <- pend %>% count(famstand)
tab_ue1
tab_ue1$pct <- prop.table(tab_ue1$n)
tab_ue1
  
  # excluding NA:
tab_ue1_noNA <- pend %>% count(famstand) %>% filter(!is.na(famstand))
tab_ue1_noNA
tab_ue1_noNA$pct <- prop.table(tab_ue1_noNA$n)
tab_ue1_noNA


# Exercise 2 -------------------------------------------------------------------
  
# + Create a contingency table for `famstand` and `zpsex` using `count()`.
# + What percentage of the respondents are divorced women? Use `prop.table()` 
  
tab_ue2 <- pend %>% count(famstand,zpsex) %>% filter(!is.na(famstand))
tab_ue2
tab_ue2$pct <- prop.table(tab_ue2$n)
tab_ue2 # 10.1% of all respondents are women who are divorced

# Exercise 3 -------------------------------------------------------------------
    
#Describe the age of respondents (`palter`) using `summary` and create your own overview using `summarise()` to compare respondent age by marital status.
#+ First, overwrite missing values with `NA`: 
pend$palter[pend$palter<0] <- NA
pend$famstand[pend$famstand<0] <- NA

    
# + Create an overview using `summary()`.
summary(pend$palter)
# + Create an overview with the minimum, median, mean, variance, and maximum age values using `summarise()`.
pend %>%
  summarise(
    Minimum =  min(palter, na.rm = TRUE),
    Median =   median(palter, na.rm = TRUE),
    Mean =     mean(palter, na.rm = TRUE),
    Variance = var(palter, na.rm = TRUE),
    Maximum =  max(palter, na.rm = TRUE)
  )

# + Extend this overview to display the summary statistics for the different `famstand` categories.
pend %>%
  summarise(
    Minimum =  min(palter, na.rm = TRUE),
    Median =   median(palter, na.rm = TRUE),
    Mean =     mean(palter, na.rm = TRUE),
    Variance = var(palter, na.rm = TRUE),
    Maximum =  max(palter, na.rm = TRUE),
    .by = famstand
  )

pend %>%
  summarise(
    Minimum =  min(palter, na.rm = TRUE),
    Median =   median(palter, na.rm = TRUE),
    Mean =     mean(palter, na.rm = TRUE),
    Variance = var(palter, na.rm = TRUE),
    Maximum =  max(palter, na.rm = TRUE),
    .by = famstand
  ) %>%  filter(!is.na(famstand))