# GradAB R 2024 
# Chapter 4: data wrangling - creating variables
# solutions
library(tidyverse)


# Exercise 1 -------------------------------------------------------------------

# + Use the `pend_small` dataset:
pend_small <- 
  haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("welle","zpsex","PEO0400a","PEO0400b","PEO0400c","PEO0400d")
                               ) %>% 
  haven::zap_labels() %>% 
  filter(welle == 2) %>% 
  slice(1:10)

# Calculate the mean for the variables PEO0400a by gender (`zpsex`):
pend_small

pend_small %>% 
  mutate(mean_PEO0400a = mean(PEO0400a),.by = zpsex)

# Use `across()` to calculate the means for all four variables `PEO0400a`, `PEO0400b`, `PEO0400c`, and `PEO0400d`
pend_small %>% 
  mutate(across(matches("PE"),~mean(.x)),.by = zpsex)

# Standardize the variables `PEO0400a` - `PEO0400d` from `pend_small` using the following pattern: ---------------
pend_small %>% 
  mutate(std_PEO0400b = (PEO0400b - mean(PEO0400b,na.rm = T))/sd(PEO0400b,na.rm = T))
# + Use a function so that you don't have to repeatedly enter the same steps.
stdize <- function(var) {
  std = (var - mean(var,na.rm = T))/sd(var,na.rm = T)
}

# + Additionally, use `across()` to apply the function to the desired variables.
# + Calculate the standardization separately by gender (`zpsex`) using `.by =`.
pend_small %>% 
  mutate(across(matches("PE"), ~stdize(.x)))
pend_small %>% 
  mutate(across(matches("PE"), ~stdize(.x)),.by=zpsex)

