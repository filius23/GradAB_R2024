# GradAB R 2024 
# Chapter 4: data wrangling - creating variables
# solutions
library(tidyverse)


# Exercise 1 -------------------------------------------------------------------

dat3 <- data.frame(
  studs = c(14954, 47269, 23659, 9415, 38079), 
  profs = c(250, 553, 438, 150, 636),
  prom_recht = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  gegr  = c(1971, 1870, 1457, 1818, 1995),
  uni = c("FH Aachen", "RWTH Aachen", "Uni Freiburg", "Uni Bonn", "FH Bonn-Rhein-Sieg")
)


dat3 %>% 
  mutate(studprof = studs/profs,
         rel_studprofs = studprof - mean(studprof)
         ) %>% 
  mutate(rel_studprofs_promrecht = studprof - mean(studprof), .by = prom_recht)


# Exercise 2 -------------------------------------------------------------------
pend_small <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                              col_select = c("welle","zpsex","PEO0400a","PEO0400b","PEO0400c","PEO0400d")
) %>% 
  filter(welle == 2) %>% 
  slice(1:15)

# Use across() to calculate the means for all four variables.
pend_small %>% 
  mutate(across(matches("PE"),~mean(.x)))
# Calculate the means separately by gender (zpsex) using .by =.
pend_small %>% 
  mutate(across(matches("PE"),~mean(.x)), .by = zpsex)
# Also add the variance (var()), and use .names= to name the columns following the pattern metric.variable.
pend_small %>% 
  mutate(across(matches("PE"),list(mean=~mean(.x),var=~var(.x)),.names = "{.fn}.{.col}"), .by = zpsex)

# Exercise 3 -------------------------------------------------------------------
pend_small %>% 
  mutate(std_PEO0400b = (PEO0400b - mean(PEO0400b,na.rm = T))/sd(PEO0400b,na.rm = T))

# Use a function so that you don’t have to repeatedly enter the same steps.
my_function1 <- function(x) {
  x2 = (x - mean(x,na.rm = T))/sd(x,na.rm = T)
  return(x2)
}
# Additionally, use across() to apply the function to the desired variables.
pend_small %>% 
  mutate(across(matches("PE"), ~my_function1(.x)))

