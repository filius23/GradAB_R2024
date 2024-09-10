# GradAB R 2024 
# Chapter 3: descriptives 

# load PASS CF data
library(haven) # data import for Stata datasets
library(tidyverse) # tidyverse
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")


# frequency tables -------------------------------------------------------------
table(pend$statakt)

# access labels:
attributes(pend$statakt)$labels

# count() displays the labels/attributes:
pend %>% count(statakt)

t1 <- table(pend$statakt)
class(t1) 
t2 <- pend %>% count(statakt)
class(t2)

# NAs --------------------------------------------------------------------------
pend$statakt[pend$statakt == -5] # only call statakt = -5

pend$statakt[pend$statakt == -5]  <- NA # replace all -5 with NA in statakt

pend$statakt[pend$statakt == -9 ]  <- NA
pend$statakt[pend$statakt == -10]  <- NA

# replace all negative values with NA
pend$statakt[pend$statakt < 0 ]  <- NA

# count still displays the NA:
pend %>% count(statakt)
# filter() to avoid them:
pend %>% filter(!is.na(statakt)) %>% count(statakt)

# recreating Stata's tab  ------------------------------------------------------
tab1 <- pend %>% filter(!is.na(statakt)) %>% count(statakt)
tab1

tab1$pct <- prop.table(tab1$n)  # add new column using prop.table() command
tab1

# cumulative values --> cumsum()
tab1$cum <- prop.table(tab1$n) %>% cumsum()
tab1

# contingency table ------------------------------------------------------------
pend %>% count(zpsex, statakt)
tab2 <- pend %>% count(zpsex, statakt)
tab2$pct <- prop.table(tab2$n)
tab2

## conditional pcts ------------------------------------------------------------
tab2 %>% mutate(pct_zpsex= prop.table(n), .by = zpsex)


# summarizing distributions ----------------------------------------------------
summary(pend$netges)
pend$netges[pend$netges < 0] <- NA # Handling Missing Data
summary(pend$netges)

## specific statistic values ----
mean(pend$netges)
mean(pend$netges, na.rm = TRUE)


## custom summaries ------------------------------------------------------------
pend %>%
  summarise(
    Minimum = min(netges, na.rm = TRUE),
    Median = median(netges, na.rm = TRUE),
    Mean = mean(netges, na.rm = TRUE),
    Maximum = max(netges, na.rm = TRUE)
  )


# using the .by =  argument ----------------------------------------------------
pend %>%
  summarise(
    Minimum = min(netges, na.rm = TRUE),
    Median = median(netges, na.rm = TRUE),
    Mean = mean(netges, na.rm = TRUE),
    Maximum = max(netges, na.rm = TRUE),
    .by = welle
  ) %>% arrange(welle)

# if we want only wave 1 and 10:
pend %>% 
  filter(welle %in% c(1, 10)) %>% 
  summarise(
    Minimum = min(netges, na.rm = TRUE),
    Median = median(netges, na.rm = TRUE),
    Mean = mean(netges, na.rm = TRUE),
    Maximum = max(netges, na.rm = TRUE),
    .by = welle
  )


