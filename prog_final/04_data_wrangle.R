# GradAB R 2024 
# Chapter 4: data wrangling - creating variables
library(tidyverse)


dat3 <- data.frame(
  studs = c(14954, 47269, 23659, 9415, 38079), 
  profs = c(250, 553, 438, 150, 636),
  prom_recht = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  gegr  = c(1971, 1870, 1457, 1818, 1995),
  uni = c("FH Aachen", "RWTH Aachen", "Uni Freiburg", "Uni Bonn", "FH Bonn-Rhein-Sieg")
)


# base R: creating variables ---------------------------------------------------
dat3$studs_to_mean <- dat3$studs - mean(dat3$studs)
dat3
dat3$studs_to_mean <- NULL
dat3


# dplyr/tidyverse: mutate() ----------------------------------------------------
dat3 %>% mutate(studs_to_mean = studs - mean(studs))


# also possible: multiple variables 
dat3 %>% mutate(
  studs_to_mean = studs - mean(studs),
  profs_to_mean = profs - mean(profs)
)


# ...and directly resuse them 
dat3 %>% mutate(
  rel_to_mean = studs - mean(studs),
  above_mean = rel_to_mean > 0
)

# dat3 remains unchanged!
dat3

# to store it we need to assign it
dat4 <- dat3 %>% mutate(
  rel_to_mean = studs - mean(studs),
  above_mean = rel_to_mean > 0
)

dat4


# by the way: creating numeric dummies
dat3 %>% mutate(
  prom_dummy = as.numeric(prom_recht),
  over10k = as.numeric(studs > 10000)
)


# grouped calculations using .by= ---------------------------------------------- 
dat5 <- dat3 %>% 
  select(-uni,-gegr) # to ensure everything is visible

dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  mutate(m_studs2 = mean(studs),
         .by = prom_recht) %>% 
  mutate(m_profs2 = mean(profs)) # not grouped


# same with summarise() as we've already seen: 
dat5 %>%
  summarise(m_studs = mean(studs),.by = prom_recht)


## group_by -----
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>%
  mutate(m_studs2 = mean(studs),
         m_profs2 = mean(profs))


# needs ungrouping!
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>%
  mutate(m_studs2 = mean(studs)) %>% 
  ungroup() %>% 
  mutate(m_profs2 = mean(profs))

dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>%
  mutate(m_studs2 = mean(studs)) %>% 
  mutate(m_profs2 = mean(profs))  ### still grouped!

# Exercise ------

# across -----------------------------------------------------------------------
dat3 %>%
  summarise(studs = mean(studs),
            profs = mean(profs))

# select cols
dat3 %>%
  summarise(across(.cols = c("studs","profs"),.fns = ~mean(.x)))
# matches()
dat3 %>%
  summarise(across(.cols = matches("studs|profs"),.fns = ~mean(.x)))


# also in combination with .by =
dat3 %>%
  summarise(across(matches("studs|profs"), ~mean(.x)), .by= prom_recht)

# Exercise -----

# Custom Functions -------------------------------------------------------------
pend <- haven::read_dta("./orig/PENDDAT_cf_W13.dta")

sat_small <- 
  pend %>% 
    filter(welle == 1) %>% 
    select(matches("PEO0300(a|b|c)")) %>% 
    slice(12:16) %>% 
    haven::zap_labels() %>% haven::zap_label() %>%  # remove labels
     mutate(across(everything(),~as.numeric(.x))) ## as numeric (again to drop labels)

# Don't Repeat Yourself (DRY):
sat_small %>% 
  mutate(dmean_PEO0300a = PEO0300a - mean(PEO0300a,na.rm = T),
         dmean_PEO0300c = PEO0300c - mean(PEO0300c,na.rm = T))


## defining a function -------------------
dtomean <- function(x){
  d_x <- x - mean(x,na.rm = T)
  return(d_x)
}

# using a custom function
dtomean(sat_small$PEO0300a)

# applying the function to the entire data set
lapply(sat_small,FUN = dtomean)
res <- lapply(sat_small,FUN = dtomean)
class(res)
sat_small %>% map(~dtomean(.x))


# using the function within across() retains the data.frame --------------------
sat_small %>% 
  mutate(across(matches("PEO0300"),~dtomean(.x),.names = "dmean_{.col}"))

# alternative: map_dfc() -> map and then bind_cols()
sat_small %>% map_dfc(~dtomean(.x))
sat_small %>% map(~dtomean(.x)) %>% bind_cols()


# Appendix: more across() -----------------------------------------------------


## multiple values in one go ---------------------------------------------------
dat3 %>%
  summarise(across(matches("studs|profs"), list(mean = ~mean(.x), sd = ~sd(.x))))


## create a list of statistics beforehand  ------------------------------------
wert_liste <- list(MEAN = ~mean(.x), SD = ~sd(.x))
dat3 %>%
  summarise(across(matches("studs|profs"), wert_liste), .by= prom_recht)


## amending the names ---------------------------------------------------------
dat3 %>%
  summarise(across(matches("studs|profs"), 
                   wert_liste,
                   .names = "{.fn}_{.col}"),
            .by= prom_recht)


dat3 %>%
  mutate(across(matches("studs|profs"),
                wert_liste, 
                .names = "{.col}XX{.fn}"))

