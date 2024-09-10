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


# multiple values in one go 
dat3 %>%
  summarise(across(matches("studs|profs"), list(mean = ~mean(.x), sd = ~sd(.x))), .by= prom_recht)


## create a list of statistics beforehand  -------------------------------------
wert_liste <- list(MEAN = ~mean(.x), SD = ~sd(.x))
dat3 %>%
  summarise(across(matches("studs|profs"), wert_liste), .by= prom_recht)


## amending the names ----------------------------------------------------------
dat3 %>%
  summarise(across(matches("studs|profs"), 
                   wert_liste,
                   .names = "{.fn}_{.col}"),
            .by= prom_recht)


dat3 %>%
  mutate(across(matches("studs|profs"),
                wert_liste, 
                .names = "{.col}XX{.fn}"))


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


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
sat_small %>% 
  mutate(across(matches("PEO0300"),~dtomean(.x),.names = "dmean_{.col}"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| include: false
haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("welle","PEO0400a","PEO0400b","PEO0400c","PEO0400d")
                               ) %>%  
  filter(welle == 2) %>% 
  mutate(id = floor(as.numeric(rownames(.))/10),
         across(matches("PE"),~.x<0)) %>% 
  mutate(na = rowSums(select(., matches("PE") )) ) %>% 
  summarise(sum_na = sum(na),.by = id) %>% 
  arrange(sum_na) %>% 
  filter(id %in% 13:15)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("welle","zpsex","PEO0400a","PEO0400b","PEO0400c","PEO0400d")
                               ) %>% 
  filter(welle == 2) %>% 
  slice(1:15)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
library(gt)
etb18_smallx <-  haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("PEO0400a","PEO0400b","PEO0400c","PEO0400d"),
                               n_max = 1)

etb18_smallx %>% select(starts_with("PEO0400")) %>% 
  map_dfr(.,~attributes(.x)$label) %>% 
      t(.) %>% data.frame(lab = .) %>% 
      rownames_to_column(.,var = "var") %>% 
  mutate(lab = str_remove(lab,"Familie/Beruf\\: "),
         var = glue::glue("`{var}`")) %>% 
  gt() %>% 
  tab_options(  table.font.size = 12) %>% 
  fmt_markdown(columns = var)


data.frame(pend_small)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_small


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
data.frame(pend_small)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| include: false
#| warning: false
#| message: false
library(gt)
   
haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                       col_select = c("PEO0400a","PEO0400b","PEO0400c","PEO0400d"),n_max = 2)  %>% 
  select(starts_with("PEO0400")) %>% 
  map_dfr(.,~attributes(.x)$label) %>% 
      t(.) %>% data.frame(lab = .) %>% 
      rownames_to_column(.,var = "var") %>% 
  mutate(lab = str_remove(lab,"Familie/Beruf\\: "),
         var = glue::glue("`{var}`")) %>% 
  gt() %>% 
  tab_options(  table.font.size = 12) %>% 
  fmt_markdown(columns = var)


haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                col_select = c("PEO0400a","PEO0400b","PEO0400c","PEO0400d"),n_max = 1) %>% 
  map_dfr(.,~attributes(.x)$labels,.id = "var") %>% 
  pivot_longer(-var) %>%
  pivot_wider(names_from = value,values_from = name) %>%
  select(matches("^1|^2|^3|^4|-10")) %>%
  rename(`-10 to -1`=`-10`) %>%
  mutate(`-10 to -1` = "n.z./n.a.") %>%
  distinct()




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_small %>% 
  mutate(std_PEO0400b = (PEO0400b - mean(PEO0400b,na.rm = T))/sd(PEO0400b,na.rm = T))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% mutate(rel_to_mean = studs-mean(studs),
                ab_mean_lab = ifelse(rel_to_mean > 0,"above","below"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small2 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("palter","PEO0400a","PEO0400b","PEO0400c","statakt"))  %>% 
  slice(5624:5640)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_small2 %>% mutate(PEO0400a = ifelse(PEO0400a<0,NA,PEO0400a))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small2 %>% mutate(across(c("PEO0400a","PEO0400b","PEO0400c","statakt"), ~ifelse(.x<0,NA,.x)))  
pend_small2 %>% mutate(across(matches("PEO0400|statakt"), ~ifelse(.x<0,NA,.x)))  # even shorter: matches()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% mutate(age = case_when(gegr < 1500 ~ "very old",
                                gegr < 1900 ~ "old"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% mutate(age = case_when(gegr < 1500 ~ "very old",
                                gegr < 1900 ~ "old",
                                TRUE ~ "relatively new"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% mutate(age = case_when(gegr < 1500 & prom_recht  == T ~ "very old university",
                                gegr < 1900 & prom_recht  == T ~ "old university",
                                gegr > 1900 & prom_recht  == T ~ "young university",
                                gegr < 1900 & prom_recht  == F ~ "old college",
                                gegr > 1900 & prom_recht  == F ~ "young college"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cut(dat3$profs,breaks = c(50, 200, 350, 500, 650))
cut(dat3$profs,breaks = seq(50,650,150))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3$prof_class <- cut(dat3$profs,breaks = seq(50,650,150))
dat3


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% count(prof_class)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
n200_350 <-  dat3 %>% count(prof_class) %>% filter(grepl("\\(20",prof_class)) %>% pull(n)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3$prof_class <- NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
bsp <- c(1990,1998,2001,2009)
bsp
cut(bsp,breaks = c(1990,2000,2010)) 
# Specify the number of digits in the labels
cut(bsp,breaks = c(1990,2000,2010),dig.lab = 4) 
# Include the lower boundary
cut(bsp,breaks = c(1990,2000,2010),dig.lab = 4,include.lowest = T) 
# Number the categories instead of labels:
cut(bsp,breaks = c(1990,2000,2010),labels = FALSE)
# Specify your own labels:
cut(bsp,breaks = c(1990,2000,2010),labels = c("90s","00s"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
sat_small %>% rename(newname = PEO0300a)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
sat_small %>% rename_with(~tolower(.))
sat_small %>% rename_with(~str_remove(.x,"PEO0300"))
sat_small %>% rename_with(~str_replace(.x,"PEO0300","Occupation_"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat3 %>% mutate(uni_fh = str_detect(uni,"Uni"))
dat3 %>% mutate(bula = case_when(str_detect(uni,"Bremen")~ "HB",
                                 str_detect(uni,"Oldenb|Vechta")~ "NDS",
                                 str_detect(uni,"Bonn|Aachen")~ "NRW",
                                 str_detect(uni,"Freiburg")~ "BW"
                                 ))
dat3 %>% mutate(ort = str_remove(uni,"Uni |FH |RWTH "))

