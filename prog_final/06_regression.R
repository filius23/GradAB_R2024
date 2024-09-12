# GradAB R 2024 
# Chapter 5:  visualizing data using ggplot

library(patchwork)
library(tidyverse)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,3),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1


# simple regression model ----
lm(var2 ~ var1, data = dat1)
m1 <- lm(var2 ~ var1, data = dat1)  
# simple regression table
summary(m1)

# access results
m1$coefficients
summary(m1)$coefficients
View(m1)


# use select observations -------------
dat1_u20 <- dat1 %>% filter(var2 < 20)
m2a <- lm(var2 ~ var1, data = dat1_u20)
summary(m2a)
m2b <- lm(var2 ~ var1, data = dat1 %>% filter(var2 < 20))
summary(m2b)




# regression table  ------------------------------------------------------------
#fdz_install("modelsummary")
library(modelsummary)
modelsummary(list(m1,m2a,m2b))

# label, stars, omit unnecessary goodnes of fit
modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log")

# quick word export
modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log",output = "./results/Regression_table.docx")


#  categorial independent variables --------------------------------------------
dat1
m3 <- lm(var2 ~ factor(educ), dat1)
summary(m3)

# change labels & 
dat1$ed_fct <- factor(dat1$educ, levels = 1:3,
                        labels = c("basic", "medium", "high"))
dat1
m3 <- lm(var2 ~ ed_fct, dat1)
summary(m3)

##  change reference category 
dat1$ed_fct <- relevel(dat1$ed_fct, ref = "medium")
m3b <- lm(var2 ~ ed_fct, dat1)
summary(m3b)


# multiple independent variables -----------------------------------------------
m4 <- lm(var2 ~ ed_fct + var1, dat1)
summary(m4)
