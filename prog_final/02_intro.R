# Intro part 2 --------------
library(tidyverse)
library(kableExtra)



# data.frame ----
studs <- c(19173, 5333, 15643)  # Store student numbers under "studs"
profs <- c(322, 67, 210)        # Store professor numbers under "profs"
dat1_orig <- data.frame(studs, profs)
dat1_orig


## Without intermediate objects ----
dat1 <- data.frame(studs = c(19173, 5333, 15643), 
                   profs = c(322, 67, 210),
                   gegr  = c(1971, 1830, 1973)) 
dat1    # Display the entire dataset


# variable: nothing else than a vector
dat1$profs  # 
x <- dat1$profs  


## some convenience functions ----
colnames(dat1) ## Display variable/column names
names(dat1) ## Display variable/column names
ncol(dat1) ## Number of columns/variables
nrow(dat1) ## Number of rows/cases


## creating a new variable ----
dat1$stu_prof <- dat1$studs/dat1$profs
## dat1 now has one more column:
ncol(dat1) 
dat1

# text: character vectors ----
dat1$uni <- c("Uni Bremen", "Uni Vechta", "Uni Oldenburg")
dat1

View(dat1)


x1 <- 1:20
class(x1)


# data types --------

      
## checking data types ----
class(dat1$profs)
class(dat1$uni)


## changing data types ----
as.character(dat1$profs) ## The "" indicate that the variable is defined as character
class(dat1$profs) ##does not change the original variable 

dat1$profs <- as.character(dat1$profs) # overwrites 
class(dat1$profs)


## calculation with characters results in error ----
# dat1$profs / 2 


## as.numeric ----
as.numeric(dat1$profs) / 2
as.numeric(dat1$uni)

# EXERCISE ---- 

# navigating data.frames ----
dat1 # complete dataset
dat1[1,1] # first row, first column
dat1[1,]  # first row, all columns
dat1[,1]  # all rows, first column (equivalent to dat1$studs)
dat1[,"studs"] # all rows, column named studs -> note: ""

## filtering using base R ----
dat1[dat1$studs > 10000, ] # rows where studs is greater than 10000, all columns
dat1[dat1$studs > 10000 & dat1$profs < 300, ] # & means AND
dat1$profs[dat1$studs > 10000] # Only see the number of professors: no comma

## tedious and verbose!

# packages ----

## install.packages("Package") # only needed once on your PC
## library(Package) # needed after every restart

## IAB-FDZ .Rprofile

## install tidyverse ----
## install.packages("tidyverse") ## anywhere but IAB
fdz_install("tidyverse") # on IAB servers with .Rprofile


## library -------
library(tidyverse) # after once using install.packages("tidyverse")


## Chaining Commands with %>% ------------
dat1 %>%
  filter(studs > 10000) %>%
  select(uni,studs)

dat1 %>% filter(.,studs > 10000) %>% select(.,uni) # the dot represents the result of the previous step
dat1 %>% filter(studs > 10000) %>% select(uni)

dat1 # does not change dat1 !

## storing filter results ------
over_10k <- filter(dat1, studs > 10000)
over_10k


# filter examples ----
filter(dat1, studs >= 10000)
filter(dat1, studs <= 10000)
filter(dat1,studs > 10000 | profs < 200) # more than 10.000 Students *or* less than 200 professors
filter(dat1, gegr %in% c(1971,1830)) # founded 1971 or 1830
filter(dat1, between(gegr,1971,1830)) # founded between 1971 and 1830 (including)


# select examples -------
dat1 %>% select(uni,studs) # columns uni and studs
dat1 %>% select(1:3) # column 1-3
dat1 %>% select(-profs) # all but profs


# slice -------------
dat1 %>% slice(1) # first row
dat1 %>% slice(2:3) # rows 2-3
dat1 %>% slice(c(1,3)) # rows 1 and 3


# arrange ---------
dat1 %>% arrange(studs)
dat1 %>% arrange(uni)
dat1 %>% arrange(desc(gegr), studs)


# factor variables -> ordering --------
factor(dat1$uni, levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))

dat1$uni_fct <- factor(dat1$uni, 
                       levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))

dat1
class(dat1$uni_fct)
dat1 %>% arrange(uni_fct)

# use the levels = and labels = for a lazy recode:
dat1$gegr_fct <- factor(dat1$gegr,
                       labels = c(1971,1830,1973) ,
                       levels = c("70s","early","70s"))
dat1
dat1 %>% arrange(desc(gegr_fct))
dat1 %>% arrange(desc(gegr_fct),gegr)

# EXERCISE ---- 


# creating a RStudio Project ----
getwd()
rstudioapi::openProject(path = "D:/Courses/R-Course")


## load dta file ----------
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta") 


class(pend)
head(pend)
nrow(pend)
ncol(pend)


pend06 <- pend %>% filter(pintjahr == 2006)
nrow(pend06)
pend06$palter



# export files --------
saveRDS(pend06, file = "./data/pend06.RData")
rm(pend06) # delete pend06 from memory

pend06_neu <- readRDS(file = "./data/pend06.RData")
head(pend06) # does not exist anymore -> we rm()ed it earlier
head(pend06_neu,n=1)


# getting help ------------
?read_dta()
vignette(read_dta)



