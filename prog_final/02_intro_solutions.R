# R Intro GradAB Sept 2024
# Chapter 2 - Solutions


#  Exercise 1 ----
  dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079), 
                     profs = c(250,553,438 ,150,636),
                     prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                     gegr  = c(1971,1870,1457,1818,1995))

# Do you see dat2 in your environment?
# Print dat2 in the console.
# Add the names of the universities as a new column to the dataset. The names are in this order:
dat2$names <-  c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg")

# Display dat2 - either in the console or using View().
dat2
View(dat2)

# Calculate the ratio of students per professor and store the results in a new variable. Check the result.
dat2$studprof <- dat2$studs/dat2$profs
dat2

# Display only the third row of dat2.
dat2[3,]
# Display only the third column of dat2.
dat2[,3]

# What would you do to copy dat2 into an object called df_unis?
df_unis <- dat2


#  Exercise 2 ----------

# Create a .Rprofile for the package installation in C:\Users\*USERNAME*\Documents.
# Install the tidyverse packages using fdz_install("tidyverse") after placing the .Rprofile file under C:\Users\*USERNAME*\Documents.
# Use the data.frame dat2 from Exercise 1.
# Use filter to display only the universities with fewer than 10,000 students. (Remember to install and load {tidyverse} with library()).
# Display the founding years (gegr) column of universities with the right to award doctorates (prom_recht).

# Exercise 3 ------

# Continue using the dataset from Exercises 1 & 2 (dat2).
# Display only the universities that were founded in 1971, 1457, or 1995, and for these cases, show only the name and founding year.
dat2 %>% filter(gegr %in% c(1971, 1457, 1995))

# Sort the dataset according to the following order. (Create a factor variable that defines this order.)
dat2$names_fct <- factor(dat2$names,levels = c("RWTH Aachen", "Uni Freiburg", "Uni Bonn", "FH Aachen", "FH Bonn-Rhein-Sieg"))
dat2 %>% arrnage(dat2)

# Exercise 4 --------------

# Create an R project in your directory for this course.
# Save the personal data from the PASS-CampusFile (PENDDAT_cf_W13.dta) in your directory in the subfolder orig.
# Read the dataset PENDDAT_cf_W13.dta as shown above into R and assign it to the object name pend.
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")
# Use head() and View() to get an overview of the dataset.
head(pend)
# How many respondents (rows) does the dataset contain?
nrow(pend)
# Display the variable names of pend using names()!
names(pend)

# How old is the respondent with the `pnr` 1000908201 in `welle` 10 (in `pintjahr` 2016)?
pend %>% filter(pnr == 1000908201) %>% select(pnr,welle,palter)
pend %>% filter(pnr == 1000908201, welle == 10) %>% select(pnr,welle,palter)
# Store the resulting data.frame in an object.
resp_1000908201  <- pend %>% filter(pnr == 1000908201, welle == 10) %>% select(pnr,welle,palter)
resp_1000908201$palter

# Exercise 5 -----------

# Export the data.frame with the smaller dataset version (resp_1000908201) created in the previous exercise as an .Rdata file.
saveRDS(resp_1000908201,file = "./data/resp_1000908201.Rdata")
# Load the exported .Rdata file under a different name, e.g., resp_1000908201_new
resp_1000908201_new <- readRDS(file = "./data/resp_1000908201.Rdata")
# Did everything work? Compare the newly loaded object with the original one: identical(resp_1000908201, resp_1000908201_new) - are both objects identical?
identical(resp_1000908201, resp_1000908201_new)
  
