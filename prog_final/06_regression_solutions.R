# GradAB R 2024 
# Chapter 6:  regression basics
# solutions


Use the following subset of the PASS CampusFile:
  
  pend_ue08 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
    filter(welle == 13, netges > 0, azges1 > 0, schul2 > 1, palter > 0)
  
  6.7.1 Regression model
  
# Create an object mod1 with a linear regression model (lm) where netges (monthly net income in EUR) is the dependent variable and azges1 (working hours) is the independent variable! (see here)
# Examine the results of mod1 - what can you infer about the relationship between netges and azges1?
    
# 6.7.2 Categorical Independent Variables
  
# Create a regression model with the income of respondents (netges) as the dependent variable and the education level of the respondents schul as the independent variable:
# Ensure that schul2 is defined as a factor. Assign the labels “No degree”, “Special education School”, “Secondary School”, “Intermediate Diploma”, “Vocational School”, “Abitur” to levels 2-7 and save the factor as a variable schul2_fct in your data.frame - see the code help below:
  
  pend_ue08$schul2_fct <-  
  factor(pend_ue08$schul2, 
         levels = 2:7, 
         labels = c("No degree", "Special education School", "Secondary School",
                    "Intermediate secondary school", 
                    "Upper secondary", "Abitur"))

# Create the regression model using this new factor variable for schul2_fct as the independent variable.
# Change the reference category to Intermediate secondary school (schul2 = 5) and estimate the model again.

# 6.7.3 Multiple Independent Variables & Coefficient Plot

# Adjust the lm() model mod1 (with all cases from pend_u08) to include the education level (schul2) as an additional independent variable.
# Also create a graphical comparison of the two models with and without the education level.

