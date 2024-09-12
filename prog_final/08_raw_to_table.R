# GradAB R 2024 
# Chapter 7:  from data to tables & graphs
# Does the gender difference in earnings differ by hours worked?

library(tidyverse)
library(haven)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_df <- read_dta("./orig/PENDDAT_cf_W13.dta", n_max = 1) # load the first row only


# Explore variables in data.frame --------------------------------------------------------------------------------------------------------------------------------------
attributes(pass_df$pnr)$label
attributes(pass_df$welle)$label


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
var_lab <- 
  pass_df %>% # call data.frame
    map(~attributes(.x)$label) %>%  # apply attributes to all variables (take the entire input)
    unlist() %>% # turn list into vector
    enframe() # turn the vector into a data.frame
head(var_lab)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
var_lab %>% filter(grepl("income",value))
var_lab %>% filter(grepl("time",value))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# var_lab %>% filter(grepl("income|school| Age | age |gender|working time",value)) # basic syntax
var_lab %>% filter(grepl("income|school|[A,a]ge |gender|working time",value)) # using regex 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_df <- read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
  filter(netges > 0,palter > 0, schul2 > 0,azges1>0, welle == 12) %>% 
  select(netges, palter, schul2, azges1, welle, zpsex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_df %>% count(schul2)
pass_df %>% count(zpsex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_df <- 
  pass_df %>% 
    mutate( 
      gender = factor(zpsex,  levels = 1:2, labels = c("Male","Female")) ,
      educ   = factor(schul2, levels = 2:7, 
                      labels = c("No degree","Special education","Lower secondary",
                                 "Intermediate secondary", "Upper secondary",
                                 "Upper secondary") )
      )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_df %>% count(gender,zpsex)
pass_df %>% count(educ,schul2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
library(wesanderson) # for colors from Wes Anderson movies
wesanderson::wes_palettes$Darjeeling2[1:2] # the package contains hex-codes lists - we'll go for Darjeeling2

# save it as an object
inc_hist <- 
  pass_df %>% 
  ggplot(aes(netges, fill = gender)) +
  geom_histogram(position = position_dodge(),bins = 50, color = "grey30", size = .1) +
  scale_fill_manual(values = wesanderson::wes_palettes$Darjeeling2[1:2])  +
  facet_grid(gender~.) + # split panel by gender
  labs(x = "Current net total income", y = "Count",
       fill = "", color = "",
       title = "Distribution of earnings by gender") +
  theme_minimal()+
  guides(fill = "none", color = "none") +
  theme(strip.text.y = element_text(angle = 0,size = rel(1.5)))
inc_hist
# export plot
ggsave(plot = inc_hist,filename = "./results/Fig01_Histogramm.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
## just to illustrate what pivot_longer does here:
pass_df %>% 
  select(palter, azges1,gender) %>% # retain only variables palter, azges1, gender
  pivot_longer(cols = -gender) %>%  # reshape variables except gender to long shape
  head(n=2) # only show lines 1-2

# store for later:
cont_desctab <- 
pass_df %>% 
  select(palter, azges1,gender) %>% 
  pivot_longer(cols = -gender) %>% 
  summarise(MEAN = mean(value), 
            SD =   sd(value),
            MIN =  min(value),
            MAX =  max(value),
            .by = c("gender","name"))

cont_desctab # quick look


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
desc_tab <- 
  pass_df %>% 
    select(educ,gender) %>% 
    count(educ,gender) %>% 
    mutate(pct = prop.table(n)*100, # to have it as percent
           pct = sprintf("%.2f",pct), # format number -> 2 digits
           pct = paste0(pct,"%"),
           .by = gender)

desc_tab # check


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod1 <- lm(netges~ gender*azges1+ I(azges1^2), data = pass_df) 
summary(mod1)
mod2 <- lm(netges~ palter + I(palter^2) + educ + gender*azges1 + I(azges1^2), data = pass_df) 
summary(mod2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
library(flextable)
library(modelsummary)
# set flextable defaults
set_flextable_defaults(font.family = "Times New Roman",font.color = "grey25",border.color = "grey25",
                       font.size = 8,padding = .5)

# create a data.frame with reference categories
ref_rows <- tribble( ~ term, ~ "Simple Model",  ~ "Full Model",
                      "Men",           'ref.',   'ref.',
                      "No degree",         '',   'ref.'
                     )
attr(ref_rows, 'position') <- c(3,16) # attach rows for ref cats

reg_flextab <- 
modelsummary(list("Simple Model" = mod1,
                  "Full Model" = mod2), 
             coef_rename = c("(Intercept)"="Intercept",
                                           "azges1" = "Working hours (h)",
                                           "I(azges1^2)" = "Working hours² (h)",
                                           "genderFemale" = "Female", # "×"
                                           "palter" = "Age",
                                           "I(palter^2)" = "Age²",
                                           "educSpecial education"      = "Special education",
                                           "educLower secondary"        = "Lower secondary",
                                           "educIntermediate secondary" = "Intermediate secondary",
                                           "educUpper secondary"        =  "Upper secondary"
                             ),
            add_rows = ref_rows,
            stars = T, gof_omit = "IC|Log|RMSE",
            output = "flextable") %>% autofit()

reg_flextab # check


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(marginaleffects)
pred_df <- 
      predictions(mod2,
                  newdata = datagrid(azges1 = 1:40, grid_type = "counterfactual"),
                  by = c("azges1","gender"))


fig02_pred <- 
  pred_df %>% 
  data.frame() %>% 
  ggplot(aes(x=azges1, y = estimate, fill = gender, color = gender,
             ymin = conf.low,
             ymax = conf.high)) + 
    geom_point() + # point estimates
    geom_line(aes(group = gender)) +
    geom_ribbon(alpha= .2, color = NA) +
  scale_fill_manual(values = wesanderson::wes_palettes$Darjeeling2[1:2] ) +
  scale_color_manual(values = wesanderson::wes_palettes$Darjeeling2[1:2] ) +
  theme_minimal()+ 
  labs(y = "Adjusted prediction of net income", x = "Working hours",
       fill = "", color = "")

fig02_pred 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave(plot = fig02_pred,filename = "./results/Fig02_Adjusted_Predictions.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
library(flextable) # install it if necessary
library(officer)   # install it if necessary

# set flextable defaults
set_flextable_defaults(font.family = "Times New Roman",font.color = "grey25",border.color = "grey25",
                       font.size = 8,padding = .5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cont_descflextab <- 
  cont_desctab %>% 
    mutate(name = 
             case_when(name == "palter" ~ "Age",
                       name == "azges1" ~ "Working hours"
                            )) %>% 
    relocate(name) %>%         # put name in first column
    arrange(name, gender) %>%  # sort by variable, then gender
        flextable() %>%        # turn data.frame into flextable
        border_remove() %>%    # remove everything to start from scratch
        hline_bottom() %>%     # bottom line
        hline_top() %>%        # top  line
        border( i = ~name != lead(name),
                border.bottom = fp_border(color = "grey25", width = .1 )) %>%  # put a border whenever name is different from following line
        set_header_labels(      # label header relabel
        gender = "Gender",
        name = "Variable") %>%  
        merge_v(j = 1) %>%      # put variable levels in column 1 into a joint cell
        fix_border_issues() %>% 
        autofit() 

cont_descflextab


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
des_flextab <- 
    desc_tab %>% 
      flextable() %>%      # turn data.frame into flextable
      border_remove() %>%  # remove everything to start from scratch
      hline_bottom() %>%   # top line
      hline_top() %>%      # bottom line
      border( i = ~educ != lead(educ),
              border.bottom = fp_border(color = "grey25", width = .1 )) %>%  # put a border whenever education is different from following line
      set_header_labels(   # label header
      educ = "Education",
      gender = "Gender", 
      n = "Frequency",
      pct = "Percent") %>%  
      merge_v(j = 1) %>%   # put education levels in column 1 into a joint cell
      fix_border_issues() %>% 
      autofit() %>% 
      padding(j = 3,padding.right = 6,part = "all")  # adjust width of column 3
des_flextab


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
reg_flextab_final <- 
  reg_flextab %>% 
    border_remove() %>%  # drop all borders
    hline_bottom() %>%   # bottom line
    hline_top() %>%      # top line
    border( i = ~ ` `== "Num.Obs.",
              border.top = fp_border(color = "grey25", width = .1 )) %>%  # line above Num.Obs.
    italic(.,j=-1,i =  ~`Full Model` == "ref.") %>%  # set ref. to italic
    autofit()

reg_flextab_final


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
read_docx("Vorlage_times_hochformat.docx") %>% # load template
  body_add_par(value = "Descriptives",style = "heading 1") %>% # create heading
  body_add_par(value = " ") %>%  # add empty row
  body_add_flextable(., value = des_flextab) %>%
  body_add_par(value = " ") %>% # add empty row
  body_add_flextable(., value = cont_descflextab) %>%
  body_add_par(value = "Regression results",style = "heading 1") %>% # create heading
  body_add_par(value = " ") %>% 
  body_add_flextable(., value = reg_flextab_final) %>%
  print(target = "./results/My_Tables.docx") # export using print with file name as target
# 

