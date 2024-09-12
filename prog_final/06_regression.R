## ----reg01, include=F------------------------------------------------------------------------------------------------------------------------------------------------
# http://hbiostat.org/rmsc/

library(patchwork)
library(tidyverse)
mark_color <- "grey25"
color1x =  "#00519E" # uol farbe
colorhex <- "#FCFCFC" #"#FCF9F0FF"7
colorhex <- NA #"#FCF9F0FF"7
library(extrafont)
windowsFonts(Nunito=windowsFont("Nunito Sans"))
# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.07/bin/gswin64.exe")
# embed_fonts("newfont.pdat1")

theme_x <- 
  theme_minimal(base_family = "Nunito",base_size = 13) +
  theme(
    plot.background = element_rect(fill = colorhex, linetype = 1, colour = NA),
    rect = element_rect(fill = colorhex, linetype = 1, colour = NA),
    axis.text =  element_text(color = mark_color,face = "plain", size = rel(.75), angle = 0), 
    axis.title = element_text(color = mark_color,face = "plain", size = rel(1), angle = 0), 
    axis.title.y = element_text(color = mark_color,face = "plain", angle = 0,vjust = .5), 
    axis.ticks = element_blank(),
    axis.line = element_line(size = .1), 
    panel.grid = element_line(colour = "grey81", linetype = 1, size = .15), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    plot.subtitle = element_text(hjust=0),
    plot.caption = element_text(hjust=1, size = rel(1.2), color = mark_color),
    plot.margin = unit(c(1, 1, 1, 1), "lines"))

# theme_set(theme_x)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,3),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1


## ----echo = F--------------------------------------------------------------------------------------------------------------------------------------------------------
m1 <- lm(var2 ~ var1, data = dat1)  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lm(var2 ~ var1, data = dat1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 <- lm(var2 ~ var1, data = dat1)  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(m1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1$coefficients
summary(m1)$coefficients


## ----ols1_str, message=F, out.width="100%", out.height="10%"---------------------------------------------------------------------------------------------------------
#| echo: false
listviewer::jsonedit(m1, mode="view") 


## ----fig.height=3, fig.width=3, echo=T, fig.align="center", warning=F, message=F-------------------------------------------------------------------------------------
library(ggplot2)
ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm")  


## ----graph2----------------------------------------------------------------------------------------------------------------------------------------------------------
#| fig.height: 3
#| fig.width: 3
#| fig-align: center
#| warning: false
#| message: false
ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm")  +
  geom_text(data = . %>% filter(var2 > 20), aes(y = var2 + 3, label = id), color = "sienna1")


## ----ols2------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1_u20 <- dat1 %>% filter(var2 < 20)
m2a <- lm(var2 ~ var1, data = dat1_u20)
summary(m2a)


## ----ols2a-----------------------------------------------------------------------------------------------------------------------------------------------------------
m2b <- lm(var2 ~ var1, data = dat1 %>% filter(var2 < 20))
summary(m2b)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
fdz_install("modelsummary")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| message: false
library(modelsummary)
modelsummary(list(m1,m2a,m2b))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log",output = "./results/Regression_table.docx")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1
m3 <- lm(var2 ~ factor(educ), dat1)
summary(m3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1$ed_fct <- factor(dat1$educ, levels = 1:3,
                        labels = c("basic", "medium", "high"))
dat1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m3 <- lm(var2 ~ ed_fct, dat1)
summary(m3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1$ed_fct <- relevel(dat1$ed_fct, ref = "medium")
m3b <- lm(var2 ~ ed_fct, dat1)
summary(m3b)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m4 <- lm(var2 ~ ed_fct + var1, dat1)
summary(m4)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| include: false
theme_set(theme_grey(base_size = 15))  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "50%"
#| out-height: "50%"
modelplot(m4)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "50%"
#| out-height: "50%"
modelplot(list("Model 1" = m1,
               "Model 4" = m4))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "50%"
#| out-height: "50%"
modelplot(list("Model 1" = m1,
               "Model 4" = m4),
          coef_map = c("var1" = "Name for var1",
                       "ed_fcthigh" = "Higher Education",
                       "ed_fctbasic" = "Basic Education"
                          ))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "50%"
#| out-height: "50%"
modelplot(list("Model 1" = m1,
               "Model 4" = m4),
          coef_map = c("var1" = "Name for var1",
                       "ed_fcthigh" = "Higher Education",
                       "ed_fctbasic" = "Basic\nEducation")) + # \n inserts a line break
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # Add a 0 line
  scale_color_manual(values = c("orange", "navy")) +
  theme_minimal(base_size = 15, base_family = "mono") 


## ----echo = T, eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
pend_ue08 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
  filter(welle == 13, netges > 0, azges1 > 0, schul2 > 1, palter > 0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
#| classes: plain
library(gt)

  haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                col_select = c("schul2"), n_max = 1) %>% 
  map_dfr(., ~attributes(.x)$labels, .id = "var") %>% 
  pivot_longer(-var) %>%
  pivot_wider(names_from = value, values_from = name) %>% 
  select(matches("-4|^2|^3|^4|^5|^6|^7")) %>% 
  rename(`<0 & 1`=`-4`) %>% 
  mutate(`<0 & 1` = "NA") %>% 
  pivot_longer(everything(), names_to = "value", values_to = "label") %>% 
  distinct()  %>% 
  mutate(value = glue::glue("`{value}`")) %>% 
  gt() %>% 
  tab_options(table.font.size = 14) %>% 
  cols_align(align = "left",columns = value) %>% 
  cols_width(value ~ px(80),
             value ~ px(40)) %>% 
  fmt_markdown(columns = value) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
#| eval: false
pend_ue08$schul2_fct <-  
  factor(pend_ue08$schul2, 
         levels = 2:7, 
         labels = c("No degree", "Special education School", "Secondary School", "Intermediate Diploma", "Vocational School", "Abitur"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1 <- dat1 %>% select(-matches("compl"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| include: false
theme_set(theme_x)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1$fitted.values


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1$lm_predictions <- m1$fitted.values
dat1


## ----fig.height=3, fig.width=3, echo=T, fig.align="center", warning=F, message=F-------------------------------------------------------------------------------------
#| code-fold: true
ggplot(dat1, aes(x = var1, y = var2)) +
  geom_point(size = 3) +      
  geom_smooth(method = "lm", color = "darkblue", se = FALSE, size = .65) +
  geom_point(aes(x = var1, y = lm_predictions), color = "dodgerblue3", size = 3) +
  expand_limits(x = c(0,8), y = c(0,8))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1$residuals


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat1$lm_residuals <- m1$residuals
dat1


## ----fig.height=2.75, fig.width=2.75, fig.align="center", eval = T, message=F----------------------------------------------------------------------------------------
#| code-fold: true
ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_smooth(method = "lm", color = "darkblue", se = FALSE, size = .65) +
  geom_segment(aes(x = var1, xend = var1, y = var2, yend = lm_predictions), color = "dodgerblue3", size = .65, linetype = 1) +
  geom_point(size = 3) +
  geom_point(aes(x = var1, y = lm_predictions), color = "dodgerblue3", size = 3) +
  expand_limits(x = c(0,8), y = c(0,8))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
install.packages("performance")


## ----reg02, fig.height=9---------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
#| message: false
library(performance)

model_test <- check_model(m4)
plot(model_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
library(ggfortify)
autoplot(m1, which = 2)

## ----echo = F, fig.align="center", out.width="60%", message=F, warning = F-------------------------------------------------------------------------------------------
library(ggfortify)
autoplot(m3, which = 2, ncol = 1, nrow = 1) + theme(aspect.ratio = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(m1$residuals) 


## ----eval = F--------------------------------------------------------------------------------------------------------------------------------------------------------
autoplot(m1, which = 1)

## ----fig.align="center",out.height ="50%", echo=F--------------------------------------------------------------------------------------------------------------------
autoplot(m1, which = 1, ncol = 1, nrow = 1)  + theme(aspect.ratio = 1)


## ----eval = F--------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("lmtest")

## ----message = F-----------------------------------------------------------------------------------------------------------------------------------------------------
library(lmtest)
bptest(m3)


## ----eval= F---------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("car")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| message: false
#| warning: false
# library(car)
pendx <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",n_max = 300)  %>% filter(netges >0, palter >0 )
mox <- lm(netges ~ palter + azges1, data=pendx)
car::vif(mox)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
install.packages("performance")


## ----reg03-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(performance)
compare_performance(m1, m4, metrics = c("R2", "R2_adj"))


## ----mod1_tidy-------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "90%"
#| out-height: "50%"
#| fig-align: "center"
library(broom) ## already loaded as part of the tidyverse
tidy(m3, conf.int = TRUE)

tidy(m3, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "ed_fct", "Education: ")) %>% 
  ggplot(aes(y = term, x = estimate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "navy") +
  geom_errorbarh(aes(xmin = conf.low, xmax  = conf.high), height = .1) + 
  geom_point(color = "orange", shape = 18, size = 7) +
  theme_minimal(base_size = 16)

