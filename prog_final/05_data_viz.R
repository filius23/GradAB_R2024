## ----setup1, echo = F, include=FALSE---------------------------------------------------------------------------------------------------------------------------------
if(Sys.getenv("USERNAME") == "filse" ) path <- "D:/uolCloud/RFS/"
library(haven)
library(tidyverse)
library(patchwork)
library(gt)
library(scico)
knitr::opts_chunk$set(message = F,warning = F,highlight = "#<<",cache = T,
                      out.height= "65%", out.width = "65%", fig.align="center")
# https://wtoivo.github.io/SGSSS-data-viz-workshop/bar-plots.html
# pend <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")

tab_df <- xtabs(~zpsex+casmin, data = pend) %>% data.frame()

theme_set(new = theme_gray(base_size = 15))

# pend$palter[pend$palter>100] <- NA
# pend$casmin[pend$casmin<0] <- NA


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
library(haven) # for data import
library(tidyverse)
# library(ggplot2) # not necessary if tidyverse is loaded


## ----out.height= "80%", out.width= "80%", fig.align="center", echo = F-----------------------------------------------------------------------------------------------
#| warning: false
library(haven)
pend %>%
  filter(welle==13) %>% 
  mutate(zpsex_fct = factor(zpsex, levels = 1:2, labels = c("Men","Women")),
         palter = ifelse(palter < 0,NA,palter),
         azges1  = ifelse(azges1 < 0,NA,azges1)
         ) %>%
ggplot(aes(x = palter, y = azges1)) +
  geom_point(aes(color = zpsex_fct)) +
  # facet_grid(~zpsex_fct) +
  theme_minimal() +
  labs(color = "Gender", y = "Work Hours/Week",
       x = "Age") +
  scale_color_manual(values = c("lightskyblue4","navy"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend %>% select(azges1,zpsex,palter) %>% head()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small <- pend %>% filter(welle==13)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small$palter[pend_small$palter>100] <- NA 
pend_small$casmin[pend_small$casmin<0] <- NA
pend_small$PAS0100[pend_small$PAS0100<0] <- NA
pend_small$azges1[pend_small$azges1<0] <- NA


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small$palter[pend_small$palter>100] <- NA 
pend_small <-
  pend_small %>%
  mutate(across(c("casmin", "PAS0100", "azges1"),  ~ ifelse(.x < 0, NA, .x)))


## ----eval=F----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = dataset, aes(x = var1, y = var2, color = var3)) +
  geom_point() +
  labs(title= "Title", subtitle = "Subtitle") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = pend_small)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
ggplot(data = pend_small, aes(x = palter, y = azges1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = pend_small, aes(x = palter, y = azges1)) + geom_point()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = pend_small, aes(x = palter, y = azges1)) + geom_point(color = "orange")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| error=TRUE
# results in an error due to labels:
ggplot(data = pend_small, aes(x = palter, y = azges1, color = zpsex )) + 
  geom_point()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.numeric(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.character(zpsex))) + 
  geom_point()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"))


## ----out.height= "75%", out.width = "75%", fig.align="center"--------------------------------------------------------------------------------------------------------
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Men", "Women") )


## ----fllplt1---------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-height: 60%
#| out-width: 100%
#| cache: true
ggplot(data = pend_small, aes(x = palter, y = azges1, 
                               shape = as.factor(zpsex),
                               color = as.factor(zpsex))) + 
  geom_point(size = 4) + 
  scale_color_manual(values = c("lightskyblue4","orange"),
                     breaks = c(1,2), labels = c("Men", "Women")
                     ) +
  scale_shape_manual(values = c(18,20),
                     breaks = c(1,2), labels = c("Men", "Women")
                     ) +
  labs(color = "Gender", 
       shape = "Gender",
       y = "Hours/Week",
       x = "Age",
       title = "Working hours and age",
       subtitle = "By Gender",
       caption = "Soruce: PASS CF 0619"
       ) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
theme_set(new = theme_grey(base_size = 14))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
ggplot(data = pend_small, aes(y = azges1)) + geom_boxplot()
ggplot(data = pend_small, aes(x = azges1)) + geom_boxplot()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = pend_small, aes(y = azges1, x = factor(zpsex))) + geom_boxplot()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
ggplot(data = pend_small, aes(x = azges1)) + 
  geom_histogram()  
ggplot(data = pend_small, aes(x = azges1)) + 
  geom_histogram(fill = "sienna1")  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| layout-ncol: 2
ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram() 

ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: "table041"
pend_small$PD0400[pend_small$PD0400<0] <- NA # exclude missings
pend_small %>% 
  count(zpsex, PD0400) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
pend_small %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = factor(zpsex),
                       y = ..count..)) +
  geom_bar(position = position_dodge()) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| layout-ncol: 2
pend_small %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = factor(zpsex),
                       y = (..count..)/sum(..count..) )) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) 
# horizontal -> swap x and y axis
pend_small %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(y = as_factor(PD0400), fill = factor(zpsex),
                       x = (..count..)/sum(..count..) )) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) 


## ----fullplot--------------------------------------------------------------------------------------------------------------------------------------------------------
pend_small %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(y = PD0400, fill = factor(zpsex),
                       x = (..count..)/sum(..count..) )) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values = c("deepskyblue3","deepskyblue4"),
                    breaks = c(1,2), labels = c("Men", "Women")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(breaks = 1:4, 
                     labels = c("Überhaupt nicht",
                                "Eher nicht",
                                "Eher schon",
                                "Sehr")) +
  labs(title = "Religiösität nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: PASS-CF 0619",
       y = "Religiösität",
       x = "Relative Häufigkeit",
       fill = "Geschlecht" ) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend <- 
  haven::read_dta("./orig/PENDDAT_cf_W13.dta", 
    col_select = c("zpsex", "welle", "bilzeit", "PA0445", "PG1270", "PEO0400c")
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_u41 <- 
  pend %>% 
  filter(welle == 13, bilzeit > 0, PA0445 > 0) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_u42 <- 
  pend %>% 
  filter(welle == 9, PG1270 > 0) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
pend_u43 <- 
  pend %>% 
  filter(welle == 11, PEO0400c > 0) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
library(flextable)
set_flextable_defaults(font.family = "Red Hat Display", font.color = "grey25", border.color = "grey25", font.size = 8, padding = .75)
c(attributes(pend$PEO0400c)$labels, 
  attributes(pend$migration)$labels) %>%
  enframe(name = "label") %>%
  filter(value > 0) %>%
  mutate(Variable = c("`PEO0400c`", rep("", 3), "`migration`", rep("", 3))) %>% 
  relocate(Variable, value) %>%
  flextable() %>% 
  border_remove() %>% 
  autofit() %>% 
  padding(j = 2, padding.left = 5, padding.right = 15) %>% 
  align(align = "left", part = "header")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
eg <- tribble(
  ~x, ~y, ~size, ~x1,
  "A", 1, 5, 1,
  "B", 1, 10, 2,
  "C", 1, 15, 3
)
# #| fig-asp: 0.3 

eg_theme <- 
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        aspect.ratio = .5,
        plot.title = element_text(size = rel(1.3),hjust = 0.5, family = "Red Hat Display"))

aes_clr <- 
  ggplot(eg, aes(x = x, y = y, color = x)) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    labs(title = "color (discrete)") +
    eg_theme   

aes_clrc <- 
  ggplot(eg, aes(x = x1, y = y, color = x1)) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    coord_cartesian(xlim = c(0.5, 3.5)) +
    labs(title=  "color (continuous)") +
    eg_theme

aes_size <- 
  ggplot(eg, aes(x = x, y = y, size = x)) +
    geom_point() +
    scale_size_discrete(range = c(1.5, 10)) +
    guides(size = FALSE) +
    labs(title = "size") +
    eg_theme 
  
aes_fill <-   
  ggplot(eg, aes(x = x, y = y, fill = x)) +
    geom_point(size = 5, pch = 21, stroke = 1.5) +
    guides(fill = FALSE) +
    eg_theme+ 
  labs(title = "fill")

aes_shape <- 
  ggplot(eg, aes(x = x, y = y, shape = x)) +
    geom_point(size = 5) +
    guides(shape = FALSE) +
    eg_theme + 
    labs(title = "shape")
# Alpha

aes_alpha <- 
  ggplot(eg, aes(x = x, y = y, alpha = x)) +
    geom_point(size = 5) +
    guides(alpha = FALSE) +
    eg_theme +
  labs(title="alpha")


aes_clr + aes_size + aes_shape + 
aes_clrc + aes_fill + aes_alpha +
  plot_layout(ncol =3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
ggplot(data = pend_small, aes(x = palter, y = azges1, color = factor(zpsex))) + 
  geom_point(size = 2) + 
  theme_minimal()

ggplot(data = pend_small, aes(x = palter, y = azges1, color = factor(zpsex))) + 
  geom_point(size = 2) +
  theme_dark()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
#| echo: false

p1 <- ggplot(data = pend_small, aes(x = palter, y = azges1, color = factor(zpsex))) + 
  geom_point(size = 2) 

p1 + theme_gray() + labs(title="theme_grey() - Standard") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_bw() + labs(title="theme_bw()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_minimal() + labs(title="theme_minimal()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_dark() + labs(title="theme_dark()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_void() + labs(title="theme_void()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(data = pend_small, aes(x = palter, y = azges1, color = factor(zpsex))) + 
  geom_point(size = 3) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
p1 +  scale_color_manual(values = c("dodgerblue4","sienna1"),
                    breaks = c(1,2), labels = c("Men", "Women") )

p1 +  scale_color_manual(values = c("#005b96","#6497b1"),
                    breaks = c(1,2), labels = c("Men", "Women") )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 +
  scale_color_brewer(palette = "RdYlBu",
                    breaks = c(1,2), labels = c("Men", "Women") ) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
p1 +
  scale_color_viridis_d(option="magma",
                    breaks = c(1,2), labels = c("Men", "Women") ) 
p1 +
  scale_color_viridis_d(option="magma",begin = .65,end = .85,
                    breaks = c(1,2), labels = c("Men", "Women") ) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| fig-height: 2
#| echo: false
knitr::include_graphics("./pic/104_viridis-scales.png")


## ----eval =F---------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages('scico')
install.packages("MetBrewer")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| out-height: 60%
scico::scico_palette_show()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: 100%
#| echo: false
knitr::include_graphics("./pic/104_metbrewer.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| layout-ncol: 2
library(scico)
p1 +
  scale_color_scico_d(palette = "oslo",begin = .5,end = .8,
                    breaks = c(1,2), labels = c("Men", "Women") ) 


library(MetBrewer)
p1 +
  scale_color_met_d(name = "Kandinsky",
                    breaks = c(1,2), labels = c("Men", "Women") ) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| out-height: 50%
#| out-width: 50%
#| fig-align: "center"
shp_df <- data.frame(shp = factor(1:25), x = rep(1:5,each=5), y = rep(1:5,5))
ggplot(shp_df,aes(x,y)) +
  geom_point(shape=shp_df$shp, size = 7, fill = "dodgerblue") +
  geom_text(aes(label=shp,x = x-.2), size = 6) +
  theme_void(base_size=15)+
  scale_y_reverse() +
  theme(plot.margin = unit(c(2,2,2,2),"lines"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| out-height: 30%
#| out-width: 50%
#| fig-align: "center"
lt_df <- data.frame(x = 0, y = seq(0,.75,.125),
                    lty = 0:6,
                     lt = c("0 'blank'"   ,"1 'solid'"   ,"2 'dashed'"  ,"3 'dotted'"  ,"4 'dotdash'" ,"5 'longdash'",  "6 'twodash'" )  )
ggplot(lt_df, aes(x,y,linetype = factor(lty))) + 
  geom_segment(aes(xend = 1,yend = y), size = 1) +
  geom_text(aes(x=-.35,label = lt),hjust= 0, size = 6) +
  theme_void(base_size=12)+
  guides(linetype = F) +
  scale_y_reverse()



## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#| out-width: 80%
#| out-height: 80%
knitr::include_graphics("./pic/104_decision.png")

