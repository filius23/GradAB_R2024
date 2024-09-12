# GradAB R 2024 
# Chapter 5:  visualizing data using ggplot

library(haven)
library(tidyverse)
# load data
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")

# filter wave 13 ---------------------------------------------------------------
pend_small <- pend %>% 
  filter(welle==13, palter < 100,
         casmin >0 , PAS0100 > 0 , azges1 > 0)


# basic ggplot structure ----
# ggplot(data = dataset, aes(x = var1, y = var2, color = var3)) +
#   geom_point() +
#   labs(title= "Title", subtitle = "Subtitle") +
#   theme_minimal()


# step by step -----------------------------------------------------------------
ggplot(data = pend_small)

ggplot(data = pend_small, aes(x = palter, y = azges1))

ggplot(data = pend_small, aes(x = palter, y = azges1)) + geom_point()

ggplot(data = pend_small, aes(x = palter, y = azges1)) + geom_point(color = "orange")

# aes() --> color is decided based on variable
ggplot(data = pend_small, aes(x = palter, y = azges1, color = zpsex )) + 
  geom_point()

# type of color scale depends on data class 
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.numeric(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.character(zpsex))) + 
  geom_point()


# custom colors ----------------------------------------------------------------
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"))

## adding labels to color legend
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Men", "Women") )


## shape  & labels----
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

 plot1 <- 
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
 
# export -------------
ggsave(plot1,filename = "./results/My_plot1.png")
ggsave(plot1,filename = "./results/My_plot1.png",height = 6,width = 6.5)


# Distributions ----------------------------------------------------------------
## boxplot ---------------------------------------------------------------------
ggplot(data = pend_small, aes(y = azges1)) + geom_boxplot()
ggplot(data = pend_small, aes(x = azges1)) + geom_boxplot()

# separate boxes by gender
ggplot(data = pend_small, aes(y = azges1, x = factor(zpsex))) + geom_boxplot()

## histogram -------------------------------------------------------------------
ggplot(data = pend_small, aes(x = azges1)) + 
  geom_histogram()  

ggplot(data = pend_small, aes(x = azges1)) + 
  geom_histogram(fill = "sienna1")  

ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram() 

ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) 

ggplot(data = pend_small, aes(x = azges1, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht")


# categorial variables ---------------------------------------------------------

pend_small$PD0400[pend_small$PD0400<0] <- NA # exclude missings
pend_small %>% 
  count(zpsex, PD0400) %>% 
  filter(!is.na(PD0400))

## bar plot ------------------
pend_small %>% 
  count(zpsex, PD0400) %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = factor(zpsex),
                       y = n)) +
  geom_col(position = position_dodge()) 

## relative frequencies ------------
pend_small %>% 
  count(zpsex, PD0400) %>% 
  filter(!is.na(PD0400)) %>% 
  mutate(pct = prop.table(n), .by = zpsex) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = factor(zpsex),
                       y = pct )) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) 

## horizontal -> swap x and y axis ---------
pend_small %>% 
  count(zpsex, PD0400) %>% 
  filter(!is.na(PD0400)) %>% 
  mutate(pct = prop.table(n), .by = zpsex) %>% 
  ggplot(data = ., aes(y = as_factor(PD0400), fill = factor(zpsex),
                       x = pct )) +
  geom_col(position = position_dodge()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) 


## ----fullplot-----------------------------------------------------------------
pend_small %>% 
  count(zpsex, PD0400) %>% 
  filter(!is.na(PD0400)) %>% 
  mutate(pct = prop.table(n), .by = zpsex) %>% 
  ggplot(data = ., aes(y = as_factor(PD0400), 
                       fill = factor(zpsex),
                       x = pct )) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("deepskyblue3","deepskyblue4"),
                    breaks = c(1,2), labels = c("Men", "Women")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Religiösität nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: PASS-CF 0619",
       y = "Religiösität",
       x = "Relative Häufigkeit",
       fill = "Geschlecht" ) 



