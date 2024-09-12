# GradAB R 2024 
# Chapter 5:  visualizing data using ggplot
# solutions

pend <- 
  haven::read_dta("./orig/PENDDAT_cf_W13.dta", 
                  col_select = c("zpsex", "welle", "bilzeit", "PA0445", "PG1270", "PEO0400c")
  )

# Exercise 1 -------------------------------------------------------------------

# Use this data set:
pend_u41 <- 
  pend %>% 
  filter(welle == 13, bilzeit > 0, PA0445 > 0) %>% 
  mutate(zpsex = factor(zpsex))

# Create a scatter plot for the variables "Duration of total unemployment experience in months" (`PA0445`, y-axis) and "Duration of education" (`bilzeit`, x-axis).
pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445)) +
  geom_point()

# Set the color to differentiate between men and women (`zpsex`).
pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = zpsex)) +
  geom_point()

# Change the colors to `goldenrod1` and `dodgerblue4` (or any other [from this list](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)).
pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = zpsex)) +
  geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"))

# Label the axes and legend!
pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = zpsex)) +
  geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = 1:2,
                     labels = c("Men","Women")) +
  labs(title = "Unemployment experience and education by gender",
       y = "Duration of total unemployment experience in months",
       x = "Duration of education") +
  theme_minimal()
  


# Exercise 2 ----------------------------------------------------------------

# Use this data set:
pend_u42 <- 
  pend %>% 
  filter(welle == 9, PG1270 > 0) 

# Create a boxplot or histogram for the distribution of the number of cigarettes and cigarillos smoked per day (in the last week) (`PG1270`).
pend_u42 %>% 
  ggplot(aes(y=PG1270)) +
  geom_boxplot()

# Customize this graphic so that the distributions for men and women are shown separately.
pend_u42 %>% 
  ggplot(aes(y=PG1270, x = factor(zpsex))) +
  geom_boxplot()
# How can you also set the colors based on gender? (Remember `color =` and `fill =`).
pend_u42 %>% 
  ggplot(aes(y=PG1270, x = factor(zpsex), 
             color = factor(zpsex) )) +
  geom_boxplot()
# Change the bar colors using `scale_color_manual`, `scale_color_brewer`, or `scale_color_viridis` .
pend_u42 %>% 
  ggplot(aes(y=PG1270, x = factor(zpsex), color = factor(zpsex))) +
  geom_boxplot() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = 1:2,
                     labels = c("Men","Women"))
pend_u42 %>% 
  ggplot(aes(y=PG1270, x = factor(zpsex), color = factor(zpsex))) +
  geom_boxplot() +
  scale_color_viridis_d(option = "rocket",end = .8)

# Exercise 3 -------------------------------------------------------------------

# Use this data set:
pend_u43 <- 
  pend %>% 
  filter(welle == 11, PEO0400c > 0, migration > 0) 

# Create a bar chart for the responses to the question, "A working mother can have just as close a relationship with her children as a mother who is not employed." (`PEO0400c`).

pend_u43 %>% 
  count(PEO0400c) %>% 
  ggplot(aes(y = PEO0400c,
             x = n)) +
  geom_col()
# Create a bar chart for `PEO0400c` separated by the `migration` variable, so set the bar colors based on `migration`. The `migration` variable captures whether the respondents have a migration background:
pend_u43 %>% 
  count(migration, PEO0400c) %>% 
  mutate(pct = prop.table(n), .by = migration) %>% 
  ggplot(aes(y = as_factor(PEO0400c),
             fill = factor(migration),
             group = factor(migration),
             x = pct)) +
  geom_col(position=position_dodge()) +
  scale_fill_manual(values = c("slategray1","slategray4"),
                    breaks = c(1,2), 
                    labels = c("No migration background", 
                               "Person has immigrated")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
   labs(title = "A working mother can have just as close a relationship\nwith her children as a mother who is not employed.",
       subtitle = "Agreement",
       caption = "Source: PASS-CF 0619",
       y = "",
       x = "Relative frequencies",
       fill = "Migration background" ) 


### set legend ----
pend_u43 %>% 
  count(migration, PEO0400c) %>% 
  mutate(pct = prop.table(n), .by = migration) %>% 
  ggplot(aes(y = as_factor(PEO0400c),
             fill = factor(migration),
             group = factor(migration),
             x = pct)) +
  geom_col(position=position_dodge()) +
  scale_fill_manual(values = c("slategray1","slategray4"),
                    breaks = c(1,2), 
                    labels = c("No migration background", 
                               "Person has immigrated")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
   labs(title = "A working mother can have just as close a relationship\nwith her children as a mother who is not employed.",
       subtitle = "Agreement",
       caption = "Source: PASS-CF 0619",
       y = "",
       x = "Relative frequencies",
       fill = "Migration background" ) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(1,.5),
        legend.justification = c(1,1))