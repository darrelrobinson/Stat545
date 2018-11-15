##Factors
rm(list = ls())
library(tidyverse)
library(forcats)
library(gapminder)
str(gapminder)

levels(gapminder$continent)
contrasts(gapminder$continent)
nlevels(gapminder$continent)

gapminder %>%
  count(continent)

fct_count(gapminder$continent)

#getting rid of unused levels
nlevels(gapminder$country)
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>%
  filter(country %in% h_countries)
nlevels(h_gap$country)

h_gap_dropped <- h_gap %>%
  droplevels()
nlevels(h_gap_dropped$country)

#forcats has fct_drop
h_gap$country %>%
  fct_drop()
?fct_drop

#factor levels ordered alphabetically by default
gapminder$continent %>% 
  levels()
#order by frequency
gapminder$continent %>%
  fct_infreq() %>% 
  levels()
#reverse frequency
gapminder$continent %>%
  fct_infreq() %>%
  fct_rev() %>%
  levels()

ggplot(gapminder, aes(x = continent)) + 
  geom_bar() + 
  coord_flip()
#or
ggplot(gapminder, aes(x = fct_rev(fct_infreq(continent)))) + 
  geom_bar() + 
  coord_flip()

#now let's order country by another variable
#the factor is the default grouping variable and the median is the default summarizing function
#order by life expectancy
fct_reorder(gapminder$country, gapminder$lifeExp) %>%
  levels() %>% head()
#according to minimum life expectancy
fct_reorder(gapminder$country, gapminder$lifeExp, min) %>%
  levels() %>% head()
#backwards
fct_reorder(gapminder$country, gapminder$lifeExp, .desc = TRUE) %>%
  levels() %>% head()

#let's create a plot as a motivating example
gap_asia_2007 <- gapminder %>% filter(year == 2007 & continent == "Asia")
ggplot(gap_asia_2007, aes(x = lifeExp, y = country)) + geom_point() #messy!!
ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) + geom_point() #nice


#use fct_reorder2() when you have a line chart if quantitative x by quantiative y with factor as color
ggplot(h_gap_dropped) + 
  geom_line(aes(x = year, y = lifeExp, col = country)) #the legend does match the line position

ggplot(h_gap_dropped) + 
  geom_line(aes(x = year, y = lifeExp, col = fct_reorder2(country, year, lifeExp))) + 
  labs(color = "Country") #so much better, easy fix too...

#Changing the order of levels
h_gap_dropped$country %>% levels
h_gap_dropped$country %>% fct_relevel("Romania", "Haiti") %>% levels() #easy...

#Recode the levels
i_gap <- gapminder %>% 
  filter(country %in% c("United States", "Sweden", "Australia")) %>%
  droplevels()
i_gap$country %>% levels()
i_gap$country %>% fct_recode("USA" = "United States", "Oz" = "Australia") %>% levels()

#Grow a factor
#let's say we start with two data frames with factors of different levels and we want to concatenate them
df1 <- gapminder %>%
  filter(country %in% c("United States", "Mexico"), year > 2000) %>%
  droplevels()
df2 <- gapminder %>%
  filter(country %in% c("France", "Germany"), year > 2000) %>%
  droplevels()

#the country variables have different levels
levels(df1$country)
levels(df2$country)
#using c() will just screw it up
c(df1$country, df2$country)
#but fct_c()
fct_c(df1$country, df2$country)


#what about binding rows?
bind_rows(df1, df2)
rbind(df1, df2)

#

