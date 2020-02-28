# title: Birth_registration_rates
# author: Maarten Vink (@maartenpvink)
# date: 28 February 2020
# description: Birth registration rates around the world, 2009-18 (latest available year)
# for plotting made use of R code from @ShandiyaB https://github.com/shandiya/TidyTuesday/tree/master/2020-1-MMR-vaccinations

# load packages
library(tidyverse)
library(ggthemes)
library(extrafont)
library(WDI)
library(ggrepel)

setwd(" ")

#search 
WDIsearch('birth registration')
#[1,] "SP.REG.BRTH.ZS"    "Completeness of birth registration (%)" 

# load data file
dat_birth = WDI(indicator='SP.REG.BRTH.ZS')
dat_pop = WDI(indicator='SP.POP.TOTL')

glimpse(dat_birth)
glimpse(dat_pop)

#merge birth and population data
dat <- merge(dat_birth,dat_pop, by= c("iso2c", "country", "year"))
glimpse(dat)
View(dat)

# remove weird values and create new columns for 95% threshold and select latest available year for each country
clean_dat <- dat %>% 
  filter(SP.REG.BRTH.ZS > 0) %>% 
  drop_na(SP.REG.BRTH.ZS) %>% 
  mutate(threshold = ifelse(SP.REG.BRTH.ZS > 95, "pass", "fail")) %>% 
  group_by(iso2c) %>% 
  filter(year == max(year))

summary(clean_dat)
View(clean_dat)
#remove regional averages
data <- clean_dat[-c(1:4,15,54,55,127,141:143,159:163,169,181:183,188:202,204,206:208,210,211), ]
summary(data)
View(data)
n_distinct(data$iso2c) #171 countries

# plotting time!   
ggplot(data, aes(x = SP.POP.TOTL, y = SP.REG.BRTH.ZS, color = threshold, label = country)) + 
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("pass" = "#3C9AB2", "fail" = "#F22300")) +
  geom_hline(yintercept = 95, linetype = "longdash", color = "darkgrey") +
  labs(
    title = "Birth registration rates around the world, by population size",
    subtitle = "(latest available year, 2009-18)",
    caption = "Source: UNICEF",
    x = "Population",
    y = "Rate of children under age 5 whose births are registered (%)") +
  geom_point() +
  geom_text_repel(aes(label=ifelse(SP.POP.TOTL>110000000,as.character(country),'')))+
  annotate(
    geom = "text",
    x = 1000000000 ,
    y = 93, 
    label = "95% threshold",
    hjust = "left") +
  theme_tufte(base_family = "GillSans") +
  theme(legend.position = "none")+ 
  ggsave("birth_registration_rates.png")