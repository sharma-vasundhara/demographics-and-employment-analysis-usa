---
title: "demographics-and-employment-united-states-analysis"
author: "Vasundhara Sharma"
date: "2/16/2022"
output: github_document
---

library(tidyverse)
library(ggrepel)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(ggalluvial)



# reading the data file
df_cps <- read.csv('CPSData.csv', na.string = '')



# checking column types and dimensions of data frame
str(df_cps)



# checking for NA values in each column 
df_cps %>% select(everything()) %>% summarise_all(list(~sum(is.na(.))))




# checking summary for Age column 
summary(df_cps$Age)



# checking summary for People In Household column
summary(df_cps$PeopleInHousehold)




# Gender Analysis
{rwarning = F}
# checking the gender percentage in the dataset
df_cps %>% group_by(Sex) %>% summarise(Total.Count = n())


## Age Groups
# creating age groups and categorizing data
df_cps_age_group <- df_cps %>% 
  select(Sex, Age) %>% 
  mutate(Age.Group = case_when(Age >= 0  & Age <= 10 ~ '0-10',
                                        Age >= 11  & Age <= 20 ~ '11-20',
                                        Age >= 21  & Age <= 30 ~ '21-30',
                                        Age >= 31  & Age <= 40 ~ '31-40',
                                        Age >= 41  & Age <= 50 ~ '41-50',
                                        Age >= 51  & Age <= 60 ~ '51-60',
                                        Age >= 61  & Age <= 70 ~ '61-70',
                                        Age >= 71  & Age <= 80 ~ '71-80',
                                        Age >= 81  & Age <= 90 ~ '81-90',
                                        Age >= 91  & Age <= 100 ~ '91-100')) %>%
  group_by(Sex, Age.Group) %>%
  summarise(Total.Count = n())

df_cps_age_group



# plotting female and male population with their age category
df_cps %>% 
  ggplot(aes(x = Age, fill = Sex)) +
  geom_histogram(color = "#e9ecef", alpha = 0.8, position = 'identity', 
                binwidth = 10, mapping = aes(x = Age), boundary = 0) +
  scale_fill_manual(values = c('#450d54', '#21918c')) +
  theme_ipsum() + 
  xlab('Age') + xlim(0, 100) + 
  ylab('Number of People') + ylim(0, 10000) +
  scale_x_continuous(breaks=seq(0, 100, 10))


## Education Status

# calculating number of females and males and their education levels
df_cps_education <- df_cps %>% 
  group_by(Sex, Education) %>%
  summarise(Total.Count = n()) 
df_cps_education



# plotting gender and their education level
ggplot(df_cps_education, 
       aes(fill = Sex, 
           y = factor(Education, 
                      levels = rev(c('NA', 'No high school diploma',
                                     'High school', 'Associate degree',
                                     'Some college, no degree', 
                                     'Bachelor\'s degree', 'Master\'s degree',
                                     'Professional degree', 'Doctorate degree'))), 
           x = Total.Count)) +
  geom_bar(width = 0.8, 
           position = position_dodge(0.8), 
           stat = "identity") +
  scale_fill_manual(values = c('#450d54', '#21918c')) + 
  theme_ipsum() +
  xlab('Number of People') + ylab('Education') + 
  ggtitle('Gender & Education')


## Employment Status

# checking number of males and females with different employment statuses
df_cps_employment <- df_cps %>% 
  group_by(Sex, EmploymentStatus) %>%
  summarise(Total.Count = n()) 
df_cps_employment




# plotting gender and employment status
ggplot(df_cps_employment, 
       aes(fill = Sex, 
           y = EmploymentStatus, 
           x = Total.Count)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('#450d54', '#21918c')) +
  theme_ipsum_es() +
  xlab('Number of People') + ylab('Employment Status') +
  ggtitle('Gender & Employment Status')


## Gender and Industry

# calculating male and female population in each industry
df_cps_industry <- df_cps %>% 
  group_by(Sex, Industry) %>%
  summarise(Total.Count = n()) 
head(df_cps_industry)



# plotting male and female population in different industries
ggplot(df_cps_industry, 
       aes(fill = Sex, y = Industry, x = Total.Count)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c('#450d54', '#21918c')) +
  theme_ipsum_es() +
  xlab('Number of People') + ylab('Type of Industry') +
  ggtitle('Gender & Industry') 


## Gender and Marital Status

# calculating percentage of males / females with different marital status
df_cps_marriage_f <- df_cps %>% 
  filter(Sex == 'Female') %>% 
  group_by(Married) %>% 
  summarise(Total.Count = n()) %>% 
  mutate(Percentage = paste0(round(((Total.Count / sum(Total.Count)) * 100), 2), "%"))

df_cps_marriage_m <- df_cps %>% 
  filter(Sex == 'Male') %>% 
  group_by(Married) %>% 
  summarise(Total.Count = n()) %>% 
  mutate(Percentage = paste0(round(((Total.Count / sum(Total.Count)) * 100), 2), "%"))



head(df_cps_marriage_f)
head(df_cps_marriage_m)



pie(df_cps_marriage_f$Total.Count,
    main = 'Females Married Status',
    labels = df_cps_marriage_f$Percentage, border="white", 
    col = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))
legend('topright', df_cps_marriage_f$Married, cex = 0.8, fill = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))


pie(df_cps_marriage_m$Total.Count,
    main = 'Males Married Status',
    labels = df_cps_marriage_m$Percentage, border="white", 
    col = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))
legend('topright', df_cps_marriage_m$Married, cex = 0.8, fill = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))




# Race Analysis

# checking for unique values of Race
unique(df_cps$Race)

## Race and Age

# plotting age data for different races
df_cps %>%
 ggplot( aes(x=Race, y=Age, fill=Race)) + 
    geom_boxplot() +
    xlab("class") +
    theme(legend.position="none") +
    xlab('Race') + ylab('Age') +
  scale_fill_manual(values = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961')) + theme_classic()


## Population Division

df_cps_race <- df_cps %>% 
  group_by(Race) %>% 
  summarise(Total.Count = n()) %>% 
  mutate(Percentage = paste0(round(((Total.Count / sum(Total.Count)) * 100), 2), "%")) %>%
  filter(!Race %in% c('American Indian', 'Pacific Islander'))

df_cps_race  %>% 
  arrange(desc(Total.Count))



# plotting population divsion
pie(df_cps_race$Total.Count,
    main = 'Population Division',
    labels = df_cps_race$Percentage, border="white", 
    col = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))
legend('topright', df_cps_race$Race, cex = 0.8, fill = c('#fde725', '#5ec962', '#21918c', '#3b528b', '#440154', '#fc8961'))



## Citizenship and Race

df_cps %>% group_by(Race, Citizenship) %>% summarise(Total.Count = n())




p1 <- ggplot(df_cps %>% 
         group_by(Race, Citizenship) %>% 
         summarise(Total.Count = n()),
       aes(fill = Citizenship,
           y = Total.Count,
           x = Race)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = rev(c('#fc8961', '#b73779', '#51127c'))) +
  xlab('Race') +
  ylab('Total Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_light()

p2 <- ggplot(df_cps %>% 
         group_by(Race, Citizenship) %>% 
         summarise(Total.Count = n()) %>% 
         filter(!Race %in% c('White')),
       aes(fill = Citizenship,
           y = Total.Count,
           x = Race)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = rev(c('#fc8961', '#b73779', '#51127c'))) +
  xlab('Race') +
  ylab('Total Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_light()

grid.arrange(p1, p2, nrow = 2)


## Education and Race

# population of each race
df_cps_race_pop <- df_cps %>% group_by(Race) %>% summarise(Total.Count.Race = n())

df_cps_race_ed <- df_cps %>%
  group_by(Race, Education) %>%
  summarise(Total.Count.Race.Education = n()) %>%
  filter(!Race %in% c('American Indian', 'Pacific Islander', 'White')) %>%
  filter(!Education %in% c('Doctorate degree', 'Professional degree'))


df_cps_race_ed <- merge(df_cps_race_ed, df_cps_race_pop, by = 'Race')

df_cps_race_ed <- df_cps_race_ed %>% mutate(Percentage = (Total.Count.Race.Education / Total.Count.Race)*100)

df_cps_race_ed



ggplot(df_cps_race_ed,
       aes(axis1 = Race, axis2 = Education, y = Percentage)) +
  geom_alluvium(aes(fill = Race)) +
  geom_stratum(width = 1/2.7) +
  geom_text(stat = 'stratum', 
            aes(label = after_stat(stratum)), 
            size = 3) +
  scale_x_discrete(limits = c("Race", "Education"), 
                   expand = c(0.15, 0.05)) +
  theme_ipsum() +
  scale_fill_manual(values = c('#fde725', '#21918c', '#440154'))

## Employment Status and Race

df_cps_race_emp <- df_cps %>% 
  group_by(Race, EmploymentStatus) %>% 
  summarise(Total.Count.EmpStat = n())

df_cps_race_emp <- merge(df_cps_race_emp, df_cps_race_pop, by = 'Race')

df_cps_race_emp <- df_cps_race_emp %>% mutate(Percentage = (Total.Count.EmpStat / Total.Count.Race)*100)

df_cps_race_emp



ggplot(df_cps_race_emp, 
       aes(fill = Race, 
           y = Percentage, 
           x = EmploymentStatus)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('#fc8961', '#fde725', '#5ec962', '#21918c', '#3b528b', '#440154')) +
  theme_ipsum_es() +
  xlab('Employment Status') + ylab('Percentage (Count / Total Count of Particular Race)') +
  ggtitle('Race & Employment Status') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


# Analysing Industry and Education columns

unique(df_cps$Industry)



df_cps_ed_ind <- df_cps %>% group_by(Education, Industry) %>% summarise(Total.Count = n())

education_level <- rev(c('NA', 'No high school diploma',
                                     'High school', 'Associate degree',
                                     'Some college, no degree', 
                                     'Bachelor\'s degree', 'Master\'s degree',
                                     'Professional degree', 'Doctorate degree'))
df_cps_ed_ind


ggplot(df_cps_ed_ind, 
       aes(fill = factor(Education, levels = education_level), y = Total.Count, x = Industry)) + 
  geom_bar(position = 'fill', stat = 'identity') +
#  scale_fill_manual(values = c('#fde725', '#addc30', '#5ec962', '#28ae80', '#21918c', '#2c728e', '#3b528b', '#472d7b', '#440154')) +
  scale_fill_brewer(palette = 'Accent') +
  theme_ipsum() +
  ylab('Total Count') +
  ggtitle('Industry and Education') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  guides(fill=guide_legend(title="Education Level"))


