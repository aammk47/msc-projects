#Loading packages

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

#Dataset loading

covid_database_main <- read_excel("owid-covid-data.xlsx", 
                                  col_types = c("text", "text", "text", 
                                                                  "date", "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "text", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric"))

#Dataset Cleaning
#Filtering dataset down to UK and Germany

covid_database_eur <- filter(covid_database_main, (location == "United Kingdom" |
                                location == "Germany"))

#To filter data based on dates, the date data type needs to be converted from character to date
#Converting date from character type to date type

class(covid_database_eur$date)
covid_database_eur <- covid_database_eur %>% filter(covid_database_eur$date > "2021-01-10")

#Creating a cleaned working dataframe for each country
#UK Dataframe

covid_dataframe_uk_main <- subset(covid_database_eur, location == "United Kingdom")
covid_dataframe_uk_working <- select(covid_dataframe_uk_main, date,
                                     new_cases_smoothed_per_million,
                                     new_deaths_smoothed_per_million,
                                     new_vaccinations,positive_rate,
                                     new_tests_smoothed_per_thousand, stringency_index,
                                     aged_65_older, male_smokers, female_smokers)

covid_dataframe_uk_working_2021 <- filter(covid_dataframe_uk_working, date < '2022-01-01', date > '2021-01-10')
covid_dataframe_uk_working_2022 <- filter(covid_dataframe_uk_working, date >= '2022-01-01')

#Germany Dataframe

covid_dataframe_ger_main <- subset(covid_database_eur, location == "Germany")
covid_dataframe_ger_working <- select(covid_dataframe_ger_main, date,
                                     new_cases_smoothed_per_million,
                                     new_deaths_smoothed_per_million,
                                     new_vaccinations,positive_rate,
                                     new_tests_smoothed_per_thousand, stringency_index,
                                     aged_65_older, male_smokers, female_smokers)

covid_dataframe_ger_working_2021 <- filter(covid_dataframe_ger_working, date < '2022-01-01', date > '2021-01-10')
covid_dataframe_ger_working_2022 <- filter(covid_dataframe_ger_working, date >= '2022-01-01')

#Summative tables for both countries for 2021 and 2022

summary(covid_dataframe_uk_working_2021)
summary(covid_dataframe_uk_working_2022)
summary(covid_dataframe_ger_working_2021)
summary(covid_dataframe_ger_working_2022)

#Plotting infection and death rate trends

colours <- c("UK" = "blue", "Germany" = "red")

#Infection Rate - 2021

ggplot() + geom_line(data = covid_dataframe_uk_working_2021, aes(x = date, y = new_cases_smoothed_per_million, colour = "UK")) +
  geom_line(data = covid_dataframe_ger_working_2021, aes(x = date, y = new_cases_smoothed_per_million, colour = "Germany")) +
  labs(x = "Time", y = "New Cases Smoothed Per Million", title = "COVID-19 Infection Rates (UK vs Germany)",
       subtitle = '2021', colour = 'Legend') + 
  scale_colour_manual(values = colours) 

#Infection Rate - 2022

ggplot() + geom_line(data = covid_dataframe_uk_working_2022, aes(x = date, y = new_cases_smoothed_per_million, colour = "UK")) +
  geom_line(data = covid_dataframe_ger_working_2022, aes(x = date, y = new_cases_smoothed_per_million, colour = "Germany")) +
  labs(x = "Time", y = "New Cases Smoothed Per Million", title = "COVID-19 Infection Rates (UK vs Germany)",
       subtitle = '2022', colour = 'Legend') + 
  scale_colour_manual(values = colours)

#Death Rate - 2021

ggplot() + geom_line(data = covid_dataframe_uk_working_2021, aes(x = date, y = new_deaths_smoothed_per_million, colour = "UK")) +
  geom_line(data = covid_dataframe_ger_working_2021, aes(x = date, y = new_deaths_smoothed_per_million, colour = "Germany")) +
  labs(x = "Time", y = "New Deaths Smoothed Per Million", title = "COVID-19 Death Rates (UK vs Germany)",
       subtitle = '2021', colour = 'Legend') + 
  scale_colour_manual(values = colours)

#Death Rate - 2022

ggplot() + geom_line(data = covid_dataframe_uk_working_2022, aes(x = date, y = new_deaths_smoothed_per_million, colour = "UK")) +
  geom_line(data = covid_dataframe_ger_working_2022, aes(x = date, y = new_deaths_smoothed_per_million, colour = "Germany")) +
  labs(x = "Time", y = "New Deaths Smoothed Per Million", title = "COVID-19 Death Rates (UK vs Germany)",
       subtitle = '2022', colour = 'Legend') + 
  scale_colour_manual(values = colours)

#Multilinear regression to establish relationship between dependent and independent variables
#Loading Hmisc package

library(Hmisc)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Building correlation matrix
#UK correlation matrix
covid_dataframe_uk_cor <- select(covid_dataframe_uk_working, new_cases_smoothed_per_million,
                                 new_deaths_smoothed_per_million,
                                 new_vaccinations,positive_rate,
                                 new_tests_smoothed_per_thousand, stringency_index,
                                 aged_65_older, male_smokers, female_smokers)

corr_matrix_uk <- rcorr(as.matrix(covid_dataframe_uk_cor))
flattenCorrMatrix(corr_matrix_uk$r, corr_matrix_uk$P)

#Germany correlation matirx
covid_dataframe_ger_cor <- select(covid_dataframe_ger_working, new_cases_smoothed_per_million,
                                 new_deaths_smoothed_per_million,
                                 new_vaccinations,positive_rate,
                                 new_tests_smoothed_per_thousand, stringency_index,
                                 aged_65_older, male_smokers, female_smokers)

corr_matrix_ger <- rcorr(as.matrix(covid_dataframe_ger_cor))
flattenCorrMatrix(corr_matrix_ger$r, corr_matrix_ger$P)

#Regression model 1 - UK Infection Rate (2021/2022)

ncs_uk_model <- lm(new_cases_smoothed_per_million ~ new_vaccinations + positive_rate + new_tests_smoothed_per_thousand
                   + stringency_index, data = covid_dataframe_uk_cor)
summary(ncs_uk_model)

#Regression model 2 - UK Death Rate (2021/2022)

nds_uk_model <- lm(new_deaths_smoothed_per_million ~ new_vaccinations + positive_rate + new_tests_smoothed_per_thousand
                   + stringency_index, data = covid_dataframe_uk_cor)
summary(nds_uk_model)

#Regression model 3 - Germany Infection Rate (2021/2022)

ncs_ger_model <- lm(new_cases_smoothed_per_million ~ new_vaccinations + positive_rate + new_tests_smoothed_per_thousand +
                      stringency_index, data = covid_dataframe_ger_cor)
summary(ncs_ger_model)

#Regression model 4 - Germany Death Rate (2021/2022)

nds_ger_model <- lm(new_deaths_smoothed_per_million ~ new_vaccinations + positive_rate + new_tests_smoothed_per_thousand +
                      stringency_index, data = covid_dataframe_ger_cor)
summary(nds_ger_model)