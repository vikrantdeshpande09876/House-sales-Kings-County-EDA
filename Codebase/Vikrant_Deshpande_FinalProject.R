---
title: "Final Project"
author: "Vikrant Deshpande, Tanvi Kolhatkar, Saishree Godbole"
date: "04/13/2022"
output: html_document
---
  
  
  # INTRODUCTION
  In this project, we look at a dataset of home selling-prices in King County, Washington, between May 2014 and May 2015. The dataset contains 21,613 homes and 21 descriptive features. We will build a multiple regression model with the following variables: y is the outcome variable, and there are two explanatory variables:

1. A numerical explanatory variable x1: sqft living, which refers to the size of a house in square feet of living space. It is worth noting that 1 square foot equals 0.09 square meters.

2. A categorical explanatory variable x2: housing condition, a five-level categorical variable with 1 indicating "poor" and 5 indicating "outstanding."

3.  a. Check if the relationship can be fitted by a linear model and explain any differences.
    b. Analyze the trends of life expectancy over time for individual continents and respective countries.
    c. Check if any other factors affect the house-price apart from the Square Footage, Condition, Bathrooms,and Grade.



require(rio)
require(tidyverse)
require(broom)
require(directlabels)
require(gridExtra)
require(skimr)
require(corrplot)




house_prices <- import('kc_house_data.csv')
glimpse(house_prices)
colnames(house_prices)

# DUPLICATE HOUSES IDENTIFIED: Resold 2 times?
house_prices %>%
  group_by(id) %>%
  summarize(cnt=n(), min_prices=min(price), max_price=max(price)) %>%
  filter(cnt>1) %>%
  View()

# REMOVING DIRTY DATA: HOUSES WITH 0 BEDROOMS AND 0 BATHROOMS, 1 HOUSE WITH 33 BEDROOMS BUT SMALL AREA
house_prices <- house_prices %>%
  filter((bedrooms!=0) & (bathrooms!=0)) %>%
  filter(bedrooms!=33)


house_prices %>%
  select(price, sqft_living, condition) %>%
  skim()


house_prices %>%
  group_by(yr_renovated) %>%
  summarize(c=n())


#INFER RELEVANT FEATURES FROM CORRELATION PLOT: sqft_living
NUMERIC_COLS <- house_prices %>%
  lapply(is.numeric) %>%
  unlist() %>%
  names() %>%
  setdiff(c('id', 'date', 'yr_built', 'zipcode', 'view', 'yr_renovated', 'waterfront'))

house_prices %>%
  select(NUMERIC_COLS) %>%
  cor() %>%
  corrplot(
    method='color', title='Correlation Matrix for all numerical features', outline=T, addgrid.col='darkgray',
    order='hclust', addrect=2, rect.lwd=2.5, tl.col='black',
    addCoef.col="white", number.digits=2, number.cex=0.75
    )



# UNIVARIATE PLOTS
# Histogram of price:
house_prices %>%
  ggplot(aes(x=price)) +
  geom_histogram(color="white") +
  labs(x="price (USD)", title="House price")

# Histogram of sqft_living:
ggplot(house_prices, aes(x=sqft_living)) +
  geom_histogram(color="white") +
  labs(x="living space (square feet)", title="House size")


# Barplot of condition:
ggplot(house_prices, aes(x=condition)) +
  geom_bar() +
  labs(x="condition", title="House condition")







# Univariate plots show right skewed distribution, so performing Box-cox log transformation.
house_prices <- house_prices %>%
  mutate(
    log10_price=log(price),
    log10_size=log(sqft_living)
  )



house_prices %>% 
  select(price=price) %>%
  mutate(type='I. Raw Data') %>%
  union_all(house_prices %>% 
              select(price=log10_price) %>%
              mutate(type='II. Transformed Data') 
        ) %>%
  ggplot(mapping=aes(x=price, fill=type)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~type, scales='free') +
  labs(title='Histogram for House-prices', x='Price', y='Count') +
  theme_bw()
  

house_prices %>% 
  select(size=sqft_living) %>%
  mutate(type='I. Raw Data') %>%
  union_all(house_prices %>% 
              select(size=log10_size) %>%
              mutate(type='II. Transformed Data')
  ) %>%
  ggplot(mapping=aes(x=size, fill=type)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~type, scales='free') +
  labs(title='Histogram for House-sizes', x='Size', y='Count') +
  theme_bw()






# MULTIVARIATE PLOTS TO IDENTIFY RELATIONSHIPS
house_prices %>%
  mutate(house_condition=as.factor(condition)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_condition), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_condition) +
  labs(title="House prices", y="Transformed price", x="Transformed size") +
  theme_bw()




