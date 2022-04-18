---
title: "Final Project"
author: "Vikrant Deshpande, Tanvi Kolhatkar, Saishree Godbole"
date: "04/13/2022"
output: html_document
---
  
  
  # INTRODUCTION
  In this project, we look at a dataset of home selling-prices in King County, Washington, between May 2014 and May 2015. The dataset contains 21,613 homes and 21 descriptive features. We will build a multiple regression model with the following variables: y is the outcome variable, and there are two explanatory variables:

    a. Analyze the relationships between features of houses and house-prices, for example- `sqft living`, refers to the square feet of living space, and `housing condition` is a five-level rating with 1 indicating "poor" and 5 indicating "outstanding.".
    b. Check if any other factors affect the house-price apart from the Square Footage, Condition, Bathrooms,and Grade.
    c. Identify if the relationship between features and home-price can be fitted by a linear model and explain any differences.



require(sf)
require(rio)
require(tidyverse)
require(broom)
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
ggplot(house_prices, aes(x=bedrooms)) +
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






#INFER RELEVANT FEATURES FROM CORRELATION PLOT: sqft_living
NUMERIC_COLS <- house_prices %>%
  lapply(is.numeric) %>%
  unlist() %>%
  names() %>%
  setdiff(c('id', 'date', 'yr_built', 'zipcode', 'view', 'yr_renovated', 'waterfront', 'price', 'sqft_living'))

house_prices %>%
  select(NUMERIC_COLS) %>%
  cor() %>%
  corrplot(
    method='color', title='Correlation Matrix for all numerical features', outline=T, addgrid.col='darkgray',
    order='hclust', addrect=2, rect.lwd=2.5, tl.col='black', tl.cex = 0.75,addtextlabel="ld",
    addCoef.col="white", number.digits=2, number.cex=0.75
  )








# MULTIVARIATE PLOTS TO IDENTIFY RELATIONSHIPS
house_prices %>%
  mutate(house_condition=as.factor(condition)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_condition), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_condition) +
  labs(title="House prices faceted by Condition", y="Transformed price", x="Transformed size") +
  theme_bw()




house_prices %>%
  mutate(house_grade=as.factor(grade)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_grade), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_grade) +
  labs(title="House prices faceted by Grade", y="Transformed price", x="Transformed size") +
  theme_bw()


house_prices %>%
  mutate(house_bedrooms=as.factor(bedrooms)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_bedrooms), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_bedrooms) +
  labs(title="House prices faceted by Bedrooms", y="Transformed price", x="Transformed size") +
  theme_bw()

house_prices %>%
  mutate(house_bathrooms=as.factor(bathrooms)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_bathrooms), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_bathrooms) +
  labs(title="House prices faceted by Bathrooms", y="Transformed price", x="Transformed size") +
  theme_bw()

# house_prices %>%
#   mutate(house_bedrooms=as.factor(bedrooms)) %>%
#   ggplot(mapping=aes(x=log10_size, y=log10_price)) +
#   geom_point(alpha = 0.05) +
#   geom_smooth(method = "lm", se = FALSE,aes(color=house_bedrooms)) +
#   labs(y = "log10 price",
#        x = "log10 size",
#        title = "House prices in Seattle")

my_sf <- house_prices %>%
  st_as_sf(coords=c('long','lat')) %>%
  st_set_crs(4326)

my_sf %>%
  ggplot() +
  geom_sf(aes(color=log10_price)) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) +
  coord_sf()


# CHOOSING THE TOP 5 MOST RELEVANT FEATURES FROM CORRELATION HEATMAP
house_prices %>%
  select(-c(log10_price,price,date,sqft_living, sqft_living15)) %>%
  map_dbl(cor, y = house_prices$log10_price) %>%
  sort(decreasing = TRUE) %>%
  .[1:6] %>%
  names %>%
  house_prices[.]


# CREATING A LINEAR REGRESSIION MODEL AND PERFORMING ANOVA TEST FOR EVAL
price_interaction <- lm(log10_price ~ (log10_size+grade+bathrooms+bedrooms)^2,
                        data = house_prices)


summary(price_interaction)


anova(price_interaction)
