---
title: 'Final Project'
author: 'Vikrant Deshpande, Tanvi Kolhatkar, Saishree Godbole'
date: '04/13/2022'
output: html_document
---
  
  
  # INTRODUCTION
  In this project, we look at a dataset of home selling-prices in King County, Washington, between May 2014 and May 2015. The dataset contains 21,613 homes and 21 descriptive features. We will build a multiple regression model with the following variables: y is the outcome variable, and there are two explanatory variables:

    a. Analyze the relationships between features of houses and house-prices, for example- `sqft living`, refers to the square feet of living space, and `housing condition` is a five-level rating with 1 indicating 'poor' and 5 indicating 'outstanding.'.
    b. Check if any other factors affect the house-price apart from the Square Footage, Condition, Bathrooms,and Grade.
    c. Identify if the relationship between features and home-price can be fitted by a linear model and explain any differences.



require(sf)
require(rio)
require(tidyverse)
require(broom)
require(gridExtra)
require(skimr)
require(corrplot)
require(ggmap)
require(ggmosaic)
require(leaps)
require(plot.matrix)


house_prices <- import('kc_house_data.csv')
glimpse(house_prices)
colnames(house_prices)

# DUPLICATE HOUSES IDENTIFIED: Resold 2 times?
house_prices %>%
  group_by(id) %>%
  summarize(cnt=n(), min_prices=min(price), max_price=max(price)) %>%
  filter(cnt>1)

# REMOVING DIRTY DATA: HOUSES WITH 0 BEDROOMS AND 0 BATHROOMS, 1 HOUSE WITH 33 BEDROOMS BUT SMALL AREA
house_prices <- house_prices %>%
  filter((bedrooms!=0) & (bathrooms!=0)) %>%
  filter(bedrooms!=33)




house_prices %>%
  group_by(yr_renovated) %>%
  summarize(c=n())



# UNIVARIATE PLOTS
# Histogram of price:
house_prices %>%
  ggplot(aes(x=price)) +
  geom_histogram(color='white') +
  labs(x='price (USD)', title='House price')

# Histogram of sqft_living:
ggplot(house_prices, aes(x=sqft_living)) +
  geom_histogram(color='white') +
  labs(x='living space (square feet)', title='House size')


# Histogram of sqft_living15:
ggplot(house_prices, aes(x=log10(sqft_living15))) +
  geom_histogram(color='white') +
  labs(x='living space (square feet)', title='Neighboring 15 house-sizes')


# Barplot of bedrooms:
ggplot(house_prices, aes(x=bedrooms)) +
  geom_bar() +
  labs(x='Bedrooms', title='Number of bedrooms in house')


# Barplot of bathrooms:
ggplot(house_prices, aes(x=bathrooms)) +
  geom_bar() +
  labs(x='bathrooms', title='HouseNumber of bathrooms in house')



scale_down_values <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}



# Univariate plots show right skewed distribution, so performing Box-cox log transformation.
house_prices <- house_prices %>%
  mutate(
    log10_price=log(price),
    log10_size=log(sqft_living),
    log10_neighbour_sizes=log(sqft_living15),
    scaled_lat=scale_down_values(house_prices$lat)
  )



house_prices %>% 
  dplyr::select(price=price) %>%
  mutate(type='I. Raw Data') %>%
  union_all(house_prices %>% 
              dplyr::select(price=log10_price) %>%
              mutate(type='II. Transformed Data') 
        ) %>%
  ggplot(mapping=aes(x=price, fill=type)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~type, scales='free') +
  labs(title='Histogram for House-prices', x='Price', y='Count') +
  theme_bw()
  

house_prices %>% 
  dplyr::select(size=sqft_living) %>%
  mutate(type='I. Raw Data') %>%
  union_all(house_prices %>% 
              dplyr::select(size=log10_size) %>%
              mutate(type='II. Transformed Data')
  ) %>%
  ggplot(mapping=aes(x=size, fill=type)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~type, scales='free') +
  labs(title='Histogram for House-sizes', x='Size', y='Count') +
  theme_bw()



# DISTRIBUTION OF BATHROOMS AND BEDROOMS IN OUR ENTIRE DATASET
house_prices %>%
  mutate(
    floor=as.factor(floors), 
    bedroom=as.factor(bedrooms),
    bathroom=as.factor(as.integer(bathrooms)),
    cat_price=ifelse(log10_price<=13, '0', ifelse(log10_price<=14, '1', '2'))
  ) %>%
  ggplot() +
  geom_mosaic(aes(x=product(bathroom), fill=bedroom)) +
  labs(
    title='Mosaic Plot for Bedrooms vs Bathrooms',
    subtitle='Most have 2 bedrooms and 2 bathrooms',
    y='Bathrooms', x='Bedrooms'
    )




# INFER RELEVANT FEATURES FROM CORRELATION PLOT: sqft_living
NUMERIC_COLS <- house_prices %>%
  lapply(is.numeric) %>%
  unlist() %>%
  names() %>%
  setdiff(c('id', 'date', 'yr_built', 'zipcode', 'view', 'yr_renovated', 'waterfront', 'price', 'sqft_living', 'sqft_living15', 'lat')) %>%
  sort()

house_prices %>%
  dplyr::select(NUMERIC_COLS) %>%
  cor() %>%
  corrplot(
    method='color', title='Correlation Matrix for all numerical features', outline=T, addgrid.col='darkgray',
    order='hclust', addrect=2, rect.lwd=2.5, tl.col='black', tl.cex=0.75,
    addCoef.col='white', number.digits=2, number.cex=0.75, mar=c(0,0,1,0)
  )


# POTENTIAL MULTICOLLINEARITY IN FEATURES
# bedrooms <-> bathrooms
# bathrooms <-> log10_size
# log10_sqft_living15 <-> grade
# log10_sqft_living15 <-> sqft_above
# log10_sqft_living15 <-> log10_size
# 




# MULTIVARIATE PLOTS TO IDENTIFY RELATIONSHIPS
house_prices %>%
  mutate(house_condition=as.factor(condition)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_condition), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_condition) +
  labs(
    title='House prices faceted by Condition',
    subtitle='Not much interaction observed',
    y='Transformed price', x='Transformed size'
    ) +
  theme_bw()





house_prices %>%
  mutate(house_bedrooms=as.factor(bedrooms)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_bedrooms), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_bedrooms) +
  labs(
    title='House prices faceted by Number of Bedrooms',
    subtitle='Some interesting trends observed',
    y='Transformed price', x='Transformed size'
  ) +
  theme_bw()



house_prices %>%
  mutate(house_grade=as.factor(grade)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_grade), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_grade) +
  labs(
    title='House prices faceted by Grade',
    subtitle='Some interesting trends observed',
    y='Transformed price', x='Transformed size'
    ) +
  theme_bw()





house_prices %>%
  mutate(house_bathrooms=as.factor(bathrooms)) %>%
  ggplot(mapping=aes(x=log10_size, y=log10_price)) +
  geom_jitter(aes(color=house_bathrooms), size=2, alpha=0.5) +
  geom_smooth(method='lm', se=FALSE, size=1, alpha=0.5, color='green') +
  facet_wrap(~house_bathrooms) +
  labs(
    title='House prices faceted by Number of Bathrooms',
    subtitle='Some interesting trends observed',
    y='Transformed price', x='Transformed size'
  ) +
  theme_bw()


# CHECK IF LOCATION PLAYS A ROLE IN DETERMINING PRICES
register_google(key='AIzaSyDf9y_5spYHDjQ-jOoDSSXeYIxmQWInBqU', write=T)
get_googlemap(center='King County, Washington', zoom=9, maptype='hybrid', source='google') %>%
  ggmap() +
  geom_point(data=house_prices, aes(x=long, y=lat, color=log10_price), alpha=0.2) +
  scale_color_gradient('Prices', low='red', high='green') +
  theme_bw() +
  labs(
    title='Map of King County house prices',
    subtitle='Central, West, and North regions show high priced homes',
    y='Latitude', x='Longitude'
    )




# CHOOSING THE TOP 5 MOST RELEVANT FEATURES FROM CORRELATION HEATMAP
house_prices %>%
  dplyr::select(-c(log10_price, price, date, sqft_living, sqft_living15)) %>%
  map_dbl(cor, y=house_prices$log10_price) %>%
  sort(decreasing=TRUE) %>%
  .[1:6] %>%
  names


# CREATING A LINEAR REGRESSIION MODEL AND PERFORMING ANOVA TEST FOR EVAL
raw_model <- lm(
  data=house_prices, 
  formula=log10_price ~ (log10_size+grade+bathrooms+bedrooms+scaled_lat)^2
  )


summary(raw_model)


model_coefs <- tidy(raw_model, conf.int=TRUE)
model_coefs[-1, ] %>%
  ggplot(aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high)) +
  geom_point(size=2) +
  geom_errorbarh(size=1) +
  geom_vline(xintercept=0, size=1) +
  theme_bw() +
  labs(title='Linear Model coefficient estimates', subtitle='All interactions between predictors considered', x='Estimate', y='Predictor variable')


model <- lm(data=house_prices, formula=log10_price ~ 
              (log10_size+grade+bathrooms+bedrooms+scaled_lat)+
              (log10_size*bathrooms + log10_size*scaled_lat + bedrooms*scaled_lat))
model_coefs <- tidy(model, conf.int=TRUE)
model_coefs[-1, ] %>%
  ggplot(aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high)) +
  geom_point(size=2) +
  geom_errorbarh(size=1) +
  geom_vline(xintercept=0, size=1) +
  theme_bw() +
  labs(
    title='Linear Model coefficient estimates',
    subtitle='Only important interactions between predictors considered',
    x='Estimate', y='Predictor variable'
    )














plot_forward_selection <- function(data, model_formula){
  forward_selection_models <- regsubsets(data=data, x=model_formula)
  features_selected <- summary(forward_selection_models)$which
  features_selected[features_selected==T] = 1
  features_selected %>%
    plot(
      col=c('red', 'green'), mar=c(0,0,1,0),
      axis.col=list(side=1, las=2), 
      main='Forward selection of Features \n(Chosen ones in green)',
      xlab='', ylab='Steps (Regression Models)'
    )
  return (forward_selection_models) 
}




forward_selection_metrics <- function(model_summaries){
  cp_metric_changes <- summary(model_summaries)$cp
  colnames <- c('cp_metric', 'features_chosen')
  cp_metric_changes_df <- create_new_df(colnames)
  cp_metric_changes_df <- rbind(cp_metric_changes_df, cbind(cp_metric_changes, 1:length(cp_metric_changes)))
  colnames(cp_metric_changes_df) <- colnames
  
  cp_metric_changes_df %>%
    ggplot() +
    geom_bar(mapping=aes(x=features_chosen, y=cp_metric, fill=cp_metric), stat='identity') +
    coord_flip() +
    scale_x_reverse() +
    labs(title='Reducing CP Metric indicates better fits', x='CP value', y='Number of Features chosen') +
    scale_fill_continuous(type='gradient') +
    theme_bw()
}


forward_selection_models <- plot_forward_selection(data=house_prices, model_formula=model$terms)
forward_selection_metrics(forward_selection_models)
  


summary(model)



augment(model, type.residuals='pearson') %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point(alpha=0.2, color='green') +
  geom_smooth(method='lm', size=1, alpha=0.6) +
  labs(
    title='Residual plot for Linear model',
    subtitle=paste0(length(model$coefficients)-1,' features'),
    x='Fitted values', y='Residuals'
  ) +
  scale_color_gradient('Prices', low='lightgreen', high='darkgreen') +
  theme_bw()


