# sds315_h8

```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(mosaic)
```
## Problem 1

#### A.

```{r, warning=FALSE, message=FALSE, echo=FALSE}

#load data set
creatinine <- read_csv("creatinine.csv")

#create model
model_creatinine = lm(creatclear ~ age, data=creatinine)
coef(model_creatinine)

#store interecept and age values 
creatinine_intercept = 147.8129158
creatinine_slope = -0.6198159

#calculate the predicted using y = B0 + B1(xi)
expected_55 = creatinine_intercept + creatinine_slope*55
expected_55

```


#### C)

```{r, warning=FALSE, message=FALSE, echo=FALSE}


#calculate what we would expect for a 40 year old 
expected_40 = creatinine_intercept + creatinine_slope*40 # 123.0203 

#calculate what we would expect for a 60 year old 
expected_60 = creatinine_intercept + creatinine_slope*60 # 110.624 

#calculate residuals 
residual_40 =  135 - expected_40 # 11.97972
residual_60 = 112 - expected_60 # 1.376038


```


## Problem 2

##### Growth rate: Italy 

```{r echo=FALSE, message=FALSE, warning=FALSE}

#load data set 
covid = read_csv("covid.csv")

#filter to only include italy
covid_italy <- covid |>
  filter(country=="Italy")

#growth rate 
#take log of deaths 
covid_italy$log_deaths_italy= log(covid_italy$deaths)

#create linear model 
italy_model= lm(log_deaths_italy ~ days_since_first_death, data=covid_italy)
coef(italy_model) #slope = 0.183218 

#bootstrap
bootstrap_italy_gr =  do(10000)*coef(lm(log_deaths_italy ~ days_since_first_death, resample(covid_italy)))
#ci
confint(bootstrap_italy_gr, level=0.95)


```


##### Doubling time: Italy 

```{r echo=FALSE, message=FALSE, warning=FALSE}

#store slope
growth_rate_italy= (0.183218 *100)

#doubling time 
DT_italy= 70/(growth_rate_italy)

#boostrap dt 
bootstrap_italy_dt = do(10000)* (70 / (coef(lm(log_deaths_italy ~ days_since_first_death, resample(covid_italy)))[2]*100))

#ci
confint(bootstrap_italy_dt, level=0.95)


```

##### Growth rate: Spain

```{r echo=FALSE, message=FALSE, warning=FALSE}

#load data set 
covid = read_csv("covid.csv")

#filter to only include spain
covid_spain <- covid |>
  filter(country=="Spain")

#growth rate 
#take log of deaths 
covid_spain$log_deaths_spain = log(covid_spain$deaths)

#create linear model 
spain_model= lm(log_deaths_spain ~ days_since_first_death, data=covid_spain)
coef(spain_model) #slope = 0.2762447

bootstrap_spain_gr =  do(10000)*coef(lm(log_deaths_spain ~ days_since_first_death, resample(covid_spain)))
#ci
confint(bootstrap_spain_gr, level=0.95)
```

##### Doubling time: Spain 

```{r echo=FALSE, message=FALSE, warning=FALSE}

#store slope
growth_rate_spain= (0.2762447*100)

#doubling time 
DT_spain= 70/(growth_rate_spain)

#boostrap dt 
bootstrap_spain_dt = do(10000)* (70 / (coef(lm(log_deaths_spain ~ days_since_first_death, resample(covid_spain)))[2]*100))

#ci
confint(bootstrap_spain_dt, level=0.95)
```

##### Line Graph 

```{r echo=FALSE, message=FALSE, warning=FALSE}

#create plot 
ggplot(data=covid)+
  geom_line(aes(x=days_since_first_death, y=deaths, color=country), size=0.8)+
  scale_color_manual(
    values = c("Italy" = "olivedrab", "Spain" = "plum")  # red for Italy, blue for Spain
  ) +
  labs(
    title="Daily COVID Deaths Over Time",
    y= "Days Since First Death",
    x= "Deaths"
  )+
  theme_minimal()


```
## Problem 3

```{r echo=FALSE, message=FALSE, warning=FALSE}

#load data set
milk <- read_csv("milk.csv")

#take log 
milk$log_sales= log(milk$sales)
milk$log_price = log(milk$price)

#calculate power law 
#create model
milk_model= lm(log_sales ~ log_price, data=milk)
coef(milk_model)

#store elasticity
milk_elasticity = -1.618578 

#bootstrap
milk_bootstrap= do(10000)*coef(lm(log_sales ~ log_price, resample(milk)))

#ci 
confint(milk_bootstrap, level=0.95)

```
