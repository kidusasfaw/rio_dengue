library(tidyverse)


## Data Preparation
#The three datasets we'll be using are San Juan Dengue data, San Juan population data and San Juan climate data.

sj_dengue = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/San_Juan_Data.csv')
sj_station = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/SanJuanStationData.csv')
sj_pop = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/San_Juan_Population_Data.csv')

head(sj_dengue)
head(sj_station)
head(sj_pop)


#### San Juan Dengue data


ggplot(data = sj_dengue, mapping = aes(x = week_start_date, y = total_cases)) + geom_line()


#### San Juan Climate data


sj_station = sj_station %>% mutate(date = as.Date(paste(YYYY, MM, DD, sep = "-")))

ggplot(data = sj_station, mapping = aes(x = date, y = PRCP)) + geom_line()


# Clearly we have missing data for three days. We shall impute missing data using global average.


sj_station[sj_station$PRCP < 0,]$PRCP = mean(sj_station$PRCP, na.rm = TRUE)
head(sj_station)

# add week_start_date variable
sj_station = sj_station%>%mutate(week_start_date = as.Date(format(as.Date(date, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u"))
sj_dengue = sj_dengue %>% rename(old_week_start_date = week_start_date)
sj_dengue = sj_dengue %>% mutate(week_start_date = as.Date(format(as.Date(old_week_start_date, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u"))
# restrict to dates for which we have Dengue data
sj_station = sj_station[sj_station$week_start_date >= as.Date('1990-04-30'),]

# plot precipitation vs. week_start_date
weekly_total_precip = sj_station%>%group_by(week_start_date)%>%
summarize(total_precip = sum(PRCP, na.rm = T))
ggplot(data = weekly_total_precip, mapping = aes(x = week_start_date, y = total_precip)) + 
geom_line()

# plot average weekly temperature vs time. We've already made sjstation contain relevant dates
weekly_avg_temp = sj_station%>%group_by(week_start_date)%>%
summarize(avg_temp = mean(TAVG, na.rm = T))
ggplot(data = weekly_avg_temp, mapping = aes(x = week_start_date, y = avg_temp)) + 
geom_line()

# Let us append columns in our dengue data that include temperature and precipitation
full_dengue = inner_join(sj_dengue, weekly_avg_temp, by = "week_start_date")
full_dengue = inner_join(full_dengue, weekly_total_precip, by = "week_start_date")

#### Preliminary analysis
# Now that we have weekly climate and disease data, let's try to fit a linear regression with temperature and precipitation as predictors. Reason being that mosquitoes thrive when they lay their eggs in puddles etc.

#arima(full_dengue$total_cases,order=c(6,0,10), xreg = full_dengue[c('total_precip', 'avg_temp')])$aic
arma52 <- arima(full_dengue$total_cases,order=c(5,0,2), xreg = full_dengue[c('avg_temp', 'total_precip')])
arma52

arma52.resid = resid(arma52)
ggplot() +  
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = arma52.resid))

acf(arma52.resid, lag = 100)

params <- coef(arma52)
ar <- params[grep("^ar",names(params))]
ma <- params[grep("^ma",names(params))]
sigma <- sqrt(arma52$sigma2)
intercept <- params["intercept"]
simulated_cases <- arima.sim(list(ar=ar,ma=ma),
          n=dim(full_dengue)[1],
          sd=sigma)+intercept
ggplot() + 
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = full_dengue$total_cases, color = 'Observed Cases')) +
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = simulated_cases, color = "Simulated Cases")) +
  scale_colour_manual("", 
                      breaks = c("Observed Cases", "Simulated Cases"),
                      values = c("blue", "red")) +
  labs(x = 'Week start date', y = 'Cases')


# moving to log transform
arma45 <- arima(log(full_dengue$total_cases + 1),order=c(4,0,5), xreg = full_dengue[c('avg_temp', 'total_precip')])

# residual plot
arma45.resid = resid(arma45)
ggplot() +  
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = arma45.resid))

# sample ACF (auto-correlation function) plot
acf(arma45.resid, lag = 100)

# compare simulated with log true case counts
params <- coef(arma45)
ar <- params[grep("^ar",names(params))]
ma <- params[grep("^ma",names(params))]
sigma <- sqrt(arma45$sigma2)
intercept <- params["intercept"]
simulated_cases <- arima.sim(list(ar=ar,ma=ma),
                             n=dim(full_dengue)[1],
                             sd=sigma)+intercept
ggplot() + 
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = log(full_dengue$total_cases+1), color = 'Observed Cases')) +
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = simulated_cases, color = "Simulated Cases")) +
  scale_colour_manual("", 
                      breaks = c("Observed Cases", "Simulated Cases"),
                      values = c("blue", "red")) +
  labs(x = 'Week start date', y = 'Cases')

library('glarma')
X = as.matrix(full_dengue[12:13])
Y = as.matrix(full_dengue[10])
glarmamod = glarma(Y, X, phiLags = 1:4, phiInit = ar, type = "Poi", method = "FS", residuals = "Pearson", maxit = 100000, grad = 0.000001)

data(Polio)
y <- Polio[, 2]
X <- as.matrix(Polio[, 3:8])
glarmamod <- glarma(y, X, thetaLags = c(1,2,5), phiLags = c(4,5,6), type = "Poi", method = "FS",
                    residuals = "Pearson", maxit = 100, grad = 1e-6)

# AFTER THIS IT'S COPY PASTA

---
  title: "Midterm Project - Dengue Modeling"
author: "Kidus Asfaw"
date: "2/26/2018"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```
## Introduction
#### Dengue fever

Dengue fever is a tropical disease that is spread by the bites of a species of mosquito called Aedes Aegypti. The cause of the disease is the dengue virus, of which there are four serotypes (sub-types).

#### Motivation for work

In this project, we are going to try to model Dengue case counts in San Juan, Puerto Rico using observed case counts between 1990 and 2009 as well as climate data for the corresponding time period. One may wonder that even though there may be much more sophisticated ways to model such case counts, a classical time-series ARIMA analysis could capture much of the variety in our data. For instance, we know that the Aedes Aegypti mosquitoes breed in stagnant water. Since we expect to see stagnant water after rainy periods, we may think that temperature and rainfall could be used as predictors of the breeding levels of the mosquitoes, which would be tied in some sense to dengue case counts. The motivation for this project is, therefore, to explore a time-series model where we consider temperature and rainfall as our signal and an ARIMA process as our noise process. 

#### Data Preparation

The two datasets we'll be using were obtained from the National Oceanic and Atmospheric Administration. One of the datasets included Dengue case counts in San Juan, Puerto Rico between April 30, 1990 and April 23, 2009. The other includes temperature and precipitation data from a station in the city. 
The three datasets we'll be using are San Juan Dengue data, San Juan population data and San Juan climate data.
```{r, message = FALSE}
sj_dengue = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/San_Juan_Data.csv')
sj_station = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/SanJuanStationData.csv')
sj_pop = read_csv('~/Desktop/UM/COURSES/SEMESTER4/STAT531/midterm_project/San_Juan_Population_Data.csv')

head(sj_dengue)
head(sj_station)
head(sj_pop)
```

#### San Juan Dengue data
sj_station = read_csv('http://dengueforecasting.noaa.gov/StationData/SanJuanRQW00011641.csv')
sj_dengue = read_csv('http://dengueforecasting.noaa.gov/Training/San_Juan_Training_Data.csv')



```{r, message = FALSE}
ggplot(data = sj_dengue, mapping = aes(x = week_start_date, y = total_cases)) + geom_line()
```

Let us immediately plot log total cases over time as well.

```{r, message = FALSE}
ggplot(data = sj_dengue, mapping = aes(x = week_start_date, y = log(total_cases+1))) + geom_line()
```
#### San Juan Climate data

```{r, message = FALSE}
sj_station = sj_station %>% mutate(date = as.Date(paste(YYYY, MM, DD, sep = "-")))

ggplot(data = sj_station, mapping = aes(x = date, y = PRCP)) + geom_line()
```

Clearly we have missing data for three days. We shall impute missing data using global average.

```{r, message = FALSE}
sj_station[sj_station$PRCP < 0,]$PRCP = mean(sj_station$PRCP, na.rm = TRUE)
head(sj_station)

# add week_start_date variable and correct for week_start_date problem with dengue data
sj_station = sj_station%>%mutate(week_start_date = as.Date(format(as.Date(date, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u"))
sj_dengue = sj_dengue %>% rename(old_week_start_date = week_start_date)
sj_dengue = sj_dengue %>% mutate(week_start_date = as.Date(format(as.Date(old_week_start_date, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u"))
# restrict to dates for which we have Dengue data
sj_station = sj_station[sj_station$week_start_date >= as.Date('1990-04-30'),]


# plot precipitation vs. week_start_date
weekly_total_precip = sj_station%>%group_by(week_start_date)%>%
  summarize(total_precip = sum(PRCP, na.rm = T))
ggplot(data = weekly_total_precip, mapping = aes(x = week_start_date, y = total_precip)) + 
  geom_line()

# plot average weekly temperature vs time
weekly_avg_temp = sj_station%>%group_by(week_start_date)%>%
  summarize(avg_temp = mean(TAVG, na.rm = T))
ggplot(data = weekly_avg_temp, mapping = aes(x = week_start_date, y = avg_temp)) + 
  geom_line()
```

Let us append columns in our dengue data that include temperature and precipitation.
```{r, message=F}
full_dengue = inner_join(sj_dengue, weekly_avg_temp, by = "week_start_date")
full_dengue = inner_join(full_dengue, weekly_total_precip, by = "week_start_date")
full_dengue
```

#### Preliminary analysis
Now that we have weekly climate and disease data, let's try to fit a linear regression with temperature and precipitation as predictors. Reason being that mosquitoes thrive when they lay their eggs in puddles etc.

```{r}
cases.lm = lm(total_cases ~ avg_temp + total_precip, data = full_dengue)
cases.lm.resid = resid(cases.lm)
plot(full_dengue$avg_temp, cases.lm.resid,
ylab="Residuals", xlab="Average temperature", 
main="Residuals vs. Avg. temperature")
abline(0,0)
plot(full_dengue$total_precip, cases.lm.resid,
ylab="Residuals", xlab="Weekly precipitation", 
main="Residuals vs. Weekly precipitation")
abline(0,0)
# and finally an autocorrelation plot
acf(cases.lm.resid, lag = 100)
```

Unsurprisingly we have found that the data are not modeled well by a linear regression with independent gaussian errors. There are a couple of reasons for this: first, we notice heteroskedasticity in the residuals with respect to both predictor variables. Second, we notice the residuals do not have a mean of 0 across the values of the regressors. And thirdly, the residuals show evidence of autocorrelation with lags of 1, 2, 3, ..., 16 and 17 weeks.

The autocorrelations suggest that we should consider ARMA models and the seasonal autocorrelations indicate the suitability of a SARMA model. We shall use an AIC table to help us get started with parameters of an ARMA model. But first, let's look at a periodogram of the log-transformed case counts.

### Periodogram for log(cases + 1)
```{r}
p.gram <- spectrum(log(full_dengue$total_cases + 1),spans=c(3,5,7), main="Smoothed periodogram",ylim=c(1,100), xlim=c(0,0.1))
```

Note that I've restricted the axis limits so that I could focus on the portion of the plot with non-negligible power. The predominant frequencies occur at 0.019 cycles per data point (week) and 0.007 cycles per week, which translate to about 52 weeks per cycle and 2.75 years per cycle.

### OLS with SARMA errors and log-transformed case counts

Let us see what model fits our data best on the log-scale.

```{r, echo=FALSE, warning = FALSE, message = FALSE}

aic_table <- function(data, P, Q, xreg = NULL){
table <- matrix(NA,(P+1),(Q+1))
for(p in 0:P) {
for(q in 0:Q) {
table[p+1,q+1] <- arima(data,order=c(p,0,q), xreg = xreg)$aic
}
}
dimnames(table) <- list(paste("<b> AR",0:P, "</b>", sep=""),paste("MA",0:Q,sep=""))
table
}
cases_lm_aic_table <- aic_table(log(full_dengue$total_cases+1),5,10, xreg = full_dengue[c('avg_temp', 'total_precip')])
require(knitr)
kable(cases_lm_aic_table,digits=2)
```
#### Choosing an (S)ARMA model

Let's put aside seasonality for a moment. If we use AIC as our criterion, it looks like ARMA(4,5) is the model that we are invited to pick. Consider, however, ARMA(3,2). Although ARMA(4,5) is better in terms of prediction accuracy, we know that larger ARMA models are prone to identifiability problems. As such we can perform a hypothesis test on whether ARMA(4,5) is significantly better than ARMA(3,2) by comparing the log-likelihoods under the two models.

```{r}
chi_sq_diff = 2*(arima(log(full_dengue$total_cases+1),order=c(4,0,5), xreg = full_dengue[c('avg_temp','total_precip')])$loglik - arima(log(full_dengue$total_cases+1),order=c(3,0,2), xreg = full_dengue[c('avg_temp','total_precip')])$loglik)
1 - pchisq(chi_sq_diff, df = 4)
```

The difference between ARMA(4,5) and ARMA(3,2) seems significant. Given the seasonality we've seen, and the suitability of ARMA(4,5), let us fit a SARMA(4,0,5) $\times$ (1,0,1) (we set our seasonal AR and MA parameter counts 1 so as to capture both forms of dependence without adding many more parameters to an already large ARMA model) and investigate our parameter estimates.

```{r}
# estimate ARMA(4,5) and beta parameters
sarma45 <- arima(log(full_dengue$total_cases + 1),order=c(4,0,5), seasonal=list(order=c(1,0,1),period=52), xreg = full_dengue[c('avg_temp', 'total_precip')])
sarma45
```

#### Discussion of seasonal parameter estimates
It seems that our seasonal AR and MA parameters are both significant. One might mistakenly think that there might not be seasonality in our noise because our regressors (temperature and rainfull) capture some seasonal behavior. However, it makes sense that there is still a remarkable amount of seasonality in our noise process because the relationship among the Aedes mosquitoes' life-cycle, seasonal weather patterns and human-mosquito interaction is highly dynamic and difficult to explain using only climate variables.

For instance, recovery from infection by one serotype of the virus leads to lifelong immunity against that particular serotype. Therefore, as a massive epidemic from a serotype of the virus ends, we expect the population to have *herd immunity* against that serotype. This immunity will protect the entire population from another massive epidemic even though we still expect annual flare-ups as seen in our first figure (these flare-ups could be caused by other serotypes of the virus). As the herd immunity wanes (due to death, migration, new births or evolution of the virus) we see the population becomes susceptible to massive epidemics again, leading to the massive spikes that we see less frequently in our first figure.

Another layer that adds to the complex cyclical nature of the system is the existence of temporary cross-immunity (immunity of a person to a  serotype of the virus that is different from the one that infected them). In fact, subsequent infections from other serotypes make individuals more susceptible to *severe dengue* (associated with more severe symptoms and possibly death).

### Model Diagnostics

Below are the time plot and sample auto-correlation function for the residuals after fitting our SARMA model.

```{r}
# residual plot
sarma45.resid = resid(sarma45)
ggplot() +  
  geom_line(mapping = aes(x = full_dengue$week_start_date, y = sarma45.resid))

# sample ACF (auto-correlation function) plot
acf(sarma45.resid, lag = 100)

# QQ plot
ggplot(data = as.data.frame(sarma45.resid)) + stat_qq(mapping = aes(sample = sarma45.resid)) + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
```


The residuals seem to be mean-zero, uncorrelated (we expect 5 out of 100 lines to cross the blue lines and 6 do, which isn't too bad) and approximately Gaussian, as our model dictates.
                                                  
                                                  ### Simulation vs. Observed Data
                                                  
                                                  ```{r echo=FALSE}
                                                  # estimate ARMA(4,5) and mean parameters
                                                  arma45 <- Arima(log(full_dengue$total_cases + 1),order=c(4,0,5), seasonal=list(order=c(1,0,1),period=52), xreg = full_dengue[c('avg_temp', 'total_precip')])
                                                  
                                                  # compare simulated with log true case counts
                                                  simulated_cases = simulate(arma45, xreg = full_dengue[c('avg_temp','total_precip')])
                                                  
                                                  ggplot() + 
                                                  geom_line(mapping = aes(x = full_dengue$week_start_date, y = log(full_dengue$total_cases+1), color = 'Observed Cases')) +
                                                  geom_line(mapping = aes(x = full_dengue$week_start_date, y = simulated_cases, color = "Simulated Cases")) +
                                                  scale_colour_manual("", 
                                                  breaks = c("Observed Cases", "Simulated Cases"),
                                                  values = c("blue", "red")) +
                                                  labs(x = 'Week start date', y = 'Log Cases')
                                                  ```
                                                  
                                                  The first key takeaway from the plot above is that _most_ of the variation in our data is not captured by temperature and rainfall. We had hoped that since these covariates can be used as a proxy to predict mosquito population, we could perhaps recover the variation in our case counts for dengue using the covariates as our regressors. However, as stated above, the systems and laws that dictate the interplay among the mosquitoes, the humans, the virus and the climate are highly complex.
                                                  
                                                  ### Summary and future works
                                                  
                                                  
                                                  
                                                  We notice that the standard estimates of our SARMA(4,0,5) $\times$ (1,0,0) model produce NA results. This suggests that there might be an overparametrization problem - we are pushing the boundary with our computational abilities. So let's go back and examine estimates for the smaller SARMA(3,0,2)$\times$(1,0,0) model.
                                                  
                                                  ```{r}
                                                  # estimate ARMA(3,2) and beta parameters
                                                  sarma32 <- arima(log(full_dengue$total_cases + 1),order=c(3,0,2), seasonal=list(order=c(1,0,1),period=52), xreg = full_dengue[c('avg_temp', 'total_precip')])
                                                  sarma32
                                                  ```
                                                  
                                                  Note that the comparison here is between SARMA(4,0,5)$\times$(1,0,0) and SARMA(3,0,2)$\times$(1,0,0). Our AIC table above didn't account for seasonality (which the reader can tell because there is no $\texttt{period}$ argument to $texttt{arima}$) because many of the possible SARMA models had convergence problems. However, SARMA(4,0,5)$\times$(1,0,0) and SARMA(3,0,2)$\times$(1,0,0) do achieve convergence and allow us to make some quick comparisons.
                                                  
                                                  Firstly, note that we don't have a problem with NA values for the standard error estimates in the SARMA(3,0,2)$\times$(1,0,0) case. Furthermore, the log-likelihood difference between the two models is not significant:
                                                    
                                                    ```{r}
                                                  chi_sq_diff = 2*(sarma45$loglik - sarma32$loglik)
                                                  1 - pchisq(chi_sq_diff, df = 4)
                                                  ```
                                                  
                                                  We, therefore, pick SARMA(3,0,2)$\times$(1,0,0) to model the noise process in the following __signal plus noise__ model:
                                                    
                                                    \[
                                                      \begin{eqnarray}
                                                      log(cases_{t} + 1) &=& X_{t}\beta + \nu_{t}\\
                                                      \text{where }\Phi(B^{12})\phi(B)\nu_{t}&=& \Psi(B^{12})\psi(B)\epsilon_{t}\\
                                                      \text{and } \epsilon_{t} \sim N(0, \sigma^2)
                                                      \end{eqnarray}
                                                      \]
                                                  
                                                  ### Model Diagnostics
                                                  
                                                  Below are the time plot and sample auto-correlation function for the residuals after fitting our SARMA model.
                                                  
                                                  ```{r}
                                                  # residual plot
                                                  sarma32.resid = resid(sarma32)
                                                  ggplot() +  
                                                    geom_line(mapping = aes(x = full_dengue$week_start_date, y = sarma32.resid))
                                                  
                                                  # sample ACF (auto-correlation function) plot
                                                  acf(sarma32.resid, lag = 100)
                                                  ```
                                                  
                                                  ```
                                                  
                                                  ```{r}
                                                  # estimate ARMA(4,5) and mean parameters
                                                  arma45 <- Arima(log(full_dengue$total_cases + 1),order=c(4,0,5), seasonal=list(order=c(1,0,0),period=52), xreg = full_dengue[c('avg_temp', 'total_precip')])
                                                  print(coef(arma45))
                                                  ```
                                                  ```{r}
                                                  
                                                  #print(sim)
                                                  #print(sim + intercept)
                                                  #print(as.numeric(full_dengue$avg_temp*avg_temp_beta))
                                                  ```
                                                  ```{r eval = FALSE}
                                                  # compare simulated with log true case counts
                                                  params <- coef(arma45)
                                                  ar <- params[grep("^ar",names(params))]
                                                  sar <- params[grep("^sar",names(params))]
                                                  ma <- params[grep("^ma",names(params))]
                                                  sigma <- sqrt(arma45$sigma2)
                                                  avg_temp_beta <- params[grep("^avg_temp", names(params))]
                                                  total_precip_beta <- params[grep("^total_precip", names(params))]
                                                  intercept <- params["intercept"]
                                                  library(forecast)
                                                  simulated_cases = arima.sim(list(ar=ar,ma=ma),
                                                                              n=dim(full_dengue)[1],
                                                                              sd=sigma) + intercept + 
                                                    full_dengue$avg_temp*avg_temp_beta + 
                                                    full_dengue$total_precip*total_precip_beta
                                                  ggplot() + 
                                                    geom_line(mapping = aes(x = full_dengue$week_start_date, y = log(full_dengue$total_cases+1), color = 'Observed Cases')) +
                                                    geom_line(mapping = aes(x = full_dengue$week_start_date, y = simulated_cases, color = "Simulated Cases")) +
                                                    scale_colour_manual("", 
                                                                        breaks = c("Observed Cases", "Simulated Cases"),
                                                                        values = c("blue", "red")) +
                                                    labs(x = 'Week start date', y = 'Log Cases')
                                                  #print(sim)
                                                  #print(sim + intercept)
                                                  #print(as.numeric(full_dengue$avg_temp*avg_temp_beta))
                                                  ```
                                                  
                                                  
                                                  ### Non log-transformed data
                                                  ```{r, echo=FALSE, warning = FALSE, message = FALSE}
                                                  
                                                  aic_table <- function(data, P, Q, xreg = NULL){
                                                    table <- matrix(NA,(P+1),(Q+1))
                                                    for(p in 0:P) {
                                                      for(q in 0:Q) {
                                                        table[p+1,q+1] <- arima(data,order=c(p,0,q), xreg = xreg)$aic
                                                      }
                                                    }
                                                    dimnames(table) <- list(paste("<b> AR",0:P, "</b>", sep=""),paste("MA",0:Q,sep=""))
                                                    table
                                                  }
                                                  cases_lm_aic_table <- aic_table(full_dengue$total_cases,5,10, xreg = full_dengue[c('avg_temp', 'total_precip')])
                                                  require(knitr)
                                                  kable(cases_lm_aic_table,digits=2)
                                                  ```
                                                  
                                                  The top two models that we're invited to pick by the table above are ARMA(5,2) and ARMA(5,6).
                                                  ```{r}
                                                  arma52_loglik = arima(full_dengue$total_cases,order=c(5,0,2), xreg = full_dengue[c('avg_temp', 'total_precip')])$loglik
                                                  arma56_loglik = arima(full_dengue$total_cases,order=c(5,0,6), xreg = full_dengue[c('avg_temp', 'total_precip')])$loglik
                                                  chi_sq_diff = 2*(arma56_loglik - arma52_loglik)
                                                  pchisq(chi_sq_diff, df = 4)
                                                  ```
                                                  
                                                  Since the difference above is not statistically significant, let us go with the smaller model, ARMA(5,2)
                                                  
                                                  ```{r}
                                                  arma52 <- arima(full_dengue$total_cases,order=c(5,0,2), xreg = full_dengue[c('avg_temp', 'total_precip')])
                                                  
                                                  # residual plot
                                                  arma52.resid = resid(arma52)
                                                  ggplot() +  
                                                  geom_line(mapping = aes(x = full_dengue$week_start_date, y = arma52.resid))
                                                  
                                                  # sample ACF (auto-correlation function) plot
                                                  acf(arma52.resid, lag = 100)
                                                  ```
                                                  
                                                  Based on the above, the ARMA(5,2) model seems to have mean 0 with uncorrelated errors. Let us simulate from this model and compare with our data.
                                                  ```{r}
                                                  params <- coef(arma52)
                                                  ar <- params[grep("^ar",names(params))]
                                                  ma <- params[grep("^ma",names(params))]
                                                  sigma <- sqrt(arma52$sigma2)
                                                  intercept <- params["intercept"]
                                                  simulated_cases <- arima.sim(list(ar=ar,ma=ma),
                                                  n=dim(full_dengue)[1],
                                                  sd=sigma)+intercept
                                                  ggplot() + 
                                                  geom_line(mapping = aes(x = full_dengue$week_start_date, y = full_dengue$total_cases, color = 'Observed Cases')) +
                                                  geom_line(mapping = aes(x = full_dengue$week_start_date, y = simulated_cases, color = "Simulated Cases")) +
                                                  scale_colour_manual("", 
                                                  breaks = c("Observed Cases", "Simulated Cases"),
                                                  values = c("blue", "red")) +
                                                  labs(x = 'Week start date', y = 'Cases')
                                                  ```
                                                  
                                                  We notice that there are a few problems with the above plot. The first is that we are simulating __negative__ cases, which is not possible. The second is that we're not capturing the high peaks from outbreaks. We can attempt to tackle both of these problems by seeing if our model is more appropriate on logarithm-transformed case counts.





We still seem to be suffering from negative log cases (which translate to impossible range of 0-1 cases). Looks like we need a proper count model.




