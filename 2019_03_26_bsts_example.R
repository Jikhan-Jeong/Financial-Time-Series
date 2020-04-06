## 2019_03_26_r_working_group_from_basic_TS_to_BSTS for Nowcasting
## reference: http://dss.princeton.edu/training/
## reference: https://rstudio-pubs-static.s3.amazonaws.com/257314_131e2c97e7e249448ca32e555c9247c6.html
## reference: Scott, Steven, and Hal Varian. 2014. ¡°Predicting the Present with Bayesian Structural Time Series¡± 5. Inderscience Publishers Ltd: 4???23.
## reference: https://www.youtube.com/user/202tylertucker
## reference: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
## reference: https://multithreaded.stitchfix.com/blog/2016/04/21/forget-arima/
## referecen: Scott, Steven, and Hal Varian. 2014. ¡°Predicting the Present with Bayesian Structural Time Series¡± 
##            5. Inderscience Publishers Ltd: 4???23.
## reference: Kitamura, A. (Ai). (2018, September 21).??Forecasting Japan¡¯s spot LNG prices using Bayesian Structural Time Series.
##            Maritime Economics and Logistics. Retrieved from http://hdl.handle.net/2105/43636


getwd()

library(dplyr)
library(tidyverse, quietly = TRUE)
library(bsts, quietly = TRUE)
library(BoomSpikeSlab, quietly = TRUE)
# iclaims <- default data : BSTS package, Bayesian Structural Time Series, BSTS
# weekly time series of US initial claims for unemployment from Federal Reserve Economic Data


## (1)___________________________________loading the default data

data(iclaims) # load data set
.data <- initial.claims
class(initial.claims) # z00 = indexed totally ordered observations including time seres
dim(initial.claims) # 456 rows (456 weeks)and 11 cols (11 variables)
view(initial.claims)
names(initial.claims)


claims <- .data$iclaimsNSA # y, dependent variables
plot(claims, ylab ="")


## (2) Model1 (time trend, seasonal)__________________________________Setting the basic model 
## 2.1 making empty list
## 2.2 add a time trend component
## 2.3 add a seasonal component
## 2.4 fitting a model with time and seasonal trends
## 2.5 prediction with (4) model


# 2.1 starting with an empty list of model componet

(model_componets <- list())

# 2. set the time loacl linear trend
model_components <- AddLocalLinearTrend(model_componets, y = claims)
# AddLocalLinearTrend : Returns a list with the elements necessary to specify a local linear trend state model
summary(model_components)

# 3. add a seasonal component

model_components <- AddSeasonal(model_components, y = claims, nseasons = 52)
# ?AddSeasonal

# The seasonal model can be thought of as a regression on nseasons dummy variables 
# with coefficients constrained to sum to 1 (in expectation). If there are "S" seasons
# then the state vector gamma is of dimension S-1. 

# "nseasons" The number of seasons to be modeled. 1year = 52 weeks

summary(model_components)

# 4. fitting a model with time and seasonal trends

model1 <- bsts(claims, model_components, niter= 1000) # model fit along with MCMC 1000 iteration, no Xs

# As a Bayesian model, the trend and seasonal component parameters are MCMC samples from a posterior. 
# see the contributions of each component to the model fit, based on posterior sample means

burnin <- 500 # throw away first 500
tibble(date = as.Date(time(claims)), trend = colMeans(model1$state.contributions[-(1:burnin), "trend",]),
  seasonality = colMeans(model1$state.contributions[-(1:burnin),"seasonal.52.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x=date, y=value)) + 
   geom_line() + theme_bw() + theme(legend.title = element_blank()) +
   ylab("") + xlab("") + facet_grid(component ~., scales ="free") + guides(colors=FALSE) +
   theme(axis.text.x=element_text(angle = -90, hjust = 0))

# 5. prediction with 90% credible interval with fitted model with time and seaonal trends

pred <- predict(model1, horizon = 100, burn = burnin, quantiles = c(.05, .95)) # 100 weeks ahead
plot(pred)

# 6. errors (acutal - predicted)

errors <- bsts.prediction.errors(model1, burn = 100)


# ?bsts.prediction.errors
# Computes the one-step-ahead prediction errors for a bsts model.
# burn : An integer giving the number of MCMC iterations to discard as burn-in.
# If burn <= 0 then no burn-in sample will be discarded.
# PlotDynamicDistribution(errors)

# (3) Model2 (skip)____________________________________________________adding features to the model (skip)

# The dataset also includes time series that correlate strongly with initial claims. 
# bsts will combine the trend and seasonal components with a regression on these other time series.

model2 <- bsts(claims ~., state.specification = model_components, data = initial.claims, niter = 1000)

# default is selectring 1 variable as a feature

names(model2)
model2$coefficients
dim(model2$coefficients) ## 200rows (200 MCMC iteration) and 11 cols(=variables)

# We can obtain parameter estimates by again taking the sample mean of the sampled beta values.

colMeans(model2$coefficient) # average value of MCMC

# plot

plot(model2) 
#  Posterior distribution of model state. Blue circles are actual data points.

plot(model2, "components") 

# (4) Model3 (time trend, seasonal, feature1______Fit a bsts model with expected model size 1, the default.

model3 <-  bsts(iclaimsNSA ~., state.specification = model_components, data = initial.claims, niter = 1000)

model3$coefficients
plot(model3, "comp")
plot(model3, "coef")
# Posterior inclusion probabilities for predictors in the "claims" nowcasting example assuming an expected model size of (a) 1 and (b) 5.


# (5) Model4 (time trend, seasonal, feature1______Fit a bsts model with expected model size 5


model4 <-  bsts(iclaimsNSA ~., state.specification = model_components, data = initial.claims, niter = 1000,
              expected.model.size=5) # passed to spikeslabprior
model4$coefficients
plot(model4, "comp")
plot(model4,"coef")

bsts.prediction.errors(model4) # 1 step predition error

## showinng the comparing results

CompareBstsModels(list("time component model" = model1,
                        "regressor model1" = model3,
                        "regressor model5" = model4),
                   colors = c("black", "red", "blue"))

## top panel is cumulative total of the mean absolute one step prediction error for each model,
## comparing base model and others, others are overlapped.
                 
# to do, long-term trend
# reference: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
# paper reference: http://people.ischool.berkeley.edu/~hal/Papers/2013/pred-present-with-bsts.pdf