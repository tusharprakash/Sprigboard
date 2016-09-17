## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

##----------------------------------------------------------------------------
##Examine the variables

table(NH11$r_maritl, useNA = 'always')
table(NH11$everwrk, useNA = 'always')
table(NH11$age_p, useNA = 'always')

## We find the the response variable everwrk has a lot of NAs.
## Examine the precentage of NAs across categorical and discrete-continous variables

NH11.data = subset(NH11, select  = c("everwrk", "r_maritl", "age_p"))

pMiss = function(x){
  if (class(x) == "factor"){
per = sum(is.na(x))/length(x)*100
}
else{
per = sum(x[length(x)])/sum(x)*100
}
return(per)
}

levels.percentage <- function(){
  DF_maritl = data.frame()
  for(i in levels(NH11.data$r_maritl)){
    DF_maritl = rbind(DF_maritl, cbind(Marital_Status = i, PercentageNA = pMiss(NH11.data[NH11.data$r_maritl == i,][,1])))
  }
  print(DF_maritl)
  DF_age= data.frame()
  t = table(NH11.data$age_p, NH11.data$everwrk, useNA = "always")
for(i in nrow(t)){
DF_age = rbind(DF_age, cbind(Age = dimnames(t)[[1]][i], PercentageNA = pMiss(t[i,])))
}
print(DF_age)
}

## We observe there's a very high percentage of NAs across all caterogical and discrete-continous variables
## Any kind of imputation method would not be suitable in this situation
## It's better to omit the NAs altogether in the models

library("reshape2")
library("nnet")
NH11.data = na.omit(NH11.data)
NH11.data$everwrk2 <- relevel(NH11.data$everwrk, ref = "1 Yes")

## This is a multinomial logistic regression as the response variable has 
## multiple levels and is not binary. So we will use a multinomial regression here

pred_mnom = multinom(formula = everwrk2 ~ r_maritl + age_p, data = NH11.data)
head(fitted(pred_mnom))
ctable <- coef(summary(pred_mnom)) # check the regression coefficients

## Prepare data for predicting probabilities
predData = with(NH11.data, expand.grid(r_maritl = levels(r_maritl), age_p = unique(age_p)))

## Predit the probabilities for different levels of marital statuses
predicted = predict(pred_mnom, predData, type = "probs")
probs =cbind(predData, predicted)
by(probs[,3:6], probs$r_maritl, colMeans)
probsm = melt (probs, id.vars=c("r_maritl", "age_p"), value.name="probability")
head(probsm, n = 50)

## head(Plot the probabilities for different levels of r_maritl
library("ggplot2")
ggplot(probsm, aes(x = r_maritl, y = probability, colour = age_p)) + geom_line() + facet_grid(variable ~ ., scales="free")





## Predit the probabilities for different levels of marital statuses





