#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
inc.sub
model<- lm(voteshare ~difflog,data= inc.sub)
summary(model)

## making a scatterplot

plot(x=inc.sub$difflog, y=inc.sub$voteshare) # Scatter plot # Either specify single value (v for vertical)
abline(0.579031,0.041666) # Or intercept and slope
abline(model) # Use intercept and slope in model object
abline(model, col="red")

## saving the residuals of the model 

model_residuals<-model$residuals
model_residuals

##Write the prediction equation.
y= a+bx
intercept<- model$coefficients[1]
intercept
coefficients_difflog<- model$coefficients[2]
coefficients_difflog
voteshare = intercept+coefficients_difflog
voteshare<- 0.579031+0.041666*difflog


## second question
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
inc.sub
model_2<- lm(presvote~ difflog, data = inc.sub)
summary(model_2)

## adding the regression line
plot(x = inc.sub$difflog, y = inc.sub$presvote)
abline(0.507583,0.023837)
abline(model_2,col='red')

## adding the residual under an object
model_residual_2 = model_2$residuals
model_residual_2

## prediction equation y = a+bx
intercept<- model_2$coefficients[1]
intercept
coefficients_difflog<- model_2$coefficients[2]
coefficients_difflog
presvote = intercept+coefficients_difflog
presvote <- 0.507583+0.023837*difflog

## third question

model_3<-lm(voteshare ~ presvote,data=inc.sub)
summary(model_3)

## scatterplot
plot(x= inc.sub$presvote,y=inc.sub$voteshare)
abline(0.441330,0.388018)
abline(model_3,col='red')

## prediction equation
y = a+bx
intercept<- model_3$coefficients[1]
intercept
coefficients_presvote<- model_3$coefficients[2]
coefficients_presvote
voteshare<-intercept+coefficients_presvote*presvote
voteshare<-0.441330+0.388018*presvote

## question 4
model_4<- lm(model_residuals ~ model_residual_2,data=inc.sub)
summary(model_4)

# scatter plot
plot(x = model_residual_2,y= model_residuals)
abline(model_4,col='red')

## prediction equation y = a+bx
intercept<- model_4$coefficients[1]
intercept
coefficients_residual_2<- model_4$coefficients[2]
coefficients_residual_2
model_residuals<- intercept+coefficients_residual_2*model_residual_2
model_residuals = -5.934e-18+2.569e-01*model_residual_2


## question 5
model_5<- lm(voteshare ~ difflog+presvote,data=inc.sub)
summary(model_5)
## prediction equation
y = a+b1x1+b2x2
intercept<- model_5$coefficients[1]
intercept
coefficients_difflog<- model_5$coefficients[2]
coefficients_difflog
coefficients_presvote<-model_5$coefficients[3]
coefficients_presvote
voteshare = intercept+coefficients_difflog*difflog+coefficients_presvote*presvote
voteshare = 0.4486442+0.0355431*difflog+0.2568770*presvote

#What is it in this output that is identical to the output in Question 4? Why do you
#think this is the case?
