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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

mean(y)
sd(y)
s.e <-sd(y)/sqrt(length(y))
s.e
df <-length(y)-1
t_score <- qt(0.95,df =df)
upper_90_t <- mean(y)+(t_score*s.e)
upper_90_t
lower_90_t <-mean(y)-(t_score*s.e)
lower_90_t
## finding t-test
t_score_critical
df2 <-length(y)-1
t_score_critical <-qt(0.95,df =df2)
t_score_critical

upper_bound_95<- mean(y)+(t_score_critical+s.e)
upper_bound_95
lower_bound_95 <- mean(y) - (t_score_critical * s.e)
lower_bound_95

population_mean<-100
t_score<- (mean(y)-population_mean)/s.e
t_score

p_value <- pt(abs(t_score), df2, lower.tail = FALSE)
p_value

## now running t- test
t.test(y,mu = 100, conf.level = 0.95,alternative = 'greater')


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
str(expenditure)
plot = pairs(expenditure[,c('Y','X1','X2','X3')])
#y and x1 has a positve corelation, when Y increase x1 does indicating a direct relationship
#y and x2 has a negative: appears to show a weaker or no correlation
#y and x3 no relation: the plot suggest a weak or no correlation
#x1 and x2 no relation: no strong correlation between the two variables 
#x2 and x3 no relation: no clear correlation between the variables 
#x1 and x3 negative correlation: there seems to be little to no correlation amongst them
 

 #Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?
boxplot(expenditure$Y ~ expenditure$Region,names = c('North_East','North_Central','South','West'),
        main ='Distribution of per capita expenditure on shelters/housing assistance in states by Region',
        xlab = 'Region', ylab = 'Expenditure',col='lightblue')
## here note that the median of the box plot of the west region shows is of approx 80 units, where we can 
# conclude that 50% of the social welfare expenditure falls within the interquartile range of 110 and 80, with the maximum expenditure value
# going beyond 120 units while the lowest reaching up-to approx 40 units, though west having relatively
# higher expenditure values with the long whiskers, based on the interquartile range and the spread of the data
# we can infer that west having higher expenditure value over other regions on average.


#Please plot the relationship between Y and X1? Describe this graph and the rela-
#tionship. Reproduce the above graph including one more variable Region and display
#diferent regions with dierent types of symbols and colors.



plot(expenditure$X1,expenditure$Y,col=as.factor(expenditure$Region),pch=as.numeric(as.factor(expenditure$Region)),
     main = ' Relationship between expenditure and per capita income in states based on region',
     xlab = 'per capita personal income in state',ylab ='per capita expenditure on shelters/housing assistance in state')

legend('topright',legend = levels(as.factor(expenditure$Region)),
       col = 1:length(levels(as.factor(expenditure$Region))),
       pch = 1:length(levels(as.factor(expenditure$Region))))
       
# the scatterplot shows evidently that there exists a positive association between personal income
# in state per capita and the per capita expenditure on shelters/hosuing assistance in state,
# thereby we can assume that the social welfare expenditures in the US is influenced by the per capita
# personal income of the people residing in a particualr region, the spread of the datapoints based on the 
# region clearly indicates 