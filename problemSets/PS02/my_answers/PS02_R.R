#### problem set 2

## calculating the chi 2 test

##
##observed data
## contigency table
upper_class_sum<- sum(14,6,7)
upper_class_sum
lower_class_sum<-7+7+1
lower_class_sum
row_total<- matrix(c(upper_class_sum,lower_class_sum))
row_total
rownames(row_total)<-c('upper class sum','lower class sum')
row_total
not_stopped_sum<-14+7
not_stopped_sum
bribe_requested_sum<-6+7
bribe_requested_sum
stopped_given_warning_sum<-7+1
col_total <- matrix(c(not_stopped_sum, bribe_requested_sum, stopped_given_warning_sum), ncol = 1)
col_total
rownames(col_total) <- c('Not Stopped', 'Bribe Requested', 'Stopped/Given Warning')
col_total
grand_total_sum<- upper_class_sum+lower_class_sum
grand_total_sum

### expected data
upper_class_not_stopped_expected<-(upper_class_sum*not_stopped_sum)/grand_total_sum
upper_class_not_stopped_expected
upper_class_bribe_requested_expected<-(upper_class_sum*bribe_requested_sum)/grand_total_sum
upper_class_bribe_requested_expected
upper_class_stopped_expected<-(upper_class_sum*stopped_given_warning_sum)/grand_total_sum
upper_class_stopped_expected
lower_class_not_stopped_expected<-(lower_class_sum*not_stopped_sum)/grand_total_sum
lower_class_not_stopped_expected
lower_class_stopped_expected<-(lower_class_sum*stopped_given_warning_sum)/grand_total_sum
lower_class_stopped_expected
lower_class_bribe_requested_expected<-(lower_class_sum*bribe_requested_sum)/grand_total_sum
lower_class_bribe_requested_expected

### now forming the expected data table
expect<-(row_total/ grand_total_sum) %*% t(col_total)
expect


chi_sq_test<-(
  (14-upper_class_not_stopped_expected)^2/upper_class_not_stopped_expected+
    (6-upper_class_bribe_requested_expected)^2/upper_class_bribe_requested_expected+
    (7-upper_class_stopped_expected)^2/upper_class_stopped_expected+
    (7-lower_class_not_stopped_expected)^2/lower_class_not_stopped_expected+
    (7-lower_class_bribe_requested_expected)^2/lower_class_bribe_requested_expected+
    (1-lower_class_stopped_expected)^2/lower_class_stopped_expected
)
chi_sq_test

### now finding the p value for 
observed_data <- matrix(c(14,6,7,7,7,1),nrow=2,byrow=TRUE)
rownames(observed_data)<-c('upper_class','lower_class')
colnames(observed_data)<-c('Not stopped','bribe requested','Stopped/given warning')
observed_data
rows = nrow(observed_data)
rows
col = ncol(observed_data)
df<- (1-rows)*(1-col)
df
p_value<- 1-pchisq(chi_sq_test,df)
p_value


### standardised residual
observed_data <- matrix(c(14,6,7,7,7,1),nrow=2,byrow=TRUE)
rownames(observed_data)<-c('upper_class','lower_class')
colnames(observed_data)<-c('Not stopped','bribe requested','Stopped/given warning')
observed_data
addmargins(observed_data)
expect<-(row_final/ grand_total_sum) %*% t(col_final)
expect
row_proportion1<-upper_class_sum/grand_total_sum
row_proportion1
row_proportion2<-lower_class_sum/grand_total_sum
row_proportion2
col_proportion1<-not_stopped_sum/grand_total_sum
col_proportion1
col_proportion2<-bribe_requested_sum/grand_total_sum
col_proportion3<-stopped_given_warning_sum/grand_total_sum
col_proportion3

standard_residual_upperclass_notstopped<- (14-upper_class_not_stopped_expected)/sqrt(upper_class_not_stopped_expected*(1-row_total_proportion_1)*(1-col_total_proportion_1))
standard_residual_upperclass_notstopped
standard_residual_upperclass_briberequested<- (6-upper_class_bribe_requested_expected)/sqrt(upper_class_bribe_requested_expected*(1-row_proportion1)*(1-col_proportion2))
standard_residual_upperclass_briberequested
standard_residuals_upperclass_stopped<- (7-upper_class_stopped_expected)/sqrt(upper_class_stopped_expected*(1-row_proportion1)*(1-col_proportion3))
standard_residuals_upperclass_stopped
standard_residuals_lowerclass_notstopped<-(7-lower_class_not_stopped_expected)/sqrt(lower_class_not_stopped_expected*(1-row_proportion2)*(1-col_proportion1))
standard_residuals_lowerclass_notstopped
standard_residual_lowerclass_bribe<-(7-lower_class_bribe_requested_expected)/sqrt(lower_class_bribe_requested_expected*(1-row_proportion2)*(1-col_proportion2))
standard_residual_lowerclass_bribe
standard_residual_lowerclass_stopped<-(1-lower_class_stopped_expected)/sqrt(lower_class_stopped_expected*(1-row_proportion2)*(1-col_proportion3))
standard_residual_lowerclass_stopped


## 2nd problem
library(tidyverse)

UNpop_URL<- 'https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv'
df2<-read_csv(UNpop_URL)
View(df2)

##h0 : the women's reservation policy has no signifcant effect on the number of new or repaired drinking water facilites in 
# in the villages
##h1: the women's reservation policy has a significant effect on the number of new or repaired drinking water
#facilites in the village

## Running the bivariate regression model
URL<- 'https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv'
df2<-read_csv(URL)
View(df2)
head(df2)
model <-lm(water~reserved,data=df2)
summary(model)
