install.packages('car')
library(car)
df<- Prestige
df
## creating a new column

df['professional']<-ifelse(df$type=='prof',1,0)
df
## running a linear regression model
table(df$professional)
model_1<- lm(prestige~income+professional*income,data=df)
model_1
summary(model_1)

Marginal_effect = (round(model_1$coefficients[2],3)+round(model_1$coefficients[4],3))
change_presitge <- Marginal_effect*1000
change_presitge

Marginal_effect_2<- (round(model_1$coefficients[3],3)+round(model_1$coefficients[4],3)*6000*1)
Marginal_effect_2  

#t_value =  coefficient/std.error
t_value = 0.042/0.016
t_value
df_2 = 131
p_value = 2*pt(abs(t_value), df_2 -2-1,lower.tail = FALSE)
p_value


#t_value
t_value_2<- 0.042/0.013
t_value_2
df_2 <-131
p_value_2<- 2*pt(t_value_2,df_2-2-1,lower.tail=FALSE)
p_value_2



