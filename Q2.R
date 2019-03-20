# import data into R
cat('Copy data file to folder: ',getwd(),'\n')
dataset <- read.csv("singapore.economy.csv")
head(dataset)
# exclude record which contains NA
dataset = na.omit(dataset)
summary(dataset) #now there is no NA
# Plot datasetapore GDP against Time
plot(dataset$gdp~dataset$time, xlab = "Time", ylab = "GDP (%)", main = "Singapore GDP growth")
# we could see that there is no significant relationship between time and gdp in Singapore
# calculate mean and std of GDP based on periods
GDP1 = as.matrix(dataset[which(dataset$period == 1),3])
GDP2 = as.matrix(dataset[which(dataset$period == 2),3])
GDP3 = as.matrix(dataset[which(dataset$period == 3),3])
stat.table = matrix(c(mean(GDP1),sd(GDP1),mean(GDP2),sd(GDP2),mean(GDP3),sd(GDP3)), ncol = 2, byrow = T)
colnames(stat.table) = c("Mean", "Standard Deviation")
rownames(stat.table) = c("GDP period 1", "GDP period 2", "GDP period 3")
stat.table
#GDP in the 3rd period has the lowest mean, while its standard deviation is highest
# create scatter plot
pairs(dataset[,-c(1,2)])
#There are significant relationships between "bci" and "gdp", "bci" and "exp", "bci" and "egp"
#There is a slight relationship between between "egp" and "hpr"
# create simple linear regression with gdp and exp
linear = lm(dataset$gdp~dataset$exp)
summary(linear)
#As the p-value is smaller than 0.05, it indicates that the predictor is significant. Also, since the model can explain 28.79% of the total variance of the response, the model could be not really good for predicting the response.
# create multiple regression model
linear2 = lm(dataset$gdp~dataset$exp+dataset$epg+dataset$hpr+dataset$gdpus+dataset$oil+dataset$crd)
summary(linear2)
# there are some insignificant predictors in the model since its p-values are larger than 0.05
# We then using backward selection method to remove insignificant predictors one by one and try to fit the model again.
linear2 = lm(dataset$gdp~dataset$exp+dataset$epg+dataset$hpr)
summary(linear2) 
#now all the predictors are significant as their p-values are all smaller than 0.05. In addition, The p-value for the F-statistic is extremely small, it indicates that the model really helps to predict the response. However, as R square is 36.95%, it could be not good enough in explaining the variance in the response variable.
# 5% quantile and assessing the economy
q5 = quantile(dataset$gdp,0.05)
q5
state = dataset$gdp < q5
state[state == TRUE] = c("Crisis")
state[state == FALSE] = c("Normal")
state = as.factor(state)
head(state)
#Now we got a new variable indicates the economy situation.
# add state variable to dataset
dataset = data.frame(dataset,state)
head(dataset)
#fit the logistic regression
model= glm( state ~ bci, family = binomial , data = dataset[dataset$time<2008,])
summary(model)
#The p-value of "bci" is smaller than 0.05, which indicates that the predictor is significant in the model. In addition, the null and residual deviance are all smaller than its degree of freedom, but not so much. Therefore, the evidence of over dispersion is not obvious, then the binomial distribution with log link that we follow is quite good.
#confusion matrix
prob = predict(model,dataset[dataset$time >= 2008,] ,type ="response")
pred = rep (c("Normal"), length(dataset$state) - length(dataset[dataset$time < 2008,]$state))
pred[prob < 0.5] = c("Crisis")
conf_matrix = table(pred, dataset[dataset$time >= 2008,]$state)
conf_matrix
# misclassification rate
(1 - mean(pred == dataset[dataset$time>=2008,]$state))*100
#the misclassification rate is quite small. This indicates that the model we use for predicting is good.