# Quiz 2
## Question 1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit<-lm(y~x)
est<-predict(fit,data.frame(x))

plot(x,y)
abline(fit,col="red")
summary(fit)

## Question 3
## In the mtcars data set, fit a linear regression model of weight (predictor) 
## on mpg (outcome). Get a 95% confidence interval for the expected mpg 
## at the average weight. What is the lower endpoint?
x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y ~ x)

predict(fit,data.frame(x=mean(x)), interval="confidence")

p1<-predict(fit,data.frame(x), interval="confidence")
plot(x,y,xlab='Weight (1000lb)',ylab='MPG')
abline(fit,col="red")
lines(x,p1[,2],col="purple")
lines(x,p1[,3],col="purple")

## Question 5
## Consider again the mtcars data set and a linear regression model with mpg 
## as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds.
## Construct a 95% prediction interval for its mpg. What is the upper endpoint?
predict(fit,data.frame(x=3), interval="prediction")

## Question 6
## Consider again the mtcars data set and a linear regression model with mpg as 
## predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. 
## Construct a 95% confidence interval for the expected change in mpg per 
## 1 short ton increase in weight. Give the lower endpoint.
fit2<-lm(y~I(x/2))
tbl2<-summary(fit2)$coefficients
mn<-tbl2[2,1]      #mean is the estimated slope
std_err<-tbl2[2,2] #standard error
deg_fr<-fit2$df    #degree of freedom
#Two sides T-Tests
mn + c(-1,1) * qt(0.975,df=deg_fr) * std_err
par(mfrow=c(1,2))
plot(x,y)
abline(fit,col="red")
plot(x/2,y)
abline(fit2,col="red")

# Question 9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as
# the predictor. About what is the ratio of the the sum of the squared errors,
# ∑ni=1(Yi−Y^i)2 when comparing a model with just an intercept (denominator) to
# the model with the intercept and slope (numerator)?

fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")


anova(fit)
anova(fit5)

# Question 10
# Do the residuals always have to sum to 0 in linear regression?

# If an intercept is included, then they will sum to 0.
