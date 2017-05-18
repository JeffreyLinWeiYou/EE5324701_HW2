mydata = read.table("/Users/linweiyou/Desktop/HW2/bankbill.txt", header =TRUE)


str(mydata)
summary(mydata)
#data to time
plot(x ="data", y = "BankBill", xlim = c(1,71),ylim = c(4,9))
lines(x = mydata$BankBill)
#all variables
model <- lm(formula = BankBill ~ ., data = mydata)
summary(model)
#modle1
model1 <- lm(formula = BankBill ~ AllOrds+Transport+Unemploy, data = mydata)
summary(model1)
#model2
model2 <- lm(formula = BankBill ~ AllOrds+Transport+Unemploy+Retail, data = mydata)
summary(model2)
#modle3&confint 95%interval
model3 <- lm(formula = BankBill ~ AllOrds+Unemploy+Retail, data = mydata)   
summary(model3)
confint(model3)
#No AllOrds
model4 <- lm(formula = BankBill ~ Unemploy+Retail, data = mydata)   
summary(model4)
RSS_AllOrds<-0.9351^2*68
RSS_AllOrds
#No Unemploy
model5 <- lm(formula = BankBill ~ AllOrds+Retail, data = mydata)   
summary(model5)
RSS_Unemploy<-1.104^2*68
RSS_Unemploy
#No Retail
model6 <- lm(formula = BankBill ~ AllOrds+Unemploy, data = mydata)   
summary(model6)
RSS_Retail<-0.5058^2*68
RSS_Retail
#mean_response
mean_response <- 28.07-1.153e-03*mean(mydata$AllOrds) -1.697e+00*mean(mydata$Unemploy)+1.820e-03*mean(mydata$Retail)
mean_response
#mean_response_2.5
mean_response_2.5 <- 26.354231060-0.001270356*mean(mydata$AllOrds) -1.839826057*mean(mydata$Unemploy)+0.001371524*mean(mydata$Retail)
mean_response_2.5
#mean_response_97.5
mean_response_97.5 <- 29.794629999-0.001036079*mean(mydata$AllOrds) -1.554775835*mean(mydata$Unemploy)+0.002268962*mean(mydata$Retail)
mean_response_97.5
#real mean y
meany <- mean(mydata$BankBill)
meany
#predict model3 coefficients more number to accurate
mean_response_real <- model3$coefficients[1]+model3$coefficients[2]*mean(mydata$AllOrds) +model3$coefficients[3]*mean(mydata$Unemploy)+model3$coefficients[4]*mean(mydata$Retail)
mean_response_real
#Plot error interval
summary(model3)
R.se = 0.3622
up.bound=model3$residuals+2*R.se
error = model3$residuals
low.bound=model3$residuals-2*R.se
plot(x ="data", y = "error", xlim = c(1,71),ylim = c(-2,2))
lines(x = up.bound, lty=3)
lines(x = error)
lines(x = low.bound, lty=3)



