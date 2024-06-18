library(car)

fit=lm(Price~.,data=hd_train)
summary(fit)
fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda,data=hd_train)

t=vif(fit)
sort(t,decreasing=T)[1:3]

fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda-Method_S,data=hd_train)

t=vif(fit)

sort(t,decreasing=T)[1:3]

fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda-Method_S-Postcode_3121,data=hd_train)

t=vif(fit)

sort(t,decreasing=T)[1:3]

summary(fit)

fit=step(fit)

summary(fit)

train_res=cbind.data.frame(Actual=hd_train$Price,Fitted=fitted(fit),Error=residuals(fit))
View(train_res)

rmse_train=sqrt(mean(train_res$Error^2))
rmse_train

library(ggplot2)
ggplot(train_res,aes(x=Actual,y=Fitted))+geom_point()

#Error~N(0,sigma2)
ggplot(train_res,aes(Error))+geom_histogram()

#Predicted vs error - Homoscedasticity/Independence of errors
ggplot(train_res,aes(x=Fitted,y=Error))+geom_point()

# Testing on hd_train2:

Price_predict=predict(fit,newdata=hd_train2)
TestRes = cbind.data.frame(Act=hd_train2$Price,Pred=Price_predict,residuals=hd_train2$Price-Price_predict)
View(TestRes)

rmse_test1=sqrt(mean(TestRes$residuals^2))
rmse_test1

# Final Model:

final_fit=lm(Price~.,data=hd)
summary(final_fit)
final_fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda,data=hd)

t=vif(final_fit)
sort(t,decreasing=T)[1:3]

final_fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda-Method_S,data=hd)

t=vif(final_fit)
sort(t,decreasing=T)[1:3]

final_fit=lm(Price~.-Postcode_other-YearBuilt_Other-Suburb_BentleighEast-Suburb_Reservoir-Suburb_StKilda-Method_S-Suburb_Richmond,data=hd)

t=vif(final_fit)
sort(t,decreasing=T)[1:3]

summary(final_fit)

final_fit=step(final_fit)

summary(final_fit)

final_fit=lm(Price ~ Rooms + Distance + Bathroom + Car + Landsize + Bedroom + 
               buildingArea + Type_h + Type_u + Method_PI + Method_SP + 
               Method_VB + YearBuilt_1970 + YearBuilt_1960 + YearBuilt_1950 + 
               YearBuilt_1900 + YearBuilt_1930 + YearBuilt_1920 + YearBuilt_1910 + 
               YearBuilt_1890 + Postcode_3073 + Postcode_3020 + Postcode_3046 + 
               Postcode_3032 + Postcode_3040 + Postcode_3058 + CouncilArea_Boroondara + 
               CouncilArea_Moreland + CouncilArea_MooneeValley + CouncilArea_Darebin + 
               CouncilArea_GlenEira + CouncilArea_Stonnington + CouncilArea_Bayside + 
               CouncilArea_Maribyrnong + CouncilArea_PortPhillip + CouncilArea_missing + 
               CouncilArea_other + Suburb_Preston + Suburb_other + SellerG_Nelson + 
               SellerG_Jellis + SellerG_hockingstuart + SellerG_Barry + 
               SellerG_Marshall + SellerG_Buxton, data=hd)

train_resi=cbind.data.frame(Actual=hd$Price,Fitted=fitted(final_fit),Error=residuals(final_fit))
View(train_resi)

rmse_train1=sqrt(mean(train_resi$Error^2))
rmse_train1

library(ggplot2)
ggplot(train_resi,aes(x=Actual,y=Fitted))+geom_point()

#Error~N(0,sigma2)
ggplot(train_resi,aes(Error))+geom_histogram()

#Predicted vs error - Homoscedasticity/Independence of errors
ggplot(train_resi,aes(x=Fitted,y=Error))+geom_point()

# Predicting values of final test data:

Price_predict1=predict(final_fit,newdata=hd_test)
TestResult = cbind.data.frame(hd_test,Price=Price_predict1)
View(TestResult)
