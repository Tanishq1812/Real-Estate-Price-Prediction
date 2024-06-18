# Random Forest:
library(randomForest)
rf_hd=randomForest(Price~.,data=hd_train,do.trace=T)
rf_hd

train_hd=predict(rf_hd,data=hd_train)

rmse_train=sqrt(mean((train_hd-hd_train$Price)^2))
rmse_train

train2_hd=predict(rf_hd,newdata=hd_train2)
rmse_train=sqrt(mean((train2_hd-hd_train2$Price)^2))
rmse_train

rf_hd_final=randomForest(Price~.,data=hd,do.trace=T)
rf_hd_final
train_hd=predict(rf_hd_final,data=hd)
rmse_train=sqrt(mean((train_hd-hd$Price)^2))
rmse_train

Predicted_Price=predict(rf_hd_final,newdata=hd_test)
Predicted_Price
Final_Output=cbind.data.frame(hd_test,Predicted_Price)
View(Final_Output)

write.csv(Final_Output,"Tanishq_Rastogi_P1_Part2.csv")
