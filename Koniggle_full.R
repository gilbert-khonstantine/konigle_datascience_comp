raw_data = read.csv("~/Downloads/train_data.csv")
invoice_date = raw_data$InvoiceDate 
invoice_date = as.Date(invoice_date, "%Y-%m-%d")
raw_data$InvoiceDate = invoice_date
#fill empty entries with median
raw_data$Quantity[is.na(raw_data$Quantity)] <- median(raw_data$Quantity, na.rm=TRUE)
temp_data = aggregate(raw_data$Quantity, by=list(raw_data$InvoiceDate), sum)
x = temp_data$x[-287]
y = temp_data$x[-1]

library(tseries)
adf.test(temp_data$x)
#using adf.test from R tseries library, we can say that it is not stationary, thus we can just model the random noises.
data = data.frame(x,y)
x = diff(data$x)
y = diff(data$y)

#Using the past 8 values as the features 
lag1 = x
lag2 = lag1[-1]
lag3 = lag2[-1]
lag4 = lag3[-1]
lag5 = lag4[-1]
lag6 = lag5[-1]
lag7 = lag6[-1]
lag8 = lag7[-1]
lag1 = lag1[-c(length(lag8)+1:length(lag1))]
lag2 = lag2[-c(length(lag8)+1:length(lag2))]
lag3 = lag3[-c(length(lag8)+1:length(lag3))]
lag4 = lag4[-c(length(lag8)+1:length(lag4))]
lag5 = lag5[-c(length(lag8)+1:length(lag5))]
lag6 = lag6[-c(length(lag8)+1:length(lag6))]
lag7 = lag7[-c(length(lag8)+1:length(lag7))]
lag8 = lag8[-c(length(lag8)+1:length(lag8))]

x_train = data.frame(lag1,lag2,lag3,lag4,lag5,lag6,lag7,lag8)

y_train = y[-c(1:7)]
library(e1071)
svm_lag = svm(array(x_train),y_train,cost = 0.01,kernel = "radial",scale = TRUE)
library(ModelMetrics)
rmse(predict(svm_lag,x_train),y_train)
Quantity1 = as.numeric(c(x_train[278,c(2:8)],y_train[278]) )
Quantity=c()
for (i in c(1:21)){
  Quantity= c(Quantity, predict(svm_lag, t(data.frame(x=c(Quantity1[c(i:(i+7))]))) ))
  #Quantity1 = as.integer(round(Quantity1))
  Quantity1= c(Quantity1, predict(svm_lag, t(data.frame(x=c(Quantity1[c(i:(i+7))]))) ))
  Quantity = as.integer(round(Quantity))
}
Quantity1
Quantity

#Use the first starting point as the median.
Quantity[1]=Quantity[1]+median(data$x)
for (i in c(2:21)){
  Quantity[i] = Quantity[i]+Quantity[i-1]
}
Quantity
Date = seq(as.Date("2011/11/19"), by = "day", length.out = 21)
result = data.frame(Date,Quantity)
result
write.csv(result,"~/Desktop/submission.csv",row.names = FALSE)

