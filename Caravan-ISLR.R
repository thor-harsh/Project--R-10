install.packages('ISLR')
library(ISLR)
str(Caravan)
summary(Caravan$Purchase)
var(Caravan[,1])
var(Caravan[,2])

# save the Purchase column in a separate variable
purchase <- Caravan[,86]

# Standarize the dataset using "scale()" R function
standardized.Caravan <- scale(Caravan[,-86])

var(standarized.Caravan[,1])
var(standarized.Caravan[,2])


# First 100 rows for test set
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

# Rest of data for training
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

any(is.na(Caravan))

#Building the KNN Model
library(class)
set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=5)
head(predicted.purchase)

misclass.error<-mean(test.purchase!=predicted.purchase)
print(misclass.error)


#Choosing K-value
predicted.purchase<-NULL
error.rate<-NULL
for(i in 1:20)
{
  set.seed(101)
  predicted.purchase<-knn(train.data,test.data,train.purchase,k=i)
  error.rate[i]<-mean(predicted.purchase!=test.purchase)
}
print(error.rate)

#Visualize
k<-1:20
error.df<-data.frame(error.rate,k)
error.df
library(ggplot2)
ggplot(error.df,aes(x=k,y=error.rate)) + geom_point()+
  geom_line(lty='dotted',color='red')
