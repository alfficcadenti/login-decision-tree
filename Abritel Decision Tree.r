
# libraries
library(caret)
library(plyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(pROC)
library(ROCR)
library(corrplot)

# Load reviewed file
setwd(dir = "C://Users/aficcadenti/Documents/R/login Decision Tree/")
import<-read.csv(file = "C://Users/aficcadenti/Documents/R/login Decision Tree/abritel login FebMarch.csv")
#import<-read.csv(file = file.choose())

#rename dataset
abritel_data<-import

#set accept as label to the not reviewed login
abritel_data$Final.Review.Status[abritel_data$Final.Review.Status==""]<-"accept"
abritel_data$Final.Review.Status<-factor(abritel_data$Final.Review.Status)

summary(abritel_data$Final.Review.Status)

abritel_data$Final.Review.Status.Numeric[abritel_data$Final.Review.Status=="accept"]<-0
abritel_data$Final.Review.Status.Numeric[abritel_data$Final.Review.Status=="reject"]<-1

# remove brackets and data cleaning
abritel_data$Custom.Output.18<-gsub(pattern = "[{\"]", replacement = "", x = abritel_data$Custom.Output.18)
abritel_data$Custom.Output.18<-gsub(pattern = "[}\"]", replacement = "", x = abritel_data$Custom.Output.18)
abritel_data$Reasons<-gsub(pattern = "[{]", replacement = "", x = abritel_data$Reasons)
abritel_data$Reasons<-gsub(pattern = "[}]", replacement = "", x = abritel_data$Reasons)
abritel_data$InputIpZeroScore<-gsub(pattern = "[}]", replacement = "", x = abritel_data$Reasons)
abritel_data$Event.Time<-as.POSIXct(substr(abritel_data$Event.Time,1,19), format = "%Y-%m-%d %H:%M:%S")




#================================================
# create new columns for the following variables:
#================================================


# account in watchlist
abritel_data$watchlist<-grepl("Account in Watchlist", abritel_data$Reasons)

# account is older than 30 days
abritel_data$OldAccount<-grepl("Pattern Account is Old", abritel_data$Reasons)

# Account with 4 Consistent Attributes
abritel_data$ConsistentAttribs<-grepl("Account with 4 Consistent Attributes", abritel_data$Reasons)

# account is older than 3 months
abritel_data$Account3MOld<-grepl("Account Number is older 3M", abritel_data$Reasons)

# account is older than 12 months
abritel_data$Account12MOld<-grepl("Account Number is older 12M", abritel_data$Reasons)

# IP already used for accounts
abritel_data$IPalreadyUsed<-grepl("Input IP to Account GT 10 Passed 1weekOFF", abritel_data$Reasons)

# New Device to Account
abritel_data$NewDeviceToAccount<-grepl("New Device to Account in a Hr (Smart ID)", abritel_data$Reasons)

# Input IP age less than a week
abritel_data$NewInputIPLTweek<-grepl("Input IP LT Week", abritel_data$Reasons)

# DNS new to Account
abritel_data$DNSNewAccount<-grepl("Account Number and DNS IP LT 2", abritel_data$Reasons)

# Profiling Incomplete
abritel_data$ProfIncomplete<-grepl("Profiling Incomplete", abritel_data$Reasons)

# Profiling Blocked 
abritel_data$ProfBlocked<-grepl("Profiling Blocked", abritel_data$Reasons)

# New IP GEO for Account 
abritel_data$NewIPGEO<-grepl("New IP GEO for Account", abritel_data$Reasons)

# IF CusAttr7 seen GT 2 Accounts
abritel_data$CustAttr7MultipleAccounts<-grepl("IF CusAttr7 seen GT 2 Accounts", abritel_data$Reasons)

# IF browser language contains EN
abritel_data$BrowserLangEN<-grepl("browser language contains en", abritel_data$Reasons)

# IF browser language suspicious for Abritel
abritel_data$BrowserLangSuspAbritel<-grepl("browser language suspicious for Abritel", abritel_data$Reasons)


#day of the week
abritel_data$Weekday<-format(abritel_data$Event.Time,format='%w')
#table(abritel_data$Weekday)

#hour of the day
abritel_data$hour<-format(abritel_data$Event.Time,format='%H')

# other variables...

# Save in a file
write.csv(abritel_data, file = "C://Users/aficcadenti/Documents/R/login Decision Tree/abritel preprocessed.csv")

#===========
#Correlation
#===========

#convert data frame to data matrix to force all columns as numeric
abritel_matrix<-data.matrix(abritel_sample, rownames.force = NA)
corrplot(cor(abritel_matrix,use = "complete.obs"),type = "lower")

#
abritelcor<-c()
for (i in 1:ncol(train)){
    abritelcor$colnames(train[i])<-cor(train$Final.Review.Status.Numeric,as.numeric(train[,i]),use = "complete.obs")
}
abritelcor


#correlation only on numeric columns
numcol<-c()
for (i in 1:ncol(train)){
    numcol[i]<-(is.numeric(train[,i]))
}
M<-cor(train[numcol],use = "complete.obs")
corrplot(M, method="circle",type="lower")


#====================================
# sampling for train and test dataset
#====================================

#Stratified sampling from the whole dataset
sample_index <- createDataPartition(abritel_data$Final.Review.Status, p = .8, list = FALSE)
train <- abritel_data[ sample_index,]
test  <- abritel_data[-sample_index,]


#to verify distribution of the samples
summary(train$Final.Review.Status)
summary(train$Final.Review.Status)/dim(train)[1]

summary(test$Final.Review.Status)
summary(test$Final.Review.Status)/dim(test)[1]

#======== Undersampling
#reduce the dataset to have only 2.5% of the accept and 100% of the reject
abritel_accept <- abritel_data[ which(abritel_data$Final.Review.Status=='accept'),]
indexes_accept <- sample(1:nrow(abritel_accept), size=0.025*nrow(abritel_accept))
abritel_reject <- abritel_data[ which(abritel_data$Final.Review.Status=='reject'),]
abritel_sample<-rbind(abritel_data[indexes_accept,],abritel_reject)

#Perform stratified sampling on the subset 80/20
sample_index <- createDataPartition(abritel_sample$Final.Review.Status, p = .8, list = FALSE)
train <- abritel_sample[ sample_index,]
test  <- abritel_sample[-sample_index,]



#==============
#Model Building
#==============

#first model
fit <- rpart(Final.Review.Status ~ Custom.Attribute.1 + Agent.Type + Input.IP.Score + Input.IP.Worst.Score,
             data=train,
             method="class")

			 
fancyRpartPlot(fit,palettes=c("Greens"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))


#second model
fit <- rpart(Final.Review.Status ~ Custom.Attribute.1 + OldAccount + Weekday + hour + True.IP.Routing.Type + Agent.Type + Input.IP.Score + Input.IP.Worst.Score + watchlist,
             data=train,
             method="class")

fancyRpartPlot(fit,palettes=c("Greens"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))

#third model
fit <- rpart(Final.Review.Status ~ Custom.Attribute.1 + Account3MOld + Account12MOld + Weekday + hour + True.IP.Routing.Type + Agent.Type + Input.IP.Worst.Score + watchlist + NewInputIPLTweek + ProfIncomplete + ProfBlocked + IPalreadyUsed + DNSNewAccount,
             data=train,
             method="class")

fancyRpartPlot(fit,palettes=c("Greens","Reds"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))

#forth model all variables)
fit <- rpart(Final.Review.Status ~ Custom.Attribute.1 + Account3MOld + Account12MOld + Weekday + hour + True.IP.Routing.Type + Agent.Type + Input.IP.Worst.Score + watchlist + NewInputIPLTweek + ProfIncomplete + ProfBlocked + IPalreadyUsed + DNSNewAccount + True.IP.Geo + Input.IP.Worst.Score + BrowserLangEN + True.IP.Result +BrowserLangSuspAbritel, data=train,method="class")

fancyRpartPlot(fit,palettes=c("Greens","Reds"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))

#forth model (based on the correlation > 0.2)
fit <- rpart(Final.Review.Status ~ True.IP.Geo + Input.IP.Worst.Score + BrowserLangEN + True.IP.Result +BrowserLangSuspAbritel, data=train,method="class")

fancyRpartPlot(fit,palettes=c("Greens","Reds"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))

#forth model (based on the correlation + rules new profile (device IP)) SAME SCORE as the PREVIOUS
fit <- rpart(Final.Review.Status ~ True.IP.Geo + Input.IP.Worst.Score + BrowserLangEN + True.IP.Result +BrowserLangSuspAbritel + NewDeviceToAccount + NewInputIPLTweek, data=train,method="class")

fancyRpartPlot(fit,palettes=c("Greens","Reds"),sub=paste("Decision Tree ATO Detection model ",format(Sys.time(), "%Y %b %d %a %X "),sep=" "))



#==================
# Model Performance
#==================

pred = predict(fit, newdata=test, type = 'class')
#ROCRpred<-prediction(pred,test$Final.Review.Status)
confusionMatrix(data=pred, test$Final.Review.Status, positive="reject")
roc(as.numeric(pred),as.numeric(test$Final.Review.Status))
plot.roc(as.numeric(pred),as.numeric(test$Final.Review.Status))