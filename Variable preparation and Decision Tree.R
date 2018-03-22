
# useful library
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load file
setwd(dir = "C://Users/aficcadenti/Documents/R/")
import<-read.csv(file = "C://Users/aficcadenti/Documents/R/login Decision Tree/data.csv")
#import<-read.csv(file = file.choose())

# remove brackets
import$Custom.Output.18<-gsub(pattern = "[{\"]", replacement = "", x = import$Custom.Output.18)
import$Custom.Output.18<-gsub(pattern = "[}\"]", replacement = "", x = import$Custom.Output.18)
import$Reasons<-gsub(pattern = "[{]", replacement = "", x = import$Reasons)
import$Reasons<-gsub(pattern = "[}]", replacement = "", x = import$Reasons)
import$InputIpZeroScore<-gsub(pattern = "[}]", replacement = "", x = import$Reasons)
import$Event.Time<-as.POSIXct(substr(import$Event.Time,1,19), format = "%Y-%m-%d %H:%M:%S")

#================================================
# create new columns for the following variables:
#================================================

# account in watchlist
import$watchlist<-grepl("Account in Watchlist", import$Reasons)

# Brand Region
import$Brand.Region<-import$Custom.Attribute.2
import$Brand.Region<-gsub(pattern = "abritel|homelidays_fr", replacement = "France", x = import$Brand.Region)
import$Brand.Region<-gsub(pattern = "homeaway_uk|odhr|homeaway_no", replacement = "UK", x = import$Brand.Region)
import$Brand.Region<-gsub(pattern = "homeaway_us|vrbo", replacement = "UK", x = import$Brand.Region)

#day of the week
import$Weekday<-format(import$Event.Time,format='%w')

#hour of the day
import$hour<-format(import$Event.Time,format='%H')

# ...


#first model
fit <- rpart(Final.Review.Status ~ Custom.Attribute.1 + Agent.Type + Input.IP.Score + Input.IP.Worst.Score,
             data=import,
             method="class")

			 
fancyRpartPlot(fit)
#second model
fit <- rpart(Final.Review.Status ~ Brand.Region + weekday + hour + True.IP.Routing.Type + Agent.Type + Input.IP.Score + Input.IP.Worst.Score + watchlist,
             data=import,
             method="class")
