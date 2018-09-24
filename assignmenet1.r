if(!file.exists("activity.csv"))
  unzip("activity.zip",exdir = ".")

Df <- read.csv("activity.csv")
sum <- tapply(Df$steps,Df$date,sum)
hist(sum)
mean(sum,na.rm= T)
median(sum,na.rm = T)

#Average number of steps all across days for each of the interval
avg <- tapply(Df$steps,Df$interval,mean,na.rm=T)
plot(names(avg),avg,type = "l",xlab="Interval",ylab = "Average number of steps", main ="Average steps across all days for 5 min inerval")
maxval<- which(avg == max(avg))
#interval with maximum number of steps
names(maxval)
######Total numer of missing values
newDf <- Df
sum(is.na(newDf$steps))
###impute
impute <- function(x) x <- replace(x, is.na(x), mean(x,na.rm = T))
dummy <- tapply(newDf$steps,list(newDf$interval),impute)
ddf <- data.frame(steps=unlist(dummy),date=rep(seq(as.Date("2012-10-01"),as.Date("2012-11-30"),by = "day"),288),interval = rep(as.numeric(names(dummy)),each = 61))
#ddf <- ddf[order(ddf$date),]

sum <- tapply(ddf$steps,ddf$date,sum)
head(sum)
hist(sum)
mean(sum,na.rm= T)
median(sum,na.rm = T)

#ddf$date = as.date(ddf.date)
ddf$day <- weekdays(ddf$date)
ddf_wkday <- subset(ddf,!day %in% c("Saturday","Sunday"))
ddf_wkend <- subset(ddf,day %in% c("Saturday","Sunday"))


avgwkday <- tapply(ddf_wkday$steps,ddf_wkday$interval,mean,na.rm=T)
avgwkend <- tapply(ddf_wkend$steps,ddf_wkend$interval,mean,na.rm=T)
par(mfrow=c(2,1))
plot(names(avgwkday),avgwkday,type = "l",xlab="Interval",ylab = "Average number of steps", main ="weekday")
plot(names(avgwkend),avgwkend,type = "l",xlab="Interval",ylab = "Average number of steps", main ="weekend")

