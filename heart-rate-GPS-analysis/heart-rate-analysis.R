# MSBX 5410 - Final Project
# Adam Resnick
# 08/17/2018

########################################################
## Reading in Data
########################################################
setwd("C:\\Users\\Adam\\OneDrive\\SCHOOL\\MSBX 5410\\Projects")
wo1RAW <-read.csv("2018-08-07 - 0629.csv", header=TRUE, stringsAsFactors = FALSE)
wo2RAW <-read.csv("2018-08-09 - 1958.csv", header=TRUE, stringsAsFactors = FALSE)
wo3RAW <-read.csv("2018-08-13 - 1705.csv", header=TRUE, stringsAsFactors = FALSE)


########################################################
# Workout 1 Code
########################################################

wo1 <- wo1RAW

#deleting unnecessary columns in wo1
wo1$Sport <- NULL; wo1$ï..Sport <- NULL; wo1$ns1.TriggerMethod <- NULL; wo1$ns1.Name <- NULL; wo1$ns1.TotalTimeSeconds <- NULL
wo1$ns1. <- NULL; wo1$ns1.DistanceMeters <- NULL; wo1$ns1.UnitId <- NULL; wo1$ns1.ProductID <- NULL
wo1$ns1.Calories <- NULL; wo1$ns1.Id <- NULL; wo1$StartTime <- NULL; wo1$ns1.Intensity <- NULL

#renaming columns in wo1
names(wo1)[1] <- "DateTime"; names(wo1)[2] <- "Latitude"; names(wo1)[3] <- "Longitude"; names(wo1)[4] <- "Altitude"
names(wo1)[5] <- "Distance"; names(wo1)[6] <- "HR"

#Formatting time
wo1$DateTime <- sub(pattern="T", replace=" ", wo1$DateTime)
wo1$DateTime <- sub(pattern=".00.*", replace="", wo1$DateTime)
wo1$DateTime <- as.POSIXct(wo1$DateTime, format = "%Y-%m-%d %H:%M:%S")
wo1$Date <- sub(pattern=" .*", replace="", wo1$DateTime)
wo1$Date <- as.Date(wo1$Date)
wo1$Time <- sub(pattern=".* ", replace="", wo1$DateTime)

#Eliminating rows with any missing data
wo1 <- wo1[complete.cases(wo1),]

#Adding Speed
wo1$Speed <- rep(NA, length(wo1$DateTime))
for(i in 2:length(wo1$DateTime)){
  wo1$Speed[i] <- (wo1$Distance[i]-wo1$Distance[i-1])*0.0006213712/(as.numeric((wo1$DateTime[i]-wo1$DateTime[i-1]))/3600)
}

#Cleaning up dataframe
wo1 <- wo1[wo1$Speed>0,]        #Gets rid of rows with negative speed (not possible)
wo1 <- wo1[wo1$Speed<15,]        #Gets rid of rows with unrealistic speed
wo1 <- wo1[2:length(wo1$DateTime),]

#Saving Workout1 as CSV
write.csv(wo1, file = ("Workout1.csv"), row.names = FALSE)

#Plotting Lat, Long, HR for Workout 1
ggplot(data=wo1, aes(x=Latitude, y=Longitude, size=HR))+geom_point()+ggtitle("Workout 1 - Mapped HR")
ggsave("Workout1_HR.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed for Workout 1
ggplot(data=wo1, aes(x=Latitude, y=Longitude, color=-Speed))+geom_point()+ggtitle("Workout 1 - Mapped Speed")
ggsave("Workout1_Speed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed, and HR for Workout 1
ggplot(data=wo1, aes(x=Latitude, y=Longitude, color=-Speed, size=HR))+geom_point()+ggtitle("Workout 1 - Mapped HR vs Speed")
ggsave("Workout1_HRSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting HR vs Speed for Workout 1
ggplot(data=wo1, aes(x=Speed, y=HR))+geom_point()+geom_smooth(method="lm", color="blue")+xlab("Speed [mph]")+ylab("HR [BPM]")+ggtitle("Workout 1 - HR vs Speed")
ggsave("Workout1_HRvsSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)






########################################################
# Workout 2 Code
########################################################

wo2 <- wo2RAW

#deleting unnecessary columns in wo2
wo2$Sport <- NULL; wo2$ï..Sport <- NULL; wo2$ns1.TriggerMethod <- NULL; wo2$ns1.Name <- NULL; wo2$ns1.TotalTimeSeconds <- NULL
wo2$ns1. <- NULL; wo2$ns1.DistanceMeters <- NULL; wo2$ns1.UnitId <- NULL; wo2$ns1.ProductID <- NULL
wo2$ns1.Calories <- NULL; wo2$ns1.Id <- NULL; wo2$StartTime <- NULL; wo2$ns1.Intensity <- NULL

#renaming columns in wo2
names(wo2)[1] <- "DateTime"; names(wo2)[2] <- "Latitude"; names(wo2)[3] <- "Longitude"; names(wo2)[4] <- "Altitude"
names(wo2)[5] <- "Distance"; names(wo2)[6] <- "HR"

#Formatting time
wo2$DateTime <- sub(pattern="T", replace=" ", wo2$DateTime)
wo2$DateTime <- sub(pattern=".00.*", replace="", wo2$DateTime)
wo2$DateTime <- as.POSIXct(wo2$DateTime, format = "%Y-%m-%d %H:%M:%S")
wo2$Date <- sub(pattern=" .*", replace="", wo2$DateTime)
wo2$Date <- as.Date(wo2$Date)
wo2$Time <- sub(pattern=".* ", replace="", wo2$DateTime)

#Eliminating rows with any missing data
wo2 <- wo2[complete.cases(wo2),]

#Adding Speed
wo2$Speed <- rep(NA, length(wo2$DateTime))
for(i in 2:length(wo2$DateTime)){
  wo2$Speed[i] <- (wo2$Distance[i]-wo2$Distance[i-1])*0.0006213712/(as.numeric((wo2$DateTime[i]-wo2$DateTime[i-1]))/3600)
}

#Cleaning up dataframe
wo2 <- wo2[wo2$Speed>0,]        #Gets rid of rows with negative speed (not possible)
wo2 <- wo2[wo2$Speed<15,]        #Gets rid of rows with unrealistic speed
wo2 <- wo2[2:length(wo2$DateTime),]

#Saving Workout2 as CSV
write.csv(wo2, file = ("Workout2.csv"), row.names = FALSE)

#Plotting Lat, Long, HR for Workout 2
ggplot(data=wo2, aes(x=Latitude, y=Longitude, size=HR))+geom_point()+ggtitle("Workout 2 - Mapped HR")
ggsave("Workout2_HR.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed for Workout 2
ggplot(data=wo2, aes(x=Latitude, y=Longitude, color=-Speed))+geom_point()+ggtitle("Workout 2 - Mapped Speed")
ggsave("Workout2_Speed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed, and HR for Workout 2
ggplot(data=wo2, aes(x=Latitude, y=Longitude, color=-Speed, size=HR))+geom_point()+ggtitle("Workout 2 - Mapped HR vs Speed")
ggsave("Workout2_HRSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting HR vs Speed for Workout 2
ggplot(data=wo2, aes(x=Speed, y=HR))+geom_point()+geom_smooth(method="lm", color="blue")+xlab("Speed [mph]")+ylab("HR [BPM]")+ggtitle("Workout 2 - HR vs Speed")
ggsave("Workout2_HRvsSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)







########################################################
# Workout 3 Code
########################################################

wo3 <- wo3RAW

#deleting unnecessary columns in wo3
wo3$Sport <- NULL; wo3$ï..Sport <- NULL; wo3$ns1.TriggerMethod <- NULL; wo3$ns1.Name <- NULL; wo3$ns1.TotalTimeSeconds <- NULL
wo3$ns1. <- NULL; wo3$ns1.DistanceMeters <- NULL; wo3$ns1.UnitId <- NULL; wo3$ns1.ProductID <- NULL
wo3$ns1.Calories <- NULL; wo3$ns1.Id <- NULL; wo3$StartTime <- NULL; wo3$ns1.Intensity <- NULL

#renaming columns in wo3
names(wo3)[1] <- "DateTime"; names(wo3)[2] <- "Latitude"; names(wo3)[3] <- "Longitude"; names(wo3)[4] <- "Altitude"
names(wo3)[5] <- "Distance"; names(wo3)[6] <- "HR"

#Formatting time
wo3$DateTime <- sub(pattern="T", replace=" ", wo3$DateTime)
wo3$DateTime <- sub(pattern=".00.*", replace="", wo3$DateTime)
wo3$DateTime <- as.POSIXct(wo3$DateTime, format = "%Y-%m-%d %H:%M:%S")
wo3$Date <- sub(pattern=" .*", replace="", wo3$DateTime)
wo3$Date <- as.Date(wo3$Date)
wo3$Time <- sub(pattern=".* ", replace="", wo3$DateTime)

#Eliminating rows with any missing data
wo3 <- wo3[complete.cases(wo3),]

#Adding Speed
wo3$Speed <- rep(NA, length(wo3$DateTime))
for(i in 2:length(wo3$DateTime)){
  wo3$Speed[i] <- (wo3$Distance[i]-wo3$Distance[i-1])*0.0006213712/(as.numeric((wo3$DateTime[i]-wo3$DateTime[i-1]))/3600)
}

#Cleaning up dataframe
wo3 <- wo3[wo3$Speed>0,]        #Gets rid of rows with negative speed (not possible)
wo3 <- wo3[wo3$Speed<15,]        #Gets rid of rows with unrealistic speed
wo3 <- wo3[2:length(wo3$DateTime),]

#Saving Workout3 as CSV
write.csv(wo3, file = ("Workout3.csv"), row.names = FALSE)

#Plotting Lat, Long, HR for Workout 3
ggplot(data=wo3, aes(x=Latitude, y=Longitude, size=HR))+geom_point()+ggtitle("Workout 3 - Mapped HR")
ggsave("Workout3_HR.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed for Workout 3
ggplot(data=wo3, aes(x=Latitude, y=Longitude, color=-Speed))+geom_point()+ggtitle("Workout 3 - Mapped Speed")
ggsave("Workout3_Speed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting Lat, Long, Speed, and HR for Workout 3
ggplot(data=wo3, aes(x=Latitude, y=Longitude, color=-Speed, size=HR))+geom_point()+ggtitle("Workout 3 - Mapped HR vs Speed")
ggsave("Workout3_HRSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)
#Plotting HR vs Speed for Workout 3
ggplot(data=wo3, aes(x=Speed, y=HR))+geom_point()+geom_smooth(method="lm", color="blue")+xlab("Speed [mph]")+ylab("HR [BPM]")+ggtitle("Workout 3 - HR vs Speed")
ggsave("Workout3_HRvsSpeed.png", width = 6, height = 6, units = "in",
       dpi = 300)



########################################################
# Regression Analysis
########################################################

mod.1 <- lm(formula=HR~Speed, data=wo1)
mod.2 <- lm(formula=HR~Speed, data=wo2)
mod.3 <- lm(formula=HR~Speed, data=wo3)
summary(mod.1)
summary(mod.2)
summary(mod.3)