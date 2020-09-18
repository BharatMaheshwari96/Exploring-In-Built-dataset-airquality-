Data <- airquality
Data
###FIRST PART ###################
##__Feature of Dataset__ 
VIEW<-View(Data)
VIEW
S_D<-sd(Data$Wind)
S_D
VAR<-var(Data$Wind)
VAR
MAXIMUM<-max(Data$Wind)
MAXIMUM
MIN<-min(Data$Wind)
MIN
HEAD<-head(Data)
HEAD
SUMMARY<-summary(Data)
SUMMARY
# compute 3sigma:

xL=mean(Data$Wind)-3*S_D
xU=mean(Data$Wind)+3*S_D
int=c(xL,xU)
int
T1=Data$Wind
T2=Data$Temp
mean(T1)
mean(T2)
Tbar1=mean(T1)
Tbar2=mean(T2) # average
s1=sd(T1) # standard deviation
s2=sd(T2)

# threesigma interval 

L1=Tbar1-3*s1
U1=Tbar1+3*s1
int1=c(L1,U1)
int1
# data normalization
z1=(T1-Tbar1)/s1
z2=(T2-Tbar2)/s2

df=data.frame(z1,z2)
View(df)


# Simple Bar Plot 
counts <- table(Data$Wind)
barplot(counts, main="Air quality", 
        xlab="Wind")


#####SECOND PART ###############
#Cleansing Data
any(is.na(Data))
clean<- Data
clean <- na.omit(clean)
View(clean)

########THIRD PART##########
#central and variational measures.
Sort <- sort(Data$Temp)
Sort
Mean <- mean(Data$Temp)
Mean
Median <- median(Data$Temp)
Median
Mode <-names(table(Data$Temp))[(table(Data$Temp))==max(table(Data$Temp))]
Mode
Summary <-summary(Data$Temp)
Summary


######FOURTH PART#############
#Box plot
boxplot(Data$Wind)
#to get the value of outliers
outliers<-boxplot(Data$Wind )$out
outliers
#### Boxplot after removing outliers#########
Data <- Data[-which(Data$Wind %in% outliers),]
boxplot(Data$Wind)