getwd()
list.files()
chi=read.csv('chicago.csv')
ny=read.csv('new_york_city.csv')
wash=read.csv('washington.csv')

library(ggplot2)


#Q1 
getwd()
list.files()
ny=read.csv('new_york_city.csv')
library(ggplot2)
#In order to make my analysis more intuitively, I would like to Use minutes instead of seconds to indicate duration
ggplot(aes(x=Gender,y=Trip.Duration/60),data = subset(ny,Gender!=""))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(0,60,5))+
  coord_cartesian(ylim = c(0,60))+
  ggtitle('Usage Duration In The NY')+
  labs(y='Duration In Minutes')
# Statistic summary on the duration in the New York
by(ny$Trip.Duration/60,ny$Gender,summary)


#Q2
getwd()
list.files()
wash=read.csv('washington.csv')
library(ggplot2)

c.start=as.data.frame(table(wash$Start.Station))
#Find out the start station with high shared bikes demand
c.start.plus=subset(c.start,Freq>=1000)
summy.fun<-c('min','25th percentile','mean','median','75th percentile','max')
#I applied round()below since bikes can only be borrowed from start point interger times like 1 time, 6 times.
summy.idx<-c(min(c.start$Freq),quantile(c.start$Freq,c(.25)),round(mean(c.start$Freq),digits = 0),
             median(c.start$Freq),quantile(c.start$Freq,c(.75)),max(c.start$Freq))
summy<-data.frame(Summary=summy.fun,Frequency=summy.idx)
# Extract the location
x<-c.start.plus[,1]
# Extract the frequency of borrowing from the station above
y<-c.start.plus[,2]
df<-data.frame(x=x,y=y)
ggplot(mapping = aes(x = x, y = y), data = df) + 
  geom_bar(stat = 'identity',color=I('black'),fill=I('#099DD9'))+
  scale_y_continuous(breaks = seq(0,1700,100))+
  ggtitle('Popular start stations in Washington')+
  labs(x='Station',y='Frequency')+
  theme(axis.text.x=element_text(angle = 90))
# Statistic summary on the frequency of borrowing in the Washington base on each start station
summy

#Q3
getwd()
list.files()
chi=read.csv('chicago.csv')
library(ggplot2)
#substract the hour from start time
hour=substr(chi$Start.Time,12,13)
ggplot(aes(x=hour,y=Trip.Duration/60),data = chi)+
  geom_point(alpha=1/10,position = position_jitter(h=0),color='orange')+
  coord_cartesian(ylim = c(0,60))+
  ggtitle('Demand for shared bike in Chicago')+
  scale_y_continuous(breaks = seq(0,60,10))+
  labs(x="Time",y="Duration in minute")

correlation=cor(h,chi$Trip.Duration/60,method = c("pearson", "kendall", "spearman"))
summy.fun2<-c('min','25th percentile','mean','median','75th percentile','max','corr')
#I applied the round()function again to make the result more reasonable since bicycles can only be borrow interger times
summy.idx2<-c(min(table(hour)),round(quantile(table(hour),c(.25)),digits = 0),round(mean(table(hour)),digits = 0),
              round(median(table(hour)),digits = 0),round(quantile(table(hour),c(.75)),digits = 0),max(table(hour)),round(correlation,digits = 2))
summy2<-data.frame(Summary=summy.fun2,Frequency=summy.idx2)
#Count how many bikes have been lent in each hour in the Chicago?
table(hour)
#Statistic summary on how many bike have been lent base on hour in Chicago.
summy2

version control is 