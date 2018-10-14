#####################################################
## Capstone Project on Mobile App User Segmentation##
#####################################################



####################
##  Used Packages ##
####################
library(lubridate)
library(anytime)
library(plyr)
library(dplyr)
library(ggplot2)
library(devtools)
library(caret)
library(e1071)
library(rpart)
library(vcd)
library(MASS)
library(chron)
library(tidyr)
library( magrittr)
library(mixtools)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(smotefamily)
library(BBmisc )





#############################################
##### Initial EDA and wrangling         #####
#############################################
df_int<-read.csv("df_with_intervals.csv")
df_int$IAT<-df_int$intervals-df_int$duration 
df_int$intervals<-NULL 
length(which(df_int$IAT<0))/nrow(df_int)

df_int<-df_int[df_int$IAT>=0,]
df_int<-df_int[df_int$duration>=5,]
df_int$Unnamed..0<-NULL
df_int$X<-NULL
ggplot(df_int, aes((duration)**0.001))+geom_histogram(bins=200)+ggthemes::theme_economist()+ggtitle("Durations in Session-Based Data")+xlab("Logarithm Transform of Duration")

ggplot(df_int, aes(screens_count))+geom_histogram(bins=200)
summary(df_int$screens_count)
nrow(df_int[df_int$screens_count==1,])

users<-as.data.frame(aggregate(df_int[,c("timestamp","IAT")],by=list(df_int$device_id),FUN="max"))
names(users)<-c("device_id","last_session","max_IAT")
users$R<-rep(max(df_int$timestamp),nrow(users))-users$last_session

df_int$dummy<-rep(1,nrow(df_int))
num_of_sessions<-aggregate(df_int$dummy,by=list(df_int$device_id),FUN="sum")

tot_dur<-aggregate(df_int$duration,by=list(df_int$device_id),FUN="sum")
avges<-aggregate(df_int[,c("duration","screens_count","IAT")],by=list(df_int$device_id),FUN="mean")
users$total_duration<-tot_dur[,2]
users$avg_duration<-avges[,2]
users$avg_screens<-avges[,3]
users$avg_IAT<-avges[,4]
df_int$crash_dummy<-ifelse(df_int$crashed=="t",1,0)
num_of_crashes<-aggregate(df_int$crash_dummy,by=list(df_int$device_id),FUN="sum")
users$num_of_crashes<-num_of_crashes[,2]
users$num_of_sessions<-num_of_sessions[,2]
users$num_of_sessions<-users$num_of_sessions+1
users$crash_rate<-users$num_of_crashes/users$num_of_sessions


##########################
## RFM Customer Analysis##
##########################
q_r<-as.numeric(quantile(users$R,probs=c(0,0.33,0.7,1)))
q_f<-as.numeric(quantile(users$num_of_sessions/6,probs=c(0,0.33,0.7,1)))
q_m<-as.numeric(quantile(users$total_duration,probs=c(0,0.33,0.7,1)))
users$R_score<-ifelse(users$R>=q_r[1] & users$R<q_r[2],3,ifelse(users$R>=q_r[2] & users$R<q_r[3],2,3))
users$F_score<-ifelse(users$num_of_sessions/6>=q_f[1] & users$num_of_sessions/6<q_f[2],1,ifelse(users$num_of_sessions/6>=q_f[2] & users$num_of_sessions/6<q_f[3],2,3))
users$M_score<-ifelse(users$total_duration>=q_m[1] & users$total_duration<q_m[2],1,ifelse(users$total_duration>=q_m[2] & users$total_duration<q_m[3],2,3))
users$RFM<-100*users$R_score+10*users$F_score+users$M_score

##############################
##
users$churn1<-ifelse(users$R>1.1*users$max_IAT,1,0)
table(users$churn1)
users$churn2<-ifelse(users$R/(users$R+users$avg_duration)>1.1*users$avg_IAT/(users$avg_IAT+users$avg_duration),1,0)
table(users$churn2)
ggplot(users,aes(log(avg_screens)))+geom_histogram(bins=200)
ggplot(users,aes(log(avg_IAT)))+geom_histogram(bins=200)
ggplot(users,aes(avg_duration,num_of_sessions))+geom_point()
ggplot(users,aes(log(num_of_sessions)))+geom_histogram(bins=200)
ggplot(users,aes(num_of_sessions,total_duration))+geom_point()

########################################################################
#  outlier detection using visualization techniques   ##################
########################################################################

# univariate outliers
ggplot(users,aes(1,avg_duration))+geom_boxplot()+xlab("")+ylab("Average Duration")+ggtitle("Boxplot of Average Session Duration")+ggthemes::theme_economist()
ggplot(users,aes(1,avg_screens))+geom_boxplot()+xlab("")+ylab("Average Screens Generated")+ggtitle("Boxplot of Average Screens Generated")+ggthemes::theme_economist()
ggplot(users,aes(1,num_of_sessions))+geom_boxplot()+xlab("")+ylab("Number of Sessions")+ggtitle("Boxplot of Number of Sessions")+ggthemes::theme_economist()

ggplot(users,aes(avg_duration))+geom_histogram(bins=200)

ggplot(users,aes(x=1,avg_duration))+geom_boxplot()
summary(users$avg_duration)
ggplot(users,aes(avg_screens))+geom_histogram(bins=200)
boxplot(users$avg_duration)
hist(users$avg_duration)
qqnorm(users$avg_duration, plot.it = TRUE)
##can't distingush outliers as mixture is suspected
# but these two can be eliminated for sure
ggplot(users,aes(num_of_sessions,total_duration))+geom_point(colour=users$RFM)
users<-filter(users,num_of_sessions<7500)
users<-filter(users,total_duration<1100000)
users<-filter(users,num_of_sessions<5000)
ggplot(users_0,aes(avg_duration,avg_screens))+geom_point()
ggplot(users,aes(avg_duration,avg_screens))+geom_point()

test<-users[1:5,]
gm<-npEM(users[,c(3:8,10,15)],mu0=2)

sub<-read.csv("subgrouped.csv")
df_int$device_id<-as.character(df_int$device_id)
df_int$device_id<-as.factor(df_int$device_id)


which<-c()
for(i in 1: nrow(num_of_sessions)){
  which[i]<-sum(num_of_sessions$x[1:i])
}
last_session_dur<-df_int[which,c("device_id","duration")]
sub<-join(sub,last_session_dur)
sub$X<-NULL
sub$last_session_dur<-sub$duration
sub$duration<-NULL
sub$Unnamed..0<-NULL
users_0<-sub[sub$gmm_label==0,-c(1,2)]
users_1<-sub[sub$gmm_label==1,-c(1,2)]
users_2<-sub[sub$gmm_label==2,-c(1,2)]
users_3<-sub[sub$gmm_label==3,-c(1,2)]
summary(users_3[,c("avg_duration","avg_IAT")])
summary(users_2[,c("avg_duration","avg_IAT")])
summary(users_1[,c("avg_duration","avg_IAT")])
summary(users_0[,c("avg_duration","avg_IAT")])



################################
# Handling outlier per subgroup#
################################
ggplot(users_0,aes(log(avg_duration)))+geom_histogram(bins=200)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
ggplot(users_0,aes(sample=log(avg_duration)))+stat_qq()+ggthemes::theme_economist()+ggtitle("QQ Plot")

ggplot(users_1,aes(log(avg_duration)))+geom_histogram(bins=200)+ggtitle("Moderate Users")+ggthemes::theme_economist()+xlab("Average Duration")
ggplot(users_1,aes(sample=log(avg_duration)))+stat_qq()+ggthemes::theme_economist()+ggtitle("QQ Plot")

########
#  !!  #
########
users_2[which(log(users_2$avg_duration)<3.5),"avg_duration"]<-mean(users_2$avg_duration)
ggplot(users_2,aes(log(avg_IAT)))+geom_histogram(bins=200)+ggtitle("Average Users")+ggthemes::theme_economist()+xlab("Average IAT")
ggplot(users_2,aes(sample=log(avg_IAT)))+stat_qq()+ggthemes::theme_economist()+ggtitle("QQ Plot")

users_2<-users_2[log(users_2$avg_IAT)>9,]
set.seed(22)
index<-sample(which(log(users_2$avg_IAT)>12 & log(users_2$avg_IAT)<12.1),250)
users_2<-users_2[-index,]

ggplot(users_1,aes(log(avg_duration)))+geom_histogram(bins=200)+ggtitle("Average Session Duration")+ggthemes::theme_economist()
ggplot(users_1,aes(sample=log(avg_duration)))+stat_qq()+ggthemes::theme_economist()+ggtitle("")


ggplot(users_3,aes(log(total_duration)))+geom_histogram(bins=200)+ggtitle("Heavy Users")+ggthemes::theme_economist()+xlab("Total Duration")
ggplot(users_3,aes(sample=log(total_duration)))+stat_qq()+ggthemes::theme_economist()+ggtitle("QQ Plot")


ggplot(users_1,aes(avg_duration))+geom_histogram(bins=200)+ggtitle("Average Session Duration")+ggthemes::theme_economist()
ggplot(users_2,aes(avg_duration))+geom_histogram(bins=200)+ggtitle("Average Session Duration")+ggthemes::theme_economist()
ggplot(users_3,aes(avg_duration))+geom_histogram(bins=200)+ggtitle("Average Session Duration")+ggthemes::theme_economist()


users_0$log_dur<-log(users_0$avg_duration)
users_0[which(users_0$log_dur>mean(users_0$log_dur)+3*sd(users_0$log_dur)),"avg_duration"]<-mean(users_0$avg_duration)
users_0[which(users_0$log_dur<mean(users_0$log_dur)-3*sd(users_0$log_dur)),"avg_duration"]<-mean(users_0$avg_duration)
users_0[which(users_0$avg_duration>mean(users_0$avg_duration)+3*sd(users_0$avg_duration)),"avg_duration"]<-mean(users_0$avg_duration)
users_0[which(users_0$avg_duration<mean(users_0$avg_duration)-3*sd(users_0$avg_duration)),"avg_duration"]<-mean(users_0$avg_duration)
ggplot(users_0,aes(avg_duration))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")

users_0$log_scr<-log(users_0$avg_screens)
users_0[which(users_0$log_scr>mean(users_0$log_scr)+3*sd(users_0$log_scr)),"avg_screens"]<-mean(users_0$avg_screens)
users_0[which(users_0$log_scr<mean(users_0$log_scr)-3*sd(users_0$log_scr)),"avg_screens"]<-mean(users_0$avg_screens)

users_0[which(users_0$avg_screens>mean(users_0$avg_screens)+3*sd(users_0$avg_screens)),"avg_screens"]<-mean(users_0$avg_screens)
users_0[which(users_0$avg_screens>mean(users_0$avg_screens)+3*IQR(users_0$avg_screens)),"avg_screens"]<-mean(users_0$avg_screens)


users_0$log_tot<-log(users_0$total_duration)
users_0[which(users_0$log_tot>mean(users_0$log_tot)+3*sd(users_0$log_tot)),"total_duration"]<-mean(users_0$total_duration)
users_0[which(users_0$log_tot<mean(users_0$log_tot)-3*sd(users_0$log_tot)),"total_duration"]<-mean(users_0$total_duration)

users_0[which(users_0$total_duration>mean(users_0$total_duration)+3*sd(users_0$total_duration)),"total_duration"]<-mean(users_0$total_duration)
users_0[which(users_0$total_duration<mean(users_0$total_duration)-3*sd(users_0$total_duration)),"total_duration"]<-mean(users_0$total_duration)
users_0[which(users_0$total_duration>mean(users_0$total_duration)+3*IQR(users_0$total_duration)),"total_duration"]<-mean(users_0$total_duration)
ggplot(users_0,aes(1,avg_duration))+geom_boxplot()+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
users_0<-filter(users_0,total_duration<90000)
users_0<-filter(users_0,avg_duration<310)
users_0<-filter(users_0,avg_screens<15)
ggplot(users_0,aes(avg_duration,avg_screens))+geom_point()
##########################################


ggplot(users_1,aes(log_dur))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
users_1$log_tot<-log(users_1$total_duration)
users_1$log_dur<-log(users_1$avg_duration)

users_1[which(users_1$log_dur>mean(users_1$log_dur)+3*sd(users_1$log_dur)),"avg_duration"]<-mean(users_1$avg_duration)
users_1[which(users_1$log_dur<mean(users_1$log_dur)-3*sd(users_1$log_dur)),"avg_duration"]<-mean(users_1$avg_duration)
users_1[which(users_1$avg_duration>mean(users_1$avg_duration)+3*sd(users_1$avg_duration)),"avg_duration"]<-mean(users_1$avg_duration)
users_1[which(users_1$avg_duration<mean(users_1$avg_duration)-3*sd(users_1$avg_duration)),"avg_duration"]<-mean(users_1$avg_duration)
ggplot(users_1,aes(avg_screens))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")

users_1$log_scr<-log(users_1$avg_screens)
users_1[which(users_1$log_scr>mean(users_1$log_scr)+3*sd(users_1$log_scr)),"avg_screens"]<-mean(users_1$avg_screens)
users_1[which(users_1$log_scr<mean(users_1$log_scr)-3*sd(users_1$log_scr)),"avg_screens"]<-mean(users_1$avg_screens)

users_1[which(users_1$avg_screens>mean(users_1$avg_screens)+3*sd(users_1$avg_screens)),"avg_screens"]<-mean(users_1$avg_screens)
users_1[which(users_1$avg_screens>mean(users_1$avg_screens)+3*IQR(users_1$avg_screens)),"avg_screens"]<-mean(users_1$avg_screens)


users_1$log_tot<-log(users_1$total_duration)
users_1[which(users_1$log_tot>mean(users_1$log_tot)+3*sd(users_1$log_tot)),"total_duration"]<-mean(users_1$total_duration)
users_1[which(users_1$log_tot<mean(users_1$log_tot)-3*sd(users_1$log_tot)),"total_duration"]<-mean(users_1$total_duration)

users_1[which(users_1$total_duration>mean(users_1$total_duration)+3*sd(users_1$total_duration)),"total_duration"]<-mean(users_1$total_duration)
users_1[which(users_1$total_duration<mean(users_1$total_duration)-3*sd(users_1$total_duration)),"total_duration"]<-mean(users_1$total_duration)
users_1[which(users_1$total_duration>mean(users_1$total_duration)+3*IQR(users_1$total_duration)),"total_duration"]<-mean(users_1$total_duration)
ggplot(users_1,aes(1,avg_screens))+geom_boxplot()+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
users_1<-filter(users_1,total_duration<60000)
users_1<-filter(users_1,avg_duration<300)

ggplot(users_1,aes(avg_duration,avg_screens))+geom_point()

#############################



ggplot(users_2,aes(total_duration))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
users_2$log_tot<-log(users_2$total_duration)
users_2$log_dur<-log(users_2$avg_duration)

users_2[which(users_2$log_dur>mean(users_2$log_dur)+3*sd(users_2$log_dur)),"avg_duration"]<-mean(users_2$avg_duration)
users_2[which(users_2$log_dur<mean(users_2$log_dur)-3*sd(users_2$log_dur)),"avg_duration"]<-mean(users_2$avg_duration)
users_2[which(users_2$avg_duration>mean(users_2$avg_duration)+3*sd(users_2$avg_duration)),"avg_duration"]<-mean(users_2$avg_duration)
users_2[which(users_2$avg_duration<mean(users_2$avg_duration)-3*sd(users_2$avg_duration)),"avg_duration"]<-mean(users_2$avg_duration)
ggplot(users_2,aes(avg_duration))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")

users_2$log_scr<-log(users_2$avg_screens)
users_2[which(users_2$log_scr>mean(users_2$log_scr)+3*sd(users_2$log_scr)),"avg_screens"]<-mean(users_2$avg_screens)
users_2[which(users_2$log_scr<mean(users_2$log_scr)-3*sd(users_2$log_scr)),"avg_screens"]<-mean(users_2$avg_screens)

users_2[which(users_2$avg_screens>mean(users_2$avg_screens)+3*sd(users_2$avg_screens)),"avg_screens"]<-mean(users_2$avg_screens)
users_2[which(users_2$avg_screens>mean(users_2$avg_screens)+3*IQR(users_2$avg_screens)),"avg_screens"]<-mean(users_2$avg_screens)


users_2$log_tot<-log(users_2$total_duration)
users_2[which(users_2$log_tot>mean(users_2$log_tot)+3*sd(users_2$log_tot)),"total_duration"]<-mean(users_2$total_duration)
users_2[which(users_2$log_tot<mean(users_2$log_tot)-3*sd(users_2$log_tot)),"total_duration"]<-mean(users_2$total_duration)

users_2[which(users_2$total_duration>mean(users_2$total_duration)+3*sd(users_2$total_duration)),"total_duration"]<-mean(users_2$total_duration)
users_2[which(users_2$total_duration<mean(users_2$total_duration)-3*sd(users_2$total_duration)),"total_duration"]<-mean(users_2$total_duration)
users_2[which(users_2$total_duration>mean(users_2$total_duration)+3*IQR(users_2$total_duration)),"total_duration"]<-mean(users_2$total_duration)
ggplot(users_2,aes(1,avg_screens))+geom_boxplot()+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")


ggplot(users_2,aes(avg_duration,avg_screens))+geom_point()




#######################################


ggplot(users_3,aes(log_tot))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")
users_3$log_tot<-log(users_3$total_duration)
users_3$log_dur<-log(users_3$avg_duration)

users_3[which(users_3$log_dur>mean(users_3$log_dur)+3*sd(users_3$log_dur)),"avg_duration"]<-mean(users_3$avg_duration)
users_3[which(users_3$log_dur<mean(users_3$log_dur)-3*sd(users_3$log_dur)),"avg_duration"]<-mean(users_3$avg_duration)
users_3[which(users_3$avg_duration>mean(users_3$avg_duration)+3*sd(users_3$avg_duration)),"avg_duration"]<-mean(users_3$avg_duration)
users_3[which(users_3$avg_duration<mean(users_3$avg_duration)-3*sd(users_3$avg_duration)),"avg_duration"]<-mean(users_3$avg_duration)
ggplot(users_3,aes(total_duration))+geom_histogram(bins=70)+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")

users_3$log_scr<-log(users_3$avg_screens)
users_3[which(users_3$log_scr>mean(users_3$log_scr)+3*sd(users_3$log_scr)),"avg_screens"]<-mean(users_3$avg_screens)
users_3[which(users_3$log_scr<mean(users_3$log_scr)-3*sd(users_3$log_scr)),"avg_screens"]<-mean(users_3$avg_screens)

users_3[which(users_3$avg_screens>mean(users_3$avg_screens)+3*sd(users_3$avg_screens)),"avg_screens"]<-mean(users_3$avg_screens)
users_3[which(users_3$avg_screens>mean(users_3$avg_screens)+3*IQR(users_3$avg_screens)),"avg_screens"]<-mean(users_3$avg_screens)


users_3$log_tot<-log(users_3$total_duration)
users_3[which(users_3$log_tot>mean(users_3$log_tot)+3*sd(users_3$log_tot)),"total_duration"]<-mean(users_3$total_duration)
users_3[which(users_3$log_tot<mean(users_3$log_tot)-3*sd(users_3$log_tot)),"total_duration"]<-mean(users_3$total_duration)

users_3[which(users_3$total_duration>mean(users_3$total_duration)+3*sd(users_3$total_duration)),"total_duration"]<-mean(users_3$total_duration)
users_3[which(users_3$total_duration<mean(users_3$total_duration)-3*sd(users_3$total_duration)),"total_duration"]<-mean(users_3$total_duration)
users_3[which(users_3$total_duration>mean(users_3$total_duration)+3*IQR(users_3$total_duration)),"total_duration"]<-mean(users_3$total_duration)
ggplot(users_3,aes(1,avg_screens))+geom_boxplot()+ggtitle("Consistent Users")+ggthemes::theme_economist()+xlab("Average Duration")


ggplot(users_3,aes(avg_duration,avg_screens))+geom_point()

########################
clean_users<-rbind(users_0,users_1,users_2,users_3)
clean_users$gmm_label<-as.factor(clean_users$gmm_label)
clean_users$R_score<-as.factor(clean_users$R_score)
clean_users$M_score<-as.factor(clean_users$M_score)
clean_users$F_score<-as.factor(clean_users$F_score)


################
## Data Split ##
################
set.seed(1)
Train<-clean_users[sample(1:nrow(clean_users),0.7*nrow(clean_users)),]
Test<-clean_users[-sample(1:nrow(clean_users),0.7*nrow(clean_users)),]

fit <- rpart(gmm_label ~ avg_duration +F_score+ avg_screens + num_of_sessions+crash_rate+RFM+total_duration+R, 
             method="class", data=Train,control=rpart.control(minsplit=500,cp=0))
prp(fit,type = 2)
fit$variable.importance


pred_class<-predict(fit, Test, type="class")
confusionMatrix(pred_class,Test$gmm_label)


for_fit<-randomForest(gmm_label ~ avg_duration +F_score+M_score+R_score+ avg_screens + num_of_sessions+crash_rate+RFM+avg_IAT+total_duration+R,data=Train,ntree=100,do.Trace=T,importance=T)

pred_class<-predict(for_fit, Test, type="class")
confusionMatrix(pred_class,Test$gmm_label)


clean_users$ren_churn<-ifelse(clean_users$R/(clean_users$R+clean_users$last_session_dur)>clean_users$avg_IAT/(clean_users$avg_IAT+clean_users$avg_duration),1,0)
table(clean_users$churn1)


hoeffd(clean_users$avg_IAT,clean_users$avg_duration)



clean_users$ren_churn<-as.factor(clean_users$ren_churn)

###################
## Data Labeling ##
###################

users_0$ren_churn<-ifelse(users_0$R/(users_0$R+users_0$last_session_dur)>1.05*users_0$avg_IAT/(users_0$avg_IAT+users_0$avg_duration),1,0)
table(users_0$ren_churn)



users_1$ren_churn<-ifelse(users_1$R/(users_1$R+users_1$last_session_dur)>1.05*users_1$avg_IAT/(users_1$avg_IAT+users_1$avg_duration),1,0)
table(users_1$ren_churn)



users_2$ren_churn<-ifelse(users_2$R/(users_2$R+users_2$last_session_dur)>1.05*users_2$avg_IAT/(users_2$avg_IAT+users_2$avg_duration),1,0)
table(users_2$ren_churn)


users_3$ren_churn<-ifelse(users_3$R/(users_3$R+users_3$last_session_dur)>1.05*users_3$avg_IAT/(users_3$avg_IAT+users_3$avg_duration),1,0)
table(users_3$ren_churn)



clean_users$scaled_r<-normalize(clean_users$R,method="range",range=c(0,1))
clean_users$scaled_d<-normalize(clean_users$avg_duration,method="range",range=c(0,1))
clean_users$scaled_ld<-normalize(clean_users$last_session_dur,method="range",range=c(0,1))
clean_users$scaled_iat<-normalize(clean_users$avg_IAT,method="range",range=c(0,1))





set.seed(22)
index<-createDataPartition(users_0$ren_churn,p=.7,list=FALSE)
train<-users_0[index,]
test<-users_0[-index,]

#################################################################################
## SMOTE is an oversampling technique used to handle class imbalance
## Check my capstone report for more details
#################################################################################
ovsamp <- SMOTE(train[,-20],train$ren_churn)$data


ovsamp$class<-as.factor(ovsamp$class)
ovsamp<-ovsamp[,-c(10:15)]
ovsamp$log_dur<-NULL
ovsamp$log_tot<-NULL
ovsamp$log_scr<-NULL
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL
test$log_scr<-NULL

######################################################
##          Supervised Learning                     ##
######################################################
ct<-trainControl(method="cv", number=5)
knn_c<-train(class~.,data=ovsamp, method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)
scaled_ovs<-as.data.frame(scale(ovsamp[,-11],center = T,scale=T))
scaled_ovs$class<-ovsamp$class

scaled_test<-as.data.frame(scale(test[,-11],center = T,scale=T))
scaled_test$class<-test$ren_churn

predict<-knn(scaled_ovs[,-c(6,3,7,9)],scaled_test[,-c(6,3,7,9)],
             scaled_ovs$class,k=5,prob=TRUE)

confusionMatrix(predict,scaled_test$class,positive = "1")

scores.knn <- attr(predict,"prob")
scores.knn[predict==0] <- 1-scores.knn[predict==0]

results <- HMeasure(data.test_knn$Episode.Bleeding,scores.knn)
results
plotROC(results)

################################################


set.seed(22)
index<-createDataPartition(users_1$ren_churn,p=.7,list=FALSE)
train<-users_1[index,]
test<-users_1[-index,]
table(train$ren_churn)
ovsamp<-ovun.sample(ren_churn~.,train,method="both")$data

ovsamp$ren_churn<-as.factor(ovsamp$ren_churn)
ovsamp<-ovsamp[,-c(10:15)]
ovsamp$log_dur<-NULL
ovsamp$log_tot<-NULL
ovsamp$log_scr<-NULL
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL

test$log_scr<-NULL
ct<-trainControl(method="cv", number=5)
knn_c<-train(ren_churn~.,data=ovsamp, method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)
scaled_ovs<-as.data.frame(scale(ovsamp[,-11],center = T,scale=T))
scaled_ovs$class<-ovsamp$ren_churn

scaled_test<-as.data.frame(scale(test[,-11],center = T,scale=T))
scaled_test$class<-test$ren_churn

predict<-knn(scaled_ovs[,-c(6,3,7,9)],scaled_test[,-c(6,3,7,9)],
             scaled_ovs$class,k=5,prob=TRUE)

confusionMatrix(predict,scaled_test$class,positive = "1")

scores.knn <- attr(predict,"prob")
scores.knn[predict==0] <- 1-scores.knn[predict==0]

results <- HMeasure(data.test_knn$Episode.Bleeding,scores.knn)
results
plotROC(results)


#####################


set.seed(22)
table(users_2$ren_churn)
users_2$ren_churn<-as.factor(users_2$ren_churn)
set.seed(22)
index<-createDataPartition(users_2$ren_churn,p=.7,list=FALSE)
train<-users_2[index,]
test<-users_2[-index,]
ovsamp <- SMOTE(train[,-20],train$ren_churn)$data
ovsamp<-train

ovsamp$ren_churn<-as.factor(ovsamp$ren_churn)
ovsamp<-ovsamp[,-c(10:15)]
ovsamp$log_dur<-NULL
ovsamp$log_tot<-NULL
ovsamp$log_scr<-NULL
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL
test$log_scr<-NULL
ct<-trainControl(method="cv", number=5)


knn_c<-train(ren_churn~.,data=ovsamp[,-c(1,7)], method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)
scaled_ovs<-as.data.frame(scale(ovsamp[,-11],center = T,scale=T))
scaled_ovs$class<-ovsamp$ren_churn

scaled_test<-as.data.frame(scale(test[,-11],center = T,scale=T))
scaled_test$class<-test$ren_churn

predict<-knn(scaled_ovs[,-c(1,7,9,3,2,10)],scaled_test[,-c(1,7,9,3,2,10)],
             scaled_ovs$class,k=11,prob=TRUE)

confusionMatrix(predict,scaled_test$class,positive = "1")



##################



set.seed(22)
table(users_3$ren_churn)
index<-createDataPartition(users_3$ren_churn,p=.7,list=FALSE)
train<-users_3[index,]
test<-users_3[-index,]
ovsamp <- SMOTE(train[,-20],train$ren_churn)$data


ovsamp$class<-as.factor(ovsamp$class)
ovsamp<-ovsamp[,-c(10:15)]
ovsamp$log_dur<-NULL
ovsamp$log_tot<-NULL
ovsamp$log_scr<-NULL
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL
test$log_scr<-NULL
ct<-trainControl(method="cv", number=5)
knn_c<-train(class~.,data=ovsamp[,-c(1,7,9,3,2,10)], method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)
scaled_ovs<-as.data.frame(scale(ovsamp[,-11],center = T,scale=T))
scaled_ovs$class<-ovsamp$class

scaled_test<-as.data.frame(scale(test[,-11],center = T,scale=T))
scaled_test$class<-test$ren_churn

predict<-knn(scaled_ovs[,-c(1,7,9,3,10)],scaled_test[,-c(1,7,9,3,10)],
             scaled_ovs$class,k=5,prob=TRUE)

confusionMatrix(predict,scaled_test$class,positive = "1")

scores.knn <- attr(predict,"prob")
scores.knn[predict==0] <- 1-scores.knn[predict==0]

results <- HMeasure(data.test_knn$Episode.Bleeding,scores.knn)
results
plotROC(results)



###########################
set.seed(22)
index<-createDataPartition(users_0$ren_churn,p=.7,list=FALSE)
train<-users_0[index,]
test<-users_0[-index,]

class(train$ren_churn)
train$ren_churn<-as.factor(train$ren_churn)
test$ren_churn<-as.factor(test$ren_churn)
train$logiat<-log(train$avg_IAT)
train$logsc<-log(train$avg_screens)
test$logiat<-log(test$avg_IAT)
test$logsc<-log(test$avg_screens)
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL
test$log_scr<-NULL
model<-naiveBayes(ren_churn~logiat+avg_duration+logsc,data=train,laplace=1)
model$apriori
model$tables

pred<-predict(model,newdata=test[,c(4,21,22)])
confusionMatrix(pred,reference = test$ren_churn,positive="1")

#####################################################
set.seed(22)
index<-createDataPartition(users_3$ren_churn,p=.7,list=FALSE)
train<-users_3[index,]
test<-users_3[-index,]
ovsamp <- SMOTE(train[,-20],train$ren_churn)$data



ovsamp$class<-as.factor(ovsamp$class)
ovsamp<-ovsamp[,-c(10:15)]
ovsamp$log_dur<-NULL
ovsamp$log_tot<-NULL
ovsamp$log_scr<-NULL
test<-test[,-c(10:15)]
test$log_dur<-NULL
test$log_tot<-NULL
test$log_scr<-NULL

svm.model <- svm(ovsamp$class ~ ., data=ovsamp[,-c(6,3,7,9)], cost = 10, gamma = 1)
svm.pred  <- predict(svm.model, test[,-c(6,3,7,9)])
confusionMatrix(svm.pred,reference = test$ren_churn,positive="1")

##########################

set.seed(22)
index<-createDataPartition(users_1$ren_churn,p=.7,list=FALSE)
train<-users_1[index,]
test<-users_1[-index,]

class(train$ren_churn)
train$ren_churn<-as.factor(train$ren_churn)
test$ren_churn<-as.factor(test$ren_churn)
train$logiat<-log(train$avg_IAT)
train$logsc<-log(train$avg_screens)
test$logiat<-log(test$avg_IAT)
test$logsc<-log(test$avg_screens)
model<-naiveBayes(ren_churn~logiat+avg_duration+logsc,data=train,laplace=1)
model$apriori
model$tables

pred<-predict(model,newdata=test[,c(4,21,22)])
confusionMatrix(pred,reference = test$ren_churn,positive="1")


######################
set.seed(22)
index<-createDataPartition(users_2$ren_churn,p=.7,list=FALSE)
train<-users_2[index,]
test<-users_2[-index,]

class(train$ren_churn)
train$ren_churn<-as.factor(train$ren_churn)
test$ren_churn<-as.factor(test$ren_churn)
train$logiat<-log(train$avg_IAT)
train$logsc<-log(train$avg_screens)
test$logiat<-log(test$avg_IAT)
test$logsc<-log(test$avg_screens)
model<-naiveBayes(ren_churn~logiat+avg_duration+logsc,data=train,laplace=1)
model$apriori
model$tables

pred<-predict(model,newdata=test[,c(4,21,22)])
confusionMatrix(pred,reference = test$ren_churn,positive="1")



######################
set.seed(22)
index<-createDataPartition(users_3$ren_churn,p=.7,list=FALSE)
train<-users_3[index,]
test<-users_3[-index,]

class(train$ren_churn)
train$ren_churn<-as.factor(train$ren_churn)
test$ren_churn<-as.factor(test$ren_churn)
train$logiat<-log(train$avg_IAT)
train$logsc<-log(train$avg_screens)
test$logiat<-log(test$avg_IAT)
test$logsc<-log(test$avg_screens)
model<-naiveBayes(ren_churn~logiat+avg_duration+logsc,data=train,laplace=1)
model$apriori
model$tables

pred<-predict(model,newdata=test[,c(4,21,22)])
confusionMatrix(pred,reference = test$ren_churn,positive="1")


########################
## Saving The Results ##
########################


knn_acc<-c(0.9648,0.9498     ,0.9997, 0.9989 )
knn_sens<-c(1, 0.666667  ,0.99361  ,0.98864)
knn_spec<-c(0.9648,0.950172  ,1.00000   ,1.00000    )

results<-as.data.frame(knn_acc)
results$users<-c("Consistent","Moderate","Average","Heavy")
naive_acc<-c(0.9989 ,0.9987,0.9517, 0.8944)
naive_sens<-c(0.666667 , 1.000000  ,0.8744   ,1.0000  )
naive_spec<-c(0.999557,0.998711  , 1.00000  ,0.8500   )
results$naive_acc<-naive_acc
results$users<-as.factor(results$users)
results$knn_spec<-knn_spec
results$knn_sens<-knn_sens
results$naive_spec<-naive_spec
results$naive_sens<-naive_sens
svm_acc<-c(0.9949  ,0.9983  ,  0.9498   ,0.9561    )
svm_sens<-c(0.5000000,0.4563,0.30691    ,  0.92254  )
svm_spec<-c( 0.9951338,0.999570  ,0.996433 ,   0.95995       )
results$svm_acc<-svm_acc
results$svm_spec<-svm_spec
results$svm_sens<-svm_sens
ggplot(results, aes(users, knn_acc)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("KNN Accuracy Results")

ggplot(results, aes(users, knn_spec)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("KNN Specificity Results")

ggplot(results, aes(users, knn_sens)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("KNN Sensitivity Results")


ggplot(results, aes(users, naive_acc)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("NB Accuracy Results")


ggplot(results, aes(users, naive_spec)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("NB Specificity Results")


ggplot(results, aes(users, naive_sens)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("NB Sensitivity Results")



ggplot(results, aes(users, svm_acc)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("SVM Accuracy Results")


ggplot(results, aes(users, svm_spec)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("SVM Specificity Results")


ggplot(results, aes(users, svm_sens)) + 
  geom_bar(position="dodge",stat="identity",fill=c("cadetblue1","grey","white","cadetblue"),width=0.5)+ggthemes::theme_economist()+xlab("User Subgroup")+ggtitle("SVM Sensitivity Results")


results$j_knn<-results$knn_sens+results$knn_spec-1
results$j_nb<-results$naive_sens+results$naive_spec-1
results$j_svm<-results$svm_sens+results$svm_spec-1
