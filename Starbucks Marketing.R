#############  installing packages 


install.packages("stringr")
install.packages("VIM")
install.packages("corrplot")
install.packages("tree")
install.packages("partykit")
install.packages("libcoin")
install.packages("randomForest")
install.packages("dplyr")
install.packages("sqldf")
install.packages("ggpplot2")
install.packages("glmnet")
install.packages("caret")
install.packages("varImp")

library(randomForest)
library(libcoin)
library(partykit)
library(randomForest)
library(tree)
library(dplyr)
library(stringr)
library(dplyr)
library(VIM)
library(sqldf)
library(ggplot2)
library(glmnet)
library(varImp)
library(caret)

source("DataAnalyticsFunctions.R")
source("PerformanceCurves.R")



portfolio <- read.csv('portfolio.csv')
profile <- read.csv('profile.csv')
transcript <- read.csv('transcript.csv')


# profile: become_member should be date
bmo = as.Date(as.character(profile$became_member_on), '%Y%m%d')
profile = data.frame(profile,bmo)
glimpse(profile)

### data range is fine
summary(portfolio)
summary(profile)
summary(transcript)

### duplication
sum(duplicated(portfolio))
sum(duplicated(profile$id))
sum(duplicated(transcript[,c('person','event','value','time')]))
dup=transcript[duplicated(transcript[,c('person','event','value','time')]),]
dup %>% arrange(person,event,value,time)

# clean duplication
transcript_new <- transcript %>%
  distinct(person, event, value, time,.keep_all = TRUE)
sum(duplicated(transcript_new[,c('person','event','value','time')]))
str(transcript_new)

#see missing values
library(visdat)
vis_miss(profile)
sum(is.na(profile))
sum(is.na(profile$income))
sum(is.na(profile$gender))

######################################### PART 1- DATA PREP ###########################################3

######## transcript_new data restructuring , extracting offer_id metrics
#############################
######## extract offer id from value

colnames(transcript_new)[2] = "person_id"

### fit pattern_1
transcript_new_1 = transcript_new %>%
  filter(str_detect(transcript_new$value,fixed("{'offer id': '")) == TRUE)
n_1 = nrow(transcript_new_1)
n_1

library(stringr)
transcript_new_1 = transcript_new_1 %>%
  mutate(offer_id = str_sub(transcript_new_1$value, 15,-3))

### fit pattern_2
transcript_new_2 = transcript_new %>%
  filter(str_detect(transcript_new$value,fixed("{'offer_id': '")) == TRUE)
n_2 = nrow(transcript_new_2)
n_2

R_2 = str_replace(str_replace(transcript_new_2$value,fixed("{'offer_id': '"),""),"', '", ">")
transcript_new_2 = transcript_new_2 %>%
    mutate(offer_id = str_sub(R_2,1,(str_locate(R_2,">")[,2]-1)))

### fit pattern_3: 
transcript_new_3 = transcript_new %>%
  filter(X %in% c(c(transcript_new_1$X),c(transcript_new_2$X)) == FALSE)
n_3 = nrow(transcript_new_3)

 
transcript_new_3 = transcript_new_3 %>%
  mutate(offer_id = 'NULL') 

table(transcript_new_3$event)

### extract 'amount' for transaction and add as a new column
transcript_new$amount = ifelse(str_detect(transcript_new$value,fixed("{'amount':")),str_sub(transcript_new$value, 11,-2),"NULL")

################################
### merge
### value = offer + transaction 
transcript_new_all = rbind(transcript_new_1, transcript_new_2, transcript_new_3)
DATA = merge(transcript_new, transcript_new_all, by="X")
DATA = DATA %>%
  select(-person_id.y,-event.y,-value.y, -time.y) 
colnames(DATA)  = c("X","person_id","event","value", "time", "amount", "offer_id")

### renaming column names for joins, provided new names to offers
colnames(profile)[4] = "person_id"
colnames(portfolio)[7] = "offer_name"
colnames(DATA)[7]="offer_name"
portfolio$offer_id=c("A","B","C","D","E","F","G","H","I","J")
#View(portfolio)
#View(DATA)

DATA_all = left_join(DATA, profile, by = 'person_id')
DATA_all = left_join(DATA_all, portfolio, by = "offer_name")
DATA_all = as.data.frame(DATA_all)
DATA_all = DATA_all %>%
  select(-X.y, -X)

### value = offer completed + offer received + offer viewed
transcript_new_offer = rbind(transcript_new_1, transcript_new_2)
DATA_offer = merge(transcript_new, transcript_new_offer, by="X")
DATA_offer = DATA_offer %>%
  select(-person_id.y,-event.y,-value.y, -time.y) 
colnames(DATA_offer)  = c("X","person_id","event","value", "time", "offer_id")

### value = offer
DATA_all_offer = DATA_all[DATA_all$amount=="NULL",]

### value = transaction
DATA_all_Trans = DATA_all[DATA_all$amount!="NULL",]

### check nothing missing
nrow(DATA_all) == nrow(DATA_all_offer)+nrow(DATA_all_Trans)

### double check duplication
sum(duplicated(DATA_all[,c('person_id','event','offer_id','time')]))

##########################################################################
### channel dummy variables
table(DATA_all$channels)
DATA_all$web <- ifelse(str_detect(DATA_all[,"channels"],"web"),1,0)
DATA_all$email <- ifelse(str_detect(DATA_all[,"channels"],"email"),1,0)
DATA_all$mobile <- ifelse(str_detect(DATA_all[,"channels"],"mobile"),1,0)
DATA_all$social <- ifelse(str_detect(DATA_all[,"channels"],"social"),1,0)

########################################################################
### offer_type dummy variables 
table(DATA_all$offer_type)
DATA_all$bogo <- ifelse(DATA_all$offer_type=="bogo",1,0)
DATA_all$discount <- ifelse(DATA_all$offer_type=="discount",1,0)
DATA_all$informational <- ifelse(DATA_all$offer_type=="informational",1,0)

#number of interactions= transactions+ offer completions
DATA_all$frequency_of_int=ifelse(DATA_all$event %in% c("offer completed","transaction"),1,0)

#############################DATA CLEANING : based on income, gender, age ######################################
##demographics inconsistencies 

str(DATA_all)

table(DATA_all$gender)
DATA_all$gender= ifelse(DATA_all$gender=='F' | DATA_all$gender== 'M', DATA_all$gender, "Other")
sum(DATA_all$gender=="Other")/nrow(DATA_all)
sum(DATA_all$gender=="Other")
#12% of gender missing

table(DATA_all$age)
sum(DATA_all$age>=95)
sum(DATA_all$age>=95)/nrow(DATA_all)
sum(DATA_all$age==118)/nrow(DATA_all)
#11% of age >95, and 118 age seems to be comprising majority of that, seems like a typo

table(DATA_all$income)
sum(is.na(DATA_all$income))
sum(is.na(DATA_all$income))/nrow(DATA_all)
lq_data= DATA_all[is.na(DATA_all$income), ]
table(lq_data$income); table(lq_data$age); table(lq_data$gender)
#all those who have age=118, gender= NA, income=NA are low quality data ie 12% of data (33749)

#percentage of person_ids of low quality
sqldf("SELECT Count(distinct(person_id)) from lq_data")/sqldf("SELECT Count(distinct(person_id)) from DATA_all")

#12% of customers have bad quality data

#dropping low quality IDs
DATA_all$quality_check=ifelse(DATA_all$age==118 & DATA_all$gender=="Other" & is.na(DATA_all$income),1,0)
sum(DATA_all$quality_check)
DATA_all = DATA_all[DATA_all$quality_check==0,]


#############################################   DATA RESTRUCTURING PART 2 ##################################3
#offer id distribution
DATA_all %>% count(offer_id) #NAs are transaction rows

#event distribution
(DATA_all %>% filter(quality_check==0) %>% count(event)) %>% transmute(event, n, percentage=n/sum(n))

sum(DATA_all$event=='offer completed')/sum(DATA_all$event=='offer received')
sum(DATA_all$event=='offer viewed')/sum(DATA_all$event=='offer received')
sum(DATA_all$event=='offer viewed')/sum(DATA_all$event=='offer completed')
#75% of those who receive, view it, and 43% of those who receive complete the offer 

#hypothesis: if transaction row before offer completion corresponds to that offer purchase
T1= sqldf("SELECT *, 
        LAG(event,1) OVER() AS PREV_TRANSAC
        FROM
(SELECT  person_id, event, amount, time
      FROM DATA_all 
      ORDER BY person_id) AS T1
      ")

T1[(T1$PREV_TRANSAC!='transaction' & T1$event=='offer completed'),]
##checking if we have transaction value of the offers, but we identify cases where there was no transaction 
#before each offer_completed. Conclusion: we cannot attribute transaction before the offer completion to the offer spend

#total amount spent by person during the campaign
amount_Data=sqldf("SELECT person_id, sum(Amount) as total_Spend,
                  sum(frequency_of_int) as totalfrequency_of_int
                  FROM DATA_all
                  GROUP BY person_id")
summary(amount_Data)

#finding primary key   
sqldf("
      SELECT person_id, offer_id, time, event,
        ROW_NUMBER() over(partition by person_id, offer_id, time,event ORDER BY person_id, time) as received
        FROM DATA_all
        GROUP BY person_id, offer_id, time,event") %>% count(received)
#tells me that person_id, offer_id, time and event is the primary key

####restructuring data
DATA_all$offer_id=as.factor(DATA_all$offer_id)
DATA_all$offer_received= ifelse(DATA_all$event=="offer received",1,0); sum(DATA_all$offer_received)
DATA_all$offer_viewed= ifelse(DATA_all$event=="offer viewed",1,0); sum(DATA_all$offer_viewed)
DATA_all$offer_completed= ifelse(DATA_all$event=="offer completed",1,0); sum(DATA_all$offer_completed)

DATA_all$month_member=format((as.POSIXct(DATA_all$bmo)),"%m")
DATA_all$year_member=format((as.POSIXct(DATA_all$bmo)),"%Y")
DATA_all$month_member_text=months(DATA_all$bmo)
DATA_all$member_since=as.numeric(max(DATA_all$year_member))- as.numeric(DATA_all$year_member)

#combining the offer_id data for each person_id
DATA1= sqldf("SELECT T2.person_id, event,offer_id,value, hours_since_beg, amount,
                      gender, age, income, reward, difficulty, duration, web, email, social, bogo, 
                      discount, informational, received ,
                      viewed,completed, month_member, year_member, member_since,
                      amount_Data.total_Spend,totalfrequency_of_int,
                      quality_check
  FROM (              SELECT *
  FROM
  (                   SELECT person_id, event,offer_id,value, time AS hours_since_beg, amount,
                      gender, age, income, reward, difficulty, duration, web, email, social, bogo, 
                      discount, informational, SUM(offer_received) as received ,
                      SUM(offer_viewed) AS viewed, SUM(offer_completed) AS completed, 
                      month_member, year_member,  member_since, quality_check
  FROM        DATA_all
  GROUP BY    person_id, event, offer_id
  ORDER BY    person_id ,hours_since_beg) AS T1
  WHERE       event IN ('offer completed', 'offer viewed', 'offer received')) AS T2
  LEFT JOIN   amount_Data
  ON          amount_Data.person_id= T2.person_id
  ") %>% filter(quality_check==0)
summary(DATA1)
str(DATA1)


DATA2=sqldf("SELECT person_id, offer_id, gender AS gender, age, income, reward, difficulty, duration, web, email,
              social, bogo, discount, informational, month_member, year_member,
              member_since, totalfrequency_of_int,total_Spend,
            sum(received) AS total_received, sum(viewed) AS total_viewed , sum(completed) AS total_completed, 
            avg(hours_since_beg) as avg_hours,
            avg(hours_since_beg)/24 as avg_day
            FROM DATA1
            GROUP BY person_id, offer_id ")
DATA2

#Target variable: completed_target- if person completed the offer at least once 
DATA2$completed_target=ifelse(DATA2$total_completed==0, 0, 1)
sum(DATA2$total_completed> DATA2$total_viewed)
#5070 rows where offer was completed but not viewed

sum(DATA2$total_completed> DATA2$total_recieved)
sum(DATA2$total_viewed> DATA2$total_recieved)
#confirmed: all those who completed, received it; 
#all those who viewed it, received it 

###############################################################################################
###################################### EDA ###################################################
##############################################################################################

colnames(DATA2)[7] = "min_spend_req"

summary(DATA2)
str(DATA2)

#correlations across variables
library(corrplot)
varstolook <- c( "age", "income", "reward", "min_spend_req", "duration","web","social","bogo","discount",
                 "informational","member_since","total_Spend","total_received","total_viewed",
                 "totalfrequency_of_int","completed_target")
CorMatrix <- cor(DATA2[,varstolook])
corrplot(CorMatrix, method = "square")



#correcting data type

DATA2$age=as.numeric(DATA2$age)
DATA2$min_spend_req= as.numeric(DATA2$min_spend_req)
DATA2$reward= as.numeric(DATA2$reward)
DATA2$completed_target= as.numeric(DATA2$completed_target)
DATA2$duration= as.numeric(DATA2$duration)
DATA2$web=as.factor(DATA2$web)
DATA2$email=as.factor(DATA2$email)
DATA2$social=as.factor(DATA2$social)
DATA2$bogo=as.factor(DATA2$bogo)
DATA2$discount=as.factor(DATA2$discount)
DATA2$informational=as.factor(DATA2$informational)
DATA2$offer_id=as.factor(DATA2$offer_id)

str(DATA2)

##################### only including the required vars
DATA3= DATA2 %>% dplyr::select("offer_id","age","income","reward","min_spend_req",
                        "duration","web","social", "bogo","discount","member_since",
                        "totalfrequency_of_int","total_Spend","total_received","total_viewed","completed_target")
str(DATA3)


### This will turn off warning messages
options(warn=-1)
## splitting data into training (80%) and test data (20%)
set.seed(1)
dt = sort(sample(nrow(DATA3), nrow(DATA3)*.8))
train_data<-DATA3[dt,] #80% of data, n=44177
test_data<-DATA3[-dt,] #20% of data, n=11045

mean(train_data$completed_target)
mean(test_data$completed_target) # balanced sample for completion rate 

mean(DATA3$completed_target==1) #almost 50% of offers were converted
n <- nrow(train_data) # the number of observations

#####################################
### Jessica- EDA ##########################
CD=DATA_all
#Conversion rates on offer ID
ConversionA =length(CD$X[CD$offer_id=="B" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="B" & CD$event=="offer received"])
ConversionA
ConversionB =length(CD$X[CD$offer_id=="B" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="B" & CD$event=="offer received"])
ConversionB
ConversionC =length(CD$X[CD$offer_id=="C" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="C" & CD$event=="offer received"])
ConversionC 
ConversionD =length(CD$X[CD$offer_id=="D" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="D" & CD$event=="offer received"])
ConversionD
ConversionE=length(CD$X[CD$offer_id=="E" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="E" & CD$event=="offer received"])
ConversionE
ConversionF=length(CD$X[CD$offer_id=="F" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="F" & CD$event=="offer received"])
ConversionF
ConversionG=length(CD$X[CD$offer_id=="G" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="G" & CD$event=="offer received"])
ConversionG
ConversionH=length(CD$X[CD$offer_id=="H" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="H" & CD$event=="offer received"])
ConversionH
ConversionI=length(CD$X[CD$offer_id=="I" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="I" & CD$event=="offer received"])
ConversionI
ConversionJ=length(CD$X[CD$offer_id=="J" & CD$event=="offer completed"])/length(CD$X[CD$offer_id=="J" & CD$event=="offer received"])
ConversionJ

Conversion_Rates = data.frame(offer_type=c("G","F","D","I","J","A","B","E"),completion_rates=c(ConversionG,ConversionF,ConversionD,ConversionI,ConversionJ,ConversionA,ConversionB,ConversionE))

Conversion_Rates=Conversion_Rates %>% arrange(desc(completion_rates)) 
Conversion_Rates$offer_type
Conversion_Rates$type = c("Discount","Discount","Bogo","Bogo","Discount","Bogo","Bogo","Discount")

ggplot(Conversion_Rates , aes(x=reorder(offer_type, -completion_rates), y= completion_rates,fill=type)) + geom_bar(stat = "identity") + labs(x="Offer_type",y="Completion Rate",title="Completion Rates vs Offer Type")+
  theme(plot.title=element_text(hjust=0.5))

#Conversion Rates across channels
ConversionEMS =length(CD$X[CD$channels=="['email', 'mobile', 'social']" & CD$event=="offer completed"])/length(CD$X[CD$channels=="['email', 'mobile', 'social']" & CD$event=="offer received"])
ConversionEMS

ConversionWEMS =length(CD$X[CD$channels=="['web', 'email', 'mobile', 'social']" & CD$event=="offer completed"])/length(CD$X[CD$channels=="['web', 'email', 'mobile', 'social']" & CD$event=="offer received"])
ConversionWEMS

ConversionWEM =length(CD$X[CD$channels=="['web', 'email', 'mobile']" & CD$event=="offer completed"])/length(CD$X[CD$channels=="['web', 'email', 'mobile']" & CD$event=="offer received"])
ConversionWEM

ConversionWE =length(CD$X[CD$channels=="['web', 'email']" & CD$event=="offer completed"])/length(CD$X[CD$channels=="['web', 'email']" & CD$event=="offer received"])
ConversionWE

Channel_conversion = data.frame(channels=c("Email+Mobile+social","web+email+mobile+social","web+email+mobile","web+email"),completion_rates=c(ConversionEMS,ConversionWEMS,ConversionWEM,ConversionWE))
Channel_conversion

ggplot(Channel_conversion, aes(x=reorder(channels,-completion_rates), y=completion_rates,fill=channels)) +
  geom_bar(stat = "identity") + 
  labs(x="Channel",y="Completion Rate",title="Completion Rates vs Channel ")+
  theme(plot.title=element_text(hjust=0.5))
  
###########################################  
#### Olivia- EDA ###############################

PT=DATA_all
table = data.frame(table(PT$gender))
x <- c(112940,155483)
labels<- c("Female","Male")
piepercent<- round(100*x/sum(x),1)
pie(x, labels=piepercent, main ="Gender", col=rainbow(length(x)))
legend("topright",c("Female","Male"),cex=0.8, fill=rainbow(length(x)))

##Age barplot
counts <-table(PT$age)
table(PT$age)
barplot(counts, main="Age", xlab="Age")

##Age boxplot
Age<- PT$age
boxplot(Age, main="Age", ylab="Age")
summary(PT$age)

##Income barplot
counts <-table(PT$income)
table(PT$income)
barplot(counts, main="Income",xlab="Income",ylab="count of IDs")

##Income boxplot
Income<- PT$income
boxplot(Income,main="Income",xlab="Income")
summary(PT$income)
sd(PT$income)

#New Members by year
member_years<-substring(PT$became_member_on,1,4)
member_years
PT_new<-cbind(PT, member_years)
PT_new
unique(PT_new$member_years)
counts <-table(PT_new$member_years)
table(PT_new$member_years)
barplot(counts, main="Number of New Members by Year",col=c(2))

#Membership vs completion rate
member_years<-substring(PT$became_member_on,1,4)
member_years
PT_new<-cbind(PT, member_years)
PT_new
unique(PT_new$member_years)
sum(PT_new$event=="offer received" & PT_new$member_years=="2017")
sum(PT_new$event=="offer completed"&PT_new$member_years=="2017")
sum(PT_new$event=="offer viewed"&PT_new$member_years=="2017")
sum(PT_new$event=="offer completed"&PT_new$member_years=="2017") / sum(PT_new$event=="offer viewed"&PT_new$member_years=="2017")*100
pct_2013 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2013") / sum(PT_new$event=="offer received"&PT_new$member_years=="2013")*100
pct_2014 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2014") / sum(PT_new$event=="offer received"&PT_new$member_years=="2014")*100
pct_2015 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2015") / sum(PT_new$event=="offer received"&PT_new$member_years=="2015")*100
pct_2016 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2016") / sum(PT_new$event=="offer received"&PT_new$member_years=="2016")*100
pct_2017 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2017") / sum(PT_new$event=="offer received"&PT_new$member_years=="2017")*100
pct_2018 <- sum(PT_new$event== "offer completed" &PT_new$member_years=="2018") / sum(PT_new$event=="offer received"&PT_new$member_years=="2018")*100

##barchart
completion_rate <- c(pct_2013,pct_2014,pct_2015,pct_2016,pct_2017,pct_2018)
year <- c("2013","2014","2015","2016","2017","2018")
df_new <- data.frame(year,completion_rate)
df_new
ggplot(data=df_new,aes(x=year, y=completion_rate))+ geom_bar(stat="identity") +
  labs(title="Active Membership Each Year vs. Completion Rates") +
  theme(plot.title=element_text(hjust=0.5))

#Income vs completion
summary(PT$income)
PT$IG = ifelse(PT$income<78000,ifelse(PT$income<48000,'low','medium'),'high')
low <- sum(PT$event== "offer completed" &PT$IG=="low") / sum(PT$event=="offer received"&PT$IG=="low")*100
medium <- sum(PT$event== "offer completed" &PT$IG=="medium") / sum(PT$event=="offer received"&PT$IG=="medium")*100
high <- sum(PT$event== "offer completed" &PT$IG=="high") / sum(PT$event=="offer received"&PT$IG=="high")*100
completion_rate <- c(low, medium, high)
Income_group<-c("low","medium","high")
IG_new <- data.frame(Income_group,completion_rate)
ggplot(data=IG_new,aes(x=reorder(Income_group,-completion_rate),Income_group, y=completion_rate))+ geom_bar(stat="identity")+labs(y='Completion Rate', x='Income Group')+ggtitle("Income Group VS Completion Rate")

####################
##### lucile- EDA ######
ggplot(data=PT,aes(x=event))+ geom_bar(fill="orange")+labs(y='Total_amount', x='Event',title="Count of each promotional activity")+
  theme(plot.title=element_text(hjust=0.5))

###############################################################################
###############################################################################
### k-means ####################################################################

DATA_k_means_1 = DATA_all

DATA_k_means_1$transactions = ifelse(DATA_k_means_1$event=="transaction",1,0)
colnames(DATA_k_means_1)
DATA_k_means = sqldf("SELECT person_id, gender, income, age, bmo, sum(offer_received),sum(offer_viewed), sum(offer_completed),sum(transactions),sum(amount)
                     FROM DATA_k_means_1
                     GROUP BY person_id")
colnames(DATA_k_means)
colnames(DATA_k_means) = c("person_id","gender","income","age","bmo","num_received","num_viewed","num_completed","num_transactions","amount")

summary(DATA_k_means$num_viewed)
summary(DATA_k_means$num_completed)
summary(DATA_k_means$num_transactions)
summary(DATA_k_means$amount)
library(ggplot2)
ggplot(DATA_k_means, aes(x=person_id,y=amount)) + geom_point()

# there are an extreme data in amount, we will just take it out.

DATA_k_means_code = sqldf("SELECT *
                          FROM DATA_k_means
                          WHERE amount < 1500")

### prepare data
set.seed(1)
xdata = as.matrix(DATA_k_means_code[,7:10])
xdata <- scale(xdata)
summary(xdata)

# choose centers
source("DataAnalyticsFunctions.R")
kfit = lapply(1:30,function(k) kmeans(xdata,k,nstart=10))
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)

# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)
# Insert labels
text(c(which.min(kaic),which.min(kbic),which.min(kHDic)),c(mean(kaic),mean(kbic),mean(kHDic)),c("AIC","BIC","HDIC"))
# k - 2

SixCenters <- kmeans(xdata,6,nstart=30)

### Centers
SixCenters$centers[1,]
SixCenters$centers[2,]
SixCenters$centers[3,]
SixCenters$centers[4,]
SixCenters$centers[5,]
SixCenters$centers[6,]

### Sizes of clusters
SixCenters$size
### variation explained with 6 clusters
1 - SixCenters$tot.withinss/ SixCenters$totss
### quite different picture across clusters...
library(factoextra)
fviz_cluster(SixCenters,data=xdata)
### near 67%

#### analyze within cluster
cluster=data.frame(SixCenters$cluster)
DATA_k_means_code = data.frame(cbind(DATA_k_means_code,cluster))
colnames(DATA_k_means_code)[11] = "cluster"
DATA_k_means_code$cluster = factor(DATA_k_means_code$cluster)

xdata_summary = data.frame(cbind(xdata,cluster))
colnames(xdata_summary)[5] = "cluster"
colnames(DATA_k_means_code)
xdata_summary$cluster = factor(xdata_summary$cluster)
colnames(xdata_summary)
DATA_k_summary = sqldf("SELECT cluster, avg(num_viewed), avg(num_completed),avg(num_transactions),avg(amount)
                       FROM xdata_summary
                       GROUP BY cluster")
colnames(DATA_k_summary)
DATA_k_means_code$Cluster_name = ifelse(DATA_k_means_code$cluster!=1,ifelse(DATA_k_means_code$cluster!=2,ifelse(DATA_k_means_code$cluster!=3,ifelse(DATA_k_means_code$cluster!=4,ifelse(DATA_k_means_code$cluster!=5,"Regular customers- 20%","Frugals - 2%"),"Offer viewers- 26%"),"High Value customers- 17%"),"Non-target group- 12%"),"Promotion seekers")
DATA_k_means_code$Cluster_name = factor(DATA_k_means_code$Cluster_name)
###

ggplot(DATA_k_means_code,aes(x=num_viewed,y=amount,color=Cluster_name))+geom_point() + 
  labs(title="Num_viewed VS Amount")+
  theme(plot.title=element_text(hjust=0.5))
ggplot(DATA_k_means_code,aes(x=num_completed,y=amount,color=Cluster_name))+geom_point() + 
  labs(title="Num_completed VS Amount")+
  theme(plot.title=element_text(hjust=0.5))
ggplot(DATA_k_means_code,aes(x=num_transactions,y=amount,color=Cluster_name))+geom_point() + 
  labs(title="Num_transactions VS Amount")+
  theme(plot.title=element_text(hjust=0.5))

### customer disctribution
ggplot(DATA_k_means_code,aes(x=cluster,fill=Cluster_name))+geom_bar()+
  labs(title="Customer Distribution")+
  theme(plot.title=element_text(hjust=0.5))

### gender distribution
ggplot(DATA_k_means_code,aes(x=cluster,fill=gender))+geom_bar(position = "dodge")+
  labs(title="Gender distribution")+
  theme(plot.title=element_text(hjust=0.5))

### age distribution
summary(DATA_k_means_code$age)
DATA_k_means_code$AR = ifelse(DATA_k_means_code$age>30,ifelse(DATA_k_means_code$age>45,ifelse(DATA_k_means_code$age>60,ifelse(DATA_k_means_code$age>75,'>76y','66-75y'), '46-60y'),'31-45y'),'18-30y')
ggplot(DATA_k_means_code,aes(x=cluster,fill=AR))+geom_bar(position = "dodge")+
  labs(title="Age distribution")+
  theme(plot.title=element_text(hjust=0.5))

### income distribution
summary(DATA_k_means_code$income)
DATA_k_means_code$IR = ifelse(DATA_k_means_code$income>50000,ifelse(DATA_k_means_code$income>70000,ifelse(DATA_k_means_code$income>95000,ifelse(DATA_k_means_code$income>105000,'>105k','96-105k'), '71-95k'),'51-70k'),'31-50k')
ggplot(DATA_k_means_code,aes(x=cluster,fill=IR))+geom_bar(position = "dodge")+
  labs(title="Income distribution")+
  theme(plot.title=element_text(hjust=0.5))



################################################################################### 
##################################################################################
### PCA 
### Lets compute the (Full) PCA 
#removed intercept and offer_id to see variation across other factors
xdata= model.matrix(completed_target ~ ., data=train_data)[,-1:-10]
pca.data <- prcomp(xdata, scale=TRUE)
### Lets plot the variance that each component explains
par(mar=c(4,4,4,4)+0.3)
plot(pca.data,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

loadings <- pca.data$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(xdata)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
data.frame(v[1:which.min(loadingfit)])
#### Looking at which are large positive and large negative
#### First factor is more discount oriented deals with high duration 
####
#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(xdata)],2]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
data.frame(v[1:which.min(loadingfit)])
#### This is more concerned towards value deals that should earn rewards with minimum spend
####
#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(xdata)],3]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
data.frame(v[1:which.min(loadingfit)])
#### This is loyal customers who purchase frequently, spend higher and been a member since a long time

PCbiplot <- function(PC, x="PC1", y="PC2") {
  data <- data.frame( PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y))
  datapc <- data.frame(varnames=row.names(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 3, vjust=1, color="darkred")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="black")
  plot
}

PCbiplot(pca.data)

#####################################     Modeling  #########################################
### This will turn off warning messages
options(warn=-1)
############################
set.seed(1)
dt = sort(sample(nrow(DATA3), nrow(DATA3)*.8))
train_data<-DATA3[dt,-1] #80% of data
test_data<-DATA3[-dt,-1] #20% of data

mean(train_data$completed_target)
mean(test_data$completed_target) #pretty balanced sample

mean(DATA3$completed_target==1) #almost 50% of offers were converted
n <- nrow(train_data) # the number of observations



### Compare different models 
### m.lr : logistic regression
### m.lr.l : logistic regression with interaction using lasso
### m.lr.pl : logistic regression with interaction using post lasso
### m.lr.tree : classification tree


#### Lets run Lasso
#### First lets set up the data for it
#### the features need to be a matrix ([,-1] removes the first column which is the intercept)

Mx<- model.matrix(completed_target ~ .^2, data=train_data)[,-1]
My<- train_data$completed_target==1
lasso <- glmnet(Mx,My, family="binomial")
lassoCV <- cv.glmnet(Mx,My, family="binomial")

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
summary(lassoTheory)
support(lassoTheory$beta)

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
features.min <- support(lassoTheory$beta)
length(features.min)
data.min <- data.frame(Mx[,features.min],My)

### prediction is a probability score
### we convert to 1 or 0 via prediction > threshold
PerformanceMeasure <- function(actual, prediction, threshold=.5) {
  #1-mean( abs( (prediction>threshold) - actual ) )  
  #R2(y=actual, pred=prediction, family="binomial")
  1-mean( abs( (prediction- actual) ) )  
}

Accuracy= function(actual, prediction, threshold=0.5){
  TP <- sum((prediction >= threshold)*actual)
  FP <- sum((prediction >= threshold)*(!actual))
  FN <- sum((prediction <  threshold)*actual)
  TN <- sum((prediction <  threshold)*(!actual))
  
  FPR <- FP / (FP + TN)
  TPR <- TP / (TP + FN)
  Acc= (TP+TN)/(TP+TN+FP+FN)
  return(data.frame(TP, FP, FN, TN,Acc,FPR, TPR  ))
}


n <- nrow(train_data)
nfold <- 10
OOS_perf <- data.frame(m.lr=rep(NA,nfold), m.lr.l=rep(NA,nfold), m.lr.pl=rep(NA,nfold), m.tree=rep(NA,nfold),m.null=rep(NA,nfold),m.rf=rep(NA,nfold), m.average=rep(NA,nfold)) 
OOS_R <- data.frame(m.lr=rep(NA,nfold), m.lr.l=rep(NA,nfold), m.lr.pl=rep(NA,nfold), m.tree=rep(NA,nfold),m.null=rep(NA,nfold),m.rf=rep(NA,nfold), m.average=rep(NA,nfold)) 
OOS_Accuracy <- data.frame(m.lr=rep(NA,nfold), m.lr.l=rep(NA,nfold), m.lr.pl=rep(NA,nfold), m.tree=rep(NA,nfold),m.null=rep(NA,nfold),m.rf=rep(NA,nfold), m.average=rep(NA,nfold)) 


#names(OOS)<- c("Logistic Regression", "Lasso on LR with Interactions", "Post Lasso on LR with Interactions", "Classification Tree", "Average of Models")
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]


#cross validation takes time to run (~30 mins)
data=train_data
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### Logistic regression
  m.lr <-glm(completed_target~., data=data, subset=train,family="binomial")
  pred.lr <- predict(m.lr, newdata=data[-train,], type="response")
  OOS_perf$m.lr[k] <- PerformanceMeasure(actual=My[-train], pred=pred.lr)
  OOS_R$m.lr[k] <- R2(y=My[-train], pred=pred.lr)
  OOS_Accuracy$m.lr[k] <- Accuracy(actual=My[-train], prediction=pred.lr)$Acc
  
  ### the Post Lasso Estimates for Logistic with interaction
  m.lr.pl <- glm(My~., data=data.min, subset=train, family="binomial")
  pred.lr.pl <- predict(m.lr.pl, newdata=data.min[-train,], type="response")
  OOS_perf$m.lr.pl[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.lr.pl)
  OOS_R$m.lr.pl[k] <- R2(y=My[-train], pred=pred.lr.pl)
  OOS_Accuracy$m.lr.pl[k] <- Accuracy(actual=My[-train], prediction=pred.lr.pl)$Acc
  
  
  ### the Lasso estimates for Logistic with interaction
  m.lr.l  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  pred.lr.l <- predict(m.lr.l, newx=Mx[-train,], type="response")
  OOS_perf$m.lr.l[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.lr.l)
  OOS_R$m.lr.l[k] <- R2(y=My[-train], pred=pred.lr.l)
  OOS_Accuracy$m.lr.l[k] <- Accuracy(actual=My[-train], prediction=pred.lr.l)$Acc
  
  
  ### the classification tree
  m.tree <- tree(factor(completed_target)~ ., data=data, subset=train) 
  pred.tree <- predict(m.tree, newdata=data[-train,], type="vector")
  pred.tree <- pred.tree[,2]
  OOS_perf$m.tree[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.tree)
  OOS_R$m.tree[k] <- R2(y=My[-train], pred=pred.tree)
  OOS_Accuracy$m.tree[k] <- Accuracy(actual=My[-train], prediction=pred.tree)$Acc
  
  
  #null_model
  m.null <- glm(factor(completed_target)~ 1, data=data, subset=train, family="binomial") 
  pred.null <- predict(m.null, newdata=data[-train,], type="response")
  OOS_perf$m.null[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.null)
  OOS_R$m.null[k] <- R2(y=My[-train], pred=pred.null)
  OOS_Accuracy$m.null[k] <- Accuracy(actual=My[-train], prediction=pred.null)$Acc
  
  
  #random forest
  m.rf <- randomForest(completed_target~., data=data[train,], nodesize=2, ntree = 50, mtry = 3)
  pred.rf <- predict(m.rf, newdata=data[-train,], type="response")
  OOS_perf$m.rf[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.rf)
  OOS_R$m.rf[k] <- R2(y=My[-train], pred=pred.rf)
  OOS_Accuracy$m.rf[k] <- Accuracy(actual=My[-train], prediction=pred.rf)$Acc
  
  
  #average
  pred.m.average <- rowMeans(cbind(pred.tree, pred.lr.l, pred.lr.pl, pred.lr,pred.rf,pred.null))
  OOS_perf$m.average[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.m.average)
  OOS_R$m.average[k] <- R2(y=My[-train], pred=pred.m.average)
  OOS_Accuracy$m.average[k] <- Accuracy(actual=My[-train], prediction=pred.m.average)$Acc
  
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

#col means for cross validation
OOS_perf 
OOS_R
OOS_Accuracy
colMeans(OOS_perf)
colMeans(OOS_R)
colMeans(OOS_Accuracy)


names(OOS_Accuracy)<-c("logistic","lasso","P.Lasso","tree","null","RF","average") 
names(OOS_perf)<-c("logistic","lasso","P.Lasso","tree","null","RF","average") 
names(OOS_R)<-c("logistic","lasso","P.Lasso","tree","null","RF","average") 


#bar plots
par(mar=c(7,5,.5,1)+0.3)
barplot(colMeans(OOS_perf), las=2,xpd=FALSE , xlab="", ylab = "", main="Average OOS Performance")
barplot(colMeans(OOS_R), las=2,xpd=FALSE , xlab="",ylim=c(0,1), ylab = bquote( "Average Out of Sample Performance"),main="Rsquare")
barplot(colMeans(OOS_Accuracy), las=2,xpd=FALSE , xlab="",ylim=c(0,1), ylab = bquote( "Average Out of Sample Accuracy"),main="TP+TN accuracy")


#boxplot R square
if (nfold >= 10){

  boxplot(OOS_R[c(-5,-4)], col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}

#boxplot accuracy
if (nfold >= 10){

  boxplot(OOS_Accuracy[c(-5,-4)], col="orange", las = 2, ylab=expression(paste("OOS accuracy")), xlab="", main="10-fold Cross Validation")

  }

#boxplot performance
if (nfold >= 10){

  boxplot(OOS_perf[c(-5,-4)], col="grey", las = 2, ylab=expression(paste("OOS performance")), xlab="", main="10-fold Cross Validation")

}


rownames(OOS_R) <- c(1:nfold)
barplot(t(as.matrix(OOS_R)), beside=TRUE, ylim=c(0,.4) ,legend=TRUE, args.legend=c(x= "topright", y=0.92,bty = "n"),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))




## random forest is the best
#entire train data predictions

#Mx_train= model.matrix(completed_target ~ .^2, data=train_data)[,-1]
#My_train=train_data$completed_target               

m.rf_train  <- randomForest(completed_target~., data=train_data, nodesize=5, ntree = 100, mtry = 4)
pred.rf_train <- predict(m.rf_train, newdata=train_data, type="response")

OOS_acc_rf_train <- PerformanceMeasure(actual=train_data$completed_target, prediction=pred.rf_train)
OOS_R_rf_train<- R2(y=train_data$completed_target, pred=pred.rf_train)
OOS_Binary_rf_train <- Accuracy(actual=train_data$completed_target, prediction=pred.rf_train,threshold=0.5)
cbind(OOS_acc_rf_train,OOS_R_rf_train,OOS_Binary_rf_train)






#confusion matrix 
confusion.matrix <- c( TP= sum( (pred.rf_train>=0.5) * train_data$completed_target ), FP= sum( (pred.rf_train>=0.5) * !train_data$completed_target ) , 
                       FN= sum( (pred.rf_train<0.5) * train_data$completed_target ), TN= sum( (pred.rf_train<0.5) * !train_data$completed_target))
confusion.matrix
#roc curve
roccurve <-  roc(p=pred.rf_train, y=train_data$completed_target, bty="n")

### we will use functions in 
par(mar=c(5,5,3,5))
roccurve <-  roc(p=pred.rf_train, y=train_data$completed_target, bty="n")


#### This is the code for the "red dot plots" ####

index <- c(50)
radius <- 0.009 *rep(1,length(index))
color <- c("red")
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
Accuracy(pred.rf_train>=0.5 , train_data$completed_target)


index <- c(40,50)
radius <- 0.009 *rep(1,length(index))
color <- c("blue","red")
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
Accuracy(pred.rf_train>=0.4, train_data$completed_target)

index <- c(75,40,50)
color <- c("black","blue","red")
radius <- 0.009 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
Accuracy(pred.rf_train>=0.75 , train_data$completed_target)

plot(roccurve,  ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate",type="l", main="")


#cumulative curve
cumulative <- cumulativecurve(p=pred.rf_train,y=train_data$completed_target)
lines(c(0,1),c(0,1), lty=2)

#lift curve
lift <- liftcurve(p=pred.rf_train,y=train_data$completed_target)


#test_data predictions
#random forest          

pred.rf_test <- predict(m.rf_train, newdata=test_data, type="response")

OOS_acc_rf <- PerformanceMeasure(actual=test_data$completed_target, prediction=pred.rf_test)
OOS_R_rf <- R2(y=test_data$completed_target, pred=pred.rf_test)
OOS_Binary_rf <- Accuracy(actual=test_data$completed_target, prediction=pred.rf_test)
cbind(OOS_acc_rf,OOS_R_rf,OOS_Binary_rf)








