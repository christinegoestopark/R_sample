setwd("/Users/christinepark/Desktop/Case Study")
data <- read.csv("MarchCPS_1995_2015.csv")

########### Q1
install.packages("tidyverse")
library(dplyr)

## Year 1995 TOTAL
### Create a new data frame for "adult civilian population" (popstat=1) in 1995 (Year=1995)
data_1995 <-as.data.frame(filter(data, year == 1995 & (popstat == 1)))

# Rearrange 1995 data so that I can make valid comparisons with 2015 data
# I will apply the same code rule done in 2015 (80-84 coded as 80, 85+ coded as 85)

data_1995 <- data_1995 %>%
  mutate(age=replace(age,age>=80 & age<=84, 80))

data_1995 <- data_1995 %>%
  mutate(age=replace(age,age>=85 & age<=90, 85))

data_1995 <- as.data.frame(data_1995)
data_1995$age <- as.numeric(data_1995$age)

# Add Weights to the 1995 data
data_1995 <- data_1995 %>% mutate(
  wgt = wtsupp * 10000
)

## Count Frequency for the total Population in 1995
totalcount_95 <- as.data.frame(data_1995 %>%
  group_by (age) %>%
  summarise(totalcount=sum(wgt,na.rm=FALSE)))

## Select adult, civilian population who is in the labor force (labforce=2)
data_1995_lf <- filter(data_1995, labforce==2)

## Count Frequency of adult, civilian population in the labor force
lfcount_95 <- as.data.frame(data_1995_lf %>%
  group_by(age) %>%
  summarise(count = sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
total_95 <- merge(lfcount_95,totalcount_95)
total_95 <- 
  total_95 %>%  mutate(
  LFPR_95 =count / totalcount * 100
)

options(scipen=999)
## Aggregate LFPR of total population in 1995
total_agg_95 <- colSums(total_95[2])/colSums(total_95[3]) * 100

## Year 1995 Male
### Create a new data frame for "Male civilian population" (sex=1) in 1995 (Year=1995)
male_1995 <- filter(data_1995, sex==1)

## Count Frequency for total Male count

malecount_95 <- as.data.frame(male_1995 %>%
                                 group_by (age) %>%
                                 summarise(totalcount=sum(wgt,na.rm=FALSE)))


## Select male, adult, civilian population who is in the labor force (sex=1 and labforce=2)
male_1995_lf <- filter(male_1995, labforce==2)

## Count Frequency
m_lfcount_95 <- as.data.frame(male_1995_lf %>%
                              group_by (age) %>%
                              summarise(count = sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
male_95 <- merge(m_lfcount_95,malecount_95)
male_95 <- 
  male_95 %>%  mutate(
    LFPR_95 =count / totalcount * 100
  )

## Aaggregate LFPR of Male Population in 1995
male_agg_95 <- colSums(male_95[2]) / colSums(male_95[3]) * 100

## Year 1995 Female
### Create a new data frame for "feale civilian population" (sex=2) in 1995 (Year=1995)
female_1995 <- filter(data_1995, sex==2)

## Count Frequency for total female count
femalecount_95 <- as.data.frame(female_1995 %>%
                                group_by (age) %>%
                                summarise(totalcount=sum(wgt,na.rm=FALSE)))

## Select female, adult, civilian population who is in the labor force (sex=2 and labforce=2)
female_1995_lf <- filter(female_1995, labforce==2)

## Count Frequency
f_lfcount_95 <- as.data.frame(female_1995_lf %>%
                                group_by(age) %>%
                                summarise(count = sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
female_95 <- merge(f_lfcount_95,femalecount_95)
female_95 <- 
  female_95 %>%  mutate(
    LFPR_95 =count / totalcount * 100
  )

## Aggregate LFPR of Female Population 1995
female_agg_95 <- colSums(female_95[2])/colSums(female_95[3])

## Year 2015 TOTAL
### Create a new data frame for "adult civilian population" (popstat=1) in 2015 (Year=2015)
data_2015 <- filter(data, year == 2015 & (popstat == 1))

## Add Weight
data_2015 <- data_2015 %>% mutate(
  wgt = wtsupp * 10000
)

## Count total Population inn 2015 
totalcount_15 <- as.data.frame(data_2015 %>%
                                 group_by (age) %>%
                                 summarise(totalcount=sum(wgt,na.rm=FALSE)))

## Select adult, civilian population who is in the labor force (labforce=2)
data_2015_lf <- filter(data_2015, labforce==2)

## Count total Population who is in the labor force in 2015 
lfcount_15 <- as.data.frame(data_2015_lf %>%
                              group_by(age) %>%
                              summarise(count=sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
total_15 <- merge(lfcount_15,totalcount_15)
total_15 <- 
  total_15 %>%  mutate(
    LFPR_15 =count / totalcount * 100
  )

## Aggregate LFPR of Full Population 2015
total_agg_15 <- colSums(total_15[2])/colSums(total_15[3])*100

## Year 2015 Male
### Create a new data frame for "Male civilian population" (sex=1) in 2015 (Year=2015)
male_2015 <- filter(data_2015, sex==1)

## Count Frequency for total Male count
malecount_15 <- as.data.frame(male_2015 %>%
                                group_by (age) %>%
                                summarise(count=sum(wgt,na.rm=FALSE)))

## Select male, adult, civilian population who is in the labor force (sex=1 and labforce=2)
male_2015_lf <- filter(male_2015, labforce==2)

## Count Frequency
m_lfcount_15 <- as.data.frame(male_2015_lf %>%
                                group_by(age) %>%
                                summarise(count=sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
male_15 <- merge(m_lfcount_15,malecount_15,by='age')
male_15 <- 
  male_15 %>%  mutate(
    LFPR_15 =count.x / count.y * 100
  )


## Aggregate LFPR of Male Population 2015
male_agg_15 <- colSums(male_15[2])/colSums(male_15[3])

## Year 2015 Female
### Create a new data frame for "feale civilian population" (sex=2) in 2015 (Year=2015)
female_2015 <- filter(data_2015, sex==2)

## Count total female population in 2015
femalecount_15 <- as.data.frame(female_2015 %>%
                                  group_by (age) %>%
                                  summarise(count=sum(wgt,na.rm=FALSE)))

## Select female, adult, civilian population who is in the labor force (sex=2 and labforce=2)
female_2015_lf <- filter(female_2015, labforce==2)

## Count total female who is in the labor force in 2015
f_lfcount_15 <- as.data.frame(female_2015_lf %>%
                                group_by(age) %>%
                                summarise(count=sum(wgt,na.rm=FALSE)))

## Combine the two dataframe
female_15 <- merge(f_lfcount_15,femalecount_15,by='age')
female_15 <- 
  female_15 %>%  mutate(
    LFPR_15 =count.x / count.y * 100
  )

## Aggregate LFPR of Female population in 2015
female_agg_15 <- colSums(female_15[2])/colSums(female_15[3])

## Graphing the LFPR of total Population in 1995 and 2005
TOTAL <- merge(total_95,total_15,by="age")
TOTAL <- TOTAL[,c("age","LFPR_95","LFPR_15")]
names(TOTAL) <- c('age','LFPR1995','LFPR2015')

require("ggplot2")

TGraph <- ggplot(TOTAL, aes(x = age)) +
  geom_line(aes(y = LFPR1995, colour = "1995")) +
  geom_line(aes(y = LFPR2015, colour = "2015")) +
  labs(title="LFPR of Total Population", y="LFPR (%)", x="Age", caption="Source: CPS Data") +
  xlim(15,85) +
  ylim(0,100) + 
  scale_colour_manual("Year", values=c("blue","red"))

## Graphing the LFPR of Male Population
MALE <- merge(male_95,male_15,by="age")
MALE <- MALE[,c("age","LFPR_95","LFPR_15")]
names(MALE) <- c('age','LFPR1995','LFPR2015')

MGraph <- ggplot(MALE, aes(x = age)) +
  geom_line(aes(y = LFPR1995, colour = "1995")) +
  geom_line(aes(y = LFPR2015, colour = "2015")) +
  ggtitle("LFPR of Male Population") +
  labs(title="LFPR of Male Population", y="LFPR (%)", x="Age", caption="Source: CPS Data") +
  xlim(15,85) +
  ylim(0,100) + 
  scale_colour_manual("Year", values=c("blue","red"))

## Graphing the LFPR of Female population in 1995 and 2005
FEMALE <- merge(female_95,female_15,by="age")
FEMALE <- FEMALE[,c("age","LFPR_95","LFPR_15")]
names(FEMALE) <- c('age','LFPR1995','LFPR2015')

FGraph <- ggplot(FEMALE, aes(x = age)) +
  geom_line(aes(y = LFPR1995, colour = "1995")) +
  geom_line(aes(y = LFPR2015, colour = "2015")) +
  labs(title="LFPR of Female Population", y="LFPR (%)", x="Age", caption="Source: CPS Data") +
  xlim(15,85) +
  ylim(0,100) + 
  scale_colour_manual("Year", values=c("blue","red"))

########### Q2
####### Aggregate LFPRs of people aged 15 and over in 1995 and 2015
####### Total / Male / Female

total_agg_95
male_agg_95
female_agg_95

total_agg_15
male_agg_15
female_agg_15

########### Q4

# calculate the  age distribution of 1995. 
agedist_95 <- as.data.frame(data_1995 %>%
                              group_by(age) %>%
                              summarise(count=n()) %>%
                              mutate(dist95=count/sum(count)))

# Multiply the age-specific distribution in 1995 by the current age-specific LFPR of 2015 

a <- merge(agedist_95,total_15,by="age") 
a <- data.frame(a %>% 
  mutate(LFPR_new=dist95*LFPR_15))

LFPR_New <-colSums(a[7])
