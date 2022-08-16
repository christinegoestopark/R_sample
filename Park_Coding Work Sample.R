rm(list=ls())

setwd("/Users/christinepark/Desktop/RA Exercise")
install.packages("readxl")
library(readxl)

# Install necessary packages
install.packages("tidyverse")
library(dplyr)
library(tidyverse)

install.packages('stringdist')
library(stringdist)
library(tibble)

install.packages("splitstackshape")

### Q1
# Import the ra_exercise_3_2018 data
data_q1 <- as_tibble(read_excel("ra_exercise_3_2018.xls"))

# extract the company column and pick only the names that do not overlap with others.
# then, arrange in the alphabetical order
distinct_values = distinct(data_q1, company)
distinct_values <- distinct_values %>%
  arrange(company)

# when looking at the distinct names of the companies,the first ten characters seem to contain the important part of the names.
# create a new column that only contains the first ten character so that it is easier to distinguish the names.
library(stringdist)
distinct_values$firstten = substr(distinct_values$company, 1, 10)

## use vector indexing 
# first, create an empty vector that is comprised of equal length as the # of rows in the distinct_values
a = vector(mode = "character", nrow(distinct_values))
for (i in 1:(nrow(distinct_values))) #go through all the rows
{ 
  matches = stringdist(c(distinct_values$firstten[i]), c(distinct_values$firstten)) #match returns the difference of each name when compared to all of the names in the dataset the list
  maxDist = 2 # I will set the max difference as 2
  true_pos = matches <= maxDist #if the difference is less than or equal to 2, it will put TRUE in the vector and else, it will put FALSE
  a[true_pos] = distinct_values$company[i]  #index the vector by putting the name of which the difference is less than two
}

# Add a new column to put the standardized names
distinct_values $ standard_name = a 

# Finally, add the standard naems as an additional field of the original dataset.
# first, again, create a new vector that is of the same length as # of the rows in original dataset 
aa = vector(mode = "character", nrow(data_q1)) 

for (i in 1:nrow(distinct_values)) # go through all the rows in the distinct_values dataset 
{
  name_match = (distinct_values$company[i] == data_q1$company) # match the company names column in the original dataset with the name column in the distinct_values dataset
  aa[name_match] = distinct_values$standard_name[i] #index the vector by putting the standardized names 
}

data_q1$standard_name = aa

write.csv(data_q1,
          file = 'Q1_standardized.csv',
          row.names = FALSE,
          na = "")

############################################## 
############### Q2 

setwd("/Users/christinepark/Desktop/RA Exercise")
ACS_2012 <- read.csv("ACS_2012.csv")
ACS_2017 <- read.csv("ACS_2017.csv")

### data cleansing process 
# YEAR 2012 ACS Data (A)
A <- ACS_2012 
A <- as_tibble(A)
A = A[-1,]

# Change all values into characters class for now
library(dplyr)
A %>% mutate_if(is.factor, as.character) -> A

# Exclude Alaska, DC, and Hawaii
A <- A %>% 
  filter(!grepl('Alaska|District of Columbia|Hawaii', GEO.display.label)) 

# YEAR 2017 ACS Data (B)
B <- ACS_2017 
B <- as.tibble(B)
B = B[-1,]

# change all values into characters class for now
B %>% mutate_if(is.factor, as.character) -> B

# Exclude Alaska, DC, and Hawaii
B <- B %>% 
  filter(!grepl('Alaska|District of Columbia|Hawaii', GEO.display.label))

####### 2-1: Total adult population at the county level
### Year 2012
# In order to get the total adult population, I will do the following calculation:
# Adult population = Total Population - children population (under 5 years + 5 to 17 years)
A_pop <- A[,c("GEO.display.label","HC01_EST_VC01","HC01_EST_VC06","HC01_EST_VC07")]

# Rename the columns and change them to numeric class values.
names(A_pop) <- c("county","total_pop","under_5","five_to_17")
A_pop$total_pop <- as.numeric(A_pop$total_pop)
A_pop$under_5 <- as.numeric(A_pop$under_5)
A_pop$five_to_17 <- as.numeric(A_pop$five_to_17)

# Add two columns of which:
# (1) percent: total percent of children population (under 17)
# (2) totalcount: total adult population
A_pop<- A_pop %>% 
  mutate(percent = (under_5+five_to_17)/100) %>%
  mutate (totalcount = total_pop * (1-percent))

### Year 2017
# Same process as what I had done above for year 2012.
# Adult population = Total Population - children population (under 5 years + 5 to 17 years)
B_pop <- B[,c("GEO.display.label","HC01_EST_VC01","HC01_EST_VC06","HC01_EST_VC07")]

# Rename the columns and change them to numeric class values.
names(B_pop) <- c("county","total_pop","under_5","five_to_17")
B_pop$total_pop <- as.numeric(B_pop$total_pop)
B_pop$under_5 <- as.numeric(B_pop$under_5)
B_pop$five_to_17 <- as.numeric(B_pop$five_to_17)

# Add two columns of which:
# (1) percent: total percent of children population (under 17)
# (2) totalcount: total adult population
B_pop<- B_pop %>% 
  mutate(percent = (under_5+five_to_17)/100) %>%
  mutate (totalcount = total_pop * (1-percent))

####### 2-2: % adults with less than HS education at the county level
### Year 2012
A_ed <- A[,c("GEO.display.label","HC01_EST_VC57")]

# Rename the columns and change to numeric values.
names(A_ed) <- c("county","less_than_HS_ed")
A_ed$less_than_HS_ed <- as.numeric(A_ed$less_than_HS_ed)

### Year 2017
B_ed <- B[,c("GEO.display.label","HC01_EST_VC54")]

#Rename the columns and change to numeric values.
names(B_ed) <- c("county","less_than_HS_ed")
B_ed$less_than_HS_ed <- as.numeric(B_ed$less_than_HS_ed)

####### 2-3: % employment rate at the county level
### Year 2012
A_emprate <- A[,c("GEO.display.label","HC01_EST_VC75")]
# Rename the columns and change to numeric values.
names(A_emprate) <- c("county","employment_rate")
A_emprate$employment_rate <- as.numeric(A_emprate$employment_rate)         

### Year 2017
B_emprate <- B[,c("GEO.display.label","HC01_EST_VC70")]
# Rename the columns and change to numeric values.
names(B_emprate) <- c("county","employment_rate")
B_emprate$employment_rate <- as.numeric(B_emprate$employment_rate)         

names(returnstats) <- c('mean','median','minvalue','maxvalue','25th quant','50th quant','75th quant','observation')

### I will now create a function that returns the descriptive statistics values
descripstats <- function(x) {
  meancalc <- mean(x)
  n <- length(x)
  median <- median(x,na.rm=FALSE)
  minvalue <- min(x)
  maxvalue <- max(x)
  quantvalue <- quantile(x,probs=c(.25,.5,.75))
  observation <- n
  returnstats <- c(meancalc,median,minvalue,maxvalue,quantvalue,observation)
  return(returnstats)
}

## Table of summary statistics 
table <- data.frame(matrix(ncol = 2, nrow=8))
names(table) <- c("2012","2017")
rownames(table) <- c("Mean","Median","Min","Max","25th Quant","50th Quant", "75th Quant", "# of Observations")

# (1) Adult population at the county level in 2012 and 2017
summary_pop <- table
summary_pop$'2012' <- descripstats(A_pop$totalcount)
summary_pop$'2017' <- descripstats(B_pop$totalcount)

# Since this is a number of population, I should round up the given statistics numbers.
summary_pop$'2012' <- round(summary_pop$'2012', digits=0)
summary_pop$'2017' <- round(summary_pop$'2017', digits=0)

# (2) % adults with less than HS Ed at the county level in 2012 and 2017
summary_ed <- table
summary_ed$'2012' <- descripstats(A_ed$less_than_HS_ed)
summary_ed$'2017' <- descripstats(B_ed$less_than_HS_ed)

# To make it neater, I will only keep the first decimal place.
summary_ed <- format(summary_ed, digits=2)

# (3) % employment rate at the county level in 2012 and 2017
summary_emp <- table
summary_emp$'2012' <- descripstats(A_emprate$employment_rate)
summary_emp$'2017' <- descripstats(B_emprate$employment_rate)

# Again, to make it neater, I will only keep the first decimal place.
summary_emp <- format(summary_emp, digits=2)

# Export the tables into one spreadsheet
require(openxlsx)
list_of_table <- list("Adult Population" = summary_pop, "Adults with less than High School Education (%)" = summary_ed, "Emplpoyment Rate" = summary_emp)
write.xlsx(list_of_table, file = "Summary Statistics.xlsx", col.names = TRUE, row.names=TRUE)

##############
### Create a graph that shows relationship in PA in 2012 b/t Ed attainment (% pop with less than HS Ed) and Employment Rate
# extract the data for counties in Pennsylvania.
Pennsylvania <- A %>%
  filter(grepl('Pennsylvania',GEO.display.label))

# then, extract the following data: % of adults with less than HS education and % of employment rate. 
Pennsylvania <- Pennsylvania[,c("GEO.display.label","HC01_EST_VC57","HC01_EST_VC75")]
names(Pennsylvania) <- c("county","less_than_HS_ed","employment_rate")

# Only keep the names of county in the county column so that I can label them in the graph.
Pennsylvania$county <- gsub("[[:blank:]]+County, Pennsylvania","",Pennsylvania$county)

# change them to numeric values
Pennsylvania$less_than_HS_ed <- as.numeric(Pennsylvania$less_than_HS_ed)
Pennsylvania$employment_rate <- as.numeric(Pennsylvania$employment_rate)

cor(Pennsylvania$less_than_HS_ed,Pennsylvania$employment_rate) #correlation is -.4542

# Use GGPLOT to draw a scatterplot
require("ggplot2")
library(ggplot2)

# pick values that seem extremly in the PA dataset and highlight them in the plot. 
highlight <- Pennsylvania %>% 
  filter(less_than_HS_ed>15 | employment_rate < 56)

# draw a scatterplot
ggplot(Pennsylvania, aes(less_than_HS_ed,employment_rate)) + 
  geom_point(size=1) +
  labs(
    x = "Adults with less than a High School Education (%)",
    y = "Employment Rate (%)",
    title ="Educational Attainment and Employment Rate",
    subtitle ="Pennsylvania in 2012",
    caption = "Source: 2008-2012 ACS 5-year Estimates") +
    geom_point(data=highlight, aes(x=less_than_HS_ed,y=employment_rate, color=county), size=3)
      
#### 3
## Import zip - county data
ziptocounty <- read.csv("ZiptoCounty.txt",header=TRUE,colClasses='character',sep=",")
ziptocounty <- ziptocounty[,c("zip","cty_name","stabbr")]
ziptocounty <- as_tibble(ziptocounty)

# I found that some county names overlap in different states.
# Since I want to make sure I am matching the correct zipcode to the correct county,
# I will standardize the county name into the following format: 'COUNTY NAME, STATE NAME(all capitalized)'

# combine the county name and the state name and put them in a new column
library(dplyr)
ziptocounty <- ziptocounty %>% 
  mutate(
    countyname = paste(cty_name,stabbr,sep=", "))

# drop unnecessary columns
ziptocounty <- ziptocounty %>%
  select(zip,countyname)

####### create a dataset that has the county level characteristics 
## YR 2012
A_2012 <- as_tibble(cbind(A_ed$county,A_ed$less_than_HS_ed,A_emprate$employment_rate))
names(A_2012) <- c("county","less_than_HS_ed","employment_rate")

# I will first split the county and state names in the 'county' column to standardize the format of county names.
library(splitstackshape)
A_2012 <- cSplit(A_2012, "county", sep=',', direction="wide", fixed=TRUE, 
                 drop=TRUE, stripWhite=TRUE, makeEqual=NULL, type.convert=TRUE)

# change the column names as 'county' and 'state'.
library(dplyr)
A_2012 <- A_2012 %>% rename(county = county_1, state=county_2)

#Then, get rid of the word "county" in the county column and capitalize the whole column.
A_2012$county <- gsub("[[:blank:]]+County","",A_2012$county)
A_2012[[3]] = toupper(A_2012[[3]])

#### Now, change the state column into abbreviations
# I will use a built-in data called 'state' 
A_2012$state <- as.character(A_2012$state)

library(stringr)
data(state)
A_2012$state <- state.abb[match(A_2012$state, state.name)]

# Finally, create a new column that matches the format of "COUNTY NAME,STATE NAME" and drop unnecessary columns.
A_2012 <- A_2012 %>% 
  mutate(countyname = paste(county,state,sep=", ")) %>%
  select(countyname,less_than_HS_ed, employment_rate)

A_2012$less_than_HS_ed <- as.numeric(as.character(A_2012$less_than_HS_ed))
A_2012$employment_rate <- as.numeric(as.character(A_2012$employment_rate))

# Merge Year 2012 county level characteristics data with the zip - county data.
A_2012_zip <- merge(ziptocounty,A_2012,by="countyname")
A_2012_zip <- A_2012_zip %>%
  rename(less_than_hs_ed_2012=less_than_HS_ed, employment_rate_2012=employment_rate)

## ## YR 2017
B_2017 <- as_tibble(cbind(B_ed$county,B_ed$less_than_HS_ed,B_emprate$employment_rate))
names(B_2017) <- c("county","less_than_HS_ed","employment_rate")

# I will first split the county and state names in the 'county' column to standardize the format of county names.
library(splitstackshape)
B_2017 <- cSplit(B_2017, "county", sep=',', direction="wide", fixed=TRUE, 
                 drop=TRUE, stripWhite=TRUE, makeEqual=NULL, type.convert=TRUE)

# change the column names as 'county' and 'state'.
library(dplyr)
B_2017 <- B_2017 %>% rename(county = county_1, state=county_2)

#Then, get rid of the word "county" in the county column and capitalize the whole column.
B_2017$county <- gsub("[[:blank:]]+County","",B_2017$county)
B_2017[[3]] = toupper(B_2017[[3]])

#### Now, change the state column into abbreviations
# I will use a built-in data called 'state' 
B_2017$state <- as.character(B_2017$state)

library(stringr)
data(state)
B_2017$state <- state.abb[match(B_2017$state, state.name)]

# Finally, create a new column that matches the format of "COUNTY NAME,STATE NAME" and drop unnecessary columns.
B_2017 <- B_2017 %>% 
  mutate(countyname = paste(county,state,sep=", ")) %>%
  select(countyname,less_than_HS_ed, employment_rate)

B_2017$less_than_HS_ed <- as.numeric(as.character(B_2017$less_than_HS_ed))
B_2017$employment_rate <- as.numeric(as.character(B_2017$employment_rate))

# Merge Year 2012 county level characteristics data with the zip - county data.
B_2017_zip <- merge(ziptocounty,B_2017,by="countyname")
B_2017_zip <- B_2017_zip %>%
  rename(less_than_hs_ed_2017=less_than_HS_ed, employment_rate_2017=employment_rate)

###### ra_hospital_2020.2 Data
library(readxl)
hospital_data <- read_excel("ra_hospital_2020.2.xlsx")

# I found some inconsistencies in the zipcode column;
# first, standardize the zip codes into a five digit format to merge two datasets correctly.
hospital_data$zip <- substr(hospital_data$zip, 0, 5)
## (This is a little extra, but while looking at the data, I also found some inconsistencies in the orgname)
## (Hence, I will add a new column to put standardized names of the organizations)
library(dplyr)
hospital_data <-
  hospital_data %>% mutate(
    standard_orgname=orgname    #create a new column that contains standardized org names
  )

for (i in 1:(nrow(hospital_data)-1)) {
  if (hospital_data$ein_num[i]==hospital_data$ein_num[i+1])  # if the ein_num of row[i] and row[i+1] are equal
    hospital_data$standard_orgname[i+1]=hospital_data$standard_orgname[i] #put the org name of row [i] as a standardized name for row [i+1]
}
# Finally, create a new dataset that contains county-level characteristics data in addition to the original hospital data.
#First, merge year 2012
hospital_data_bycounty <- left_join(hospital_data,A_2012_zip,by="zip")

# then, merge year 2017
hospital_data_bycounty <- left_join(hospital_data_bycounty,B_2017_zip,by="zip")

# Drop the countyname from the dataset (since it is not required)
hospital_data_bycounty <- hospital_data_bycounty %>%
  select(-countyname.x,-countyname.y) 

# Create a new dataset that has the county-level characteristics for each of the hospital dataset
write.csv(hospital_data_bycounty,
          file = 'Q3 Hospital Data By County.csv',
          row.names = TRUE,
          na = "")

######## 4
# Import Orange Book Data
orangebook <- as_tibble(read.csv("products.txt",header=TRUE,colClasses='character',sep="~"))

# need to only consider currently marketed branded drugs
# hence, 1) get rid of all the discontinued products and 2) get rid of generic drugs.
orangebook_rev <- orangebook %>%
  filter(!grepl('DISCN', Type)) %>%
  filter(grepl('N', Appl_Type))

# Now, create a new dataset that contains information related to statins.
# First, extract the necessray columns : brand name, the chemical name, the name of the manufacturer, approval date, and application number.
statin_data <- orangebook_rev %>%
  select(Ingredient,Trade_Name, Applicant_Full_Name, Approval_Date, Appl_No)

# According to FDA website (https://www.fda.gov/drugs/information-drug-class/statins), 
# here are the ingredients used for marketed drugs that contain statin: 
# atorvastatin, fluvastatin, lovastatin, pitavastatin, pravastatin, rosuvastatin, simvastatin

# first, use the ingredients to extract the drugs that contain statin.
statin_data$Ingredient <- tolower(statin_data$Ingredient)

statin_data <- statin_data %>% 
  filter(grepl('atorvastatin|fluvastatin|lovastatin|pitavastatin|pravastatin|rosuvastatin|simvastatin', Ingredient))

statin_data <- statin_data[c(5,2,1,3,4)]
names(statin_data)=c("number", "Brand Name", "Chemical Name", "Manufacturer", "Approval Date")

# when closely looking at the data, it shows that exist some drugs that share the same brand name, same ingredient, and made by the same manufacturer but has different approval date.
# I will keep the ealiest date as the approval date. (the rationale behind this iswritten in my supplement document called 'Park_RA Exercise.doc')
statin_data <- statin_data[!duplicated(statin_data$number), ]

statin_data <- statin_data %>%
  arrange(number)

### use patent data to see what is available
patent <- as_tibble(read.csv("patent.txt",header=TRUE,colClasses='character',sep='~'))

# only extract statin drugs from the patent list and get rid of duplicated patent numbers
patent_statin <- subset(patent, Appl_No %in% statin_data$number)
patent_statin <- patent_statin[!duplicated(patent_statin$Patent_No),]

patent_statin$Patent_Expire_Date_Text <- format(as.Date(patent_statin$Patent_Expire_Date_Text, "%B %d, %Y"), "%m/%d/%Y")
patent_statin <- patent_statin %>% arrange(desc(Patent_No))

# From looking at the data, it seems like when the patent_no ends with *PED, their patent end date is extended for six months.
# however, other than that, I could not find any other relatable pattern.
# although some drugs share same application #, they have different patent date
# Hence, I will just export the statin_data as a new spreadsheet and manually enter the expiration date.

# Create a new dataset that has information about all the branded statins currently marketed in the US market.
statin_data <- statin_data %>%
  select('Brand Name', 'Chemical Name', 'Manufacturer', 'Approval Date')

statin_data$'Patent/Market Exclusivity End Date' = ""

write.csv(statin_data,
          file = 'Statins Data.csv',
          row.names = FALSE,
          na = "")

