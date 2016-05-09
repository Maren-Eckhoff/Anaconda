## 05/06/2016 ###
## Examine user profile ####
rm(list=ls())
setwd('/Users/Tianwei Zhang/Desktop/Anaconda/Beat_the_KNOW_search/Beat_the_KNOW_search/files/')
library(data.table)
library(dplyr)
library(sqldf)

# read user files
user=fread('20160211_PersonInfo_Merged_updated.csv',colClasses = 'char',data.table = F)

View(user[1:10,])

# read document files
documentData1 <- read.csv("20160211_DocumentDetails_Part1.csv", header = T,colClasses = 'character')
documentData2 <- read.csv("20160211_DocumentDetails_part2.csv", header = T,colClasses = 'character')
documentData3 <- read.csv("20160211_DocumentDetails_part3.csv", header = T,colClasses = 'character')
documentData <- as.data.table(rbind(documentData1,documentData2,documentData3))
#rm(list = c("documentData1", "documentData2", "documentData3"))

nrow(documentData) #54888
length(unique(documentData$DOC.ID)) #54888
View(documentData[1:10,])

# read search and download history
searchData1 <- fread("20160211_SearchDownload_Part1.csv", header = T,colClasses = 'char')
searchData2 <- fread("20160211_SearchDownload_part2.csv", header = T,colClasses = 'char')
searchData3 <- fread("20160211_SearchDownload_part3.csv", header = T,colClasses = 'char')
searchData <- as.data.table(rbind(searchData1,searchData2,searchData3))
rm(list = c("searchData1","searchData2","searchData3"))

View(searchData[1:10,])

searchData=as.data.frame(searchData)

# Joint user and document data to search and download history
joint_transaction=searchData%>%
  left_join(user,by=c('PIDX'='PERSON_ID'))%>%
  left_join(documentData,by=c('KO_ID'='DOC.ID'))

nrow(joint_transaction) #2015450
nrow(searchData) #2015450

View(joint_transaction_test[1:10,])

# number of downloads by document content and positions
Content_Position=joint_transaction%>%
  filter(DownloadTime!="" & !is.na(DownloadTime)
         & Content.type!="" & !is.na(Content.type)
         & POSITION_FIRMWIDE_NAME!="" & !is.na(POSITION_FIRMWIDE_NAME)  
           )%>%
  group_by(POSITION_FIRMWIDE_NAME,Content.type)%>%
  summarise(n=n())


write.csv(Content_Position,'position_type.csv')

# number of downloads by staffing office location
Location=joint_transaction%>%
  filter(DownloadTime!="" & !is.na(DownloadTime)
         & Content.type!="" & !is.na(Content.type)
         & STAFFING_OFFICE_REGION!="" & !is.na(STAFFING_OFFICE_REGION)  
  )%>%
  group_by(STAFFING_OFFICE_REGION,Content.type)%>%
  summarise(n=n())

write.csv(Location,'location_content.csv')

# number of downloads by role in the firm
L2_department=joint_transaction%>%
  filter(DownloadTime!="" & !is.na(DownloadTime)
         & Content.type!="" & !is.na(Content.type)
         & L2_DEPARTMENT_NAME!="" & !is.na(L2_DEPARTMENT_NAME)  
  )%>%
  group_by(L2_DEPARTMENT_NAME,Content.type)%>%
  summarise(n=n())

write.csv(L2_department,'L2_department.csv')