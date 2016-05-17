rm(list=ls())
setwd('/Users/Tianwei Zhang/Desktop/Anaconda/Data/')
library(data.table)
library(dplyr)
library(sqldf)

# read user files
user=fread('20160211_PersonInfo_Merged_updated.csv',colClasses = 'char',data.table = F)


# read document files
documentData1 <- read.csv("20160211_DocumentDetails_Part1.csv", header = T,colClasses = 'character')
documentData2 <- read.csv("20160211_DocumentDetails_part2.csv", header = T,colClasses = 'character')
documentData3 <- read.csv("20160211_DocumentDetails_part3.csv", header = T,colClasses = 'character')
documentData <- as.data.table(rbind(documentData1,documentData2,documentData3))
rm(list = c("documentData1", "documentData2", "documentData3"))


# read search and download history
searchData1 <- fread("20160211_SearchDownload_Part1.csv", header = T,colClasses = 'char')
searchData2 <- fread("20160211_SearchDownload_part2.csv", header = T,colClasses = 'char')
searchData3 <- fread("20160211_SearchDownload_part3.csv", header = T,colClasses = 'char')
searchData <- as.data.table(rbind(searchData1,searchData2,searchData3))
rm(list = c("searchData1","searchData2","searchData3"))

searchData=as.data.frame(searchData)

# Joint user and document data to search and download history
joint_transaction=searchData%>%
  left_join(user,by=c('PIDX'='PERSON_ID'))%>%
  left_join(documentData,by=c('KO_ID'='DOC.ID'))

######### create a test set ###############
joint_transaction_download=joint_transaction%>%
  filter(DownloadTime!="")

set.seed(1)
test_index=sample(1:nrow(joint_transaction_download),floor(nrow(joint_transaction_download)*0.3))

test=joint_transaction_download[test_index,]
train=joint_transaction_download[-test_index,]


#### function to create user scores ########
weight=c(
  0.05, #L3_DEPARTMENT_NAME
  0.05, #L2_DEPARTMENT_NAME
  0.1,  #FUNCTIONAL_AREA
  0.1,  #Category
  
  0.1,  #EDUCATION_FLAG
  0.05, #POSITION_CODE
  0.1, #POSITION_ORDER
  0.05, #POSITION_CATEGORY
  0.1, #MGM_FLAG
  
  0.1, #STAFFING_OFFICE_NAME
  0.05,#STAFFING_OFFICE_REGION
  0.05,#STAFFING_OFFICE_COUNTRY
  0.1 #Tenure
)

# input user= PIDX of the user who initiated the search
# user_group= a group of users who have similar searched
# user data= data of all user information
# w= weight for each variable

compare_user=function(input_user,user_group,user_data,w){
  user_data=user_data%>%
    select(PERSON_ID,
           
           # Team category
           L3_DEPARTMENT_NAME,
           L2_DEPARTMENT_NAME,
           FUNCTIONAL_AREA,
           Category,
           
           # Position category
           EDUCATION_FLAG,
           POSITION_CODE,
           POSITION_ORDER,
           POSITION_CATEGORY,
           MGM_FLAG,
           
           # Location category
           STAFFING_OFFICE_NAME,
           STAFFING_OFFICE_REGION,
           STAFFING_OFFICE_COUNTRY,
           
           # Tenure category
           Tenure
           
    )
  
  user_data$Tenure=as.numeric(substr(user_data$Tenure,1,regexpr('\\+',user_data$Tenure)-1))
  user_data$POSITION_ORDER=as.numeric(user_data$POSITION_ORDER)
  
  input_user_data=user_data%>%filter(PERSON_ID==input_user)
  user_group_data=user_data%>%filter(PERSON_ID %in% user_group)
  user_group_data$Tenure[which(is.na(user_group_data$Tenure))]=mean(user_group_data$Tenure,na.rm = T)
  
  if(nrow(input_user_data)>0 & nrow(user_group_data)>0){
    output=matrix(ncol=2,nrow=nrow(user_group_data)) 
    for(i in 1:nrow(user_group_data)){
      output[i,1]=user_group_data$PERSON_ID[i]
      output[i,2]=(user_group_data$L3_DEPARTMENT_NAME[i]==input_user_data$L3_DEPARTMENT_NAME)*w[1]+
        (user_group_data$L2_DEPARTMENT_NAME[i]==input_user_data$L2_DEPARTMENT_NAME)*w[2]+
        (user_group_data$FUNCTIONAL_AREA[i]==input_user_data$FUNCTIONAL_AREA)*w[3]+
        (user_group_data$Category[i]==input_user_data$Category)*w[4]+
        (user_group_data$EDUCATION_FLAG[i]==input_user_data$EDUCATION_FLAG)*w[5]+
        (user_group_data$POSITION_CODE[i]==input_user_data$POSITION_CODE)*w[6]+
        (user_group_data$POSITION_ORDER[i]-input_user_data$POSITION_ORDER)/100*w[7]+
        (user_group_data$POSITION_CATEGORY[i]==input_user_data$POSITION_CATEGORY)*w[8]+
        (user_group_data$MGM_FLAG[i]==input_user_data$MGM_FLAG)*w[9]+
        (user_group_data$STAFFING_OFFICE_NAME[i]==input_user_data$STAFFING_OFFICE_NAME)*w[10]+
        (user_group_data$STAFFING_OFFICE_REGION[i]==input_user_data$STAFFING_OFFICE_REGION)*w[11]+
        (user_group_data$STAFFING_OFFICE_COUNTRY[i]==input_user_data$STAFFING_OFFICE_COUNTRY)*w[12]+
        (1-(user_group_data$Tenure[i]-input_user_data$Tenure)/max(user_group_data$Tenure,na.rm = T))*w[13]
    }
  }else{
    output=NA
  }
  
  return(output)
}

