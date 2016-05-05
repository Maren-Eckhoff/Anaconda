###################################
########## Load data ##############
###################################

basePath <- "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat_the_KNOW_search/"
library(data.table)

# document details data
documentData1 <- read.csv(paste0(basePath, "20160211_DocumentDetails_Part1.csv"), header = T)
documentData2 <- read.csv(paste0(basePath, "20160211_DocumentDetails_part2.csv"), header = T)
documentData3 <- read.csv(paste0(basePath, "20160211_DocumentDetails_part3.csv"), header = T)
documentData <- as.data.table(rbind(documentData1,documentData2,documentData3))
rm(list = c("documentData1", "documentData2", "documentData3"))

# Match site ID to practice
basePath2 <- "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat the Know seacrh - new data 04-08/"
siteData <- as.data.table(read.csv(paste0(basePath2, "Site ID_description.csv"), header = T))

# engagement data
engagementData1 <- read.csv(paste0(basePath, "20160211_EngagementData_Part1.csv"), header = T)
engagementData2 <- read.csv(paste0(basePath, "20160211_EngagementData_part2.csv"), header = T)
engagementData3 <- read.csv(paste0(basePath, "20160211_EngagementData_part3.csv"), header = T)
engagementData <- as.data.table(rbind(engagementData1,engagementData2,engagementData3))
rm(list = c("engagementData1","engagementData2","engagementData3"))

# person information
personData <- as.data.table(read.csv(paste0(basePath, "20160211_PersonInfo_Merged_updated.csv"), header = T))

# search download
searchData1 <- read.csv(paste0(basePath, "20160211_SearchDownload_Part1.csv"), header = T)
searchData2 <- read.csv(paste0(basePath, "20160211_SearchDownload_part2.csv"), header = T)
searchData3 <- read.csv(paste0(basePath, "20160211_SearchDownload_part3.csv"), header = T)
searchData <- as.data.table(rbind(searchData1,searchData2,searchData3))
rm(list = c("searchData1","searchData2","searchData3"))


# evaluation data
evalData <- as.data.table(read.csv(paste0(basePath, "Evaluation Data Set.csv"), header = T))
# submission file
submissionData <- as.data.table(read.csv(paste0(basePath, "Submission_File.csv"), header = T))

# featured documents: If search for one of the keywords, the top 1-5 documents shown in Know come from this list
# convert document to csv and clean the document ID. Document 813034 has an odd letter that needs removing.
basePath3 <- "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat the Know seacrh - new data 04-14/"
featuredDocs <- as.data.table(read.csv(paste0(basePath3, "Featured Documents.csv"), header = T))

# what documents does current know search return. Note that searches before these documents were created would not have returned those docs.
solrSearchData <- as.data.table(read.csv(paste0(basePath3, "Joined all Solr results.csv"), header = T))

#########################################
######## Understand documentData ########
#########################################

colnames(documentData)

# Site_ID: List of practices that selected the document. Different practice IDs are separated by <>. Empty string if no practice selected the document.
# Topic_ID: If a practice selected a document, it can recommend or not recommend it for certain documents. The topics are organised in a tree structure. 
# The topic ID gives the path from the root up to a certain node X. The levels in the path are separated with //.
# The recommendation for X and all children of X is recoreded in Recommended_flag: Y = Recommended. N = Not receommended.
# Different paths are separated by ## and different practices are separated by <>.
# See also Beat the Know seacrh - new data 04-08, Description of fields, point 6).
View(documentData)

setkey(documentData, PRACTICE_RECOMMENDED)
View(documentData["Y"])

# PRACTICE_RECOMMENDED: Superficial checks suggest: Y if any practice recommends the document for anything. N if all practices who selected the document do not recommend the document.


#########################################
######## Understand evalData ############
#########################################

all.equal(evalData, submissionData)
# [1] TRUE
# the two data sets are the same.

###########################################
######## Cleaning searchData ##############
###########################################

# clean date formats
searchData$SearchTime           <- as.POSIXct(searchData$SearchTime, tz = "GMT")
searchData$NextSearchTime[searchData$NextSearchTime == ""] <- NA
searchData$NextSearchTime       <- as.POSIXct(searchData$NextSearchTime, tz = "GMT")
searchData$DownloadTime[searchData$DownloadTime == ""]     <- NA
searchData$DownloadTime         <- as.POSIXct(searchData$DownloadTime, tz = "GMT")

# basic stats
numObservations   <- nrow(searchData)
numDownloads      <- sum(!is.na(searchData$DownloadTime))
proportionSuccess <- numDownloads/numObservations
proportionSuccess
# [1] 0.5769709

# time between searches for each user
setkey(searchData, PIDX)
searchData <- searchData[order(PIDX, SearchTime),]
searchData <- searchData[, timeSinceLastSearch:= c(NA, diff(as.numeric(SearchTime))), by = PIDX]

View(searchData)


###########################################
######## Cleaning engagementData ##########
###########################################

engagementData$PERSON_PROJECT_START_DATE <- as.Date(as.POSIXct(engagementData$PERSON_PROJECT_START_DATE, tz = "GMT"))
engagementData$PERSON_PROJECT_END_DATE   <- as.Date(as.POSIXct(engagementData$PERSON_PROJECT_END_DATE, tz = "GMT"))

View(engagementData)

