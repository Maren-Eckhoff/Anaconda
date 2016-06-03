#load required packages
library(lubridate)
library('tm')
install.packages('wordcloud')
library('wordcloud')
library('stringr')
install.packages('stringdist')
library('stringdist')

#read data from csv files
DocumentDetails_Part1 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_DocumentDetails_Part1.csv')
DocumentDetails_Part2 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_DocumentDetails_Part2.csv')
DocumentDetails_Part3 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_DocumentDetails_Part3.csv')
EngagementData_Part1 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_EngagementData_Part1.csv')
EngagementData_Part2 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_EngagementData_Part2.csv')
EngagementData_Part3 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_EngagementData_Part3.csv')
PersonInfo_Merged_updated <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_PersonInfo_Merged_updated.csv')
SearchDownload_Part1 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_SearchDownload_Part1.csv')
SearchDownload_Part2 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_SearchDownload_Part2.csv')
SearchDownload_Part3 <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/20160211_SearchDownload_Part3.csv')
Evaluation_Data_Set <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/Evaluation Data Set.csv')
Submission_File <- read.csv('/Users/musa.bilal/Desktop/anaconda/Data/Beat_the_KNOW_search/Submission_File.csv')

#load Rdata
load('/Users/musa.bilal/Desktop/anaconda/Data/anaconda_data.RData')

save.image('/Users/musa.bilal/Desktop/anaconda/Data/anaconda_data_new.RData')
save.image('/Users/musa.bilal/Desktop/anaconda/Data/anaconda_data_new_final.RData')

load('/Users/musa.bilal/Desktop/anaconda/Data/anaconda_data_new_final.RData')

#merge files into single dataframe
doc_details <- rbind(DocumentDetails_Part1,DocumentDetails_Part2,DocumentDetails_Part3)
eng_details <- rbind(EngagementData_Part1,EngagementData_Part2,EngagementData_Part3)
search_details <- rbind(SearchDownload_Part1,SearchDownload_Part2,SearchDownload_Part3)

#Document Profiling
#order searches by the same user and time
search_details <- search_details[order(search_details$PIDX,search_details$SearchTime),]
#get time spent between searches
search_details$SearchTime <- ymd_hms(search_details$SearchTime)
search_details$NextSearchTime <- ymd_hms(search_details$NextSearchTime)
search_details$time_diff <- difftime(search_details$NextSearchTime,search_details$SearchTime)
#aggregate by user_id and get median time that each user spends between searches
median_time_spent_without_download <- aggregate(subset(search_details,time_diff!=0 & is.na(KO_ID)!=TRUE)$time_diff, 
                                                by=list(Category=subset(search_details,time_diff!=0 & is.na(KO_ID)!=TRUE)$PIDX), 
                                                FUN=median)
median_time_spent_with_download <- aggregate(subset(search_details,time_diff!=0 & is.na(KO_ID)==TRUE)$time_diff, 
                                                by=list(Category=subset(search_details,time_diff!=0 & is.na(KO_ID)==TRUE)$PIDX), 
                                                FUN=median)

#Join median time spent with and without download to the original search details dataframe
search_details <- merge(x=search_details,y=median_time_spent_with_download,by.x="PIDX",by.y="Category",all.x=TRUE)
search_details <- merge(x=search_details,y=median_time_spent_without_download,by.x="PIDX",by.y="Category",all.x=TRUE)
names(search_details)[9:10,] <- c("median_time_w_dl","median_time_wo_dl")

#Stemming documents by keywords
#create corpus
title_corpus<-Corpus(VectorSource(doc_details$Title))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
title_corpus <- tm_map(title_corpus, toSpace, "/",lazy=TRUE)
title_corpus <- tm_map(title_corpus, toSpace, "@",lazy=TRUE)
title_corpus <- tm_map(title_corpus, toSpace, "\\|",lazy=TRUE)
# Convert the text to lower case
title_corpus <- tm_map(title_corpus, content_transformer(tolower),lazy=TRUE)
# Remove numbers
title_corpus <- tm_map(title_corpus, removeNumbers,lazy=TRUE)
# Remove english common stopwords
title_corpus <- tm_map(title_corpus, removeWords, stopwords("english"),lazy=TRUE)
# Remove your own stop word
# specify your stopwords as a character vector
#title_corpus <- tm_map(title_corpus, removeWords, c("blabla1", "blabla2"),lazy=TRUE) 
# Remove punctuations
title_corpus <- tm_map(title_corpus, removePunctuation,lazy=TRUE)
# Eliminate extra white spaces
title_corpus <- tm_map(title_corpus, stripWhitespace,lazy=TRUE)
strwrap(title_corpus[[500]])

#Stemming abstract by keywords
abstract_corpus<-Corpus(VectorSource(doc_details$Short.Abstract))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
abstract_corpus <- tm_map(abstract_corpus, toSpace, "/",lazy=TRUE)
abstract_corpus <- tm_map(abstract_corpus, toSpace, "@",lazy=TRUE)
abstract_corpus <- tm_map(abstract_corpus, toSpace, "\\|",lazy=TRUE)
# Convert the text to lower case
abstract_corpus <- tm_map(abstract_corpus, content_transformer(tolower),lazy=TRUE)
# Remove numbers
abstract_corpus <- tm_map(abstract_corpus, removeNumbers,lazy=TRUE)
# Remove english common stopwords
abstract_corpus <- tm_map(abstract_corpus, removeWords, stopwords("english"),lazy=TRUE)
# Remove your own stop word
# specify your stopwords as a character vector
#abstract_corpus <- tm_map(abstract_corpus, removeWords, c("blabla1", "blabla2"),lazy=TRUE) 
# Remove punctuations
abstract_corpus <- tm_map(abstract_corpus, removePunctuation,lazy=TRUE)
# Eliminate extra white spaces
abstract_corpus <- tm_map(abstract_corpus, stripWhitespace,lazy=TRUE)
strwrap(abstract_corpus[[200]])

#string comparisons between search queries and document abstracts
for (i in 1:100) {
  temp=0
  for (j in 1:100) {
    cat(length(intersect(as.vector(unlist(strsplit(strwrap(search_corpus[[i]]), " "))),as.vector(unlist(strsplit(strwrap(abstract_corpus[[j]]), " "))))))
  }
}

searchMatching <- function(numOfSearches=49509,numOfAbstractsAndTitles=54888) {
  search_doc_matrix <- data.frame(search_terms=as.character(),doc_id=as.integer(),score=as.numeric())
  for (i in 1:numOfSearches) {
    abstract_temp=rep(0,length(unlist(strsplit(strwrap(search_corpus[[i]]), " "))))
    title_temp=rep(0,length(unlist(strsplit(strwrap(search_corpus[[i]]), " "))))
    doc_no <- 1
    for (k in 1:length(unlist(strsplit(strwrap(search_corpus[[1]]), " ")))) {
      for (j in 1:numOfAbstractsAndTitles) {
        abstract_temp <- adist(as.vector(unlist(strsplit(strwrap(search_corpus[[i]]), " ")))[k],
                               as.vector(unlist(strsplit(strwrap(abstract_corpus[[j]]), " "))))
        abstract_score = mean(abstract_temp) * (min(abstract_temp)+1)
      
        title_temp <- adist(as.vector(unlist(strsplit(strwrap(search_corpus[[i]]), " ")))[k],
                            as.vector(unlist(strsplit(strwrap(title_corpus[[j]]), " "))))
        title_score = mean(title_temp) * (min(title_temp)+1)
      }
      abstract_temp[k] <- abstract_score
      title_temp[k] <- title_score
      abstract_final_score <- mean(abstract_temp)
      title_final_score <- mean(title_temp)
      final_score <- (0.7 * (title_final_score)) + (0.3 * (abstract_final_score))
      cat("final abstract score",abstract_final_score,'\n')
      cat("final title score",title_final_score,'\n')
      cat("final score",final_score,'\n\n')
      #rbind(search_details$SEARCHED_TERM[i],doc_details$DOC.ID[doc_no])
    }
  }
}

searchMatching()

cat(length(intersect(as.vector(unlist(strsplit(strwrap(search_corpus[[i]]), " "))),as.vector(unlist(strsplit(strwrap(abstract_corpus[[j]]), " "))))))
rep(0,3)


#Stemming search querries by keywords
search_corpus<-Corpus(VectorSource(Evaluation_Data_Set$SEARCHED_TERM[1:49509]))
temp_search_corpus<-search_corpus


search_corpus<-temp_search_corpus
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
search_corpus <- tm_map(search_corpus, toSpace, "/",lazy=TRUE)
search_corpus <- tm_map(search_corpus, toSpace, "@",lazy=TRUE)
search_corpus <- tm_map(search_corpus, toSpace, "\\|",lazy=TRUE)
# Convert the text to lower case
search_corpus <- tm_map(search_corpus, content_transformer(tolower),lazy=TRUE)
# Remove numbers
search_corpus <- tm_map(search_corpus, removeNumbers,lazy=TRUE)
# Remove english common stopwords
search_corpus <- tm_map(search_corpus, removeWords, stopwords("english"),lazy=TRUE)
# Remove your own stop word
# specify your stopwords as a character vector
#search_corpus <- tm_map(search_corpus, removeWords, c("blabla1", "blabla2"),lazy=TRUE) 
# Remove punctuations
search_corpus <- tm_map(search_corpus, removePunctuation,lazy=TRUE)
# Eliminate extra white spaces
search_corpus <- tm_map(search_corpus, stripWhitespace,lazy=TRUE)
strwrap(search_corpus[11])

length(unlist(strsplit(strwrap(search_corpus[[1]]), " ")))

strsplit(strwrap(search_corpus[[10]]), " ")
merging <- merge(doc_details,eng_details,by.x="KDO",by.y="PRIMARY_INDUSTRY_PRACTICE")


#Ad-hoc
names(eng_details)
names(search_details)
head(search_details)
head(doc_details)
View(head(doc_details,100))

#join industry sector to documents and people

