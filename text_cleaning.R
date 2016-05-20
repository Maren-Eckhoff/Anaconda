######################################################################
######### Create table that matches search terms to documents ########
######################################################################

# prepare search term to document map
searchTermToDocumentMap <- data.table(SEARCHED_TERM = unique(c(levels(searchData$SEARCHED_TERM), levels(submissionData$SEARCHED_TERM))))
# nrow(searchTermToDocumentMap)
# [1] 625015

# prepare solrSearch table
solrSearchWithResult <- solrSearchData[!solrSearchData$Results == "",]
solrSearchWithResult <- solrSearchWithResult[, strsplit(as.character(Results), ",", fixed = T), by = terms]
colnames(solrSearchWithResult) <- c("terms", "docID")
solrSearchWithResult <- solrSearchWithResult[, solrRank := 1:.N, by = terms]

# join the solr search results to search terms
setkey(searchTermToDocumentMap,SEARCHED_TERM)
setkey(solrSearchWithResult,terms)
searchTermToDocumentMap <- solrSearchWithResult[searchTermToDocumentMap]
colnames(searchTermToDocumentMap) <- c("SEARCHED_TERM", "docID", "solrRank")

searchTermStatus <- searchTermToDocumentMap[, max(solrRank), by = SEARCHED_TERM]
colnames(searchTermStatus) <- c("SEARCHED_TERM", "NumDocsFound")

# nrow(searchTermStatus)
# [1] 625015

searchTermStatus$NumDocsFound[is.na(searchTermStatus$NumDocsFound)] <- 0

# sum(searchTermStatus$NumDocsFound <25)/nrow(searchTermStatus)
# [1] 0.3091654

# hist(searchTermStatus$NumDocsFound)
# max(searchTermStatus$NumDocsFound)

# 30 % of the search terms need more work.
# idea1: Use cleaned search terms for those
# idea2: Use exact matching of title and abstract
# idea3: Use edit distance

##############################################
######### Clean all text data ################
##############################################

library(tm)

cleanTextPunctSpace <- function(textVector){
  textVector  <- as.character(textVector)
  cleanedText <- tm::removePunctuation(as.character(textVector))
  cleanedText <- gsub("\\s+", " ", cleanedText)
  return(cleanedText)
}

cleanedTextFull <- function(textVecor){
  cleanedText <- cleanTextPunctSpace(textVecor)
  textCorpus  <- Corpus(VectorSource(cleanedText))
  # remove some special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  textCorpus <- tm_map(textCorpus, toSpace, "/",lazy=TRUE)
  textCorpus <- tm_map(textCorpus, toSpace, "@",lazy=TRUE)
  textCorpus <- tm_map(textCorpus, toSpace, "\\|",lazy=TRUE)
  # convert to lower case
  textCorpus <- tm_map(textCorpus, content_transformer(tolower),lazy=TRUE)
  # Remove numbers
  textCorpus <- tm_map(textCorpus, removeNumbers,lazy=TRUE)
  # Remove english common stopwords
  textCorpus <- tm_map(textCorpus, removeWords, stopwords("english"),lazy=TRUE)
  # Remove punctuations
  textCorpus <- tm_map(textCorpus, removePunctuation,lazy=TRUE)
  # Eliminate extra white spaces
  textCorpus <- tm_map(textCorpus, stripWhitespace,lazy=TRUE)
}

searchData$SEARCHED_TERM_cleaned     <- cleanTextPunctSpace(searchData$SEARCHED_TERM)
submissionData$SEARCHED_TERM_cleaned <- cleanTextPunctSpace(submissionData$SEARCHED_TERM)

# clean solr search data and remove the instances that we can't clean
solrSearchData$terms_cleaned <- cleanTextPunctSpace(solrSearchData$terms)
# solrSearchData               <- solrSearchData[solrSearchData$terms_cleaned != "\\s+",]


nrow(searchData) + nrow(submissionData)
length(unique(searchData$SEARCHED_TERM_cleaned))
length(searchTermToDocumentMap)

head(searchTermToDocumentMap)

# clean document data
documentData$Title_cleaned_fully          <- cleanedTextFull(documentData$Title)
documentData$Short_Abstract_cleaned_fully <- cleanedTextFull(documentData$Short.Abstract)

searchData$SEARCHED_TERM_cleaned_fully    <- cleanedTextFull(searchData$SEARCHED_TERM)