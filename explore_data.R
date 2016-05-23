

#
processDocumentData <- function(){

  # read (code by Maren E)
  documentData1 <- read.csv(paste0(env$basePath, "20160211_DocumentDetails_Part1.csv"), header = T)
  documentData2 <- read.csv(paste0(env$basePath, "20160211_DocumentDetails_part2.csv"), header = T)
  documentData3 <- read.csv(paste0(env$basePath, "20160211_DocumentDetails_part3.csv"), header = T)
  documentData <- as.data.table(rbind(documentData1,documentData2,documentData3))
  rm(list = c("documentData1", "documentData2", "documentData3"))

  # clean
  documentData[documentData == ""] <- NA
  documentData$Authored.date <- as.Date(documentData$Authored.date)
  auditDoc <- data_audit(as.data.frame(documentData))

  assign("documentData", documentData, envir = globalenv())

}


processPersonData <- function(){

  personData <- as.data.table(read.csv(paste0(env$basePath, "20160211_PersonInfo_Merged_updated.csv"), header = T))

  personData[personData == ""] <- NA
  personData$HIRE_DATE <- as.Date(dmy(paste0("01-", personData$HIRE_DATE)))
  auditPerson <- data_audit(as.data.frame(personData))

  # engagementData[engagementData == ""] <- NA
  # auditEngage <- data_audit(as.data.frame(engagementData))
  assign("personData", personData, envir = globalenv())
}




#
processSearchData <- function(){

  # remove searched w/o downloads
  searchDataProcessed <- searchData[!is.na(searchData$KO_ID),]

  # count how many times a document has been downloaded at any user, search-time and search-term pair
  searchDataProcessed <- searchDataProcessed[, list(nDownloads = .N), by = c( "PIDX", "SEARCHED_TERM", "KO_ID", "SearchTime")]

  # no puncutation, no extra sapces
  searchDataProcessed$SEARCHED_TERM_cleaned <- tm::removePunctuation(as.character(searchDataProcessed$SEARCHED_TERM))
  searchDataProcessed$SEARCHED_TERM_cleaned <- gsub("\\s+", " ", searchDataProcessed$SEARCHED_TERM_cleaned)

  return(searchDataProcessed)

}



meltSolrResults <- function(){

  # prepare solrSearch table
  solrSearchWithResult <- solrSearchData[!solrSearchData$Results == "",]
  solrSearchWithResult <- solrSearchWithResult[, strsplit(as.character(Results), ",", fixed = T), by = terms]
  colnames(solrSearchWithResult) <- c("terms", "docID")
  solrSearchWithResult <- solrSearchWithResult[, solrRank := 1:.N, by = terms]

  return(solrSearchWithResult)
}


exploreDocData <- function(){

  documentData$Authored.date <- as.Date(documentData$Authored.date)
}


exploreKnowData <- function(){

  if (!exists("searchDataProcessed")){searchDataProcessed <- processSearchData()}

  solrSearchData$terms_cleaned <- tm::removePunctuation(as.character(solrSearchData$terms))
  solrSearchData$terms_cleaned <- gsub("\\s+", " ", solrSearchData$terms_cleaned)
  solrSearchData <- solrSearchData[solrSearchData$terms_cleaned != "\\s+",]

  searchTermFreq <- searchDataProcessed[, list(nSearches = length(unique(SearchTime))), by = SEARCHED_TERM]

  # joined downloads with solr results
  joinedSolrWithDowloads <- merge(as.data.frame(searchDataProcessed), as.data.frame(solrSearchData), by.x = "SEARCHED_TERM", by.y = "terms")

  joinedSolrWithDowloads$Results <- as.character(joinedSolrWithDowloads$Results)

  # is the doc in list
  joinedSolrWithDowloads$isReturnedByKnow <- sapply(1:nrow(joinedSolrWithDowloads), function(x){
    is.element(as.character(joinedSolrWithDowloads[x, "KO_ID"]), unlist(strsplit(joinedSolrWithDowloads[x,"Results"], ",")) )
  })

  # number of solr results
  joinedSolrWithDowloads$nReturnedByKnow <- sapply(1:nrow(joinedSolrWithDowloads), function(x){
    length(unlist(strsplit(joinedSolrWithDowloads[x,"Results"], ",")) )
  })

  # rank in list of solr results
  joinedSolrWithDowloads$rankReturnedByKnow <- sapply(1:nrow(joinedSolrWithDowloads), function(x){
    min(which(unlist(strsplit(joinedSolrWithDowloads[x,"Results"], ",")) == as.character(joinedSolrWithDowloads[x, "KO_ID"])))
  })

  return(joinedSolrWithDowloads)
}




scoreAnacondaRanking <- function(rankedSearchDocuments, rankColumn =  "rankReturnedByKnow"){

  # add points next to ranks
  result <- merge(rankedSearchDocuments, scoringTable, by.x = rankColumn, by.y = "rank", all.x = TRUE)
  result$points[is.na(result$points)] <- 0

  result$SearchTime <- as.character(result$SearchTime)
  result <- as.data.table(result)

  searchScores <- result[, list(score = sum(points),
                                nHits = sum(isReturnedByKnow == TRUE)), by = c("SEARCHED_TERM", "PIDX", "SearchTime")]

  # find max score achievable with KNow results
  searchScores$nHits <- ifelse(searchScores$nHits > 20, 20, searchScores$nHits)

  maxScoreTable <- data.frame(nHits = 1:20, maxScore = cumsum(scoringTable$points))
  searchScores <- as.data.frame(merge(searchScores, maxScoreTable, by = "nHits", all.x = TRUE))
  searchScores$maxScore[is.na(searchScores$maxScore)] <- 0

  return(searchScores)
}




dummyModel <- function(){

  submissionData <- as.data.frame(submissionData)
  joinedSubmissionWithSolr <- merge(submissionData[, c("PIDX", "SEARCHED_TERM")], solrSearchData, by.x = "SEARCHED_TERM", by.y = "terms")

  joinedSubmissionWithSolr <- joinedSubmissionWithSolr[joinedSubmissionWithSolr$Results != "",]

  list_of_df <- lapply(1:nrow(joinedSubmissionWithSolr), FUN = function(x, joinedSubmissionWithSolr){

    KO_ID <- unlist(strsplit(as.character(joinedSubmissionWithSolr[x,"Results"]), ","))

    df <- data.frame(KO_ID = KO_ID, Rank = 1:length(KO_ID))
    df$PIDX <- joinedSubmissionWithSolr[x,"PIDX"]
    df$SEARCHED_TERM <- joinedSubmissionWithSolr[x,"SEARCHED_TERM"]

    return(df[1:min(20,nrow(df)),])
  }
  , joinedSubmissionWithSolr)

  #bound_df <- do.call("rbind", list_of_df)
  list_of_bound_dt <- lapply(1:38, function(x,k){
    rbindlist(list_of_df[ (1 + (x-1) * k) : (min(c(x*k, length(list_of_df))))])
  }, k = 1000)


  bound_dt <- rbindlist(list_of_bound_dt)
  bound_df <- as.data.frame(bound_dt)

  bound_df <-bound_df[, c("PIDX", "SEARCHED_TERM", "Rank", "KO_ID")]

  write.table(bound_df, "output/dummyModel.csv", sep = ",", row.names = FALSE, qmethod = "double")


}