


exploreKnowData <- function(){

  # remove searched w/o downloads
  searchData <- searchData[!is.na(searchData$KO_ID),]

  # no puncutation, no extra sapces
  searchData$SEARCHED_TERM_cleaned <- tm::removePunctuation(as.character(searchData$SEARCHED_TERM))
  searchData$SEARCHED_TERM_cleaned <- gsub("\\s+", " ", searchData$SEARCHED_TERM_cleaned)

  solrSearchData$terms_cleaned <- tm::removePunctuation(as.character(solrSearchData$terms))
  solrSearchData$terms_cleaned <- gsub("\\s+", " ", solrSearchData$terms_cleaned)
  solrSearchData <- solrSearchData[solrSearchData$terms_cleaned != "\\s+",]

  # joined downloads with solr results
  joinedSolrWithDowloads <- merge(as.data.frame(searchData), as.data.frame(solrSearchData), by.x = "SEARCHED_TERM", by.y = "terms")

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

  # add points next to ranks
  result <- merge(joinedSolrWithDowloads,scoringTable, by.x = "rankReturnedByKnow", by.y = "rank", all.x = TRUE)
  result$points[is.na(result$points)] <- 0


  result$SearchTime <- as.character(result$SearchTime)
  result <- as.data.table(result)

  searchScores <- result[, list(score = sum(points),
                                nHits = .N), by = c("SEARCHED_TERM", "PIDX", "SearchTime")]

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

  write.table(submission, gsub(" ", "_", paste0(data_dir, "submission_", gsub("[[:punct:]]", "_", as.character(d)), ".csv")),
              qmethod = "double", sep=",", row.names = F)

}