# script containing function definitions for ranking retrieved documents using
# the output of a binary classifier predicting the probability of a document
# being downloaded



# main function for creating a classification data frame
prepareClassficationData <- function(){

  searchDataProcessed <- processSearchData()
  meltedSolrResults <- meltSolrResults()
  meltedSolrResults$docID <- as.integer(meltedSolrResults$docID)

  # group by term, user, time to find searching instances
  searchInstances <- searchDataProcessed[, list(nDocumentsDowloaded = .N), by = c( "PIDX", "SEARCHED_TERM",  "SearchTime") ]

  # for each searchInstance, get one line for each document retrieved by solr
  setkey(meltedSolrResults, terms)
  setkey(searchInstances, SEARCHED_TERM)
  result <- meltedSolrResults[searchInstances, nomatch=0, allow.cartesian = TRUE]

  # provide labels
  setkeyv(result, c("terms", "PIDX", "SearchTime", "docID"))
  setkeyv(searchDataProcessed, c("SEARCHED_TERM", "PIDX", "SearchTime", "KO_ID"))

  resultWithLabel <- searchDataProcessed[result]
  resultWithLabel$IsDownloaded <- !is.na(resultWithLabel$nDownloads)

  return(resultWithLabel)
}



# classification data frame
buildClassificationData <- function(){

  binaryDownloadData <- prepareClassficationData()
  binaryDownloadData$SearchTime <- as.Date(binaryDownloadData$SearchTime)

  # join with Document Cols
  processDocumentData()
  docCols <- c( "DOC.ID"
                 , "Authored.date"
                 , "KDO"
                 , "Shelf.life"
                 , "Last.Review.Date"
                 , "Recommended.use"
                 , "Content.type"
                 , "State"
                 , "Downloads.12.months."
                 , "Downloads.24.months.")

  documentData <- documentData[, docCols, with = FALSE]
  documentData <- documentData[complete.cases(documentData)]

  setkey(documentData, DOC.ID)
  setkey(binaryDownloadData, KO_ID)
  result <- documentData[binaryDownloadData, nomatch=0]
  # only doc authored before the date of search
  result <- result[result$SearchTime > result$Authored.date]


  # join with Person Cols
  processPersonData()
  personCols <- c("PERSON_ID"
                  , "HIRE_DATE"
                  , "POSITION_CODE"
                  , "FUNCTIONAL_AREA"
                  , "L2_DEPARTMENT_NAME"
                  , "Role"
                  , "GENDER")

  personData <- personData[, personCols, with = FALSE]
  personData <- personData[complete.cases(personData)]

  setkey(personData, PERSON_ID)
  setkey(result, PIDX)
  result <- personData[result, nomatch=0]

  result[, seniorityInDays := as.integer(SearchTime - HIRE_DATE),]

  assign("classifData", result, envir = globalenv())
}




processClassificationData <- function(dframe){

  depVar <- "IsDownloaded"
  indepVars <- setdiff(names(dframe), c(depVar, "PERSON_ID", "HIRE_DATE", "DOC.ID", "Authored.date"
                                             , "SEARCHED_TERM", "SearchTime", "nDownloads"
                                             ,  "SEARCHED_TERM_cleaned", "SearchDate",  "nDocumentsDowloaded" ))

  categoricalVars <- c()
  for (varName in indepVars){
    if ("character" %in% class(dframe[, varName])) {categoricalVars <- c(categoricalVars, varName)}
    if ("factor" %in% class(dframe[, varName])) {
      dframe[, varName] <- as.character(dframe[, varName])
      categoricalVars <- c(categoricalVars, varName)
    }
  }
  message(paste0("Categorical Independent Variables: ", paste(categoricalVars, collapse = ", ")))

  # build dummy vars
  out <- CreateDummyVariables(dataA = dframe, categoricalFeatures = categoricalVars, min.count = 1000)
  dframe <- out$first

}



