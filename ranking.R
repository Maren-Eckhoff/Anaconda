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
  binaryDownloadData$SearchTime <- as.character(binaryDownloadData$SearchTime)

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
  result <- result[as.Date(result$SearchTime) > result$Authored.date]


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

  result[, seniorityInDays := as.integer(as.Date(SearchTime) - HIRE_DATE),]

  assign("classifData", result, envir = globalenv())
}





processClassificationData <- function(dframe){

  depVar <- "IsDownloaded"
  indepVars <- setdiff(names(dframe), c(depVar, "PERSON_ID", "HIRE_DATE", "DOC.ID", "Authored.date", "Last.Review.Date"
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
  out <- CreateDummyVariables(dataA = dframe, categoricalFeatures = categoricalVars, min.count = nrow(dframe) * 0.05)
  dframe <- out$first

  return(dframe)
}





splitAnacondaData <- function(dframe){

  dt <- as.data.table(dframe)
  searchInstances <- dt[,list(nrows = .N) ,by = c( "PERSON_ID", "SEARCHED_TERM",  "SearchTime") ]

  # split into
  outSD <- splitData(data = as.data.frame(searchInstances), trainPercent = 70)
  trainInstances <- as.data.table(outSD$trainData)
  testInstances <- as.data.table(outSD$testData)

  setkeyv(dt, c( "PERSON_ID", "SEARCHED_TERM",  "SearchTime"))
  setkeyv(trainInstances, c("PERSON_ID", "SEARCHED_TERM",  "SearchTime"))
  trainDf <- as.data.frame(dt[trainInstances, nomatch = 0])
  setkeyv(testInstances, c("PERSON_ID", "SEARCHED_TERM",  "SearchTime"))
  testDf <- as.data.frame(dt[testInstances, nomatch = 0])

  assign("trainDf", trainDf, envir = globalenv())
  assign("testDf", testDf, envir = globalenv())

}


fitModel <- function(){

  depVar <- "IsDownloaded"
  indepVars <- setdiff(names(trainDf), c(depVar, "PERSON_ID", "HIRE_DATE", "DOC.ID", "Authored.date", "Last.Review.Date"
                                        , "SEARCHED_TERM", "SearchTime", "nDownloads"
                                        ,  "SEARCHED_TERM_cleaned", "SearchDate",  "nDocumentsDowloaded"
                                        , "nrows"))

  trainDf <- trainDf[complete.cases(trainDf[, c(depVar, indepVars)]), ]

  startTime <- proc.time()
  lasso.binomial <- glmnet(x = data.matrix(trainDf[, indepVars])
                                 , y = as.factor(trainDf[, depVar])
                                 , standardize = TRUE, family = "binomial")
  timeDiff <- proc.time() - startTime
  message(paste0("fitting a glmnet took ", timeDiff[["elapsed"]], " seconds"))

  startTime <- proc.time()
  cv.lasso.binomial <- cv.glmnet(x = data.matrix(trainDf[, indepVars])
                              , y = as.factor(trainDf[, depVar])
                              , standardize = TRUE, nfolds = 10, family = "binomial", parallel = TRUE)
  timeDiff <- proc.time() - startTime
  message(paste0("fitting a cv glmnet took ", timeDiff[["elapsed"]], " seconds"))


}




assessModel <- function(){

  predTrain <- predict.glmnet(lasso.binomial, newx = data.matrix(trainDf[, indepVars]), s = min(lasso.binomial$lambda), type = "response")
  predTrain <- sapply(predTrain, function(x){ 1 / (1 + exp(-x))})

  predTest <- predict.glmnet(lasso.binomial, newx = data.matrix(testDf[, indepVars]), s = min(lasso.binomial$lambda), type = "response")
  predTest <- sapply(predTest, function(x){ 1 / (1 + exp(-x))})

  indices <- sample(1:nrow(trainDf), 1e5)
  roc(response = trainDf$IsDownloaded[indices], predictor = as.vector(predTrain[indices])
      , levels = levels(as.factor(trainDf$IsDownloaded)), plot = TRUE)

  indices <- sample(1:nrow(testDf), 1e5)
  roc(response = testDf$IsDownloaded[indices], predictor = as.vector(predTest[indices])
      , levels = levels(as.factor(testDf$IsDownloaded)), plot = TRUE)


  # Anaconda score
  testDf$predicted <- predTest
  testDf <- as.data.table(testDf)
  testDf[order(predicted), "rankQb" := order(predicted, decreasing = TRUE), by = c( "PERSON_ID", "SEARCHED_TERM",  "SearchTime")]

  setnames(testDf,"PERSON_ID","PIDX")
  scoreSolr <- scoreAnacondaRanking(rankedSearchDocuments = testDf[testDf$IsDownloaded == 1,], rankColumn = "solrRank")
  scoreQb <- scoreAnacondaRanking(rankedSearchDocuments = testDf[testDf$IsDownloaded == 1,], rankColumn = "rankQb")
}


runRanking <- function(){

  # horrible:
  source("util.R")
  source("explore_data.R")
  source("ranking.R")

  buildClassificationData()
  classifData <- processClassificationData(as.data.frame(classifData))
  splitAnacondaData(dframe = classifData)

}

