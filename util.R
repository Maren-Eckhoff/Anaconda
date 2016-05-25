
CreateDummyVariables <- function(dataA, dataB = c(), categoricalFeatures, referenceLevels = c(), min.count = 10, verbose = TRUE) {

  if (length(referenceLevels) == 0){
    referenceLevels <- vector(mode = "character", length = length(categoricalFeatures))
    names(referenceLevels) <- categoricalFeatures
  }
  allLevels <- vector(mode = "list", length = length(categoricalFeatures))
  names(allLevels) <- categoricalFeatures


  for (feature in categoricalFeatures){

   # message(paste0("processing ", as,character(feature)))
    featureHistogram <- table(dataA[, feature])

    # get reference level as the most frequent category, unless some reference
    # level has already been provided in reference Levels
    if (referenceLevels[feature] == ""){
      crtReferenceLevel <- names(which.max(featureHistogram))
      referenceLevels[feature] <- crtReferenceLevel
    } else {
      crtReferenceLevel <- referenceLevels[feature]
    }

    # convert all levels but the reference one to binary values, and add them to the data frame
    dummyVars <- c()

    myLevels <- names(featureHistogram)[featureHistogram > min.count]
    message(paste0("levels: ", paste(myLevels, collapse = ", ")))

    for (level in setdiff(myLevels, crtReferenceLevel)) {
      friendlyLevel <- gsub("\\s", "", level)
      friendlyLevel <- gsub("-", "", friendlyLevel)
      friendlyLevel <- gsub("/", "", friendlyLevel)
      crtDummyVar <- paste(feature, friendlyLevel, sep = ".")
        dataA[, crtDummyVar] <- ifelse(dataA[, feature] == level, 1, 0)
        dataB[, crtDummyVar] <- ifelse(dataB[, feature] == level, 1, 0)
        #message(paste0("omitting dummy variable ", as.character(friendlyLevel), " due to finding less than ", as.character(min.count), " activations"))


      dummyVars <- c(dummyVars, crtDummyVar)
    }
    allLevels[[feature]] <- dummyVars

    # remove original categorical variable
    dataA <- dataA[, names(dataA) != feature]
    dataB <- dataB[, names(dataB) != feature]
  }


  # store the reference levels in data frame
  referenceLevels <- as.data.frame(referenceLevels)
  referenceLevels$VariableName <- row.names(referenceLevels)
  referenceLevels <- referenceLevels[, c(2,1)]


  out = list(first = dataA, second = dataB, third = referenceLevels, fourth = allLevels)
  return(out)

}