

View(documentData)

# Match document to industry

sum(!(documentData$FUNCTIONAL_TAXONOMY_LINEAGE== ""))/nrow(documentData)
# [1] 0.6427088

sum(!(documentData$GEOGRAPHY_TAXONOMY_LINEAGE== ""))/nrow(documentData)
# [1] 0.2348601


# data to identify area for which document is relevant
matchDocData <- documentData[, .(DOC.ID, KDO, Related.KDO, Taxonomy, FUNCTIONAL_TAXONOMY_LINEAGE)]
View(matchDocData)

# For each person and time what d
matchPersonEngamentData <- engagementData[,.(PERSON_ID,PERSON_PROJECT_START_DATE,PERSON_PROJECT_END_DATE,PRIMARY_INDUSTRY_PRACTICE,SECONDARY_INDUSTRY_PRACTICE)]

View(matchPersonEngamentData)

person_ID = 144
search_date = as.Date("2016-08-16")

# given a person, and a time get the list of documents that might be relevant to the person
getRelevantDocuments <- function(person_ID, search_date){
  personEngagementData <- matchPersonEngamentData[PERSON_ID == person_ID]
  
  numProjectsInLastYear <- sum(personEngagementData$PERSON_PROJECT_END_DATE > (search_date - 365))
  if (numProjectsInLastYear>0){
    personEngagementData <- personEngagementData[PERSON_PROJECT_END_DATE > (search_date - 365)] 
  }
  
  # get list of industry names
  primary <- as.character(unique(personEngagementData$PRIMARY_INDUSTRY_PRACTICE))
  secondary <-  as.character(unique(personEngagementData$SECONDARY_INDUSTRY_PRACTICE))
  
  
  documents <- matchDocData[KDO %in% primary]
  
}


# get num



sum(matchPersonEngamentData$PRIMARY_INDUSTRY_PRACTICE == "?")
match(industryPractices, KDOs)

industryPractices <- as.character(levels(matchPersonEngamentData$PRIMARY_INDUSTRY_PRACTICE))
numIndustryPractices <- length(industryPractices)
industryPractices <- data.frame(practiceID = 1:numIndustryPractices, practiceName = industryPractices)
library(stringr)
industryPractices$practiceName_cleaned <- str_replace_all(industryPractices$practiceName, "[[:punct:]]","")
industryPractices$practiceName_cleaned <- str_replace_all(industryPractices$practiceName_cleaned, "and","")


KDOs          <- sort(as.character(levels(matchDocData$KDO)))
numKDOs       <- length(KDOs)
KDOs <- data.frame(KDO_id = 1:numKDOs, KDO_name = KDOs, industryPract = NA)
KDOs$KDO_name <- as.character(KDOs$KDO_name)
KDOs$KDO_name_cleaned <- str_replace_all(KDOs$KDO_name, "[[:punct:]]", "")
KDOs$KDO_name_cleaned <- str_replace_all(KDOs$KDO_name_cleaned, "and", "")
View(industryPractices)
View(KDOs)

head(KDOs)
thisKDO = 1
thisIP = 5
for (thisKDO in 1:numKDOs){
  vec = NULL
  for (thisIP in 1:numIndustryPractices){
      numMatches = length(intersect(as.vector(unlist(strsplit(strwrap(KDOs$KDO_name_cleaned[thisKDO]), " "))),as.vector(unlist(strsplit(strwrap(industryPractices$practiceName_cleaned[thisIP]), " ")))))
      if (numMatches>0){
        vec = c(vec, industryPractices$practiceID[thisIP])
      }
  }
  if (!is.null(vec)){
    KDOs$industryPract[thisKDO] <- paste(vec, collapse = ",")
  }
}


View(industryPractices)
View(as.data.frame(KDOs))


unique(documentData$KDO)
documentData$Taxonomy
documentData$FUNCTIONAL_TAXONOMY_LINEAGE
documentData$GEOGRAPHY_TAXONOMY_LINEAGE


View(personData)
personData$Practice.affiliation
personData$Department
personData$STAFFING_OFFICE_COUNTRY
personData$DEPARTMENT_NAME
personData$FUNCTIONAL_AREA
personData$L3_DEPARTMENT_NAME

View(siteData)




