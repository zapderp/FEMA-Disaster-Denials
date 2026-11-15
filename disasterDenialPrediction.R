library(dplyr)
library(stringr)

# https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
disasterSummary = read.csv("DisasterDeclarationsSummaries.csv")
# https://www.fema.gov/openfema-data-page/declaration-denials-v1
disasterDenials = read.csv("DeclarationDenials.csv")

# Don't see any meaningful missing values but see 59 different states?
summary(disasterSummary)
colSums(is.na(disasterSummary))
colSums(disasterSummary == "")
# Again no meaningful missing values
summary(disasterDenials)
colSums(is.na(disasterDenials))
colSums(disasterDenials == "")

# Remove/update fields (Still to do, need to add Predictor variable to each df)
# Need to remove disasters that show up in both databases (same incidentID) as 
# this indicates a partial denial. Partial denial means the requested program
# for the disaster was not fully met (If requested both ih and pa and only got pa)
denialsNoDate = disasterDenials[, !grepl("Date",names(disasterDenials))]
summaryNoDate = disasterSummary[, !grepl("Date",names(disasterSummary))]

# Replace denials state field with abbreviation to match summary
denialsNoDate$state = NULL
denialsNoDate$state = denialsNoDate$stateAbbreviation
denialsNoDate$stateAbbreviation = NULL

denialsNoDate$id = NULL # Weird looking id
summaryNoDate$id = NULL
denialsNoDate$declarationRequestNumber = NULL # These aren't the same between the dbs, use incidentID
summaryNoDate$declarationRequestNumber = NULL
denialsNoDate$currentRequestStatus = NULL
summaryNoDate$femaDeclarationString = NULL # Concatenation of declarationType, disasterNumber and state
summaryNoDate$disasterNumber = NULL # Number identifier from the 1950's
summaryNoDate$fyDeclared = NULL # Fiscal year for declaration
summaryNoDate$declarationTitle = NULL # Ex: Name of hurricane
denialsNoDate$incidentName = NULL # Descriptive version of declarationTitle above
summaryNoDate$fipsCountyCode = NULL # Don't have counties within denials csv
summaryNoDate$fipsStateCode = NULL # Already have state field
summaryNoDate$placeCode = NULL # Another version of the county code
summaryNoDate$lastRefresh = NULL
summaryNoDate$hash = NULL

# Summary declarationType:
# DR = Major Disaster
# FM = Fire Management
# EM = Emergency
# Need to convert denials declarationRequestType to two character code above
replacements = c("Major Disaster" = "DR", "Fire Management" = "FM", "Emergency" = "EM")
denialsNoDate$declarationType = str_replace_all(denialsNoDate$declarationRequestType,replacements)
denialsNoDate$declarationRequestType = NULL

# Rename program fields to match
summaryNoDate = summaryNoDate %>% rename_with(~str_replace(.,'Declared', ''))
denialsNoDate = denialsNoDate %>% rename_with(~str_replace(.,'Requested', ''))

# Filtering out partial denials
denialsNoDateNoPartial = denialsNoDate[!(denialsNoDate$incidentId %in% summaryNoDate$incidentId),]
summaryNoDateNoPartial = summaryNoDate[!(summaryNoDate$incidentId %in% denialsNoDate$incidentId),]

# Need to combine as the declarationRequestNumber for denials doesn't appear in
# the summary
combined = bind_rows(disasterSummary,disasterDenials)
