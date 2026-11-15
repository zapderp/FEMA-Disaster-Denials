library(dplyr)

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
# this indicates a partial denial
denialsNoDate = disasterDenials[, !grepl("Date",names(disasterDenials))]
summaryNoDate = disasterSummary[, !grepl("Date",names(disasterSummary))]
denialsNoDateNoPartial = denialsNoDate[!(denialsNoDate$incidentId %in% summaryNoDate$incidentId),]
summaryNoDateNoPartial = summaryNoDate[!(summaryNoDate$incidentId %in% denialsNoDate$incidentId),]

denialsNoDate$state = NULL
denialsNoDate$state = denialsNoDate$stateAbbreviation
denialsNoDate$stateAbbreviation = NULL

denialsNoDate$id = NULL
summaryNoDate$id = NULL

denialsNoDate$incidentName = NULL
denialsNoDate$incidentId = NULL
summaryNoDate$incidentId = NULL

# Need to combine as the declarationRequestNumber for denials doesn't appear in
# the summary
combined = bind_rows(disasterSummary,disasterDenials)
