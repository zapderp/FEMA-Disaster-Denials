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
disasterDenials$state = NULL
disasterDenials$state = disasterDenials$stateAbbreviation
disasterDenials$stateAbbreviation = NULL

# Need to combine as the declarationRequestNumber for denials doesn't appear in
# the summary
combined = bind_rows(disasterSummary,disasterDenials)
