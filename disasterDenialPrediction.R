library(dplyr)
library(stringr)
library(ggplot2)

# https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
disasterSummary = read.csv("DisasterDeclarationsSummaries.csv")
# https://www.fema.gov/openfema-data-page/declaration-denials-v1
disasterDenials = read.csv("DeclarationDenials.csv")

# Don't see any meaningful missing values but see 59 different states?
# AS (American Samoa)
# GU (Guam)
# MP (Northern Marina Islands)
# PR (Puerto Rico)
# VI (US Virgin Islands)
# DC (District of Columbia)
# FM (Federated states of Micronesia)
# MH (Marshall Islands)
# PW (Palau)
summary(disasterSummary)
colSums(is.na(disasterSummary))
colSums(disasterSummary == "")
summaryVars = dim(disasterSummary)
# Again no meaningful missing values
summary(disasterDenials)
colSums(is.na(disasterDenials))
colSums(disasterDenials == "")
denialVars = dim(disasterDenials)

# Add predictor class to dataframes
disasterDenials$Denied = 1
disasterSummary$Denied = 0

# Remove/update fields (Still to do, need to add Predictor variable to each df)
# Need to remove disasters that show up in both databases (same incidentID) as 
# this indicates a partial denial. Partial denial means the requested program
# for the disaster was not fully met (If requested both ih and pa and only got pa)
denialsNoDate = disasterDenials[, !grepl("Date",names(disasterDenials))]
summaryNoDate = disasterSummary[, !grepl("Date",names(disasterSummary))]

# Replace denials state field with abbreviation to match summary
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
summaryNoDate$designatedIncidentTypes = NULL # Info not found in denials csv. Contains more specific identifiers for a disaster
summaryNoDate$designatedArea = NULL # Info not found in denials csv, denotes county

# Summary declarationType:
# DR = Major Disaster
# FM = Fire Management
# EM = Emergency
# Need to convert denials declarationRequestType to two character code above
replacements = c("Major Disaster" = "DR", "Fire Management" = "FM", "Emergency" = "EM")
denialsNoDate$declarationType = str_replace_all(denialsNoDate$declarationRequestType,replacements)
denialsNoDate$declarationRequestType = NULL

# Rename requestedIncidentTypes to match summary csv
denialsNoDate$incidentType = denialsNoDate$requestedIncidentTypes
denialsNoDate$requestedIncidentTypes = NULL

# Rename program fields to match
summaryNoDate = summaryNoDate %>% rename_with(~str_replace(.,'Declared', ''))
denialsNoDate = denialsNoDate %>% rename_with(~str_replace(.,'Requested', ''))

# Filtering out partial denials
denialsNoDateNoPartial = denialsNoDate[!(denialsNoDate$incidentId %in% summaryNoDate$incidentId),]
summaryNoDateNoPartial = summaryNoDate[!(summaryNoDate$incidentId %in% denialsNoDate$incidentId),]

# Need to combine as the declarationRequestNumber for denials doesn't appear in
# the summary
combined = bind_rows(summaryNoDateNoPartial,denialsNoDateNoPartial)
summary(combined)

# Will need to do resampling techniques due to the denied predictor class being 
# the minority. Can either use upsampling on the denials df or downsample the
# summary df

# Need to remove duplicates as the original disaster summary countains counties
# and different counties got different aid packages
# Apparently have multi-state incidents in the csv, along with states declaring 
# multiple emergencies. Will need to group by incidentId, state and type.
# Actually indian reservations will also request aid too and not always follow
# the same incidentType
combined_squashed = combined %>% group_by(incidentId, state, declarationType, incidentType) %>%
  summarise(
    state = unique(state),
    declarationType = unique(declarationType),
    incidentType = unique(incidentType),
    ihProgram = max(ihProgram),
    iaProgram = max(iaProgram),
    paProgram = max(paProgram),
    hmProgram = max(hmProgram),
    tribalRequest = max(tribalRequest),
    incidentId = unique(incidentId),
    region = unique(region),
    Denied = unique(Denied)
  )

summary(combined_squashed)

# See some instances where no program was requested (all zeros) and subsequently
# the request was denied

# Disaster count by state
ggplot(combined_squashed,aes(x=state)) + geom_histogram(stat = "count") + 
  labs(title = "Count of Disaster Submissions by State") + xlab("State") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count of disaster declaration type
ggplot(combined_squashed,aes(x=declarationType)) + geom_histogram(stat = "count") + 
  labs(title = "Count of Disaster Declaration Type") + xlab("Declaration Type") + ylab("Count")

# Count of disaster incident type
ggplot(combined_squashed,aes(x=incidentType)) + geom_histogram(stat = "count") + 
  labs(title = "Count of Disaster Incident Type") + xlab("Incident Type") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Have a lot more of some disaster/incident types than others. Will def have to standardize the values

# To Do
# Split data into test and training
# Standardize the training set so no data leakage
# Upsample the training set due to minority positive predictor