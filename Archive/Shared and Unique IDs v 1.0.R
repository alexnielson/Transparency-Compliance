# Author Comment & File Description ####

# Code written by Michael Jensen.

# Purpose:
# 1) Create a report that allows us to verify that the entities listed in 
#    Salesforce have been assigned the correct Transparency ID.
#    - The name of the report is "Report - Transparency IDs Shared."
#    - Use Excel's Fuzzy Lookup function to check entity names for equality.

# 2) Create a report that allows us to identify Transparency IDs that are not
#    shared between Salesforce and Transparency, so discrepancies can be
#    reconciled.
#    - The name of the report is "Report - Transparency IDs Unique."

# Inputs ####

# Import the Salesforce report titled "Transparency - Master List."
sf.master <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# Import the entity information from Transparency.
library(RODBC)
aws <- odbcConnect("transpAWS")
aws.ids <- sqlQuery(aws, paste("
                               SELECT id, name
                               FROM entity"))
odbcClose(aws)
rm(aws)

# Report - Transparency IDs Shared ####

# Format and merge the Transparency and Salesforce data frames.
colnames(aws.ids)[2] <- "AWS Name"

osa.ids <- sf.master[!is.na(sf.master$Transparency.ID), c(1, 3)]
colnames(osa.ids)[1] <- "OSA Name"
colnames(osa.ids)[2] <- "id"

shared.ids <- merge(aws.ids, osa.ids, by = "id")
colnames(shared.ids)[1] <- "Transparency ID"

# Export the report to the S: drive.
write.csv(shared.ids,
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report - Transparency IDs Shared.csv",
          row.names = FALSE)

# Clear the R environment.
rm(shared.ids)

# Report - Transparency IDs Unique ####

## Note that an alternative to setdiff is dplyr's anti join.

# Format the Transparency and Salesforce data frames.
colnames(aws.ids)[2] <- "Entity Name"
colnames(osa.ids)[1] <- "Entity Name"

# Identify the unique IDs and whether the ID is in the Transparency or
# Salesforce data.
unique.aws <- as.matrix(setdiff(aws.ids$id, osa.ids$id))
unique.osa <- as.matrix(setdiff(osa.ids$id, aws.ids$id))

colnames(unique.aws)[1] <- "id"
colnames(unique.osa)[1] <- "id"

unique.aws2 <- merge(unique.aws, aws.ids, by = "id")
unique.osa2 <- merge(unique.osa, osa.ids, by = "id")
rm(unique.aws, unique.osa)

unique.aws2$source <- "AWS"
unique.osa2$source <- "Salesforce"

unique.ids <- rbind(unique.aws2, unique.osa2)
rm(aws.ids, osa.ids, unique.aws2, unique.osa2)

# Delete the IDs we don't care about or have already addressed.
unique.ids <- 
  subset(unique.ids,
         subset = (
           id != 1    &  # State of Utah - not enforced
           id != 21   &  # Utah State Board of Regents - not enforced
           id != 137  &  # Success Charter School - duplicate of Success 
                         #   Academy
           id != 273  &  # Blank
           id != 701  &  # zzzz - test entity
           id != 707  &  # zzzzz - test entity
           id != 711  &  # zz - test entity
           id != 823  &  # Wasatch County SSD #1 (Heber Estates) - inactive
           id != 1129 &  # xk12 - test entity
           id != 1145 &  # xxk12 - test entity
           id != 1169 &  # Weber ... Strike Force (Weber County) - inactive and
                         #   now reported as Weber ... Strike Force (Ogden City)
           id != 1215 &  # XXSSD - test entity
           id != 1380 &  # x - test entity
           id != 1419 &  # Blank
           id != 1442 &  # Wellsville-Mendon Conservation District - duplicate
           id != 1546 &  # Canyonland County Improvement District - duplicate
           id != 1553 &  # Grand County Water Conservancy District -duplicate
           id != 1574    # Southern Utah Shooting Sports Park SSD - dissolved
         ))

# Export the report to the S: drive.
write.csv(unique.ids,
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report - Transparency IDs Unique.csv",
          row.names = FALSE)

# Clear the R environment.
detach(package:RODBC)
rm(unique.ids, sf.master)