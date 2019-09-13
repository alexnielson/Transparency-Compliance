# Transparency Uploads-in-Queue Report                                      ####

# Written by Michael Jensen.

# Purpose: Identify local governments with unprocessed but uploaded batches. 
# These are entities we will assume are compliant and not hold funds.

#   ____________________________________________________________________________
#   Import and Format Salesforce "Transparency - Master List"               ####
sf.master     <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

sf.master1 <- subset(sf.master, subset = (
  Entity.Record.Type != "Redevelopment Agency/Project Area" &
  Entity.Record.Type != "CPA Firm" &
  Entity.Record.Type != "Non Profits" &
  Entity.Record.Type != "Institute of Higher Education" &
  Entity.Record.Type != "State of Utah (agencies/depts/independent state entities/comp units/ etc.)" &
  Entity.Record.Type != "Educational Foundation" &
  Entity.Record.Type != "Justice Court" &
  Entity.Record.Type != "Community User" &
  Entity.Record.Type != "Conservation District" &
  Entity.Record.Type !=  "" &
  Entity.Record.Type != "Financial Institution"))

sf.master2 <- subset(sf.master1, subset = (
  Entity.Status != "Inactive" &
  Entity.Status != "Dissolved"))

sf.master2[sf.master2 == ""] <- NA
sf.master3 <- sf.master2[!is.na(sf.master2$Transparency.ID), ]
sf.master4 <- sf.master3[!is.na(sf.master3$Expense.Revenue.Start.Date), ]
sf.master5  <- sf.master4[ , c(1:3, 6, 7)]

# Keep sf.master2 for use when identifying entities without T IDs.
rm(sf.master1, sf.master2, sf.master3, sf.master4)

colnames(sf.master5)[1] <- "Entity Name"
colnames(sf.master5)[2] <- "SF ID"
colnames(sf.master5)[3] <- "t.id"
colnames(sf.master5)[4] <- "begin.er"
colnames(sf.master5)[5] <- "begin.comp"

sf.master5[4] <- as.Date(sf.master5$begin.er, format = "%m/%d/%Y")
sf.master5[5] <- as.Date(sf.master5$begin.comp, format = "%m/%d/%Y")

# Create the Hold Report template.
upload.report <- data.frame(
  t.id             = numeric(nrow(sf.master5)),
  Entity.Name      = character(nrow(sf.master5)), 
  Upload.Present   = character(nrow(sf.master5)),
  Analyst.Note     = character(nrow(sf.master5)), 
  stringsAsFactors = FALSE)

upload.report[ , 1] <- sf.master5$t.id

#   ____________________________________________________________________________
#   Define Functions                                                        ####

GetEntityName <- function(x) {
  
  # Args:
  #   x: The entity's Transparency ID.
  
  # Returns:
  #   The entity's name, as reported to Transparency.
  
  a <- as.matrix(sqlQuery(aws, paste("
                           SELECT name
                           FROM entity
                           WHERE id = ", x)))
}

IdentifyUploadInQueue <- function(x) {
  
  # Args:
  #   x: The entity's Transparency ID.
  
  # Returns:
  #   Yes/No, depending on whether the entity has a batch with a status of 
  #   "UPLOADED" in AWS.
  
  a <- sqlQuery(aws, paste("
                      SELECT status
                      FROM batch
                      WHERE entity_id = ", x, "
                      AND status = 'UPLOADED'
                      LIMIT 1"))
  
  if (nrow(a) == 0) {
    "No"
  } else {
    "Yes"
  }
}

#   ____________________________________________________________________________
#   Generate Report                                                         ####
library(RODBC)
aws <- odbcConnect("transpAWS")

for (i in 1:nrow(upload.report)) {
  upload.report[i, 2] <- GetEntityName(upload.report[i, 1])
  upload.report[i, 3] <- IdentifyUploadInQueue(upload.report[i, 1])
}
rm(i)

write.csv(upload.report, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report 4 - Uploads In Queue.csv",
          row.names = FALSE)

upload.report2 <- upload.report[upload.report[ , 3] != "No", ]

#   ____________________________________________________________________________
#   Clear the R Environment                                                 ####
odbcClose(aws)
detach(package:RODBC)
rm(aws, sf.master, sf.master5, upload.report, upload.report2, GetEntityName, 
   IdentifyUploadInQueue)