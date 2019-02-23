# Transparency Compliance Report - Many Entities v. 1.5                     ####

# Written by Michael Jensen.

# Purpose: Report the fiscal periods for which a local government is 
# non-compliant with the following Transpareny reporting requirements:
# - One expense transaction per fiscal quarter.
# - One revenue transaction per fiscal quarter.
# - One employee compensation transaction per fiscal year.

#   ____________________________________________________________________________
#   Define Inputs & Format the Salesforce Report                            ####

# Define the following:
# - The Salesforce report titled "Transparency - Master List."
# - The calendar quarter through which we will enforce Transparency compliance.
# - The hold date, which is the first date during the compliance cycle that the
#   compliance report was run and from which we will determine the 60-day 
#   grace period.
# - The reporting period templates (in calendar quarters).

sf.master     <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
report.qtr    <- as.numeric(2017.4)
hold.date     <- as.Date("2018-02-05")
template.er   <- as.matrix(as.numeric(c("2013.1", "2013.2", "2013.3", "2013.4",
                                        "2014.1", "2014.2", "2014.3", "2014.4",
                                        "2015.1", "2015.2", "2015.3", "2015.4",
                                        "2016.1", "2016.2", "2016.3", "2016.4",
                                        "2017.1", "2017.2", "2017.3", "2017.4",
                                        "2018.1", "2018.2", "2018.3", "2018.4")))
template.comp <- as.matrix(as.numeric(c("2013",   "2014",   "2015",   "2016", 
                                        "2017",   "2018",   "2019",   "2020")))

# Format the Salesforce report:
# Note: Remove the RDA/Project Area selection from below for version 2.0.

sf.report1 <- subset(sf.master, subset = (
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

sf.report2 <- subset(sf.report1, subset = (
  Entity.Status != "Inactive" &
  Entity.Status != "Dissolved"))

sf.report2[sf.report2 == ""] <- NA
sf.report3 <- sf.report2[!is.na(sf.report2$Transparency.ID), ]
sf.report4 <- sf.report3[!is.na(sf.report3$Expense.Revenue.Start.Date), ]
sf.report  <- sf.report4[ , c(1:3, 6, 7)]

# Keep sf.report2 for use when identifying entities without T IDs.
rm(sf.report1, sf.report3, sf.report4)

colnames(sf.report)[1] <- "Entity Name"
colnames(sf.report)[2] <- "SF ID"
colnames(sf.report)[3] <- "t.id"
colnames(sf.report)[4] <- "begin.er"
colnames(sf.report)[5] <- "begin.comp"

sf.report[4] <- as.Date(sf.report$begin.er, format = "%m/%d/%Y")
sf.report[5] <- as.Date(sf.report$begin.comp, format = "%m/%d/%Y")

# Create the Hold Report template.
hold.report <- data.frame(
  t.id             = numeric(nrow(sf.report)), 
  EXP              = character(nrow(sf.report)), 
  REV              = character(nrow(sf.report)), 
  COMP             = character(nrow(sf.report)), 
  Entity.Name      = character(nrow(sf.report)), 
  Analyst.Note     = character(nrow(sf.report)), 
  Last.Upload      = character(nrow(sf.report)), 
  Last.Processed   = character(nrow(sf.report)), 
  stringsAsFactors = FALSE)

hold.report[ , 1] <- sf.report$t.id

#   ____________________________________________________________________________
#   Define Functions                                                        ####

missing.exp <- function(x) {
  
  # This function:
  # - Finds an expense transaction for each quarter the entity has uploaded data
  #     to Transparency.
  # - Modifies the expense/revenue reporting template to create a template for
  #     indicating what the entity should have reported.
  # - Compares what the entity should have reported to what the entity did
  #     report.
  # - Formats the missing periods and inserts them into the Hold Report.
  
  reported.exp <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT DISTINCT
                        CONCAT(YEAR(posting_date), '.', QUARTER(posting_date))
                        FROM transaction
                        WHERE batch_id
                        IN (
                        SELECT id
                        FROM batch
                        WHERE entity_id = ", x,"
                        AND status IN ('PROCESSED', 'PROCESSING'))
                        AND type = 1")))
  
  a <- as.numeric(
    template.er[template.er >= quarter(ymd(
      sf.report$begin.er[sf.report$t.id == x]), with_year = TRUE)])
  
  template.er2 <- a[a <= report.qtr]
  rm(a)
  
  missing.exp1 <- as.data.frame(setdiff(template.er2, reported.exp))
  rm(reported.exp)
  
  if (nrow(missing.exp1) == 0) {
    missing.exp1[1, ] <- 0
  }
  
  missing.exp1[1] <- gsub(".1", " JAN-MAR", missing.exp1[ ,1], fixed = TRUE)
  missing.exp1[1] <- gsub(".2", " APR-JUN", missing.exp1[ ,1], fixed = TRUE)
  missing.exp1[1] <- gsub(".3", " JUL-SEP", missing.exp1[ ,1], fixed = TRUE)
  missing.exp1[1] <- gsub(".4", " OCT-DEC", missing.exp1[ ,1], fixed = TRUE)
  
  paste(missing.exp1[ , 1], collapse = ", ")
}

missing.rev <- function(x) {
  
  # This function:
  # - Finds an revenue transaction for each quarter the entity has uploaded data
  #     to Transparency.
  # - Modifies the expense/revenue reporting template to create a template for
  #     indicating what the entity should have reported.
  # - Compares what the entity should have reported to what the entity did
  #     report.
  # - Formats the missing periods and inserts them into the Hold Report.
  
  reported.rev <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT DISTINCT
                        CONCAT(YEAR(posting_date), '.', QUARTER(posting_date))
                        FROM transaction
                        WHERE batch_id
                        IN (
                        SELECT id
                        FROM batch
                        WHERE entity_id = ", x,"
                        AND status IN ('PROCESSED', 'PROCESSING'))
                        AND type = 2")))
  
  a <- as.numeric(
    template.er[template.er >= quarter(ymd(
      sf.report$begin.er[sf.report$t.id == x]), with_year = TRUE)])
  
  template.er2 <- a[a <= report.qtr]
  rm(a)
  
  missing.rev1 <- as.data.frame(setdiff(template.er2, reported.rev))
  rm(reported.rev)
  
  if (nrow(missing.rev1) == 0) {
    missing.rev1[1, ] <- 0
  }
  
  missing.rev1[1] <- gsub(".1", " JAN-MAR", missing.rev1[ ,1], fixed = TRUE)
  missing.rev1[1] <- gsub(".2", " APR-JUN", missing.rev1[ ,1], fixed = TRUE)
  missing.rev1[1] <- gsub(".3", " JUL-SEP", missing.rev1[ ,1], fixed = TRUE)
  missing.rev1[1] <- gsub(".4", " OCT-DEC", missing.rev1[ ,1], fixed = TRUE)
  
  paste(missing.rev1[ , 1], collapse = ", ")
}

# Note: missing.comp was originally set to dynamically capture COMP that should
# be reported during and after April. The switch messed with our FEB compliance
# cycle, so on 4/2/2018 I changed the requirement to be July. This will need
# to be changed back during version 2.0.

missing.comp <- function(x) {
  
  # This function:
  # - Finds a compensation transaction for each quarter the entity has uploaded 
  #     data to Transparency.
  # - Modifies the compensation reporting template to create a template for
  #     indicating what the entity should have reported (note that this code 
  #     should account for the different reporting requirements for entities
  #     based on the entity's fiscal year and the time of year we initiate the
  #     enforcement cycle).
  # - Compares what the entity should have reported to what the entity did
  #     report.
  # - Formats the missing periods and inserts them into the Hold Report.
  
  reported.comp <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT DISTINCT fiscal_year
                        FROM transaction
                        WHERE batch_id
                        IN (
                        SELECT id
                        FROM batch
                        WHERE entity_id = ", x,"
                        AND status IN ('PROCESSED', 'PROCESSING'))
                        AND type = 3"))
    )
  
  if (month(sf.report$begin.comp[sf.report$t.id == x]) == 1) {
    b <- template.comp[template.comp >= as.numeric(year(ymd(
      sf.report$begin.comp[sf.report$t.id == x])))]
  } else {
    b <- template.comp[template.comp > as.numeric(year(ymd(
      sf.report$begin.comp[sf.report$t.id == x])))]
  }
  
  # As indicated in the note above, "< 7" and ">= 7" was originally "< 4" and
  # ">= 4." This caused a problem during our compliance cycle when a bunch of
  # entities were flagged as missing 2017 COMP.
  
  if        ((month(sf.report$begin.comp[sf.report$t.id == x]) == 1) & 
             (month(today()) < 7)) {
    template.comp2 <- b[b < (as.numeric(year(today())) - 1)]
  } else if ((month(sf.report$begin.comp[sf.report$t.id == x]) == 1) & 
             (month(today()) >= 7)) {
    template.comp2 <- b[b < as.numeric(year(today()))]
  } else if ((month(sf.report$begin.comp[sf.report$t.id == x]) != 1) & 
             (month(today()) < 10)) {
    template.comp2 <- b[b < as.numeric(year(today()))]
  } else if (month(sf.report$begin.comp[sf.report$t.id == x] != 1) & 
             (month(today()) <= 10)) {
    template.comp2 <- b[b <= as.numeric(year(today()))]
  }
  rm(b)
  
  missing.comp1 <- as.data.frame(setdiff(template.comp2, reported.comp))
  rm(reported.comp)
  
  if (nrow(missing.comp1) == 0) {
    missing.comp1[1, ] <- 0
  }
  
  missing.comp1[1] <- gsub(".1", " JAN-MAR", missing.comp1[ ,1], fixed = TRUE)
  missing.comp1[1] <- gsub(".2", " APR-JUN", missing.comp1[ ,1], fixed = TRUE)
  missing.comp1[1] <- gsub(".3", " JUL-SEP", missing.comp1[ ,1], fixed = TRUE)
  missing.comp1[1] <- gsub(".4", " OCT-DEC", missing.comp1[ ,1], fixed = TRUE)
  
  paste(missing.comp1[ , 1], collapse = ", ")
}

GetLastUploadedBatch <- function(x) {
  
  # This function gets the date of the last batch upload.
  
  a <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT upload_date
                        FROM batch
                        WHERE entity_id = ", x, "
                        ORDER BY upload_date DESC
                        LIMIT 1")))
  if (nrow(a) > 0) {
    a
  } else if (nrow(a) == 0) {
    "1/1/1900"
  }
}

GetLastProcessedBatch <- function(x) {
  
  # This function gets the date of the last processed batch.
  
  b <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT processed_date
                        FROM batch
                        WHERE entity_id = ", x, "
                        ORDER BY processed_date DESC
                        LIMIT 1")))
  if (nrow(b) > 0) {
    b
  } else if (nrow(b) == 0) {
    "1/1/1900"
  }
}

#   ____________________________________________________________________________
#   Generate the Compliance Report                                          ####

# Load required packages and open the ODBC.
library(lubridate)
library(RODBC)
aws <- odbcConnect("transpAWS")

# Create and format the Hold Report for entities that have reported to 
# Transparency.
for (i in 1:nrow(hold.report)) {
  hold.report[i, 2] <- missing.exp(hold.report[i, 1])
  hold.report[i, 3] <- missing.rev(hold.report[i, 1])
  hold.report[i, 4] <- missing.comp(hold.report[i, 1])
  hold.report[i, 7] <- GetLastUploadedBatch(hold.report[i, 1])
  hold.report[i, 8] <- GetLastProcessedBatch(hold.report[i, 1])
}
rm(i)

hold.report2 <- hold.report

hold.report2[ , 7] <- as.Date(hold.report2[ , 7])
hold.report2[ , 8] <- as.Date(hold.report2[ , 8])

hold.report2[ , 2] <- paste("EXP", hold.report2[ , 2])  
hold.report2[ , 3] <- paste("REV", hold.report2[ , 3]) 
hold.report2[ , 4] <- paste("COMP", hold.report2[ , 4])

hold.report2[ , 2] <- gsub("EXP 0", " ", hold.report2[ , 2])
hold.report2[ , 3] <- gsub("REV 0", " ", hold.report2[ , 3])
hold.report2[ , 4] <- gsub("COMP 0", " ", hold.report2[ ,4])

hold.report3               <- merge(hold.report2, sf.report, by = "t.id")
hold.report3$report.date   <- Sys.Date()
colnames(hold.report3)[13] <- "Report Date"
hold.report3$hold.date     <- hold.date
colnames(hold.report3)[14] <- "'60-Days From' Date"
hold.report4               <- hold.report3[ , 
                                            c(13, 14, 9, 10, 2, 3, 4, 6, 7, 8)]
colnames(hold.report4)[8]  <- "Analyst Note"
colnames(hold.report4)[9]  <- "Last Uploaded Batch"
colnames(hold.report4)[10] <- "Last Processed Batch"

# Get the Hold Report for entities that have not reported to Transparency.
no.ids1 <- sf.report2[is.na(sf.report2$Transparency.ID), ]
rm(sf.report2)

no.ids2 <- no.ids1[ , 1:2]
rm(no.ids1)
colnames(no.ids2)[1] <- "Entity Name"
colnames(no.ids2)[2] <- "SF ID"

no.ids2$report.date <- Sys.Date()
colnames(no.ids2)[3] <- "Report Date"
no.ids2$hold.date <- hold.date
colnames(no.ids2)[4] <- "'60-Days From' Date"

no.ids2 <- no.ids2[ , c(3, 4, 1, 2)]

no.ids2$EXP           <- "No EXP Reported"
no.ids2$REV           <- "No REV Reported"
no.ids2$COMP          <- "No COMP Reported"
no.ids2$analyst.note  <- " "
colnames(no.ids2)[8]  <- "Analyst Note"
no.ids2$uploaded      <- NA
colnames(no.ids2)[9]  <- "Last Uploaded Batch"
no.ids2$processed     <- NA
colnames(no.ids2)[10] <- "Last Processed Batch"

# Combine the hold reports.
hold.report <- rbind(hold.report4, no.ids2)

#   ____________________________________________________________________________
#   Export the Compliance Report                                            ####

write.csv(hold.report, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report 3 - Hold Report.csv",
          row.names = FALSE)

#   ____________________________________________________________________________
#   Clear the R Environment                                                 ####

odbcClose(aws)
detach(package:RODBC)
rm(hold.report, hold.report2, hold.report3, hold.report4, 
   sf.report, template.er, template.comp, report.qtr, report.yr, 
   missing.comp, missing.exp, missing.rev, aws, i, no.ids1,
   no.ids2, no.ids3, sf.master, GetLastProcessedBatch, GetLastUploadedBatch, 
   hold.date)