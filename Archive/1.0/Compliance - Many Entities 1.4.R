# Purpose ####

# Determine whether a local government has complied with the following
# Transparency reporting requirements, beginning with the first calendar
# quarter or year the entity is required to have data uploaded for:
# - One expense transaction per quarter.
# - One revenue transaction per quarter.
# - One compensation transaction per fiscal year.

# Contents ####

# Packages:
# - lubridate
# - RODBC

# Commands:
# - as.data.frame
# - as.Date
# - as.numeric
# - data.frame
# - gsub
# - nrow
# - odbcConnect (RODBC)
# - paste
# - quarter (lubridate)
# - setdiff
# - sqlQuery (RODBC)
# - ymd (lubridate)

# Import the Salesforce report titled "Transparency - Master List" ####
sf_master <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# Report 1: Shared Transparency IDs ####

# This code matches the Transparency IDs in AWS with the Transparency IDs in
# Salesforce. The resulting data frame is exported as a .csv file so the entity
# names can be compared for equality (use Excel's Fuzzy Lookup function).

library(RODBC)
aws <- odbcConnect("transpAWS")

aws_ids <- sqlQuery(aws, paste("
                               SELECT id, name
                               FROM entity"))
colnames(aws_ids)[2] <- "AWS Name"

osa_ids <- sf_master[!is.na(sf_master$Transparency.ID), c(1, 3)]
colnames(osa_ids)[1] <- "OSA Name"
colnames(osa_ids)[2] <- "id"

shared_ids <-merge(aws_ids, osa_ids, by = "id")
colnames(shared_ids)[1] <- "Transparency ID"
rm(aws_ids, osa_ids)

# Export the report to the S: drive.
write.csv(shared_ids, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report 1 - Shared Transparency IDs.csv",
          row.names = FALSE)

# Clear the R environment.
odbcClose(aws)
detach(package:RODBC)
rm(aws, shared_ids)

# Report 2: Unique Transparency IDs ####

# This code identifies the Transparency IDs that are not shared between AWS and
# SF. This allows us to:
# - Identify entities new to Transparency so we can update Salesforce with the
#     entity's Transparency ID.
# - Correct discrepancies between the Transparency IDs in AWS and the IDs in 
#     Salesforce.

# Get the IDs and entity names.
library(RODBC)
aws <- odbcConnect("transpAWS")

aws_ids <- sqlQuery(aws, paste("
                               SELECT id, name
                               FROM entity"))
colnames(aws_ids)[2] <- "Entity Name"

osa_ids <- sf_master[!is.na(sf_master$Transparency.ID), c(1, 3)]
colnames(osa_ids)[1] <- "Entity Name"
colnames(osa_ids)[2] <- "id"

# Find the differences in IDs and identify whether the ID came from AWS or OSA.
unique_aws <- as.matrix(setdiff(aws_ids$id, osa_ids$id))
unique_osa <- as.matrix(setdiff(osa_ids$id, aws_ids$id))

colnames(unique_aws)[1] <- "id"
colnames(unique_osa)[1] <- "id"

unique_aws2 <- merge(unique_aws, aws_ids, by = "id")
unique_osa2 <- merge(unique_osa, osa_ids, by = "id")
rm(unique_aws, unique_osa)

unique_aws2$source <- "AWS"
unique_osa2$source <- "Salesforce"

unique_ids <- rbind(unique_aws2, unique_osa2)
rm(aws_ids, osa_ids, unique_aws2, unique_osa2)

# Filter entities we don't care about and export the report to the S: drive.
unique_ids <- 
  subset(unique_ids,
         subset = (
          id != 1 &    # State of Utah
          id != 21 &   # Utah State Board of Regents
          id != 137 &  # Success Charter School
          id != 273 &  # Blank
          id != 701 &  # zzzz
          id != 707 &  # zzzzz
          id != 711 &  # zz
          id != 823 &  # Wasatch County SSD #1 (Heber Estates) - inactive (?)
          id != 1129 & # xk12
          id != 1145 & # xxk12
          id != 1169 & # Weber ... Strike Force (Weber County) - inactive
          id != 1215 & # XXSSD
          id != 1380 & # x
          id != 1419 & # Blank
          id != 1442 & # Wellsville-Mendon Conservation District - duplicate
          id != 1546 & # Canyonland County Improvement District - duplicate
          id != 1553 & # Grand County Water Conservancy District -duplicate
          id != 1574   # Southern Utah Shooting Sports Park SSD - dissolved
         ))

write.csv(unique_ids, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report 2 - Unique Transparency IDs.csv",
          row.names = FALSE)

# Clear the R Environment
odbcClose(aws)
detach(package:RODBC)
rm(aws, unique_ids)

# Notes on unique_ids: 

# Blank or nonsense names
# _ These are test entities.

# Success Charter School (ID 137)
# _ 1/29/2018: Data doesn't appear on the Transparency website and this entity
#     is not in Salesforce. I assume this is a duplicate of Success Academy,
#     which does show up on the Transparency website and is in Salesforce.

# Utah State Board of Regents (ID 21)
# - 1/29/2018: We do not track this entity for compliance, so I will ignore it.

# Wasatch County SSD #1 (Heber Estates)
# - 1/29/2018: This entity appears to be inactive or dissolved. This entity
#     does not match a Salesforce entity, but may be the same as Wasatch County
#     Subdivision SSD No. 1.

# Weber Morgan Narcotics Strike Force (Weber County) (ID 1169)
# - 1/29/2018: Accounting for this entity was previously done by Weber County,
#     but is now done by Ogden City and reported in Transparency as Weber 
#     Morgan Narcotics Strike Force (Ogden City). In Salesforce, this entity is
#     listed as Weber Morgan Strike Force.

# Report 3: Hold Report ####

# This code creates a report of the periods for which an entity is missing 
# a required Transparency upload.

# Define the period through which we will enforce Transparency compliance.
report_qtr <- as.numeric(2017.4)
report_yr  <- as.numeric(2017)

# Define the reporting period master templates.
template_er1   <- 
  as.matrix(as.numeric(c("2013.1", "2013.2", "2013.3", "2013.4",
                         "2014.1", "2014.2", "2014.3", "2014.4",
                         "2015.1", "2015.2", "2015.3", "2015.4",
                         "2016.1", "2016.2", "2016.3", "2016.4",
                         "2017.1", "2017.2", "2017.3", "2017.4",
                         "2018.1", "2018.2", "2018.3", "2018.4")))

template_comp1 <- 
  as.matrix(as.numeric(c("2013", "2014", "2015", "2016", "2017",
                         "2018", "2019", "2020")))

# Format the Salesforce report.
sf_report1 <- subset(sf_master, subset = (
  Entity.Record.Type != "CPA Firm" &
  Entity.Record.Type != "Non Profits" &
  Entity.Record.Type != "Institute of Higher Education" &
  Entity.Record.Type != "State of Utah (agencies/depts/independent state entities/comp units/ etc.)" &
  Entity.Record.Type != "Educational Foundation" &
  Entity.Record.Type != "Justice Court" &
  Entity.Record.Type != "Redevelopment Agency/Project Area" &
  Entity.Record.Type != "Community User" &
  Entity.Record.Type != "Conservation District" &
  Entity.Record.Type !=  "" &
  Entity.Record.Type != "Financial Institution"))

sf_report2 <- subset(sf_report1, subset = (
  Entity.Status != "Inactive" &
  Entity.Status != "Dissolved"))

sf_report2[sf_report2 == ""] <- NA
sf_report3 <- sf_report2[!is.na(sf_report2$Transparency.ID), ]
sf_report4 <- sf_report3[!is.na(sf_report3$Expense.Revenue.Start.Date), ]
sf_report <- sf_report4[ , c(1:3, 6, 7)]

# Keep sf_report2 for use when identifying entities without T IDs.
rm(sf_report1, sf_report3, sf_report4)

colnames(sf_report)[1] <- "Entity Name"
colnames(sf_report)[2] <- "SF ID"
colnames(sf_report)[3] <- "t_id"
colnames(sf_report)[4] <- "begin_er"
colnames(sf_report)[5] <- "begin_comp"

sf_report[4] <- as.Date(sf_report$begin_er, format = "%m/%d/%Y")
sf_report[5] <- as.Date(sf_report$begin_comp, format = "%m/%d/%Y")

# Create the Hold Report template.
hold_report <- data.frame(
  t_id = numeric(nrow(sf_report)), EXP = character(nrow(sf_report)), 
  REV = character(nrow(sf_report)), COMP = character(nrow(sf_report)), 
  Entity_Name = character(nrow(sf_report)), 
  Analyst_Note = character(nrow(sf_report)), stringsAsFactors = FALSE)

hold_report[ , 1] <- sf_report$t_id

# Define the functions.
missing_exp <- function(x) {
  
  # This function:
  # - Finds an expense transaction for each quarter the entity has uploaded data
  #     to Transparency.
  # - Modifies the expense/revenue reporting template to create a template for
  #     indicating what the entity should have reported.
  # - Compares what the entity should have reported to what the entity did
  #     report.
  # - Formats the missing periods and inserts them into the Hold Report.
  
  reported_exp <- as.matrix(
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
  
  # I'm not sure how this works, but even though I can't see any dates in a batch that is 
  # processing when I look for that batch directly, I'm able to see some of the dates from
  # that batch if it's being processed and I query it like I did above... Strange...
  
  # I think the batch begin_txn and end_txn dates aren't populated until the batch is completely processed
  # and the program can actually determine the first and last dates. But as the batch is being processed,
  # the transaction table is being uploaded. I'm certain this is why I can get the information I want
  # from the transaction table - and why I can't select dates later than the end_txn_date in the batch 
  # table (because the batch being processed doesn't have such a date yet).
  
  a <- as.numeric(
    template_er1[template_er1 >= quarter(ymd(
      sf_report$begin_er[sf_report$t_id == x]), with_year = TRUE)])
  
  template_er2 <- a[a <= report_qtr]
  rm(a)
  
  missing_exp1 <- as.data.frame(setdiff(template_er2, reported_exp))
  rm(reported_exp)
  
  if (nrow(missing_exp1) == 0) {
    missing_exp1[1, ] <- 0
  }
  
  missing_exp1[1] <- gsub(".1", " JAN-MAR", missing_exp1[ ,1], fixed = TRUE)
  missing_exp1[1] <- gsub(".2", " APR-JUN", missing_exp1[ ,1], fixed = TRUE)
  missing_exp1[1] <- gsub(".3", " JUL-SEP", missing_exp1[ ,1], fixed = TRUE)
  missing_exp1[1] <- gsub(".4", " OCT-DEC", missing_exp1[ ,1], fixed = TRUE)
  
  paste(missing_exp1[ , 1], collapse = ", ")
}
missing_rev <- function(x) {
  
  # This function:
  # - Finds an revenue transaction for each quarter the entity has uploaded data
  #     to Transparency.
  # - Modifies the expense/revenue reporting template to create a template for
  #     indicating what the entity should have reported.
  # - Compares what the entity should have reported to what the entity did
  #     report.
  # - Formats the missing periods and inserts them into the Hold Report.
  
  reported_rev <- as.matrix(
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
    template_er1[template_er1 >= quarter(ymd(
      sf_report$begin_er[sf_report$t_id == x]), with_year = TRUE)])
  
  template_er2 <- a[a <= report_qtr]
  rm(a)
  
  missing_rev1 <- as.data.frame(setdiff(template_er2, reported_rev))
  rm(reported_rev)
  
  if (nrow(missing_rev1) == 0) {
    missing_rev1[1, ] <- 0
  }
  
  missing_rev1[1] <- gsub(".1", " JAN-MAR", missing_rev1[ ,1], fixed = TRUE)
  missing_rev1[1] <- gsub(".2", " APR-JUN", missing_rev1[ ,1], fixed = TRUE)
  missing_rev1[1] <- gsub(".3", " JUL-SEP", missing_rev1[ ,1], fixed = TRUE)
  missing_rev1[1] <- gsub(".4", " OCT-DEC", missing_rev1[ ,1], fixed = TRUE)
  
  paste(missing_rev1[ , 1], collapse = ", ")
}
missing_comp <- function(x) {
  
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
  
  reported_comp <- as.matrix(
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
  
  if (month(sf_report$begin_comp[sf_report$t_id == x]) == 1) {
    b <- template_comp1[template_comp1 >= as.numeric(year(ymd(
      sf_report$begin_comp[sf_report$t_id == x])))]
  } else {
    b <- template_comp1[template_comp1 > as.numeric(year(ymd(
      sf_report$begin_comp[sf_report$t_id == x])))]
  }
  
  if ((month(sf_report$begin_comp[sf_report$t_id == x]) == 1) & 
      (month(today()) < 4)) {
    template_comp2 <- b[b < (as.numeric(year(today())) - 1)]
  } else if ((month(sf_report$begin_comp[sf_report$t_id == x]) == 1) & 
             (month(today()) >= 4)) {
    template_comp2 <- b[b < as.numeric(year(today()))]
  } else if ((month(sf_report$begin_comp[sf_report$t_id == x]) != 1) & 
             (month(today()) < 10)) {
    template_comp2 <- b[b < as.numeric(year(today()))]
  } else if (month(sf_report$begin_comp[sf_report$t_id == x] != 1) & 
             (month(today()) <= 10)) {
    template_comp2 <- b[b <= as.numeric(year(today()))]
  }
  rm(b)
  
  missing_comp1 <- as.data.frame(setdiff(template_comp2, reported_comp))
  rm(reported_comp)
  
  if (nrow(missing_comp1) == 0) {
    missing_comp1[1, ] <- 0
  }
  
  missing_comp1[1] <- gsub(".1", " JAN-MAR", missing_comp1[ ,1], fixed = TRUE)
  missing_comp1[1] <- gsub(".2", " APR-JUN", missing_comp1[ ,1], fixed = TRUE)
  missing_comp1[1] <- gsub(".3", " JUL-SEP", missing_comp1[ ,1], fixed = TRUE)
  missing_comp1[1] <- gsub(".4", " OCT-DEC", missing_comp1[ ,1], fixed = TRUE)
  
  paste(missing_comp1[ , 1], collapse = ", ")
}

# Load required packages and open the ODBC.
library(lubridate)
library(RODBC)
aws <- odbcConnect("transpAWS")

# Create and format the Hold Report for entities that have reported to 
# Transparency.
for (i in 1:nrow(hold_report)) {
  hold_report[i, 2] <- missing_exp(hold_report[i, 1])
  hold_report[i, 3] <- missing_rev(hold_report[i, 1])
  hold_report[i, 4] <- missing_comp(hold_report[i, 1])
}

hold_report2 <- hold_report

hold_report2[ , 2] <- paste("EXP", hold_report[ , 2])  
hold_report2[ , 3] <- paste("REV", hold_report[ , 3]) 
hold_report2[ , 4] <- paste("COMP", hold_report[ , 4])

hold_report2[ , 2] <- gsub("EXP 0", " ", hold_report2[ , 2])
hold_report2[ , 3] <- gsub("REV 0", " ", hold_report2[ , 3])
hold_report2[ , 4] <- gsub("COMP 0", " ", hold_report2[ ,4])

hold_report3 <- merge(hold_report2, sf_report, by = "t_id")
hold_report3$report_date <- Sys.Date()
colnames(hold_report3)[11] <- "Report Date"
hold_report4 <- hold_report3[ , c(11, 7, 8, 2, 3, 4, 6)]
colnames(hold_report4)[7] <- "Analyst Note"

# Get the Hold Report for entities that have not reported to Transparency.
no_ids1 <- sf_report2[is.na(sf_report2$Transparency.ID), ]
rm(sf_report2)

no_ids2 <- no_ids1[ , 1:2]
rm(no_ids1)
colnames(no_ids2)[1] <- "Entity Name"
colnames(no_ids2)[2] <- "SF ID"

no_ids2$report_date <- Sys.Date()
colnames(no_ids2)[3] <- "Report Date"

no_ids2 <- no_ids2[ , c(3, 1, 2)]

no_ids2$EXP  <- "No EXP Reported"
no_ids2$REV  <- "No REV Reported"
no_ids2$COMP <- "No COMP Reported"
no_ids2$analyst_note <- " "
colnames(no_ids2)[7] <- "Analyst Note"

# Combine the hold reports.
hold_report <- rbind(hold_report4, no_ids2)

# Export the Hold Report.
write.csv(hold_report, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Report 3 - Hold Report.csv",
          row.names = FALSE)

# Clear the R environment. 
odbcClose(aws)
detach(package:RODBC)
rm(hold_report, hold_report2, hold_report3, hold_report4, 
   sf_report, template_er1, template_comp1, report_qtr, report_yr, 
   missing_comp, missing_exp, missing_rev, aws, i, no_ids1,
   no_ids2, no_ids3, sf_master)
