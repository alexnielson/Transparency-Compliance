# Purpose ####

# Determine whether a local government has complied with the following
# Transparency reporting requirements, beginning with the first calendar
# quarter or year the entity is required to have data uploaded for:
# - One expense transaction per quarter.
# - One revenue transaction per quarter.
# - One compensation transaction per fiscal year.

# Contents ####

# Code summary:
# - Define the inputs.
# - Get the entity's Transparency data from AWS.
# - Analyze the entity's AWS data for compliance.
# - Report the entity's missing uploads.

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

# Define the enforcement period and the reporting templates ####

# Define the period through which we will enforce Transparency compliance.
report_qtr <- as.numeric(2017.4)
report_yr  <- as.numeric(2017)

# Define the reporting period master templates.
template_er1   <- as.matrix(as.numeric(c("2013.1", "2013.2", "2013.3", "2013.4",
                                         "2014.1", "2014.2", "2014.3", "2014.4",
                                         "2015.1", "2015.2", "2015.3", "2015.4",
                                         "2016.1", "2016.2", "2016.3", "2016.4",
                                         "2017.1", "2017.2", "2017.3", "2017.4",
                                         "2018.1", "2018.2", "2018.3", "2018.4")))

template_comp1 <- as.matrix(as.numeric(c("2013", "2014", "2015", "2016", "2017",
                                         "2018", "2019", "2020")))

# Import and format the Salesforce "Transparency Compliance Dates" report ####
sf_report1 <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

sf_report2 <- sf_report1[!is.na(sf_report1$Transparency.ID), ]
sf_report2[sf_report2 == ""] <- NA

sf_report3 <- sf_report2[!is.na(sf_report2$Expense.Revenue.Start.Date), ]
rm(sf_report1, sf_report2)

sf_report <- sf_report3[ , c("Entity.Name", "Entity.ID", "Transparency.ID", 
                             "Expense.Revenue.Start.Date", "Wage.Start.Date")]
rm(sf_report3)

colnames(sf_report)[1] <- "Entity Name"
colnames(sf_report)[2] <- "SF ID"
colnames(sf_report)[3] <- "t_id"
colnames(sf_report)[4] <- "begin_er"
colnames(sf_report)[5] <- "begin_comp"

sf_report[4] <- as.Date(sf_report$begin_er, format = "%m/%d/%Y")
sf_report[5] <- as.Date(sf_report$begin_comp, format = "%m/%d/%Y")

# Import and format the Salesforce "Entities without T IDs" report ####
no_ids1 <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

detail <- nrow(no_ids1) - 5

no_ids2 <- no_ids1[1:detail, 2:3]





detail <- nrow(no_ids1) - 5

no_ids2 <- data.frame(Entity.Name = character(no_ids1[1:detail, 2]), 
                      SF.ID = numeric(no_ids1[1:detail, 3]), )

no_ids2 <- data.frame(Entity.Name = character(no_ids1[1:detail, 2]),
                      SF.ID = numeric(no_ids1[1:detail, 3]), 
                      EXP = character)

data.frame(
  t_id = numeric(nrow(sf_report)), EXP = character(nrow(sf_report)), 
  REV = character(nrow(sf_report)), COMP = character(nrow(sf_report)), 
  Entity_Name = character(nrow(sf_report)), 
  Analyst_Note = character(nrow(sf_report)), stringsAsFactors = FALSE)



rm(no_ids1, detail)

no_ids2$EXP  <-"Missing all EXP"
no_ids2$REV  <- "Missing all REV"
no_ids2$COMP <- "Missing all COMP"

no_ids2$report_date <- Sys.Date()
colnames(no_ids2)[6] <- "Report Date"
colnames(no_ids2)[1] <- "Entity Name"



# Create the hold report template ####
hold_report <- data.frame(
  t_id = numeric(nrow(sf_report)), EXP = character(nrow(sf_report)), 
  REV = character(nrow(sf_report)), COMP = character(nrow(sf_report)), 
  Entity_Name = character(nrow(sf_report)), 
  Analyst_Note = character(nrow(sf_report)), stringsAsFactors = FALSE)

hold_report[ , 1] <- sf_report$t_id

# Define the missing_exp function ####

# This function:
# - Finds an expense transaction for each quarter the entity has uploaded data
#     to Transparency.
# - Modifies the expense/revenue reporting template to create a template for
#     indicating what the entity should have reported.
# - Compares what the entity should have reported to what the entity did
#     report.
# - Formats the missing periods and inserts them into the Hold Report.

missing_exp <- function(x) {
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
                          AND status = 'PROCESSED'
                          AND end_txn_date >= ",
                              sf_report$begin_er[sf_report$t_id == x],")
                        AND type = 1")))

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

# Define the missing_rev function ####

# This function:
# - Finds an revenue transaction for each quarter the entity has uploaded data
#     to Transparency.
# - Modifies the expense/revenue reporting template to create a template for
#     indicating what the entity should have reported.
# - Compares what the entity should have reported to what the entity did
#     report.
# - Formats the missing periods and inserts them into the Hold Report.

missing_rev <- function(x) {
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
                          AND status = 'PROCESSED'
                          AND end_txn_date >= ",
                              sf_report$begin_er[sf_report$t_id == x],")
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

# Define the missing_comp function ####

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

missing_comp <- function(x) {
  reported_comp <- as.matrix(
    sqlQuery(aws, paste("
                        SELECT DISTINCT fiscal_year
                        FROM transaction
                        WHERE batch_id
                        IN (
                          SELECT id
                          FROM batch
                          WHERE entity_id = ", x,"
                          AND status = 'PROCESSED'
                          AND end_txn_date >= ",
                              sf_report$begin_comp[sf_report$t_id == x],")
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

# Get the Hold Report ####

# Load required packages and open the ODBC.
library(lubridate)
library(RODBC)
aws <- odbcConnect("transpAWS")

for (i in 1:nrow(hold_report)) {
  hold_report[i, 2] <- missing_exp(hold_report[i, 1])
  hold_report[i, 3] <- missing_rev(hold_report[i, 1])
  hold_report[i, 4] <- missing_comp(hold_report[i, 1])
}

# Format the Hold Report ####

# This formats the Hold Report so that it is in the format Alex requires for 
# uploading into Salesforce.

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


# Export the Hold Report ####
write.csv(hold_report4, 
          file = "S:/Localgov/LG Compliance/Transparency Compliance/Hold Report.csv")

# Clean up the R environment ####
odbcClose(aws)
rm(entity_names2, hold_report, hold_report2, hold_report3, hold_report4, 
   sf_report, template_er1, template_comp1, report_qtr, report_yr, 
   template_er2, missing_comp, missing_exp, missing_rev, aws, i)
