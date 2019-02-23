# Purpose ####

# Determine if local governments are compliant with the following
# Transparency reporting requirements:
# 1) One expense transaction per quarter.
# 2) One revenue transaction per quarter.
# 3) One compensation transaction per fiscal year.

# Contents ####

# Code summary:
# - Get an entity's expense, revenue, and compensation data via ODBC.
# - Analyze the data to determine whether the entity is compliant with 
#   Transparency reporting requirements.
# - Create a report of the entity's missing uploads.

# Packages used:
# - RODBC
# - lubridate

# Commands used:
# - apply
# - as.data.frame
# - as.matrix
# - do.call
# - function
# - gsub
# - if
# - paste
# - quarter
# - rbind
# - setdiff
# - sqlQuery (using SELECT, DISTINCT, CASE, WHEN, THEN, MONTH, YEAR, CONCAT,
#     FROM, AND)
# - unique
# - year
# - ymd

# Define the inputs ####

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

# Create a sample Salesforce report (for testing)
data <- read.csv(file = "C:/Users/mjensen1/Documents/Next Action Support/SF Report 3.csv",
                 header = TRUE, stringsAsFactors = FALSE)

sf_report <- data[1:5, 2:4]
rm(data)

colnames(sf_report)[1] <- "t_id"
colnames(sf_report)[2] <- "begin_er1"
colnames(sf_report)[3] <- "begin_comp1"

sf_report$begin_er1 <- as.Date(sf_report$begin_er1)
sf_report$begin_comp1 <- as.Date(sf_report$begin_comp1)

# Create complete Salesforce report
data <- read.csv(file = "C:/Users/mjensen1/Documents/Next Action Support/SF Report 3.csv",
                 header = TRUE, stringsAsFactors = FALSE)

sf_report <- data[ , 2:4]
rm(data)

colnames(sf_report)[1] <- "t_id"
colnames(sf_report)[2] <- "begin_er1"
colnames(sf_report)[3] <- "begin_comp1"

sf_report$begin_er1 <- as.Date(sf_report$begin_er1)
sf_report$begin_comp1 <- as.Date(sf_report$begin_comp1)

# Define the find_exp function ####

# Query AWS for a by-quarter summary of the expense transactions the entity
# has uploaded to Transparency since the date the entity should have started
# reporting (the entity's processed batch IDs are used to find the entity's
# transactions).

find_exp <- function(x) {
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
                            sf_report$begin_er1[sf_report$t_id == x],")
                      AND type = 1"))
}

# Define the find_rev function ####

# Query AWS for a by-quarter summary of the revenue transactions the entity
# has uploaded to Transparency since the date the entity should have started
# reporting (the entity's processed batch IDs are used to find the entity's
# transactions).

find_rev <- function(x) {
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
                            sf_report$begin_er1[sf_report$t_id == x], ") 
                      AND type = 2"))
}

# Define the find_comp function ####

# Query AWS for a by-year summary of the compensation transactions the 
# entity has uploaded to Transparency since the date the entity should have 
# started reporting (the entity's processed batch IDs are used to find the 
# entity's transactions).

find_comp <- function(x) {
  sqlQuery(aws, paste("
                      SELECT DISTINCT
                        fiscal_year
                      FROM transaction
                      WHERE batch_id
                      IN (
                        SELECT id
                        FROM batch
                        WHERE entity_id = ", x,"
                        AND status = 'PROCESSED'
                        AND end_txn_date >= ",
                      sf_report$begin_comp1[sf_report$t_id == x],")
                      AND type = 3"))
}

# Define the missing_exp function ####

# - Get a summary of the entity's expense transactions.
# - Define the reporting period for which the entity should have started
#     reporting to Transparency.
# - Create a reporting template for the entity's expense uploads.
# - Find the periods for which the entity is missing an expense upload by
#     comparing the reporting template to the summary of uploads.
# - Format the missing periods, add the entity's Transparency ID, and append to
#     the report of missing expenses uploads for other entities.

missing_exp <- function(x) {
  exp2 <- as.matrix(find_exp(x))
  
  a <- as.numeric(template_er1[template_er1 >= quarter(ymd(sf_report$begin_er1[sf_report$t_id == x]),
                                                       with_year = TRUE)])
  template_er2 <- a[a <= report_qtr]
  rm(a)
  
  missing_exp1 <- as.data.frame(setdiff(template_er2, exp2))
  rm(exp2)
  
  if (nrow(missing_exp1) == 0) {
    missing_exp1[1, ] <- 0
  }
  
  colnames(missing_exp1) <- "missing"
  
  missing_exp1$t_id <- x
  
  missing_exp <- rbind(empty_data, missing_exp1)
}

# Define the missing_rev function ####

# - Get a summary of the entity's revenue transactions.
# - Define the reporting period for which the entity should have started
#     reporting to Transparency.
# - Create a reporting template for the entity's revenue uploads.
# - Find the periods for which the entity is missing a revenue upload by
#     comparing the reporting template to the summary of uploads.
# - Format the missing periods, add the entity's Transparency ID, and append to
#     the report of missing revenue uploads for other entities.

missing_rev <- function(x) {
  rev2 <- as.matrix(find_rev(x))
  
  a <- as.numeric(template_er1[template_er1 >= quarter(ymd(sf_report$begin_er1[sf_report$t_id == x]),
                                                       with_year = TRUE)])
  template_er2 <- a[a <= report_qtr]
  rm(a)
  
  missing_rev1 <- as.data.frame(setdiff(template_er2, rev2))
  rm(rev2)
  
  if (nrow(missing_rev1) == 0) {
    missing_rev1[1, ] <- 0
  }
  
  colnames(missing_rev1) <- "missing"
  
  missing_rev1$t_id <- x
  
  missing_rev <- rbind(empty_data, missing_rev1)
}

# Define the missing_comp function ####

# - Get a summary of the entity's compensation transactions.
# - Define the reporting period for which the entity should have started
#     reporting to Transparency.
# - Create a reporting template for the entity's compensation uploads, where
#     the template accounts for how an entity's fiscal year affects whether it
#     should have reported compensation data.
# - Find the periods for which the entity is missing a compensation upload by
#     comparing the reporting template to the summary of uploads.
# - Format the missing periods, add the entity's Transparency ID, and append to
#     the report of missing compensation uploads for other entities.
missing_comp <- function(x) {
  comp2 <- as.matrix(find_comp(x))
  
  if (month(sf_report$begin_comp1[sf_report$t_id == x]) == 1) {
    b <- template_comp1[template_comp1 >= as.numeric(year(ymd(sf_report$begin_comp1[sf_report$t_id == x])))]
  } else {
    b <- template_comp1[template_comp1 > as.numeric(year(ymd(sf_report$begin_comp1[sf_report$t_id == x])))]
  }
  
  if (month(sf_report$begin_comp1[sf_report$t_id == x]) == 1) {
    template_comp2 <- b[b < (as.numeric(year(today())) - 1)]
  } else {
    template_comp2 <- b[b <= (as.numeric(year(today())) - 1)]
  }
  rm(b)
  
  missing_comp1 <- as.data.frame(setdiff(template_comp2, comp2))
  rm(comp2)
  
  if (nrow(missing_comp1) == 0) {
    missing_comp1[1, ] <- 0
  }
  
  colnames(missing_comp1) <- "missing"
  
  missing_comp1$t_id <- x
  
  missing_comp <- rbind(empty_data, missing_comp1)
}

# Get the hold report ####

# Format the empty data frame.
empty_data <- data.frame(numeric(), numeric())
colnames(empty_data)[1] <- "missing"
colnames(empty_data)[2] <- "t_id"

# Load required libraries.
library(RODBC)
library(lubridate)

# Connect to AWS.
aws <- odbcConnect("transpAWS")

# Get the missing expenses report. 
missing_exp1 <- apply(sf_report[1], 1, missing_exp)
missing_exp1 <- do.call("rbind", missing_exp1)

# Get the missing revenues report.
missing_rev1 <- apply(sf_report[1], 1, missing_rev)
missing_rev1 <- do.call("rbind", missing_rev1)

# Get the missing compensation report.
missing_comp1 <- apply(sf_report[1], 1, missing_comp)
missing_comp1 <- do.call("rbind", missing_comp1)

# Close the ODBC.
odbcClose(aws)
rm(aws)

# Format the reports so each entity has only one row of information and the
# missing periods are clearly identified.
missing_exp2  <- aggregate(missing ~ t_id, data = missing_exp1, toString)
missing_rev2  <- aggregate(missing ~ t_id, data = missing_rev1, toString)
missing_comp2 <- aggregate(missing ~ t_id, data = missing_comp1, toString)
# rm(missing_exp1, missing_rev1, missing_comp1, empty_data)

missing_exp2[2] <- gsub(".1", " JAN-MAR", missing_exp2[ , 2], fixed = TRUE)
missing_exp2[2] <- gsub(".2", " APR-JUN", missing_exp2[ , 2], fixed = TRUE)
missing_exp2[2] <- gsub(".3", " JUL-SEP", missing_exp2[ , 2], fixed = TRUE)
missing_exp2[2] <- gsub(".4", " OCT-DEC", missing_exp2[ , 2], fixed = TRUE)

missing_rev2[2] <- gsub(".1", " JAN-MAR", missing_rev2[ , 2], fixed = TRUE)
missing_rev2[2] <- gsub(".2", " APR-JUN", missing_rev2[ , 2], fixed = TRUE)
missing_rev2[2] <- gsub(".3", " JUL-SEP", missing_rev2[ , 2], fixed = TRUE)
missing_rev2[2] <- gsub(".4", " OCT-DEC", missing_rev2[ , 2], fixed = TRUE)
missing_rev2[2] <- as.data.frame(paste("REV", missing_rev2[ , 2]))

missing_comp2[2] <- as.data.frame(paste("COMP", missing_comp2[ , 2]))

# Merge to get Salesforce ID and entity name ####

# Get the Salesforce report containing entity names and Salesforce IDs and
# format it in preparation to merge with the missing uploads reports.
entity_names1 <- read.csv(file = "C:/Users/mjensen1/Documents/Next Action Support/SF Report.csv")
# entity_names1 <- read.csv(file.choose(), header = TRUE)
entity_names2 <- subset(entity_names1, select = c(Entity.Name, Entity.ID, Transparency.ID))
rm(entity_names1)

colnames(entity_names2)[1] <- "Entity Name"
colnames(entity_names2)[2] <- "Salesforce ID" 
colnames(entity_names2)[3] <- "t_id"

# Merge and format the reports.
a <- merge(missing_exp2, missing_rev2, by = "t_id")
b <- merge(a, missing_comp2, by = "t_id")
missing_uploads <- merge(b, entity_names2, by = "t_id")
rm(a, b, entity_names2, missing_exp2, missing_rev2, missing_comp2)

missing_uploads$report_date <- Sys.Date()
colnames(missing_uploads)[7] <- "Report Date"

missing_uploads$analyst <- as.character(NA)
colnames(missing_uploads)[8] <- "Analyst Note"

colnames(missing_uploads)[2] <- "EXP Note"
colnames(missing_uploads)[3] <- "REV Note"
colnames(missing_uploads)[4] <- "COMP Note"

hold_report <- missing_uploads[ , c(7, 5, 6, 2, 3, 4, 8)]
rm(missing_uploads)

# Export the Hold Report.
write.csv(hold_report, file = "C:/Users/mjensen1/Documents/Next Action Support/Hold Report.csv")
