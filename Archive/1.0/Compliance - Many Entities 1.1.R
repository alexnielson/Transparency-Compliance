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

# Define the entity's Transparency ID and beginning compliance dates.


# Define the period through which we will enforce Transparency compliance.
report_qtr <- as.numeric(2017.4)
report_yr  <- as.numeric(2017)

# Define the reporting period master templates.
template_er1   <- as.matrix(c("2013.1", "2013.2", "2013.3", "2013.4",
                              "2014.1", "2014.2", "2014.3", "2014.4",
                              "2015.1", "2015.2", "2015.3", "2015.4",
                              "2016.1", "2016.2", "2016.3", "2016.4",
                              "2017.1", "2017.2", "2017.3", "2017.4",
                              "2018.1", "2018.2", "2018.3", "2018.4")) 

template_comp1 <- as.matrix(c("2013", "2014", "2015", "2016", "2017", 
                              "2018", "2019", "2020"))

# Import the Salesforce "Transparency Compliance Dates" report.
sf_report1 <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# Create a report of entities missing a Transparency ID ####
missing_id1 <- sf_report1[is.na(sf_report1$Transparency.ID), ]
missing_id2 <- subset(missing_id1, select = c(Entity.Name, Entity.Record.Type))
write.csv(missing_id2,
          file = "S:/Localgov/LG Compliance/Compliance/Internal Reports on Compliance/Michael's Transparency Reports/Entities Missing a T ID.csv")
rm(missing_id1, missing_id2)

# Create a report of entities with a Transparency ID but missing dates ####
missing_dates1 <- sf_report1[!is.na(sf_report1$Transparency.ID), ]
missing_dates1[missing_dates1 == ""] <- NA
missing_dates2 <- missing_dates1[is.na(missing_dates1$Expense.Revenue.Start.Date), ]
missing_dates3 <- subset(missing_dates2, select = c(Entity.Name, Entity.Record.Type))
write.csv(missing_dates3, 
          file = "S:/Localgov/LG Compliance/Compliance/Internal Reports on Compliance/Michael's Transparency Reports/Entities Missing Start Dates.csv")
rm(sf_report1, missing_dates2, missing_dates3)

# Create a report of entities with a Transparency ID and compliance dates ####
sf_report2 <- missing_dates1[!is.na(missing_dates1$Expense.Revenue.Start.Date), ]
rm(missing_dates1)

sf_report3 <- subset(sf_report2, select = c(Transparency.ID,
                                            Expense.Revenue.Start.Date,
                                            Wage.Start.Date))
rm(sf_report2)

colnames(sf_report3)[1] <- "T.ID"
colnames(sf_report3)[2] <- "Begin.ER"
colnames(sf_report3)[3] <- "Begin.COMP"

sf_report3[2] <- as.Date(sf_report3$Begin.ER, format = "%m/%d/%Y")
sf_report3[3] <- as.Date(sf_report3$Begin.COMP, format = "%m/%d/%Y")
write.csv(sf_report3,
          file = "C:/Users/mjensen1/Documents/Next Action Support/SF Report 3.csv")

# Define the find_exp function ####

# The find_exp function uses the entity's batch IDs to find the expense
# transactions for that entity.
find_exp <- function(x) {
  sqlQuery(aws_connection, paste("
                                 SELECT DISTINCT 
                                 CASE
                                 WHEN (MONTH(posting_date) = '1')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '2')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '3')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '4')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '5')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '6')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '7')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '8')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '9')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '10')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 WHEN (MONTH(posting_date) = '11')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 WHEN (MONTH(posting_date) = '12')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 END
                                 AS    uploads
                                 FROM  transaction
                                 WHERE batch_id = ", x,
                                 "AND type = 1"))
}

# Define the find_rev function ####

# The find_rev function uses the entity's batch IDs to find the revenue
# transactions for that entity.

find_rev <- function(x) {
  sqlQuery(aws_connection, paste("
                                 SELECT DISTINCT 
                                 CASE
                                 WHEN (MONTH(posting_date) = '1')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '2')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '3')
                                 THEN CONCAT(YEAR(posting_date), '.', '1')
                                 WHEN (MONTH(posting_date) = '4')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '5')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '6')
                                 THEN CONCAT(YEAR(posting_date), '.', '2')
                                 WHEN (MONTH(posting_date) = '7')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '8')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '9')
                                 THEN CONCAT(YEAR(posting_date), '.', '3')
                                 WHEN (MONTH(posting_date) = '10')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 WHEN (MONTH(posting_date) = '11')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 WHEN (MONTH(posting_date) = '12')
                                 THEN CONCAT(YEAR(posting_date), '.', '4')
                                 END
                                 AS    uploads
                                 FROM  transaction
                                 WHERE batch_id = ", x,
                                 "AND  type = 2"))
}

# Define the find_comp function ####

# The find_comp function uses the entity's batch IDs to find the compensation
# transactions for that entity.
find_comp <- function(x) {
  sqlQuery(aws_connection, paste("
                                 SELECT DISTINCT 
                                 YEAR(posting_date)
                                 AS    uploads
                                 FROM  transaction
                                 WHERE batch_id = ", x,
                                 "AND type = 3"))
}

# Define the find_missing function ####

# This function finds the reporting periods for which the entity is not
# compliant with Transparency reporting requirements.

# The inputs for this function are the entity's Transparency ID ("t_id") and 
# the beginning dates the entity is to have reported transactions.

find_missing <- function(x, y, z) {
  
  # Get the entity's batch IDs and rename id to batch_id in preparation for
  # finding the entity's transactions.
  batch_ids <- as.matrix(sqlQuery(aws_connection, paste("
                                                        SELECT id
                                                        FROM   batch
                                                        WHERE  entity_id = ", x,
                                                        "AND   status = 'PROCESSED'
                                                        AND   end_txn_date >= ", "'", y, "'")))
  
  colnames(batch_ids) <- "batch_id"
  
  # Get the entity's transactions, by type.
  reported_exp1   <- as.matrix(unique(do.call("rbind", apply(batch_ids, 1, find_exp))))
  reported_rev1   <- as.matrix(unique(do.call("rbind", apply(batch_ids, 1, find_rev))))
  reported_comp1  <- as.matrix(unique(do.call("rbind", apply(batch_ids, 1, find_comp))))
  rm(batch_ids)
  
  # Define (as a numeric) the reporting period for which the entity should have
  # started reporting to Transparency.
  library(lubridate)
  begin_er2   <- quarter(ymd(y), with_year = TRUE)
  begin_comp2 <- year(ymd(z))
  
  # Create the reporting template for the entity.
  a <- template_er1[template_er1 >= begin_er2]
  template_er2 <- as.matrix(a[a <= report_qtr])
  rm(a)
  
  b <- template_comp1[template_comp1 >= begin_comp2]
  c <- b[b <= report_yr]
  
  # Adjust the template so entities with a fiscal year not equal to the 
  # calendar year aren't reported as missing the beginning compliance year.
  if (month(y) != 01) {
    template_comp2 <- as.matrix(c[c > begin_comp2])
  }
  rm(b, c, begin_er2, begin_comp2)
  
  # Find the periods for which the entity did report and the periods for which
  # the entity did not report, and compare them.
  missing_exp1  <- as.data.frame(setdiff(template_er2, reported_exp1))
  missing_rev1  <- as.data.frame(setdiff(template_er2, reported_rev1))
  missing_comp1 <- as.data.frame(setdiff(template_comp2, reported_comp1))
  rm(reported_exp1, reported_rev1, reported_comp1)
  
  # Format the missing periods.
  missing_exp2 <- as.data.frame(gsub(".", " Q", missing_exp1[ ,1], fixed = TRUE))
  missing_rev2 <- as.data.frame(gsub(".", " Q", missing_rev1[ ,1], fixed = TRUE))
  rm(missing_exp1, missing_rev1)
  
  missing_exp3 <- as.data.frame(paste("EXP", missing_exp2[ , 1]))
  missing_rev3 <- as.data.frame(paste("REV", missing_rev2[ , 1]))
  missing_comp2 <- as.data.frame(paste("COMP", missing_comp1[ , 1]))
  rm(missing_exp2, missing_rev2, missing_comp1)
  
  colnames(missing_exp3) <- "Missing Uploads"
  colnames(missing_rev3) <- "Missing Uploads"
  colnames(missing_comp2) <- "Missing Uploads"
  
  # Combine the reports and add the Transparency ID.
  missing_uploads <- rbind(missing_exp3, missing_rev3, missing_comp2)
  rm(missing_exp3, missing_rev3, missing_comp2)
  missing_uploads$t.id <- x
  
  return(missing_uploads)
}

# Attempt 1: Get the hold report ####

# This attempt worked for 5 entities.

# Create sample data
data <- sf_report3[1:5, ]

# Connect to AWS via ODBC.
library(RODBC)
aws_connection <- odbcConnect("transpAWS")

# Generate the hold report.
data_list <- vector("list", 100)

for (row in 1:nrow(data)) {
  x <- as.numeric(data[row, "T.ID"])
  y <- data[row, "Begin.ER"]
  z <- data[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

hold_report <- do.call("rbind", data_list)

# Close the ODBC and clean up the environment.
odbcClose(aws_connection)

rm(aws_connection, template_er1, report_qtr, report_yr, 
   template_comp1, find_exp, find_rev, find_comp, find_missing)

# Attempt 2: Get the hold report ####

# This attempt worked for 155 entities, and then I got an "object not found"
# error.

# Connect to AWS via ODBC.
library(RODBC)
aws_connection <- odbcConnect("transpAWS")

# Generate the hold report.
data_list <- vector("list", 5000)

for (i in 1:nrow(sf_report)) {
  x <- sf_report[i, 1]
  y <- sf_report$Begin.ER[sf_report$T.ID == x]
  z <- sf_report$Begin.COMP[sf_report$T.ID == x]
  data_list[[i]] <- find_missing(x, y, z)
}

hold_report <- do.call("rbind", data_list)

# Close the ODBC and clean up the environment.
odbcClose(aws_connection)

rm(aws_connection, template_er1, report_qtr, report_yr, 
   template_comp1, find_exp, find_rev, find_comp, find_missing)