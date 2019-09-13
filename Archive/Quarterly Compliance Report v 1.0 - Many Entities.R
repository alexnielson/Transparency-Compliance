# Purpose ####
# Determine which local governments are non-compliant with the following
# Transparency reporting requirements:
# 1) One expense transaction per quarter.
# 2) One revenue transaction per quarter.
# 3) One compensation transaction per fiscal year.

# Contents ####
# Code summary:
# - Create a report of entities that are missing Transparency IDs.
# - Create a report of entities that are have a Transparency ID but are missing
#   compliance start dates.
# - Get each entity's expense, revenue, and compensation data via ODBC.
# - Analyze the data to determine whether each entity is compliant with 
#   Transparency reporting requirements.
# - Create a report of entities that are missing required Transparency uploads.

# Packages used:
# - lubridate
# - RODBC

# Commands used:
# - apply
# - as.data.frame
# - as.Date
# - as.matrix
# - as.numeric
# - colnames
# - do.call
# - gsub
# - floor
# - function
# - if else
# - month
# - odbcClose
# - odbcConnect
# - quarter
# - rbind
# - round
# - setdiff
# - sort
# - sqlQuery
# - subset
# - unique
# - year
# - ymd

# Define inputs ####

# Define the period through which we will enforce Transparency compliance.
report_qtr <- 2017.4
report_yr <- 2017

# Create a master template for determining whether the entity is compliant.
template_er1 <- as.numeric(c("2013.1", "2013.2", "2013.3", "2013.4",
                             "2014.1", "2014.2", "2014.3", "2014.4",
                             "2015.1", "2015.2", "2015.3", "2015.4",
                             "2016.1", "2016.2", "2016.3", "2016.4",
                             "2017.1", "2017.2", "2017.3", "2017.4",
                             "2018.1", "2018.2", "2018.3", "2018.4")) 
template_comp1 <- as.numeric(c("2013", "2014", "2015", "2016", "2017", "2018",
                               "2019", "2020"))

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

# Define the get_batches function ####

# This function gets the entity's batch IDs from AWS.
# The input for this function is the entity's Transparency ID ("t.id").
get_batches <- function(x) {
  sqlQuery(aws_connection, paste("select id
                                 from batch
                                 where entity_id = ", x,
                                 "and status = 'PROCESSED'"))
}

# Define the find_transactions function ####

# This function uses the entity's batch IDs to find the entity's transactions.
# The input for this function is the matrix of the entity's batch IDs found by
# calling get_batches ("batch_ids").
find_transactions <- function(x) {
  sqlQuery(aws_connection, paste("select posting_date, type
                                 from transaction
                                 where batch_id = (", x, ")", sep = ""))
}

# Define the get_transactions function ####

# This function results in a list of the transactions associated with each
# batch ID.
# The input for this function is the entity's Transparency ID ("t.id").
get_transactions <- function(x) {
  batch_ids <- as.matrix(get_batches(x))
  colnames(batch_ids) <- "batch_id"
  
  # Find the transactions associated with each batch ID.
  apply(batch_ids, 1, find_transactions)
}


# Define the find_missing function ####

# This function finds the periods for which the entity is not compliant with
# Transparency reporting requirements.
# The inputs for this function are the entity's Transparency ID ("t.id"), the
# date the entity is required to begin reporting expense and revenue transac-
# tions ("begin.er1"), and the date the entity is required to begin reporting
# employee compensation transactions.
find_missing <- function(x, y, z) {
  # Get the entity's transactions and convert the list to a data frame.
  transactions1 <- get_transactions(x)
  transactions2 <- do.call("rbind", transactions1)
  rm(transactions1)
  
  # Separate transactons by type.
  exp1  <- subset(transactions2, type == 1)
  rev1  <- subset(transactions2, type == 2)
  comp1 <- subset(transactions2, type == 3)
  rm(transactions2)
  
  # Get rid of transactions with a posting date earlier than the beginning
  # compliance date.
  exp2  <- subset(exp1,  posting_date >= y)
  rev2  <- subset(rev1,  posting_date >= y)
  comp2 <- subset(comp1, posting_date >= z)
  rm(exp1, rev1, comp1)
  
  # Convert the expense and revenue dates to quarters, the compensation dates
  # to years, and eliminate duplicates.
  library(lubridate)
  reported_exp  <- sort(unique(quarter(ymd(exp2$posting_date), with_year = TRUE)))
  reported_rev  <- sort(unique(quarter(ymd(rev2$posting_date), with_year = TRUE)))
  reported_comp <- sort(unique(year(ymd(comp2$posting_date))))
  rm(exp2, rev2, comp2)
  
  # Define (as a numeric) the reporting period for which the entity should have
  # begun reporting to Transparency.
  begin_er2   <- quarter(ymd(y), with_year = TRUE)
  begin_comp2 <- year(ymd(z))
  rm(z)
  
  # Because I haven't built in code that only proceeds if the below three
  # values are non-zero, this code below isn't necessary.
  # I don't need to build an if else statement, because I want the entities
  # without missing uploads to show up as "EXP ", "REV ", and "COMP ", so  
  # Salesforce can identify which entities need to go on hold.
  
  # Define the number of quarters for which the entity should have reported
  # expense and revenue transactions.
  # a <- round(report_qtr - begin_er2, digits = 1)
  # b <- round((round(a, digits = 1) - (floor(round(a, digits = 1)))), digits = 1)
  # c <- floor(a)
  # d <- c * 4
  # e <- round(a - c, digits = 1)
  # f <- e * 10
  # reporting_qtrs <- d + f + 1 # Add one to account for the period lost during 
  # the arithmetic.
  # rm(a, b, c, d, e, f)
  
  # Define the number of years for which the entity should have reported
  # compensation transactions.
  # reporting_yrs1 <- report_yr - begin_comp2
  # if (month(y) == 1) {
  #   reporting_yrs2 <- reporting_yrs1 -1  # For entities that haven't yet
  # completed the fiscal year.
  # } else {
  #   reporting_yrs2 <- reporting_yrs1
  # }
  # rm(reporting_yrs1)
  
  # Determine if the entity has periods for which the required transactions are
  # not reported.
  # missing_exp  <- reporting_qtrs - length(reported_exp)
  # missing_rev  <- reporting_qtrs - length(reported_rev)
  # missing_comp <- reporting_yrs2 - length(reported_comp)
  # rm(missing_exp, missing_rev, missing_comp)
  
  # Format the reporting template for the entity.
  a <- template_er1[template_er1 >= begin_er2]
  template_er2 <- a[a <= report_qtr]
  rm(a)
  
  b <- template_comp1[template_comp1 >= begin_comp2]
  c <- b[b <= report_yr] 
  
  # Adjust the template so entities with a fiscal year not equal to the 
  # calendar year aren't reported as missing the beginning compliance year.
  if (month(y) != 01) {
    template_comp2 <- c[c > begin_comp2]
  } else {
    template_comp2 <- c
  }
  rm(b, c)
  
  # Find the periods for which the entity did report and the periods for which
  # the entity did not report, and compare them.
  missing_exp1  <- as.data.frame(setdiff(template_er2, reported_exp))
  missing_rev1  <- as.data.frame(setdiff(template_er2, reported_rev))
  missing_comp1 <- as.data.frame(setdiff(template_comp2, reported_comp))
  
  # Format the missing periods.
  missing_exp2 <- as.data.frame(gsub(".", " Q", missing_exp1[ ,1], fixed = TRUE))
  missing_rev2 <- as.data.frame(gsub(".", " Q", missing_rev1[ ,1], fixed = TRUE))
  rm(missing_exp1, missing_rev1)
  
  colnames(missing_exp2) <- "Missing EXP"
  colnames(missing_rev2) <- "Missing REV"
  colnames(missing_comp1) <- "Missing COMP"
  
  missing_exp3  <- as.data.frame(paste("EXP", missing_exp2$'Missing EXP'))
  missing_rev3  <- as.data.frame(paste("REV", missing_rev2$'Missing REV'))
  missing_comp2 <- as.data.frame(paste("COMP", missing_comp1$'Missing COMP'))
  rm(missing_exp2, missing_rev2, missing_comp1)
  
  colnames(missing_exp3) <- "Missing Uploads"
  colnames(missing_rev3) <- "Missing Uploads"
  colnames(missing_comp2) <- "Missing Uploads"
  
  # Combine the reports for the missing uploads and add the Transparency ID.
  missing_reports <- rbind(missing_exp3, missing_rev3, missing_comp2)
  missing_reports$T.ID <- x
  rm(missing_exp3, missing_rev3, missing_comp2, y, begin_er2,
     begin_comp2, reported_exp, reported_rev, reported_comp, template_er2, 
     template_comp2)
  # If I use code I highlited out above, then I will need to remove the 
  # following variables in the step above: reporting.qtrs and 
  # reporting.yrs2.

  return(missing_reports)
}



# Attempt 1: rbind the list (2 entities) ####

# Dr. Brough suggested I use a loop and then rbind the list.
# This worked for a list of two entities.

# Create a sample data set.
data <- sf_report3[1:2, ]

# Connect to AWs.
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

# Close the ODBC to AWS.
odbcClose(aws_connection)


# Attempt 2: rbind the list (all entities) ####

# Dr. Brough suggested I use a loop and then rbind the list.
# This works, but only up through the 188th observation (I think memory in R
# hits a limit).

# Create a blank list.
data_list <- vector("list", 20000)

# Connect to AWs.
library(RODBC)
aws_connection <- odbcConnect("transpAWS")

# Generate the hold report.
for (row in 1:nrow(sf_report3)) {
  x <- as.numeric(sf_report3[row, "T.ID"])
  y <- sf_report3[row, "Begin.ER"]
  z <- sf_report3[row, "Begin.COMP"]
  data_list[[row]] <- find_missing(x,y,z)
}

# Transform the hold report list into a data frame.
hold_report <- do.call("rbind", data_list)

# Close the ODBC to AWS.
odbcClose(aws_connection)

# Attempt 3: split data frame and rbind the list ####

# The problem is that the code stops at the 188th observation - and I think the
# code times out. My attempt to fix it will be by breaking up the observations.

# This resulted in the same problem as  the code that stops at the 188th
# observation. I'm not sure what the problem is, but I suspect it is something
# to do with R running out of memory.

# Create a blank list.
data_list <- vector("list", 20000)

# Connect to AWs.
library(RODBC)
aws_connection <- odbcConnect("transpAWS")

data1 <- sf_report3[1:100, ]
data2 <- sf_report3[101:200, ]
data3 <- sf_report3[201:300, ]
data4 <- sf_report3[301:400, ]
data5 <- sf_report3[401:500, ]
data6 <- sf_report3[501:600, ]
data7 <- sf_report3[601:700, ]
data8 <- sf_report3[701:800, ]
data9 <- sf_report3[801:875, ]

# Generate the hold report.
for (row in 1:nrow(data1)) {
  x <- as.numeric(data1[row, "T.ID"])
  y <- data1[row, "Begin.ER"]
  z <- data1[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data2)) {
  x <- as.numeric(data2[row, "T.ID"])
  y <- data2[row, "Begin.ER"]
  z <- data2[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data3)) {
  x <- as.numeric(data3[row, "T.ID"])
  y <- data3[row, "Begin.ER"]
  z <- data3[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data4)) {
  x <- as.numeric(data4[row, "T.ID"])
  y <- data4[row, "Begin.ER"]
  z <- data4[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data5)) {
  x <- as.numeric(data5[row, "T.ID"])
  y <- data5[row, "Begin.ER"]
  z <- data5[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data6)) {
  x <- as.numeric(data6[row, "T.ID"])
  y <- data6[row, "Begin.ER"]
  z <- data6[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data7)) {
  x <- as.numeric(data7[row, "T.ID"])
  y <- data7[row, "Begin.ER"]
  z <- data7[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data8)) {
  x <- as.numeric(data8[row, "T.ID"])
  y <- data8[row, "Begin.ER"]
  z <- data8[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

for (row in 1:nrow(data9)) {
  x <- as.numeric(data9[row, "T.ID"])
  y <- data9[row, "Begin.ER"]
  z <- data9[row, "Begin.COMP"]
  data_list[[row]] <-  find_missing(x,y,z)
}

# Transform the hold report list into a data frame.
hold_report <- do.call("rbind", data_list)


# Close the ODBC to AWS.
odbcClose(aws_connection)
# Attempt 4: create a list and then pull data ####
# This code prints all three columns of the list a, which is a list of all the
# entity IDs and dates.
# a <- split(sf_report3, sf_report3$T.ID)
# for (row in 1:length(a)) {
#  x <- as.numeric(a[[row]]["T.ID"])
#  y <- a[[row]]["Begin.ER"]
#  z <- a[[row]]["Begin.COMP"]
#  print(x)
#  print(y)
#  print(z)
# }

# The below doesn't work... and I'm not sure why. The error I get is: 
# replacement has 1 row, data has 40.
data_list <- c()
entity_info <- split(sf_report3, sf_report3$T.ID)

# Connect to AWs.
library(RODBC)
aws_connection <- odbcConnect("transpAWS")

for (row in 1:length(entity_info)) {
  x <- as.matrix(as.numeric(entity_info[[row]]["T.ID"]))
  y <- as.matrix(entity_info[[row]]["Begin.ER"])
  z <- as.matrix(entity_info[[row]]["Begin.COMP"])
  data_list[[row]] <- find_missing(x, y, z)
}


# Transform the hold report list into a data frame.
hold_report <- do.call("rbind", data_list)


# Close the ODBC to AWS.
odbcClose(aws_connection)