# Author comment & file description ####

# Code written by Michael Jensen.

# Report the periods for which a local government has not complied with the
# following Transparency reporting requirements:
# - One expense transaction for every quarter the entity is required to upload.
# - One revenue transaction for every quarter the entity is required to upload.
# - One compensation transaction for every year the entity is required to
#   upload.

# Define inputs ####

# Define the following:
# - The entity's Transparency ID.
# - The entity's beginning EXP/REV date.
# - The entity's beginning COMP date.
# - The calendar quarter through which we will enforce Transparency compliance.
# - The reporting period templates (in calendar quarters).

t.id         <- as.numeric(1105)
begin.er     <- as.Date("2015-01-01")
begin.comp   <- as.Date("2017-01-01")
end.qtr      <- as.numeric(2017.4)
template.er  <- as.numeric(c("2013.1", "2013.2", "2013.3", "2013.4",
                             "2014.1", "2014.2", "2014.3", "2014.4",
                             "2015.1", "2015.2", "2015.3", "2015.4",
                             "2016.1", "2016.2", "2016.3", "2016.4",
                             "2017.1", "2017.2", "2017.3", "2017.4",
                             "2018.1", "2018.2", "2018.3", "2018.4",
                             "2019.1", "2019.2", "2019.3", "2019.4")) 
template.comp <- as.numeric(c("2014",   "2015",   "2016",   "2017",
                             "2018",   "2019",   "2020"))

# Query AWS data ####
library(RODBC)
aws <- odbcConnect("transpAWS")

name <- sqlQuery(aws, paste("SELECT name FROM entity WHERE id = ", t.id))

reported.exp <- as.matrix(
  sqlQuery(aws, paste("
                      SELECT DISTINCT CONCAT (YEAR(posting_date), '.',
                      QUARTER(posting_date))
                      FROM transaction
                      WHERE batch_id
                      IN (
                      SELECT id
                      FROM batch
                      WHERE entity_id = ", t.id, "
                      AND status IN ('PROCESSED', 'PROCESSING'))
                      AND type = 1")))

reported.rev <- as.matrix(
  sqlQuery(aws, paste("
                      SELECT DISTINCT CONCAT(YEAR(posting_date), '.',
                      QUARTER(posting_date))
                      FROM transaction
                      WHERE batch_id
                      IN (
                      SELECT id
                      FROM batch
                      WHERE entity_id = ", t.id, "
                      AND status IN ('PROCESSED', 'PROCESSING'))
                      AND type = 2")))

reported.comp <- as.matrix(
  sqlQuery(aws, paste("
                      SELECT DISTINCT fiscal_year
                      FROM transaction
                      WHERE batch_id
                      IN (
                      SELECT id
                      FROM batch
                      WHERE entity_id = ", t.id, "
                      AND status IN ('PROCESSED', 'PROCESSING'))
                      AND type = 3")))

odbcClose(aws)
detach(package:RODBC)
rm(aws)

# Analyze the reported EXP, REV, and COMP data for compliance ####

# Modify template.er and template.comp to get a template specific to the
# entity, remembering the following:
# - The reporting requirements for each entity depend on the entity's fiscal
#     year and the time of year we enforce compliance.
library(lubridate)
a <- as.numeric(
  template.er[template.er >= quarter(ymd(begin.er), with_year = TRUE)])
rqrd.er <- a[a <= end.qtr]
rm(a)

if (month(begin.comp) == 1) {
  b <- template.comp[template.comp >= as.numeric(year(ymd(begin.comp)))]
} else {
  b <- template.comp[template.comp > as.numeric(year(ymd(begin.comp)))]
}

if ((month(begin.comp) == 1) & (month(today()) < 4)) {
  rqrd.comp <- b[b < (as.numeric(year(today())) - 1)]
} else if ((month(begin.comp) == 1) & (month(today()) >= 4)) {
  rqrd.comp <- b[b < as.numeric(year(today()))]
} else if ((month(begin.comp) != 1) & (month(today()) < 10)) {
  rqrd.comp <- b[b < as.numeric(year(today()))]
} else if (month(begin.comp != 1) & (month(today()) <= 10)) {
  rqrd.comp <- b[b <= as.numeric(year(today()))]
}
rm(b)

# Compare the periods for which the entity did report to the periods for which
# the entity should have reported.
missing.exp1  <- as.data.frame(setdiff(rqrd.er,   reported.exp))
missing.rev1  <- as.data.frame(setdiff(rqrd.er,   reported.rev))
missing.comp1 <- as.data.frame(setdiff(rqrd.comp, reported.comp))

# Report the entity's missing uploads ####

# Format the data frames for entities with no missing uploads
if (nrow(missing.exp1) == 0) {
  missing.exp1[1, ] <- 0
}

if (nrow(missing.rev1) == 0) {
  missing.rev1[1, ] <- 0
}

if (nrow(missing.comp1) == 0) {
  missing.comp1[1, ] <- 0
}

# Label the missing periods
missing.exp1[1] <- gsub(".1", " JAN-MAR", missing.exp1[ ,1], fixed = TRUE)
missing.exp1[1] <- gsub(".2", " APR-JUN", missing.exp1[ ,1], fixed = TRUE)
missing.exp1[1] <- gsub(".3", " JUL-SEP", missing.exp1[ ,1], fixed = TRUE)
missing.exp1[1] <- gsub(".4", " OCT-DEC", missing.exp1[ ,1], fixed = TRUE)

missing.rev1[1] <- gsub(".1", " JAN-MAR", missing.rev1[ ,1], fixed = TRUE)
missing.rev1[1] <- gsub(".2", " APR-JUN", missing.rev1[ ,1], fixed = TRUE)
missing.rev1[1] <- gsub(".3", " JUL-SEP", missing.rev1[ ,1], fixed = TRUE)
missing.rev1[1] <- gsub(".4", " OCT-DEC", missing.rev1[ ,1], fixed = TRUE)

# Consolidate the missing uploads.
missing.exp2  <- paste(missing.exp1[ , 1], collapse = ", ")
missing.rev2  <- paste(missing.rev1[ , 1], collapse = ", ")
missing.comp2 <- paste(missing.comp1[ , 1], collapse = ", ")

# Create the report of missing uploads.
hold.report1 <- data.frame("T ID" = numeric(3), "Entity Name" = character(3),
                           "Missing Uploads" = character(3), 
                           stringsAsFactors = FALSE)

hold.report1[ , 1] <- t.id

hold.report1[ , 2] <- paste(name[1, 1])
rm(name)

hold.report1[1, 3] <- paste("EXP", missing.exp2)
hold.report1[2, 3] <- paste("REV", missing.rev2)
hold.report1[3, 3] <- paste("COMP", missing.comp2)

print(hold.report1)

# Clear the R environment ####
detach(package:lubridate)
rm(missing.exp1, missing.rev1, missing.comp1, reported.exp, reported.rev,
   reported.comp, t.id, begin.er, begin.comp, end.qtr,
   template.er, rqrd.er, template.comp, rqrd.comp, 
   missing.comp2, missing.exp2, missing.rev2, hold.report1)