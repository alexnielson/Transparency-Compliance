# Old Material
### Introduction

Transparency Board policy requires local governments to report the payee, customer, or vendor name for transactions reported to Transparency, 
with two exceptions:^[*]

* Report "Not Applicable" when the transaction does not have a payee, customer, or vendor (e.g., a journal voucher entry).
* Report "Not Provided" when the payee, customer, or vendor's information is private and will always remain private (e.g., the person's name is protected by GRAMA, HIPPA, or FERPA).

In situations where the payee, customer, or vendor's information is protected but may become unprotected in the future (e.g., an undercover officer or an employee with a 
protective order), local governments are required to report the information but indicate it is temporarily protected. Local governments do this by reporting a "P" in the 
Protection Indicator field of the *State of Utah Transparency Website File Layout* (when a transaction is marked as temporarily protected, Transparency will not display the 
name, "Doing Business As" name, and gender of the payee, customer, or vendor).

As of October 2018, we are not aware of any person or organization that has conducted a review of the validity of the submitted payee/customer/vendor names. Auditor Dougall, 
therefore, decided that reviewing and identifying and correcting invalid names falls within his responsibilities as State Auditor.


"Not Applicable" and "Not Provided" are valid names, but not valid when they are a high percentage of an entities total.

Also valid: Anything that a person would recognize as a useful name.


### Analysis

#### Identify Valid and Invalid Names

Considering Transparency Board policy, valid payee/customer/vendor names include:

* A human-friendly name (i.e., a random character string is not meaningful and therefore invalid)
* "Not Applicable"
    + "NA" and "N/A" are valid if they are an abbreviation for "Not Applicable"
* "Not Provided"

Invalid payee/customer/vendor names include:

* Blank fields (i.e., no name reported)
* A high ratio of "Not Applicable" names to total reported names, as journal voucher entries should not comprise most of the data.
* "Not Applicable" or "Not Provided" paired with a "P" in the Protection Indicator field (i.e., temporarily-protected information must be reported)
* "Unavailable"
* "NA" or "N/A" if they are an abbreviation for "Not Available"
* Non human-friendly names (i.e., a random character string)
* Numbers

```{r}
# knitr::opts_chunk$set(echo = FALSE)

library(odbc)
library(stringr)
library(tidyverse)

odbc_aws <- dbConnect(odbc::odbc(), "transpAWS")
odbc_sf  <- dbConnect(odbc::odbc(), "Salesforce") 
```

Before reviewing the reported payee/customer/vendor names, I restricted my analysis to the following government types (which OSA monitors for compliance):

* Salesforce designation (must also be an active government):
    + Association of Government
    + City
    + Conservation District (not required to report until July 2019)
    + County
    + District Health
    + Housing
    + Interlocal
    + Local and Special Service District
    + Mental Health
    + Redevelopment Agency/Project Area
    + School District or Charter School
    + Town
    + Community Reinvestment Agency

* Transparency designation:
    + City
    + County
    + Housing Authority
    + Interlocal
    + Independent
    + K12 Education
    + Local District
    + Service District

```{sql, connection=odbc_sf, output.var="sf_filtered_entity_types"}
SELECT DISTINCT DeveloperName
FROM RecordType
WHERE DeveloperName NOT IN (
 'Chat', 
 'Community', 
 'Email', 
 'Phone', 
 'Social', 
 'CPA_Firm', 
 'Community_User',
 'Educational_Foundation', 
 'Financial_Institution', 
 'State_Agency',
 'Justice_Court', 
 'Non_Profits', 
 'Subpoena', 
 'LT_Gov_Document', 
 'Consolidation', 
 'AUP_Checklist', 
 'Budget_Checklist', 
 'LEA_Checklist', 
 'Large_Entity_Checklist',
 'Small_Entity_Checklist', 
 'Special_Purpose_Audit')
```

```{sql, connection=odbc_sf, output.var="sf_active_local_govs"}
SELECT Transparency_ID__c AS transparency_id,
       Name               AS sf_entity_name
FROM Account
WHERE RecordTypeId IN (
  SELECT Id
  FROM RecordType
  WHERE DeveloperName IN (
    'AOG', 
    'City',
    'Conservation_District',
    'County', 
    'District_Health',     
    'Housing', 
    'Interlocal', 
    'Local_and_Special_Service_District', 
    'Mental_Health', 
    'Redevelopment_Agency_Project_Area', 
    'School_District_or_Charter_School',
    'Town',
    'Community_Reinvestment_Agency'))
AND Entity_Status__c IN (
  'Current',
  'On hold',
  'Delinquent',
  'Suspended')
```

```{sql, connection=odbc_aws, output.var="aws_entity_types"}
SELECT DISTINCT govt_lvl
FROM entity
```

```{sql, connection=odbc_aws, output.var="entity_table"}
SELECT id, name, govt_lvl
FROM entity
WHERE govt_lvl IN (
  'CITY',
  'COUNTY',
  'HOUSING AUTHORITIES',
  'INTERLOCAL',
  'INDEPENDENT',
  'K12 EDUCATION',
  'LOCAL DISTRICTS'
  'SERVICE DISTRICTS')
```

```{sql, connection=odbc_aws, output.var="exp_names"}
SELECT 
  vendor_summary.entity_id,
  vendor_summary.vendor_id,
  vendor.name AS vendor_name
FROM vendor_summary
LEFT JOIN vendor
ON vendor.id = vendor_summary.vendor_id
WHERE vendor_summary.type = 1
AND vendor_summary.entity_id IN (
  SELECT id
  FROM entity
  WHERE govt_lvl IN (
    'CITY',
    'COUNTY',
    'HOUSING AUTHORITIES',
    'INTERLOCAL',
    'INDEPENDENT',
    'K12 EDUCATION',
    'LOCAL DISTRICTS'
    'SERVICE DISTRICTS'))
```

```{sql, connection=odbc_aws, output.var="rev_names"}
SELECT 
  vendor_summary.entity_id,
  vendor_summary.vendor_id,
  vendor.name AS vendor_name
FROM vendor_summary
LEFT JOIN vendor
ON vendor.id = vendor_summary.vendor_id
WHERE vendor_summary.type = 2
AND vendor_summary.entity_id IN (
  SELECT id
  FROM entity
  WHERE govt_lvl IN (
    'CITY',
    'COUNTY',
    'HOUSING AUTHORITIES',
    'INTERLOCAL',
    'INDEPENDENT',
    'K12 EDUCATION',
    'LOCAL DISTRICTS'
    'SERVICE DISTRICTS'))
```

```{r}
exp_names <- 
  as_tibble(exp_names)

rev_names <- 
  as_tibble(rev_names)

entity_table$id <- 
  as.integer(entity_table$id)

exp_names$entity_id <- 
  as.integer(exp_names$entity_id)

exp_names$vendor_id <- 
  as.integer(exp_names$vendor_id)

rev_names$entity_id <- 
  as.integer(rev_names$entity_id)

rev_names$vendor_id <- 
  as.integer(rev_names$vendor_id)

exp_names <- 
  exp_names %>% 
  left_join(entity_table,
            by = c("entity_id" = "id")) %>% 
  select(entity_id, entity_name = name, vendor_id, vendor_name) %>% 
  semi_join(sf_active_local_govs,
            by = c("entity_id" = "transparency_id"))

rev_names <- 
  rev_names %>% 
  left_join(entity_table,
            by = c("entity_id" = "id")) %>% 
  select(entity_id, entity_name = name, vendor_id, vendor_name) %>% 
  semi_join(sf_active_local_govs,
            by = c("entity_id" = "transparency_id"))

rm(aws_entity_types, sf_active_local_govs, entity_table)
```

```{r, eval=FALSE}
exp_distinct_names <- 
  exp_names %>% 
  distinct(vendor_name) %>% 
  arrange(vendor_name)

rev_distinct_names <- 
  rev_names %>% 
  distinct(vendor_name) %>% 
  arrange(vendor_name)

# rm(exp_distinct_names, rev_distinct_names)
```

I searched the names and found the following (as of October 2018):

* "Not Applicable"
* Blank fields
* "% VENDOR ID%" (Granite SD, 2019-09-11)


'"DONT USE"" use #20425"
"-USE VENDOR #2641"
***USE VENDOR #22058***
****DO NOT USE** USE 23559
***DO NOT USE*** SEE V011273 SANDYS FINE FOD
.
....
???
  _____________________
____________________________________
#N/A
Names surrounded with ""
Names preceeded by "~"
Number/numbers
* 0
* 1.24.18
* 105126
numbers and names (some of which make sense and some of which do not)
* 100594 TODD WOFFINDEN 65938
* 102152 RMFMA 62287
* 102164 ICC 63124
Vendor names containing information that appears like it belongs in an org, cat, or fund column:
  * "0030 Athletics"
* "0050 General"
* "02 sales tax bonds"
* "10.6Elem750"
* "10.6Misc3975"

Non-sensicals that include a date, transaction number (?), and a city
* "01/04 509655EX 665646 11.8 BLUFF"
* "01/11 Workers Compensation accrual"


```{r}
grady_augmented_dictionary <- 
  qdapDictionaries::GradyAugmented %>% 
  as_tibble()

colnames(grady_augmented_dictionary) <- "name"

mark_kantrowitzs_names_dictionary <- 
  read_tsv("http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/other/names.txt", 
           col_names = "name") %>% 
  as_tibble()

dictionary <- 
  grady_augmented_dictionary %>% 
  bind_rows(mark_kantrowitzs_names_dictionary)
```







After excluding local governments that are inactive or dissolved, I searched for names that are blank, contain only numbers, or contain text like "Not Available", "Not Provided," "Unavailable", "NA," and "N/A." I found the following:
  
  
  NO "NOT APPLICABLE"s?
  
  
  * Blank fields
* Fields containing some variation of "Not Provided"
* Fields containing some variation of "Not Available" (e.g., "N/A")
* Fields containing numbers unaccompanied by a name
* Fields containing incoherent or unhelpful names (e.g., a mixture of numbers and letters).

```{r, echo=FALSE}
exp_invalid_names <- 
  exp_names %>% 
  filter(vendor_name == "" |
           str_detect(vendor_name, regex("^not a", ignore_case = TRUE)) |
           str_detect(vendor_name, regex("^not pro", ignore_case = TRUE)) |
           str_detect(vendor_name, "^[0-9]+") |
           str_detect(vendor_name, regex("vendor id", ignore_case = TRUE)))

rev_invalid_names <- 
  rev_names %>% 
  filter(vendor_name == "" |
           str_detect(vendor_name, regex("^not a", ignore_case = TRUE)) |
           str_detect(vendor_name, regex("^not pro", ignore_case = TRUE)) |
           str_detect(vendor_name, "^[0-9]+"))

exp_number_of_entities_with_invalid_names <- 
  exp_invalid_names %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

rev_number_of_entities_with_invalid_names <- 
  rev_invalid_names %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

exp_number_of_entities_with_not_provided <- 
  exp_invalid_names %>% 
  filter(str_detect(vendor_name, regex("^not pro", ignore_case = TRUE))) %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

rev_number_of_entities_with_not_provided <- 
  rev_invalid_names %>% 
  filter(str_detect(vendor_name, regex("^not pro", ignore_case = TRUE))) %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

exp_number_of_entities_with_blank_or_number <- 
  exp_invalid_names %>% 
  filter(vendor_name == "" |
           str_detect(vendor_name, "^[0-9]+")) %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

rev_number_of_entities_with_blank_or_number <- 
  rev_invalid_names %>% 
  filter(vendor_name == "" |
           str_detect(vendor_name, "^[0-9]+")) %>% 
  distinct(entity_name) %>% 
  count() %>% 
  as.numeric()

exp_number_of_distinct_invalid_names <- 
  exp_invalid_names %>% 
  distinct(vendor_name) %>% 
  count() %>% 
  as.numeric()

rev_number_of_distinct_invalid_names <- 
  rev_invalid_names %>% 
  distinct(vendor_name) %>% 
  count() %>% 
  as.numeric()
```

For expenses, there are:
  
  * `r exp_number_of_entities_with_invalid_names` vendor names that may be invalid.
* `r exp_number_of_distinct_invalid_names` unique vendor names.
* `r exp_number_of_entities_with_invalid_names` local governments with vendor names that may be invalid.
+ `r exp_number_of_entities_with_not_provided` local governments report at least one "Not Provided" vendor name.
+ `r exp_number_of_entities_with_blank_or_number` local governments report at least one blank name or a name including a number.

For revenues, there are:
  
  * `r rev_number_of_entities_with_invalid_names` vendor names that may be invalid.
* `r rev_number_of_distinct_invalid_names` unique vendor names.
* `r rev_number_of_entities_with_invalid_names` local governments with vendor names that may be invalid.
+ `r rev_number_of_entities_with_not_provided` local governments report at least one "Not Provided" vendor name.
+ `r rev_number_of_entities_with_blank_or_number` local governments report at least one blank name or a name including a number. 

```{r}
rm(exp_number_of_distinct_invalid_names,
   exp_number_of_entities_with_blank_or_number,
   exp_number_of_entities_with_invalid_names,
   exp_number_of_entities_with_not_provided,
   rev_number_of_distinct_invalid_names,
   rev_number_of_entities_with_blank_or_number,
   rev_number_of_entities_with_invalid_names,
   rev_number_of_entities_with_not_provided)
```

**Expense Invalid Name Totals, by Entity**
  
  ```{r}
exp_total_vendors <- 
  exp_names %>% 
  group_by(entity_name) %>% 
  summarise(total_vendors = n())

exp_entity_totals <- 
  exp_invalid_names %>% 
  group_by(entity_name) %>% 
  summarise(invalid_vendor_count = n()) %>% 
  left_join(exp_total_vendors, by = "entity_name") %>% 
  mutate(percent_invalid = (invalid_vendor_count / total_vendors) * 100) %>% 
  mutate(percent_invalid = round(percent_invalid, digits = 1)) %>% 
  select(entity_name, percent_invalid, invalid_vendor_count, total_vendors) %>% 
  arrange(desc(percent_invalid), desc(invalid_vendor_count)) %>% 
  print()
```

**Revenue Invalid Name Totals, by Entity**
  
  ```{r}
rev_total_vendors <- 
  rev_names %>% 
  group_by(entity_name) %>% 
  summarise(total_vendors = n())

rev_entity_totals <- 
  rev_invalid_names %>% 
  group_by(entity_name) %>% 
  summarise(invalid_vendor_count = n()) %>% 
  left_join(rev_total_vendors, by = "entity_name") %>% 
  mutate(percent_invalid = (invalid_vendor_count / total_vendors) * 100) %>% 
  mutate(percent_invalid = round(percent_invalid, digits = 1)) %>% 
  select(entity_name, percent_invalid, invalid_vendor_count, total_vendors) %>% 
  arrange(desc(percent_invalid), desc(invalid_vendor_count)) %>% 
  print()
```

**Expense Invalid Name Totals, by Entity, by Invalid Name** 
  
  ```{r, echo=FALSE}
exp_by_name_totals <- 
  exp_invalid_names %>% 
  group_by(entity_name, vendor_name) %>% 
  summarise(name_count = n()) %>% 
  left_join(exp_total_vendors, by = "entity_name") %>% 
  mutate(percent_invalid = (name_count / total_vendors) * 100) %>% 
  mutate(percent_invalid = round(percent_invalid, digits = 1)) %>% 
  select(entity_name, vendor_name, percent_invalid, name_count, total_vendors) %>%
  arrange(desc(percent_invalid, desc(name_count))) %>% 
  print()
```

**Revenue Invalid Name Totals, by Entity, by Invalid Name** 
  
  ```{r, echo=FALSE}
rev_by_name_totals <- 
  rev_invalid_names %>% 
  group_by(entity_name, vendor_name) %>% 
  summarise(name_count = n()) %>% 
  left_join(rev_total_vendors, by = "entity_name") %>% 
  mutate(percent_invalid = (name_count / total_vendors) * 100) %>% 
  mutate(percent_invalid = round(percent_invalid, digits = 1)) %>% 
  select(entity_name, vendor_name, percent_invalid, name_count, total_vendors) %>%
  arrange(desc(percent_invalid, desc(name_count))) %>% 
  print()
```

## Local Government Specific Reports

```{r}
transparency_id <- 1231
```

```{sql, connection = odbc_aws, output.var = "exp_detail"}
SELECT DISTINCT 
transaction.fiscal_year,
transaction.posting_date,
transaction.description,
transaction.amount,
transaction.org2,
transaction.cat1,
transaction.cat2,
transaction.fund1,
transaction.fund2,
vendor.name AS vendor_name
FROM transaction
LEFT JOIN vendor 
ON vendor.id = transaction.vendor_id
WHERE transaction.batch_id IN (
  SELECT id
  FROM batch
  WHERE entity_id = ?transparency_id
  AND status = 'PROCESSED')
AND transaction.type = 1
```

```{sql, connection = odbc_aws, output.var = "rev_detail"}
SELECT DISTINCT 
transaction.fiscal_year,
transaction.posting_date,
transaction.description,
transaction.amount,
transaction.org2,
transaction.cat1,
transaction.cat2,
transaction.fund1,
transaction.fund2,
vendor.name AS vendor_name
FROM transaction
LEFT JOIN vendor 
ON vendor.id = transaction.vendor_id
WHERE transaction.batch_id IN (
  SELECT id
  FROM batch
  WHERE entity_id = ?transparency_id
  AND status = 'PROCESSED')
AND transaction.type = 2
```

```{sql, connection = odbc_aws, output.var = "transaction_group_table"}
SELECT id, name
FROM transaction_group
```

```{r}
exp_detail <- 
  exp_detail %>% 
  as.tibble()

rev_detail <- 
  rev_detail %>% 
  as.tibble()

transaction_group_table <- 
  transaction_group_table %>% 
  as.tibble()
```

```{r}
exp_detail <- 
  exp_detail %>% 
  left_join(transaction_group_table, by = c("org2" = "id")) %>% 
  rename(org2_name = name) %>% 
  left_join(transaction_group_table, by = c("cat1" = "id")) %>% 
  rename(cat1_name = name) %>% 
  left_join(transaction_group_table, by = c("cat2" = "id")) %>%
  rename(cat2_name = name) %>% 
  left_join(transaction_group_table, by = c("fund1" = "id")) %>%
  rename(fund1_name = name) %>% 
  left_join(transaction_group_table, by = c("fund2" = "id")) %>%
  rename(fund2_name = name) %>% 
  select(-c(org2, cat1, cat2, fund1, fund2)) %>% 
  arrange(posting_date)

rev_detail <- 
  rev_detail %>% 
  left_join(transaction_group_table, by = c("org2" = "id")) %>% 
  rename(org2_name = name) %>% 
  left_join(transaction_group_table, by = c("cat1" = "id")) %>% 
  rename(cat1_name = name) %>% 
  left_join(transaction_group_table, by = c("cat2" = "id")) %>%
  rename(cat2_name = name) %>% 
  left_join(transaction_group_table, by = c("fund1" = "id")) %>%
  rename(fund1_name = name) %>% 
  left_join(transaction_group_table, by = c("fund2" = "id")) %>%
  rename(fund2_name = name) %>% 
  select(-c(org2, cat1, cat2, fund1, fund2)) %>% 
  arrange(posting_date)
```

```{r}
entity_name <- 
  dbGetQuery(
    odbc_aws,
    paste("
          SELECT name
          FROM entity
          WHERE id = ", transparency_id)) %>% 
  as.character()

exp_path <- paste0(entity_name, " EXP.csv")
rev_path <- paste0(entity_name, " REV.csv")

write_csv(exp_detail, exp_path)

write_csv(rev_detail, rev_path)
```

## Close

```{r, echo=FALSE}
dbDisconnect(odbc_aws, odbc_dw)
rm(list = ls())
```