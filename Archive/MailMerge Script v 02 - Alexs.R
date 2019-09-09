# Comments from email from Alex regarding this R code:
# Up to now, the person running the code has been responsible for pulling the reports. Rob should be able to help you if you have questions. 
# 
# The CFO and CAO files are reports from salesforce that contain the contact information of individuals who have been designated in one of those roles. These reports have to be downloaded every time the code is run as the CFO and CAO frequently get updated. I added the CAO file to the same location as all of the other reports (I couldn't find the CFO file): S:\Localgov\LG Compliance\Compliance\Noncompliance Correspondence\Held Funds Correspondence\Hold Funds\60 day notices\2019\Files for Code 
# 
# There's a folder on Salesforce under reports called "Compliance Cycle - 19" should contain the salesforce reports I ran to generate missing report information. 
# Brooke will have to request the D&I information from Anne or Rob will have to run the report in Salesforce (only the Treasurer's Office has access to that info in Salesforce)

require(tidyverse)
require(magrittr)
require(splitstackshape)
require(readxl)
#Import delinquency lists
#Import entity information list 
setwd("S:/Localgov/LG Compliance/Compliance/Noncompliance Correspondence/Held Funds Correspondence/Hold Funds/Hold notices/2019/Files for Code")

Auditor_Missing <-  read_excel("Auditor_Hold_8_5_19.xlsm", sheet = "Taxing")
colnames(Auditor_Missing)[8]<-"CFO_Full_Name"
colnames(Auditor_Missing)[9]<-"CAO_Full_Name"
step_1 <- Auditor_Missing[c(1, 2,3)]
Transparency_Missing <- read_excel("Transparency_Hold_8_6_19.xlsx")
colnames(Transparency_Missing)[2]<- "CFO_Full_Name"
#Transparency_Missing %<>% unite(CFO_Full_Name, `CFO: First Name`, `CFO: Last Name`, sep = " ", remove = FALSE)

Transparency_CAO <- read_csv("S:/Localgov/LG Compliance/Compliance/Noncompliance Correspondence/Held Funds Correspondence/Hold Funds/60 day notices/2019/Files for Code/Transparency_CAO.csv")
Transparency_CAO%<>% unite(CAO_Full_Name, `First Name`, `Last Name`, sep = " ", remove = FALSE)
Transparency_CAO<-Transparency_CAO[c(1:2)]


Entity_Info<-Auditor_Missing %>% group_by(`Entity Name`, CFO_Full_Name, CAO_Full_Name, `Mailing Street`, `Mailing City`, `Mailing Zip/Postal Code`, `Mailing State/Province`) %>% summarise(n=n()) %>% select(-n)

#Step 1: Combine year and report into one column 
step_1 %<>% unite(Report, `Report Year`, `Reports Name`, sep = " ", remove = FALSE)

#Step 2: Obtain unique list of type of missing reports and assign each report type an id "Report_Id".
step_2 <- step_1 %>% group_by(Report) %>% summarise(n =n()) %>% rowid_to_column("Report_ID") %>% select(-n)

#Step 3: Merge report id back over to master list
step_1 <- left_join(step_1, step_2)

#Step 4: spread out missing reports and then collapse by entity name 
step_3<-step_1 %>% spread(Report_ID, Report)
step_3<-step_3[c(1, 4:13)]
step_4 <- step_3 %>% group_by(`Entity Name`) %>% summarise_all(funs(toString(na.omit(.)))) %>%
  mutate_all(funs(replace(., . == 0, NA)))
step_4[step_4 == ""] = NA

#Step 5: Shift reports to left for mailmerge 
step_5<-as.data.frame(t(apply(step_4,1, function(x){return(c(x[!is.na(x)], x[is.na(x)]) )} ))) 
colnames(step_5) = colnames(step_4)
step_5<-step_5[c(1:10)]

#Step 6: Merge with address and cao/cfo info 
step_6<-left_join(Entity_Info, step_5)

#step 7: split transparency delinquencies into separate columns
Transparency_Missing<- cSplit(Transparency_Missing, c("Revenue Problem(s)", "Expenditure Problem(s)", "Wage Problem(s)"), sep = ",", direction = "wide")
Transparency_Missing<-left_join(Transparency_Missing, Transparency_CAO, by = c("Entity Name"))

step_6$`Mailing Zip/Postal Code`<-as.character(step_6$`Mailing Zip/Postal Code`)
#Step 8: Add transparency delinquencies 
step_7<-full_join(step_6, Transparency_Missing, by = c("Entity Name", "Mailing Street", "Mailing City", "Mailing Zip/Postal Code", "Mailing State/Province"))

#Step 9: Add D&I Reports
# DI_Missing <- read_csv("DI_Hold_4_3_19.csv")
# DI_Missing %<>% group_by(`Entity Name`, `Deposit & Investment Report Name`, `Mailing Street`, `Mailing City`, `Mailing Zip/Postal Code`, `Mailing State/Province`) %>% summarise(n=n()) %>% select(-n)
# CFO <- read_csv("C:/Users/anelson/Downloads/CFO.csv")
# CAO <- read_csv("C:/Users/anelson/Downloads/CAO.csv")
CFO <- read_csv("C:/Users/mjensen1/Downloads/CFO.csv")
CAO <- read_csv("C:/Users/mjensen1/Downloads/CAO.csv")

CAO%<>% unite(CAO_Full_Name, `First Name`, `Last Name`, sep = " ", remove = FALSE)
CFO%<>% unite(CFO_Full_Name, `First Name`, `Last Name`, sep = " ", remove = FALSE)
CAO<-CAO[c(1:2)]
CFO<-CFO[c(1:2)]

DI_Missing<- left_join(DI_Missing, CAO, by = c("Entity Name"))
DI_Missing<- left_join(DI_Missing, CFO, by = c("Entity Name"))

DI_Missing$`Mailing Zip/Postal Code`<-as.character(DI_Missing$`Mailing Zip/Postal Code`)

step_7 %<>% mutate(CFO = ifelse(is.na(CFO_Full_Name.x)== TRUE, CFO_Full_Name.y, CFO_Full_Name.x)) 
step_7 %<>% mutate(CAO = ifelse(is.na(CAO_Full_Name.x)== TRUE, CAO_Full_Name.y, CAO_Full_Name.x))

# step_8 <- full_join(step_7, DI_Missing, by = c("Entity Name", "Mailing Street", "Mailing City", "Mailing Zip/Postal Code", "Mailing State/Province"))
step_8 <- step_7
step_8 %<>% mutate(CFO = ifelse(is.na(CFO)==TRUE, CFO_Full_Name, CFO, CFO), 
                   CAO = iflese(is.na(CAO)==TRUE, CAO_Full_Name, CAO, CAO))

step_8 %<>% mutate(CFO = ifelse(is.na(CFO)==TRUE, "Chief Financial Office", CFO), 
                   CAO = ifelse(is.na(CAO)==TRUE, "Chief Administrative Officer", CAO))

step_8 <- step_8[c(1,4:16, 19:63,65:67)]

write.csv(step_8, file = "April 3 - Hold List NonTaxing.csv", row.names = FALSE)
