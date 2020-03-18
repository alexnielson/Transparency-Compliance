sf_entity_exemption_base <- 
  dbGetQuery(
    odbc_sf,
    "SELECT 
      a.Name                          AS name,
      a.Id                            AS id,
      a.Transparency_ID__c            AS t_id,
      a.Fiscal_Year_Begins__c         AS begin_fy,
      a.Expense_Revenue_Start_Date__c AS begin_report_er, 
      a.Wage_Start_Date__c            AS begin_report_w2,
      a.Fiscal_Year_End_New__c,
      a.Fiscal_Year_Begins__c,
      r.Name AS govt_type
    FROM Account         AS a
    LEFT JOIN RecordType AS r
      ON a.RecordTypeId = r.Id
    WHERE a.RecordTypeId IN (
      SELECT Id
      FROM RecordType
      WHERE SobjectType = 'Account'
      AND IsActive = 'TRUE'
      AND Name NOT IN (
        'Community User',
        'Component', -- This govt type contains local governments, but not ones for which we enforce compliance.
        'Court (Search Under Parent Entity)',
        'CPA Firm',
        'Educational Foundation or Component Unit', -- Not an entity we review for compliance.
        'Financial Institution',
        'Health Provider',
        'Non Profits'))
    AND a.Name NOT IN (
      'Intermountain Power Agency',
      'test city 2',
      'Utah Associated Municipal Power Systems',
      'Utah Municipal Power Agency')
    AND Entity_Status__c NOT IN ('Inactive', 'Dissolved')
    AND (
      a.Expense_Revenue_Start_Date__c <= DATE() OR
      a.Expense_Revenue_Start_Date__c IS NULL)") %>% 
  as_tibble() %>% 
  mutate(
    govt_type = 
      if_else(
        name %in% 
          c("State of Utah",
            "Utah System of Higher Education - Student Loan Guarantee Program",
            "Utah System of Higher Education - Student Loan Purchase Program",
            "Utah System of Higher Education - Utah Educational Savings Plan dba my529",
            "Utah System of Higher Education - Utah State Board of Regents"),
        "Monitored State Agency",
        govt_type)) %>% 
  filter(govt_type != "State of Utah (agencies/depts/comp units/ etc.)") %>% 
  # The State of Utah is required to report to Transparent Utah, but in most instances we do not monitor its data:
  filter(name != "State of Utah") %>% 
  select(-govt_type)

sf_exemptions <- 
  dbGetQuery(
    odbc_sf,
    paste("
          SELECT *
          FROM (
            SELECT 
              Account__c,
              Exemption_Start_Date__c,
              Exemption_End_Date__c,
              Recurring_Exemption__c,
              Transparency_type_exempted__c,
              Permanent_or_Temporary__c,
              Exemption_Reason__c
            FROM Transparency_Exemption__c
            WHERE IsDeleted = FALSE) t
          LEFT JOIN Account a
          ON t.Account__c = a.Id
          WHERE a.Entity_Status__c IN (
            'Current', 
            'On hold', 
            'Delinquent', 
            'Suspended')")) %>% 
  as_tibble()


file_path <-
  "E:\\alex-nielson\\temp\\"

sf_entity_exemption_base %>% write_csv(
  path = paste0(file_path, Sys.Date(), " mass_exemption.csv"),
  na = "")
