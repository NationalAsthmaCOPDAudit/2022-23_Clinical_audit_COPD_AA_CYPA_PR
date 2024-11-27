#### Case ascertainment (England) ####
library(readxl)
library(dplyr)
library(stringr)
library(data.table)

# Load HES name codes data for linkage
linking <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/audit_names_codes_HES_linkage_from_Tim_Alex_format.csv")


## CYPA HES data
#--------
# Load CYPA HES data
CYPA_HES <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/rawData/NIC-665377-Z3Q8K-v1.1_suppressed_Harley_cleaned.xlsx", sheet = "Period 1")

# Remove NA in site code
CYPA_HES <- CYPA_HES %>%
  filter(`Site Code` != "NA")

# Check if the number of episodes of "provider code" ended with "-X" always larger then that without "-X" ###
# with_x <- COPD_HES[COPD_HES$`Provider Code` %like% "-X$", ]
# without_x <- COPD_HES[COPD_HES$`Provider Code` %like% "-X$", ]
# 
# if(all(with_x$FDEs > without_x$FDEs)) {
#   cat("All rows with '-X' in Provider Code have larger values in FDEs\n")
# } else {
#   cat("Not all rows with '-X' in Provider Code have larger values in FDEs\n")
# }
# 
# # Identify rows where "FDEs" values are not larger for rows with "-X"
# with_x <- COPD_HES[grep("-X$", COPD_HES$`Provider Code`)]
# without_x <- COPD_HES[!grepl("-X$", COPD_HES$`Provider Code`)]
# mismatched_rows <- with_x[FDEs <= without_x$FDEs]



# Remove "-X" in "Trust Code" and then identify same "Site Code"
# Keep the row with larger "Episodes_all"
CYPA_HES$`Trust Code` <- sub("-X$", "", CYPA_HES$`Trust Code`)

# CYPA_HES <- CYPA_HES %>%
#   group_by(`Site Code`) %>%
#   filter(Episodes_all == max(Episodes_all)) %>%
#   ungroup()

# Group data by "provider code" and "site code" and then add up the episode number if they have the same "site code"
# Retain the first row of each duplicated "site code"

CYPA_HES <- CYPA_HES %>%
  group_by(`Trust Code`, `Site Code`) %>%
  mutate(Episodes_all = as.integer(Episodes_all),
         Episodes_1_5 = as.integer(Episodes_1_5)) %>%
  summarise(Episodes_sum = sum(Episodes_all),
            Episodes_1_5_sum = sum(Episodes_1_5)) %>%
  left_join(CYPA_HES, by = c("Trust Code", "Site Code")) %>%
  distinct(`Site Code`, .keep_all = TRUE)
  

# Check if the remaining hospital in the dataset is unique
any_duplicated <- any(duplicated(CYPA_HES$`Site Code`))
print(any_duplicated)
# Great no duplicate now!

# Identify abnormal site code (not a 5-character code)
abnormal <- CYPA_HES %>%
  filter(nchar(`Site Code`) != 5)
print(abnormal)

# Manually change episode number of the correct hospital row
correct <- CYPA_HES %>%
  filter(`Trust Code` == "RA2")

CYPA_HES$Episodes_sum[CYPA_HES$`Trust Code` == "RA2" & CYPA_HES$`Site Code` == "RA201"] <- 45

# Remove the row with abnormal site code
CYPA_HES <- CYPA_HES %>%
  filter(nchar(`Site Code`) >= 5)

# Replace "NA" with "4" (mid-point of 1 & 7) in Episodes_sum
CYPA_HES <- CYPA_HES %>%
  mutate(Episodes_sum = coalesce(Episodes_sum, 4),
         Episodes_1_5_sum = coalesce(Episodes_1_5_sum, 4))
colnames(CYPA_HES)
# Keep necessary columns
CYPA_HES <- CYPA_HES %>%
  select(`Trust Code`, `Trust name`, `Site Code`, `Hospital name`, Episodes_sum, Episodes_1_5_sum) %>%
  rename(Episodes_1_5 = Episodes_1_5_sum,
         Episodes = Episodes_sum)



#--------

## CYPA audit data
#--------
# Load audit data
CYPA_audit <- readRDS("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/tidyData/CYPA_SCC_2022-2023_clean_data.RDS")
# n=11209

# Filter participants aged 1-16 only
CYPA_audit <- CYPA_audit %>%
  filter(age >= 1 & age <= 16)   # n=11154

# Count the number of episodes per hospital
CYPA_audit <- CYPA_audit %>%
  group_by(hosp_code) %>%
  mutate(row_count = n(), .after = trust_code) %>%
  filter(row_number() == 1)

# Check if the hospital in the dataset is unique
any_duplicated <- any(duplicated(CYPA_audit$hosp_code))
print(any_duplicated)

# Keep necessary columns for analysis
CYPA_audit <- CYPA_audit %>%
  select(ODS_code, hosp_code, hosp_name, trust_code, trust_name, row_count, country)

# Change "Queen Elizabeth Hospital" ODS_code in audit dataset
CYPA_audit <- CYPA_audit %>%
  mutate(ODS_code = as.character(ODS_code))
CYPA_audit$ODS_code[CYPA_audit$ODS_code == "RRK02"] <- "RRK15"

# change Broomsfield code to match with 'mid essex hospital', which we assume is Broomsfield

CYPA_audit$ODS_code[CYPA_audit$ODS_code == "RDEP3"] <- "RAJ32"

# Change the ODS_code manually (according to hosp_name and change their mismatched ODS_code)
CYPA_audit$ODS_code[CYPA_audit$ODS_code == "RM318"] <- "R0A66"
CYPA_audit$ODS_code[CYPA_audit$ODS_code == "RX2N2"] <- "E0A3H"

# RTD10 goes to RTD02
CYPA_audit$ODS_code[CYPA_audit$ODS_code == "RTD10"] <- "RTD02"


#--------


# Extract the hospitals in England
CYPA_audit <- CYPA_audit %>%
  filter(country == "England")

# # Check number of hospital not having info in linking file (England)
# 
# Join CYPA_audit_link with HES data
CYPA_audit_final <- left_join(CYPA_audit, CYPA_HES, by = c("ODS_code" = "Site Code"))

# Check any mismatched hospital data (appear in audit but not appear in HES Data)
sum(is.na(CYPA_audit_final$ODS_code))   # 3 is missing

# Locate which rows are missing
rows_missing <- CYPA_audit_final[!complete.cases(CYPA_audit_final$Episodes), ]
print(rows_missing)

#"RDEP3" Broomfield Chelmsford cannot be found in linking file



# Combine the 2 episodes columns
head(CYPA_audit_final)


# Calculate the case ascertainment (no. records in audit / no. records in HES)
CYPA_audit_final <- CYPA_audit_final %>%
  mutate(Episodes = as.integer(Episodes)) %>%
  mutate(case_ascertainment_perc = (row_count/Episodes)*100)

# add in the young ones

CYPA_young_audit <- readRDS("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/tidyData/CYPA_SCC_2022-2023_clean_data.RDS")

CYPA_young_audit <- CYPA_young_audit %>% filter(CYPA_young_audit$age < 6) %>% group_by(hosp_code) %>% 
  summarise(audit_admissions_1_5_numerator = n())

# Keep necessary columns
CYPA_audit_final <- CYPA_audit_final %>% left_join(CYPA_young_audit)
CYPA_audit_final$audit_admissions_1_5_numerator[is.na(CYPA_audit_final$audit_admissions_1_5_numerator)] <- 0
CYPA_audit_final <- CYPA_audit_final %>%
  mutate(case_ascertainment_1_5_percentage = round((audit_admissions_1_5_numerator/Episodes_1_5)*100, 1)) %>%  #(HES_admissions_1_5_denominator
  select(hosp_code, hosp_name, ODS_code, trust_code, trust_name, country, row_count, Episodes, case_ascertainment_perc,
         audit_admissions_1_5_numerator, Episodes_1_5, case_ascertainment_1_5_percentage) %>%
  rename(admission_no_audit_numerator = row_count,
         admission_no_EHR_denominator = Episodes,
         admission_no_EHR_1_5_denominator = Episodes_1_5,
         case_ascertainment_percentage = case_ascertainment_perc,
         )

# Round the numeric values to 1 d.p.
CYPA_audit_final <- CYPA_audit_final %>%
  mutate(case_ascertainment_percentage = sprintf("%.1f", case_ascertainment_percentage),
         case_ascertainment_1_5_percentage = sprintf("%.1f", case_ascertainment_1_5_percentage))

# after all that - doesn't actually help because it is only a subset of the episodes (the wheeze episodes)
# so just remove those columns

CYPA_audit_final <- CYPA_audit_final %>% select(-audit_admissions_1_5_numerator,
                                                -admission_no_EHR_1_5_denominator,
                                                -case_ascertainment_1_5_percentage)

# Export as csv file
write.csv(CYPA_audit_final, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/CYPA_CA_England.csv", row.names = FALSE)





#### Case ascertainment (Wales) ####

# Load COPD PEDW data
CYPA_PEDW <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/rawData/Request_750_CYP - Version2_20240223.xlsx", sheet = "Period_1_Data")

# Combine the discharge number in age 1-5 and 6-16 together
CYPA_PEDW <- CYPA_PEDW %>%
  mutate(Discharges = Discharges1To5 + Discharges6To16) %>%
  select(-Discharges17To18)

# Delete last row which does not contain hospital data
CYPA_PEDW <- CYPA_PEDW %>%
  filter(!is.na(`Site Code`))

# Check if hospitals in the dataset is unique
any_duplicated <- any(duplicated(CYPA_PEDW$`Site Code`))
print(any_duplicated)
# Great no duplicate now!


# Load the audit data again
CYPA_audit <- readRDS("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/tidyData/CYPA_SCC_2022-2023_clean_data.RDS")

# Filter participants aged 1-16 only
CYPA_audit <- CYPA_audit %>%
  filter(age >= 1 & age <= 16)   # n=11154

# Count the number of episodes per hospital
CYPA_audit <- CYPA_audit %>%
  group_by(hosp_code) %>%
  mutate(row_count = n(), .after = trust_code) %>%
  filter(row_number() == 1)

# Check if the hospital in the dataset is unique
any_duplicated <- any(duplicated(CYPA_audit$hosp_code))
print(any_duplicated)

# Keep necessary columns for analysis
CYPA_audit <- CYPA_audit %>%
  select(ODS_code, hosp_code, hosp_name, trust_code, trust_name, row_count, country)


# Join audit data with linking file
CYPA_audit_link <- left_join(CYPA_audit, linking, by = c("ODS_code" = "ODS.Site.Code"))

# Extract the hospitals in Wales
CYPA_audit_link <- CYPA_audit_link %>%
  filter(country == "Wales")

# Check number of hospital not having info in linking file (Wales)
sum(is.na(CYPA_audit_link$Crown.hosp_code))   # 2

# Join CYPA_audit_link with PEDW data
CYPA_audit_final <- left_join(CYPA_audit_link, CYPA_PEDW, by = c("ODS_code" = "Site Code"))

# Check any mismatched hospital data (appear in audit but not matched with HES Data)
sum(is.na(CYPA_audit_final$Discharges))   # 0 

# Calculate the case ascertainment (no. records in audit / no. records in HES)
CYPA_audit_final <- CYPA_audit_final %>%
  mutate(case_ascertainment_perc = (row_count/Discharges)*100)


# Keep necessary columns
CYPA_audit_final <- CYPA_audit_final %>%
  select(hosp_code, hosp_name, ODS_code, trust_code.x, trust_name.x, country, row_count, Discharges, case_ascertainment_perc) %>%
  rename(trust_code = trust_code.x,
         trust_name = trust_name.x,
         admission_no_audit_numerator = row_count,
         admission_no_EHR_denominator = Discharges,
         case_ascertainment_percentage = case_ascertainment_perc)

# Round the numeric values to 1 d.p.
CYPA_audit_final <- CYPA_audit_final %>%
  mutate(case_ascertainment_percentage = sprintf("%.1f", case_ascertainment_percentage))


# Export as csv file
write.csv(CYPA_audit_final, "C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/CYPA_CA_Wales.csv", row.names = FALSE)




##### Checking for total number of people in raw audit data #####
dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/rawData/NACAP-CYPA-2204-2303-v103+LSOA-NDO-Imperial.csv",
                header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))


