#### Case ascertainment (England) ####
library(readxl)
library(dplyr)
library(stringr)
library(data.table)

# Load HES name codes data for linkage
linking <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/audit_names_codes_HES_linkage_from_Tim_Alex_format.csv")


## AA HES data
#--------
# Load AA HES data
AA_HES <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/rawData/NIC-353206-K3Q9H_V3_suppressed_Harley_cleaned.xlsx", sheet = "Period 1")

# Remove NULL in site code
AA_HES <- AA_HES %>%
  filter(`Site Code` != "NULL")

# Check if the number of episodes of "provider code" ended with "-X" always larger then that without "-X" ###
# with_x <- AA_HES[AA_HES$`Provider Code` %like% "-X$", ]
# without_x <- AA_HES[AA_HES$`Provider Code` %like% "-X$", ]
# 
# if(all(with_x$FDEs > without_x$FDEs)) {
#   cat("All rows with '-X' in Provider Code have larger values in FDEs\n")
# } else {
#   cat("Not all rows with '-X' in Provider Code have larger values in FDEs\n")
# }
# 
# # Identify rows where "FDEs" values are not larger for rows with "-X"
# with_x <- AA_HES[grep("-X$", AA_HES$`Provider Code`)]
# without_x <- AA_HES[!grepl("-X$", AA_HES$`Provider Code`)]
# mismatched_rows <- with_x[FDEs <= without_x$FDEs]



# # Remove rows where the "site code" is the same but the "provider code" does not end with "-X"
# AA_HES <- AA_HES %>%
#   group_by(`Site Code`) %>%
#   mutate(is_duplicate = n() > 1) %>%
#   filter(!is_duplicate | grepl("-X$", `Provider Code`)) %>%
#   select(-is_duplicate)
# 
# # Check if the remaining hospital in the dataset is unique
# any_duplicated <- any(duplicated(AA_HES$`Site Code`))
# print(any_duplicated)
# # Great no duplicate now!
# 
# # Replace "*" with "4" (mid-point of 1 & 7) in FDEs
# AA_HES <- AA_HES %>%
#   mutate(FDEs = str_replace_all(FDEs, "\\*", "4"))

# Remove the last row of the dataset
# AA_HES <- AA_HES %>%
#   filter(`Provider Code` != "All" | `Site Code` != "All")

# Remove "-X" in "Provider code"
AA_HES$`Provider Code` <- sub("-X$", "", AA_HES$`Provider Code`)


# Group data by "provider code" and "site code" and then add up the episode number if they have the same "site code"
# Retain the first row of each duplicated "site code"
AA_HES <- AA_HES %>%
  group_by(`Provider Code`, `Site Code`) %>%
  mutate(Episodes = as.integer(Episodes)) %>%
  summarise(Episodes_sum = sum(Episodes)) %>%
  left_join(AA_HES, by = c("Provider Code", "Site Code")) %>%
  distinct(`Site Code`, .keep_all = TRUE)

# Check if the remaining hospital in the dataset is unique
any_duplicated <- any(duplicated(AA_HES$`Site Code`))
print(any_duplicated)
# Great no duplicate now!

# Identify abnormal site code (not a 5-character code)
abnormal <- AA_HES %>%
  filter(nchar(`Site Code`) != 5)
print(abnormal)

# Manually change episode number of the correct hospital row
correct <- AA_HES %>%
  filter(`Provider Code` == "RA2")

AA_HES$Episodes_sum[AA_HES$`Provider Code` == "RA2" & AA_HES$`Site Code` == "RA201"] <- 160

# Remove the row with abnormal site code
AA_HES <- AA_HES %>%
  filter(nchar(`Site Code`) >= 5)

# Replace "NA" with "4" (mid-point of 1 & 7) in Episodes_sum
AA_HES <- AA_HES %>%
  mutate(Episodes_sum = coalesce(Episodes_sum, 4))

# Keep necessary columns
AA_HES <- AA_HES %>%
  select(`Provider Code`, Trust, `Site Code`, `Hospital Description`, Episodes_sum) %>%
  rename(Episodes = Episodes_sum)

#--------

## AA audit data
#--------
# Load audit data
AA_audit <- readRDS("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/tidyData/AA_SCC_2022-23_clean_data.RDS")

# Count the number of episodes per hospital
AA_audit <- AA_audit %>%
  group_by(hosp_code) %>%
  mutate(row_count = n(), .after = trust_code) %>%
  filter(row_number() == 1)

# Check if the hospital in the dataset is unique
any_duplicated <- any(duplicated(AA_audit$hosp_code))
print(any_duplicated)

# Keep necessary columns for analysis
AA_audit <- AA_audit %>%
  select(ODS_code, hosp_code, hosp_name, trust_code, trust_name, row_count, country)

# Change "Queen Elizabeth Hospital" ODS_code in audit dataset
AA_audit <- AA_audit %>%
  mutate(ODS_code = as.character(ODS_code))
AA_audit$ODS_code[AA_audit$ODS_code == "RRK02"] <- "RRK15"

# change Broomsfield code to match with 'mid essex hospital', which we assume is Broomsfield

AA_audit$ODS_code[AA_audit$ODS_code == "RDEP3"] <- "RAJ32"

# Change the ODS_code manually (according to hosp_name and change their mismatched ODS_code)
AA_audit$ODS_code[AA_audit$ODS_code == "RM318"] <- "R0A66"
AA_audit$ODS_code[AA_audit$ODS_code == "RX2N2"] <- "E0A3H"


#--------


# Extract the hospitals in England
AA_audit <- AA_audit %>%
  filter(country == "England")

# # Check number of hospital not having info in linking file (England)
# 
# Join AA_audit_link with HES data
AA_audit_final <- left_join(AA_audit, AA_HES, by = c("ODS_code" = "Site Code"))

# Check any mismatched hospital data (appear in audit but not appear in HES Data)
sum(is.na(AA_audit_final$`Provider Code`))   # 3 is missing

# Locate which rows are missing
rows_missing <- AA_audit_final[!complete.cases(AA_audit_final$`Provider Code`), ]
print(rows_missing)

#"RDEP3" Broomfield Chelmsford cannot be found in linking file


# Combine the 2 episodes columns
head(AA_audit_final)


# Calculate the case ascertainment (no. records in audit / no. records in HES)
AA_audit_final <- AA_audit_final %>%
  mutate(Episodes = as.integer(Episodes)) %>%
  mutate(case_ascertainment_perc = (row_count/Episodes)*100)


# Keep necessary columns
AA_audit_final <- AA_audit_final %>%
  select(hosp_code, hosp_name, ODS_code, trust_code, trust_name, country, row_count, Episodes, case_ascertainment_perc) %>%
  rename(admission_no_audit_numerator = row_count,
         admission_no_EHR_denominator = Episodes,
         case_ascertainment_percentage = case_ascertainment_perc)

# Round the numeric values to 1 d.p.
AA_audit_final <- AA_audit_final %>%
  mutate(case_ascertainment_percentage = sprintf("%.1f", case_ascertainment_percentage))

# Export as csv file
write.csv(AA_audit_final, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/AA_CA_England.csv", row.names = FALSE)





#### Case ascertainment (Wales) ####

# Load AA PEDW data
AA_PEDW <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/rawData/Request_769_AdultAsthma_Harley_cleaned.xlsx")

# Check if hospitals in the dataset is unique
any_duplicated <- any(duplicated(AA_PEDW$`Site Code`))
print(any_duplicated)
# Great no duplicate now!

# Join audit data with linking file
AA_audit_link <- left_join(AA_audit, linking, by = c("ODS_code" = "ODS.Site.Code"))

# Extract the hospitals in Wales
AA_audit_link <- AA_audit_link %>%
  filter(country == "Wales")

# Check number of hospital not having info in linking file (Wales)
sum(is.na(AA_audit_link$Crown.hosp_code))   # 1

# Join AA_audit_link with PEDW data
AA_audit_final <- left_join(AA_audit_link, AA_PEDW, by = c("ODS_code" = "Site Code"))

# Check any mismatched hospital data (appear in audit but not matched with HES Data)
sum(is.na(AA_audit_final$Discharges))   # 0 

# Calculate the case ascertainment (no. records in audit / no. records in HES)
AA_audit_final <- AA_audit_final %>%
  mutate(case_ascertainment_perc = (row_count/Discharges)*100)


# Keep necessary columns
AA_audit_final <- AA_audit_final %>%
  select(hosp_code, hosp_name, ODS_code, trust_code.x, trust_name.x, country, row_count, Discharges, case_ascertainment_perc) %>%
  rename(trust_code = trust_code.x,
         trust_name = trust_name.x,
         admission_no_audit_numerator = row_count,
         admission_no_EHR_denominator = Discharges,
         case_ascertainment_percentage = case_ascertainment_perc)

# Round the numeric values to 1 d.p.
AA_audit_final <- AA_audit_final %>%
  mutate(case_ascertainment_percentage = sprintf("%.1f", case_ascertainment_percentage))

# Export as csv file
write.csv(AA_audit_final, "C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Analysis/Output/AA_CA_Wales.csv", row.names = FALSE)
