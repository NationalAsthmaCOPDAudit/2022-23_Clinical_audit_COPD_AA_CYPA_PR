#-------------------------------------------------------------#
# A d u l t   a s t h m a   s c c   b u i l d   s c r i p t   #
#                                                             #
# Author: Alex Adamson                                        #
#-------------------------------------------------------------#




library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
source("H:/My R functions/lintestOR.R")
source("H:/My R functions/tidyoutput.R")
# library(janitor)
# library(officer)
# library(flextable)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(lme4)
'%!in%' <- function(x,y)!('%in%'(x,y))
library(car)
library(extrafont)
loadfonts()
fonts()
library(forcats)
nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}


tablex <- function(x, y, z) { x %>% select(!!y, !!z) %>% table(useNA = "ifany") }

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

medTableforadmiss <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- sum(eng, na.rm = TRUE)
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- sum(wal, na.rm = TRUE)
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- sum(scot, na.rm = TRUE)
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- sum(all, na.rm = TRUE)
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
  return(ret)
}


meanSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, mean, sd)
  varcol[ ,3:4] <- format(round(varcol[ ,3:4], roundno), nsmall = roundno)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
  
}

mediSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, median, lo.quart, hi.quart)
  # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno), nsmall = roundno) # otherwise use 'roundno'
  
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  
  if(nrow(gen) == 0) {return(var_N)}
  
  else {
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
    # gen.E2 <- select(gen.E2, Var1, England)
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      var_N <- cbind(var_N, gen3[ ,2:3])
    }
    return(var_N)
    
  }
}



medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
  return(ret)
}

# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  print(gen.A2)
  
  gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
    inner_join(gen.A2, by = "Var1")
  colnames(gen.table) <- c(varname, paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}




histnorm <- function(g) {
  
  h <- hist(g, breaks = 10, density = 10,
            col = "lightgray", xlab = "Accuracy", main = "Overall") 
  xfit <- seq(min(g, na.rm = TRUE), max(g, na.rm = TRUE), length = 40) 
  yfit <- dnorm(xfit, mean = mean(g, na.rm = TRUE), sd = sd(g, na.rm = TRUE)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  
  plot(h, ylim = c(0, max(yfit)))
  lines(xfit, yfit, col = "black", lwd = 2)
}



CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}

# When we read this in, if we use stringsasfactors = true then we can say that blank strings count as missing,
# without needing to recode them all. Then we can change all the necessary factors back to characters if 
# need be.





# Rename the columns so that they are easier to read and if possible match up to the adult asthma column
# names:

# Safer to use rename here first, so I know which columns I include and which ones I drop...
# But after doing that, it's fine




dat_old <- read.csv("C:/Alex Harley/Adult Asthma/SCC 2021-2022/Data/rawData/AA-2104-2203-v103-Imperial.csv",
header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))



dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/rawData/NACAP-AA-2204-2303-v102+LSOA-NDO-Imperial.csv",
                header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))


full_join(data.frame(names = colnames(dat), dataset = "new"), data.frame(names = colnames(dat_old), dataset = "old"), by = "names")

str(dat)

summary(dat$imd)

dat <- dat %>% select(study_ID = STUDYID,
                      patient_ID = PATIENTID,
                      LSOA = lsoa11,
                      hosp_code = Org,
                    #  hosp_name = OrgName,
                      trust_code = TrustCode,
                    #  trust_name = Trust.Now,
                    #  region = Region,
                      country = Country,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                      hospital_transfer = X1.1c.Hospital.Transfer,
                      first_department = X1.2.First.Department,
                      age = X.Age.At.Arrival,
                      gender = X2.3.Gender,
                      smoke_status = X2.5.Smoking,
                      heart_rate = X3.1.Heart.Rate.BPM,
                      resp_rate = X3.2.Respiratory.Rate.BPM,
                      oxygen_sat_value = X3.3.Oxygen.Saturation.,
                      oxygen_sat_recorded = X3.3nr.Oxygen.Saturation.NR,
                      oxygen_sat_measurement_type = X3.3a.Oxygen.Measurement,
                      PEF_init_value = X3.4a.First.Recorded.Peak.Flow,
                      PEF_init_recorded = X3.4.Peak.Flow.Measurement,
                      PEF_init_date = X3.4b.Peak.Flow.Date,
#                      PEF_init_date_recorded = X3.4anr.Peak.Flow.Recoding.Date.Not.Recorded,
                      PEF_init_time = X3.4c.Peak.Flow.Time,
                      PEF_init_time_recorded = X3.4nr.Peak.Flow.Time.NR,
                      PEF_prev_value = X3.5.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X3.5nr.Previous.Peak.Flow.Not.Recorded,
                      PEF_predict_value = X3.5a.Predicted.Peak.Flow,
                      PEF_predict_recorded = X3.5anr.Predicted.Peak.Flow.Not.Recorded,
                      RSR = X4.1.Specialist.Respiratory.Review,
                      RSR_date = X4.1a.Date.of.respiratory.review,
                      RSR_time = X4.1b.Time.of.respiratory.review,
                      oxygen_prescribed = X4.2.Oxygen.Prescribed,
                      oxygen_date = X4.2a.Oxygen.Prescribed.Date, # don't actually need this variable
                      oxygen_time = X4.2b.Oxygen.Prescribed.Time, # don't actually need this variable
                      # X4.2.Oxygen.Prescribed...No # drop
                      # X4.2.Oxygen.Prescribed...Prescribed # drop
                      # X4.2.Oxygen.Prescribed...Administered # drop
                      oxygen_admin = X4.3.Oxygen.Administered,
                      steroids_admin = X4.4.Steroids.Administered,
                      steroids_admin_date = X4.4a.Steroids.Date,
                      steroids_admin_time = X4.4b.Steroids.Time,
                      steroids_24hr_prev = X4.5.Steroids.Administered.24.Hours.Before.Arrival,
                      b2a_1hr_prev = X4.6.B2.Agonists.Before.Arrival,
                      b2a_admin = X4.7.B2.Agonists.After.Arrival,
                      b2a_admin_date = X4.7a.B2.Agonists.Date,
                      b2a_admin_time = X4.7b.B2.Agonists.Time,
                      life_status = X5.1.Discharge.Life.Status,  # feel weird about having 'died'
                      discharge_date = X5.2a.Discharge.Date,
                      discharge_time = X5.2b.Discharge.Time,
                      discharge_bundle = X5.3.Discharge.Bundle,
                      # X5.4.Discharge.Elements # completely useless: drop.
                      DB_inhaler = X5.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X5.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X5.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X5.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X5.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X5.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_comm_FU_2_days = X5.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
                      DB_spec_review_4_weeks = X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks,
                      DB_none = X5.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X6.1.Discharge.Inhaled.Steroids,
                      oral_steroids_dis = X6.2.Discharge.Oral.Steroids,
                      oral_steroids_rescue_history = X6.3.Rescue.Steroids.Last.12.Months,
                      overseas = X..Overseas.or.Non.NHS)
                      # dataset = X..Dataset # useless - drop
                      # referred_for_FU = X6.4.Referred.for.Followup) # no longer in dataset
                      # Dataset.Version   # useless - drop


# 2021-22 names:
# dat <- dat %>% select(study_ID = RowID,
#                       patient_ID = PatientID,
#                       LSOA = ONSPD_AUG_2021_UK.lsoa11,
#                       hosp_code = Org,
#                       hosp_name = OrgName,
#                       trust_code = Tcode.Now,
#                       trust_name = Trust.Now,
#                       region = Region,
#                       country = Country_1,
#                       arrival_date = X1.1a.Arrival.Date,
#                       arrival_time = X1.1b.Arrival.Time,
#                       hospital_transfer = X1.1c.Hospital.Transfer,
#                       first_department = X1.2.First.Department,
#                       age = X.Age.At.Arrival,
#                       gender = X2.3.Gender,
#                       smoke_status = X2.5.Smoking,
#                       heart_rate = X3.1.Heart.Rate,
#                       resp_rate = X3.2.Respiratory.Rate,
#                       oxygen_sat_value = X3.3.Oxygen.Saturation.,
#                       oxygen_sat_recorded = X3.3nr.Oxygen.Saturation.NR,
#                       oxygen_sat_measurement_type = X3.3a.Oxygen.Measurement,
#                       PEF_init_value = X3.4a.First.Recorded.Peak.Flow,
#                       PEF_init_recorded = X3.4.Peak.Flow.Measurement,
#                       PEF_init_date = X3.4b.Peak.Flow.Date,
# #                      PEF_init_date_recorded = X3.4anr.Peak.Flow.Recoding.Date.Not.Recorded,
#                       PEF_init_time = X3.4c.Peak.Flow.Time,
#                       PEF_init_time_recorded = X3.4nr.Peak.Flow.Time.NR,
#                       PEF_prev_value = X3.5.Previous.Best.Peak.Flow,
#                       PEF_prev_recorded = X3.5nr.Previous.Peak.Flow.Not.Recorded,
#                       PEF_predict_value = X3.5a.Predicted.Peak.Flow,
#                       PEF_predict_recorded = X3.5anr.Predicted.Peak.Flow.Not.Recorded,
#                       RSR = X4.1.Specialist.Respiratory.Review,
#                       RSR_date = X4.1a.Date.of.respiratory.review,
#                       RSR_time = X4.1b.Time.of.respiratory.review,
#                       oxygen_prescribed = X4.2.Oxygen.Prescribed,
#                       oxygen_date = X4.2a.Oxygen.Prescribed.Date, # don't actually need this variable
#                       oxygen_time = X4.2b.Oxygen.Prescribed.Time, # don't actually need this variable
#                       # X4.2.Oxygen.Prescribed...No # drop
#                       # X4.2.Oxygen.Prescribed...Prescribed # drop
#                       # X4.2.Oxygen.Prescribed...Administered # drop  
#                       oxygen_admin = X4.3.Oxygen.Administered,
#                       steroids_admin = X4.4.Steroids.Administered,
#                       steroids_admin_date = X4.4a.Steroids.Date,
#                       steroids_admin_time = X4.4b.Steroids.Time,
#                       steroids_24hr_prev = X4.5.Steroids.Administered.24.Hours.Before.Arrival,
#                       b2a_1hr_prev = X4.6.B2.Agonists.Before.Arrival,
#                       b2a_admin = X4.7.B2.Agonists.After.Arrival,
#                       b2a_admin_date = X4.7a.B2.Agonists.Date,
#                       b2a_admin_time = X4.7b.B2.Agonists.Time,
#                       life_status = X5.1.Discharge.Life.Status,  
#                       discharge_date = X5.2a.Discharge.Date,
#                       discharge_time = X5.2b.Discharge.Time,
#                       discharge_bundle = X5.3.Discharge.Bundle,
#                       # X5.4.Discharge.Elements # completely useless: drop.
#                       DB_inhaler = X5.4.Discharge.Elements...Inhaler.technique.checked,
#                       DB_maintenance = X5.4.Discharge.Elements...Maintenance.medication.reviewed,
#                       DB_adherence = X5.4.Discharge.Elements...Adherence.discussed,
#                       DB_PAAP = X5.4.Discharge.Elements...PAAP.issued.reviewed,
#                       DB_triggers = X5.4.Discharge.Elements...Triggers.discussed,
#                       DB_smoke = X5.4.Discharge.Elements...Tobacco.dependency.addressed,
#                       DB_comm_FU_2_days = X5.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
#                       DB_spec_review_4_weeks = X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks,
#                       DB_none = X5.4.Discharge.Elements...None,
#                       inhaled_steroids_dis = X6.1.Discharge.Inhaled.Steroids,
#                       oral_steroids_dis = X6.2.Discharge.Oral.Steroids,
#                       oral_steroids_rescue_history = X6.3.Rescue.Steroids.Last.12.Months)
#                       # referred_for_FU = X6.4.Referred.for.Followup) # no longer in dataset
#                       # Dataset.Version   # useless - drop 
                      

# need to add in the hospital details

# two records have switched trusts - recode

dat %>% filter(hosp_code == "NMG") %>% select(trust_code) %>% table()

dat$trust_code[dat$hosp_code == "NMG" & dat$trust_code == "R0A"] <- "RM3"

# recode the two trusts that changed to RM3


hosp_details <- read.csv("C:/Users/aadamson/Documents/Audit_2023_onwards/General UK data/NACAP-ORGS-COPD-230621.csv")

hosp_details <- hosp_details %>% rename(hosp_code = Crown.Code, trust_code = TrustCode, hosp_name = Description, trust_name = Trust,
                                        region = Region..SHA., ODS_code = ODS.Code)

hosp_details <- hosp_details %>% select(-Country)

dat <- left_join(dat, hosp_details, by = c("hosp_code", "trust_code"))

table(dat$region, useNA = "ifany")
table(dat$hosp_name, useNA = "ifany")
table(dat$trust_name, useNA = "ifany")



nlc("No. records in original dataset:")
nrow(dat) %>% nlc()

nlc("No. records that are drafts or test hospitals:")
# dat %>% filter(completion_status != "100%" | hosp_code == "YYY") %>% nrow()
dat %>% filter(hosp_code == "YYY" | trust_code == "YYYT") %>% nrow() %>% nlc()


# dat <- dat %>% filter(completion_status == "100%" & hosp_code != "YYY")
# nlc("No. records after draft records and test hospitals removed:")
nrow(dat)

# remove that column because we don't need it any more
# dat <- dat %>% select(-completion_status)


# Remove overseas patients
 nlc("Number of overseas patients:") # bear in mind this has gone down because some overseas 
                                    # were from test hospitals
dat %>% filter(!is.na(overseas)) %>% nrow()

# nlc("Overseas patients removed before analysis")
# colnames(dat)

dat <- dat %>% filter(is.na(overseas))
nlc("No. records after overseas removed:")
nrow(dat)


# remove that column because we don't need it any more
dat <- dat %>% select(-overseas)

# Now, write csv for Phil.


# Need to add in the empty gender factors and smoking status factors, and while we're doing it we might as well
# put it in the correct order

summary(dat$smoke_status)

summary(dat$gender)

# has changed from before
# dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Other", "Not recorded"))
dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Other", "Not recorded/Preferred not to say"))

summary(dat$gender)

summary(dat$smoke_status)

dat$smoke_status <- factor(dat$smoke_status,
                           levels = c("Never smoked", "Ex-smoker", "Current smoker", "Current vaper", 
                                      "Never smoked and current vaper", "Ex-smoker and current vaper",
                                      "Current smoker and current vaper",  "Not recorded"))
summary(dat$smoke_status)
# Now we do more building and cleaning:

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))

# Everything seems to be alright at this point.

# Now would probably be a good point to link to the LSOAs.

# Read in the IMD quintiles
# let's use the joint quintiles seeing as we no longer have scotland

IMDeng_wal <- read.csv("C:/Users/aadamson/Documents/Audit_2023_onwards/General UK data/IMD/2019_England_and_Wales_Income_Employment_IMD_clean.csv",
                       header = TRUE, stringsAsFactors = FALSE)

summary(IMDeng_wal$new_IMD_quintile)

IMDeng_wal <- IMDeng_wal %>% select(LSOA = LSOA_code_2011, IMD_quintile = new_IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng_wal, by = "LSOA")
summary(dat$IMD_quintile)

dat$IMD_quintile <- as.character(dat$IMD_quintile)
dat$IMD_quintile[is.na(dat$IMD_quintile)] <- "Missing IMD quintile"
dat$IMD_quintile <- factor(dat$IMD_quintile, levels = c("1", "2", "3", "4", "5", "Missing IMD quintile"))


dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)
summary(dat$IMD_quintile)

# Country already in the dataset so can skip this bit:

# # Add country variable
# 
# country <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_CYP_asthma_R_format.csv",
#                     stringsAsFactors = FALSE)
# 
# # country <- country %>% rename(country = Country, hosp_code = HospCode, trust_code = TrustCode)
# 
# # Drop the provided trust code because the trust code in Tim's provided spreadsheet should be the most
# # up-to-date
# 
# dat <- dat %>% select(-trust_code)
# 
# dat <- left_join(dat, country, by = "hosp_code")






# dat$country <- factor(dat$country, levels = c("England", "Scotland", "Wales"), ordered = FALSE)
dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)
colnames(dat)

table(dat$arrival_date)

# Having to bind the rows together has made all of our date and time variables character. Let's change them back to factor.

dat <- dat %>% mutate_if(is.character, ~factor(.))

str(dat) # Not too painful actually


# Okay now we've added IMD quintile and additional hospital details... let's do some cleaning.



# Let's convert all our dates to dates and times to times (as they're currently factors)

dat$arrival_date <- as.Date(dat$arrival_date, "%d/%m/%Y")
dat$steroids_admin_date <- as.Date(dat$steroids_admin_date, "%d/%m/%Y")
dat$b2a_admin_date <- as.Date(dat$b2a_admin_date, "%d/%m/%Y")
dat$discharge_date <- as.Date(dat$discharge_date, "%d/%m/%Y")
dat$PEF_init_date <- as.Date(dat$PEF_init_date, "%d/%m/%Y")
dat$RSR_date <- as.Date(dat$RSR_date, "%d/%m/%Y")
dat$oxygen_date <- as.Date(dat$oxygen_date, "%d/%m/%Y")

summary(dat$PEF_init_date)

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)
dat$PEF_init_time <- as.character(dat$PEF_init_time)
dat$RSR_time <- as.character(dat$RSR_time)
dat$oxygen_time <- as.character(dat$oxygen_time)


summary(dat$arrival_date)
summary(dat$arrival_time)
summary(dat$steroids_admin_date)
summary(dat$steroids_admin_time)
summary(dat$b2a_admin_date)
summary(dat$b2a_admin_time)
summary(dat$discharge_date)
summary(dat$discharge_time)

# dat$arrival_time <- as.character(dat$arrival_time)
# dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
# dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
# dat$discharge_time <- as.character(dat$discharge_time)

# dat$arrival_time[dat$arrival_time != ""] <- paste0(dat$arrival_time[dat$arrival_time != ""]
#                                                    
#                                                    
#                                                    
# dat$steroids_admin_time[dat$steroids_admin_time)
# dat$b2a_admin_time[dat$b2a_admin_time)
# dat$discharge_time[dat$discharge_time)
# 
# 
head(dat$arrival_time)
head(dat$steroids_admin_time)
head(dat$b2a_admin_time)

# different format this time, don't need this bit:

# dat <- dat %>% mutate(arrival_time = ifelse(arrival_time == "", "", 
#                                      paste0(arrival_time, ":00")),
#                       steroids_admin_time = ifelse(steroids_admin_time == "", "", 
#                                             paste0(steroids_admin_time, ":00")),
#                       b2a_admin_time = ifelse(b2a_admin_time == "", "", 
#                                             paste0(b2a_admin_time, ":00")),
#                       discharge_time = ifelse(discharge_time == "", "", 
#                                             paste0(discharge_time, ":00")),
#                       PEF_init_time = ifelse(PEF_init_time == "", "", 
#                                               paste0(PEF_init_time, ":00")),
#                       RSR_time = ifelse(RSR_time == "", "", 
#                                               paste0(RSR_time, ":00")))


head(dat$steroids_admin_time)
head(dat$arrival_time)
head(dat$arrival_date)
head(dat$discharge_time)
head(dat$discharge_date)
head(dat$RSR_time)

# We can try converting times to 'difftime' when we try and create the table of when people are given
# particular things.


# if the times have :00 on the end use this:

# dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M:%S"),
#                       steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time), 
#                                                      format="%Y-%m-%d %H:%M:%S"),
#                       b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M:%S"),
#                       discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M:%S"),
#                       PEF_init_DT = as.POSIXct(paste(PEF_init_date, PEF_init_time), format="%Y-%m-%d %H:%M:%S"),
#                       RSR_DT = as.POSIXct(paste(RSR_date, RSR_time), format="%Y-%m-%d %H:%M:%S"),
#                       oxygen_DT = as.POSIXct(paste(oxygen_date, oxygen_time), format="%Y-%m-%d %H:%M:%S"))


# otherwise, use this:

dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M"),
                      steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time),
                                                     format="%Y-%m-%d %H:%M"),
                      b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M"),
                      discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M"),
                      PEF_init_DT = as.POSIXct(paste(PEF_init_date, PEF_init_time), format="%Y-%m-%d %H:%M"),
                      RSR_DT = as.POSIXct(paste(RSR_date, RSR_time), format="%Y-%m-%d %H:%M"),
                      oxygen_DT = as.POSIXct(paste(oxygen_date, oxygen_time), format="%Y-%m-%d %H:%M"))


summary(dat$arrival_DT)
head(dat$arrival_DT)
head(dat$steroids_admin_DT)
summary(dat$steroids_admin_DT)
head(dat$RSR_DT)

summary(dat$oxygen_DT)
summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)



nlc("This is how many people were discharged before the discharge date of 2022-04-01 and need to be removed")
dat %>% filter(discharge_date < "2022-04-01") %>% nrow() %>% nlc()

nlc("This is how many people were discharged after the date of 2023-03-31 and have to be removed:")
dat %>% filter(discharge_date > "2023-03-31") %>% nrow() %>% nlc()


nlc("Our new N is:")
nrow(dat) %>% nlc()



# Life status
summary(dat$life_status)

# Already coded like this
# dat <- dat %>% mutate(life_status = recode_factor(life_status, Yes = "Alive"))
# dat$life_status <- factor(dat$life_status, levels = c(levels(dat$life_status), "Died as inpatient"))

head(dat)


# Time to b2a minutes

dat <- dat %>% mutate(arrival_to_b2a_minutes = difftime(b2a_admin_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_b2a_minutes)
dat$arrival_to_b2a_minutes <- as.integer(dat$arrival_to_b2a_minutes)

summary(dat$arrival_to_b2a_minutes)

dat %>% select(arrival_to_b2a_minutes) %>% arrange(arrival_to_b2a_minutes) %>% head(20)

nlc("This many people received beta agonists before they arrived so need to be removed:")
dat %>% filter(arrival_to_b2a_minutes < 0) %>% nrow() %>% nlc()

dat %>% filter(b2a_1hr_prev == "Yes - up to 1 hour prior to arrival") %>% select(arrival_to_b2a_minutes) %>% summary()

summary(dat$b2a_1hr_prev)

table(dat$b2a_admin)
nrow(dat)
dat <- dat %>% filter(arrival_to_b2a_minutes > -1 | is.na(arrival_to_b2a_minutes))

nlc("Our new N is:")
nrow(dat) %>% nlc()

# Time to steroids in hours

dat <- dat %>% mutate(arrival_to_steroids_hours = difftime(steroids_admin_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_steroids_hours)
dat$arrival_to_steroids_hours <- as.numeric(dat$arrival_to_steroids_hours)

summary(dat$arrival_to_steroids_hours)

nlc("This many people received steroids before they arrived so need to be removed:")
dat %>% filter(arrival_to_steroids_hours < 0) %>% nrow() %>% nlc()


# Time to peak flow

dat <- dat %>% mutate(arrival_to_PEF_init_hours = difftime(PEF_init_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_PEF_init_hours)
dat$arrival_to_PEF_init_hours <- as.numeric(dat$arrival_to_PEF_init_hours)

summary(dat$arrival_to_PEF_init_hours)

dat %>% filter(arrival_to_PEF_init_hours < 0) %>% nrow() # 167 people had peak flow measured before arrival
dat %>% filter(arrival_to_PEF_init_hours < 0) %>% summary() # earliest time accepted should be up to 1 hour before

dat %>% filter(arrival_to_PEF_init_hours < -1) %>% arrange(arrival_to_PEF_init_hours) %>% select(arrival_to_PEF_init_hours)

nlc("This many people received PEF over an hour before admission so need to be removed:")
dat %>% filter(arrival_to_PEF_init_hours < -1) %>% nrow() %>% nlc()

dat <- dat %>% filter(arrival_to_PEF_init_hours >= -1 | is.na(arrival_to_PEF_init_hours))

nlc("This is our new N:")
dat %>% nrow() %>% nlc()


# Time to RSR

dat <- dat %>% mutate(arrival_to_RSR_hours = difftime(RSR_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_RSR_hours)
dat$arrival_to_RSR_hours <- as.numeric(dat$arrival_to_RSR_hours)

summary(dat$arrival_to_RSR_hours)

nlc("This many people received an RSR before they arrived so need to be removed:")
dat %>% filter(arrival_to_RSR_hours < 0) %>% nrow() %>% nlc()

# summary(dat$RSR_DT)
# summary(dat$b2a_admin_DT)
# summary(dat$steroids_admin_DT)
# length of stay hours

dat <- dat %>% mutate(LOS_hours = difftime(discharge_DT, arrival_DT, units = "hours"))
head(dat$LOS_hours)
dat$LOS_hours <- as.numeric(dat$LOS_hours)
summary(dat$LOS_hours)

# length of stay hours just for those who died
table(dat$life_status)

dat <- dat %>% mutate(LOS_hours_died = ifelse(life_status == "Died as inpatient", LOS_hours, NA))

dat %>% select(LOS_hours_died) %>% filter(!is.na(LOS_hours_died))


# length of stay days

dat <- dat %>% mutate(LOS_days = difftime(discharge_date, arrival_date, units = "days"))
head(dat$LOS_days)
head(dat)
dat$LOS_days <- as.numeric(dat$LOS_days)
summary(dat$LOS_hours)

# length of stay hours just for those who are alive
table(dat$life_status)

dat <- dat %>% mutate(LOS_days_alive = ifelse(life_status == "Alive", LOS_days, NA))

dat %>% select(LOS_days_alive) %>% summary()


# Length of stay categorised

dat <- dat %>% mutate(LOS_3day = NA)
dat$LOS_3day[dat$LOS_days_alive < 4] <- "<=3 days"
dat$LOS_3day[dat$LOS_days_alive > 3] <- ">3 days"
dat$LOS_3day <- factor(dat$LOS_3day, levels = c("<=3 days", ">3 days"))
summary(dat$LOS_3day)

nlc("This many people were discharged before they arrived and need to be removed:")

dat %>% filter(LOS_hours < 0) %>% nrow() %>% nlc()

# Need to remove those who were transferred from this (only used in child asthma)
# dat <- dat %>% mutate(LOS_hours = ifelse(discharge_bundle == "Patient transferred to another hospital", 
#                                          NA, LOS_hours))


# day of arrival and day of discharge


dat <- dat %>% mutate(arrival_day_of_week = weekdays(arrival_date, abbreviate = FALSE))

dat$arrival_day_of_week <- ordered(dat$arrival_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

table(dat$arrival_day_of_week, useNA = "ifany")

# new format for discharge day of week



dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))

summary(dat$discharge_day_of_week)

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday")] <- "Weekday"

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Saturday", "Sunday")] <- "Weekend"


dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Weekday", "Weekend"))

table(dat$discharge_day_of_week, useNA = "ifany")


# dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))
# 
# dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
#                                                                      "Friday", "Saturday", "Sunday"))
# 
# table(dat$discharge_day_of_week, useNA = "ifany")


# Those who transferred need to be removed from the discharge day of week

# Using ifelse here changes them to factor levels so I'm doing it the old school method

# Only for child asthma:
# dat$discharge_day_of_week[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA

summary(dat$discharge_day_of_week)

# time of arrival for the time of arival table.
# basically times are so annoying to work with, so what I've done is given each time an associated date of 
# 1970-01-01 (the common origin date) and then subtracted the time from 1970-01-01 00:00:00 to give minutes
# into the day. Seems to have worked well

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.POSIXct(paste("1970-01-01", arrival_time))) %>%
  mutate(arrival_time_mins_from_midnight = difftime(arrival_time_mins_from_midnight, 
                                                    "1970-01-01 00:00:00", units = "mins"))
head(dat$arrival_time_mins_from_midnight)

dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(arrival_time_mins_from_midnight) %>% head(10)
dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(desc(arrival_time_mins_from_midnight)) %>% head(10)

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.integer(arrival_time_mins_from_midnight))


# Peak flow things

# first off, PEF_init...

dat %>% select(contains("PEF")) %>% summary()

# is everyone who is missing a value down as not being recorded etc? 
dat %>% filter(is.na(PEF_init_value)) %>% select(PEF_init_recorded) %>% table(useNA = "ifany") # yes

# does everyone down as recorded have a value?
dat %>%  filter(PEF_init_recorded == "Yes") %>% select(PEF_init_value) %>% summary() # yes

# 43 people managed to add that the time wasn't recorded even though they shouldn't have been able to
# get through to that question if they answered that PEF wasn't recorded - we recode

dat %>% filter(PEF_init_recorded != "Yes") %>% select(contains("PEF")) %>% summary()
dat$PEF_init_time_recorded[dat$PEF_init_recorded != "Yes"] <- NA



# 

# 7 people have an initial value but are also marked as not having it - removed.

nlc("This many people are marked as being both too ill for PEF or having it not recorded, but also had a value, so need to be removed
as inconsistent:")

summary(dat$PEF_init_recorded)

dat %>% filter(PEF_init_recorded != "Yes") %>%  filter(!is.na(PEF_init_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print() %>% nlc()

nrow(dat)
# dat <- dat %>% filter(is.na(PEF_init_value) | is.na(PEF_init_recorded))

nlc("New total:")
nrow(dat) %>% nlc()

dat %>% filter(!is.na(PEF_init_value) & !is.na(PEF_init_recorded)) %>% select(PEF_init_value) %>% summary() # table(useNA = "ifany")

dat %>% select(contains("PEF")) %>% summary()

# Therefore, we recode all the missing from that variable as 'Recorded'.
dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
dat$PEF_init_recorded[!is.na(dat$PEF_init_value)] <- "Recorded" 
dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded", "Not recorded", "No - patient unable to do PEF", "No - not done"))
table(dat$PEF_init_recorded, useNA = "ifany")

# Do the same for the previous and predicted values

dat %>% filter(is.na(PEF_init_time)) %>% nrow() # 787 people missing a time for PEF
dat %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_date) %>% table(useNA = "ifany") # 3 people missing a time do have a date though
# but this is ok.

dat %>% filter(!is.na(PEF_prev_recorded)) %>%  filter(!is.na(PEF_prev_value)) %>%
  select(contains("PEF"))

nlc("This many people are marked as not having previous PEF recorded but also had a value, so need to be removed
as inconsistent:")


dat %>% filter(!is.na(PEF_prev_recorded)) %>%  filter(!is.na(PEF_prev_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print() %>% nlc()

dat <- dat %>% filter(is.na(PEF_prev_value) | is.na(PEF_prev_recorded))

nlc("New total:")
nrow(dat) %>% nlc()

dat %>% filter(is.na(PEF_prev_value)) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(PEF_prev_recorded)) %>% select(PEF_prev_value) %>% summary() # table(useNA = "ifany")
dat %>% filter(!is.na(PEF_prev_recorded)) %>% select(PEF_prev_value) %>% summary() # table(useNA = "ifany")


summary(dat$PEF_prev_recorded)

dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
dat$PEF_prev_recorded[!is.na(dat$PEF_prev_value)] <- "Recorded" 
dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded, levels = c("Recorded", "Not recorded"))
table(dat$PEF_prev_recorded, useNA = "ifany")


summary(dat$PEF_prev_value)
summary(dat$PEF_prev_recorded)


nlc("This many people are marked as not having predicted PEF recorded but also had a value, so need to be removed
as inconsistent:")

dat %>% filter(!is.na(PEF_predict_recorded)) %>%  filter(!is.na(PEF_predict_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print() %>% nlc()


nlc("This many people had a predicted PEF in addition to the previous, when it should only be possible to put in one or the other:")


dat %>% filter(!is.na(PEF_prev_value)) %>%  filter(!is.na(PEF_predict_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print() %>% nlc()

nrow(dat)

nlc("Inconsistent so removed, leaving our new N as:")

dat <- dat %>% filter((!is.na(PEF_prev_value) & is.na(PEF_predict_value)) |
                        (is.na(PEF_prev_value) & !is.na(PEF_predict_value)) |
                        (is.na(PEF_prev_value) & is.na(PEF_predict_value)))

nrow(dat) %>% nlc()

dat %>% select(contains("PEF")) %>% summary()
dat %>% filter(PEF_init_recorded != "Recorded") %>% select(contains("PEF")) %>% summary()
dat %>% filter(PEF_init_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()

dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(PEF_prev_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()


dat$PEF_predict_recorded[dat$PEF_prev_recorded == "Recorded" &  dat$PEF_predict_recorded == "Not recorded"] <- NA

dat %>% filter(PEF_init_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()
dat %>% filter(!is.na(PEF_predict_value)) %>% select(contains("PEF")) %>% summary() # everyone with a recorded PEF_predict value
                                                                                    # is marked as they should be
dat %>% filter(PEF_predict_recorded == "Not recorded") %>% select(contains("PEF")) %>% summary()



dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)
dat$PEF_predict_recorded[!is.na(dat$PEF_predict_value)] <- "Recorded" 
dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded, levels = c("Recorded", "Not recorded"))
table(dat$PEF_predict_recorded, useNA = "ifany")

dat %>% filter(PEF_predict_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()




dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")

summary(dat$PEF_prev_value)
dat %>% filter(is.na(PEF_prev_value)) %>% select(PEF_predict_value) %>% summary()


# All now consistent!

# Invalid PEF values? No.
dat %>% filter(PEF_init_value < 60) %>% nrow()
dat %>% filter(PEF_init_value > 800) %>% nrow()

dat %>% filter(PEF_prev_value < 60) %>% nrow()
dat %>% filter(PEF_prev_value > 800) %>% nrow()

dat %>% filter(PEF_predict_value < 60) %>% nrow()
dat %>% filter(PEF_predict_value > 800) %>% nrow()

# PEF / recorded inconsistencies? No

dat %>% filter(!is.na(PEF_init_value)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & !is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% select(PEF_init_recorded) %>% table()

dat %>% filter(!is.na(PEF_prev_value)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_prev_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & is.na(PEF_prev_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_predict_value)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & !is.na(PEF_predict_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & is.na(PEF_predict_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_predict_value)) %>% nrow()

summary(dat$PEF_init_recorded)



# Make the 'recorded' variable clearer, by adding in 'recorded' for people that have an associated value
# Did this above in a much more convoluted way

# dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
# dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
# dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)
# 
# dat$PEF_init_recorded[(!is.na(dat$PEF_init_value))] <- "Recorded"
# dat$PEF_prev_recorded[(!is.na(dat$PEF_prev_value))] <- "Recorded"
# dat$PEF_predict_recorded[(!is.na(dat$PEF_predict_value))] <- "Recorded"
# 
# dat$PEF_init_recorded <- factor(dat$PEF_init_recorded)
# dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded)
# dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded)

summary(dat$PEF_init_recorded)
summary(dat$PEF_prev_recorded)
summary(dat$PEF_predict_recorded)

nlc("This many people are missing an initial value for PEF but still have a date, so must be removed:")

dat %>% filter(!is.na(PEF_init_date) & is.na(PEF_init_value))  %>% nrow() %>% nlc()

# Removing the date issues also sorts out the time issues
# nlc("This many people are missing an initial value for PEF but still have a time, so must be removed:")
# 
# dat %>% filter(!is.na(PEF_init_time) & is.na(PEF_init_value))  %>% nrow() %>% nlc()


nlc("This is our new N for the dataset:")
dat <- dat %>% filter(!(!is.na(PEF_init_date) & is.na(PEF_init_value)))
# dat <- dat %>% filter(!(!is.na(PEF_init_time) & is.na(PEF_init_value)))
nrow(dat) %>% nlc()


dat %>% filter(!is.na(PEF_init_time) & is.na(PEF_init_time))  %>% nrow() 
dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(!is.na(PEF_init_time) & !is.na(PEF_init_time_recorded)) %>% nrow()
# 0 people have a time down in addition to saying 'time not recorded' (so this is good)

dat %>% filter(PEF_init_recorded == "Recorded") %>% select(PEF_init_time_recorded) %>% table(useNA = "ifany")
dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(is.na(PEF_init_time) & is.na(PEF_init_time_recorded)) %>% nrow()
#  select(contains("PEF")) # nrow()


dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_time_recorded) %>% 
  table(useNA = "ifany") # 


# No date checks this time


dat %>% filter(PEF_init_recorded == "Recorded") %>% select(PEF_init_DT) %>% summary()
dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_DT) %>% summary()

# Let's add in some things to make all this time/date not recorded stuff easier:

# Have to change this as PEF_init_date_recorded not available - have to create it instead

# dat$PEF_init_date_recorded <- as.character(dat$PEF_init_date_recorded)
# dat$PEF_init_date_recorded[!is.na(dat$PEF_init_date)] <- "Recorded"
# dat$PEF_init_date_recorded <- factor(dat$PEF_init_date_recorded, levels = c("Recorded", "Not recorded"))

dat$PEF_init_date_recorded <- NA
dat$PEF_init_date_recorded[!is.na(dat$PEF_init_value)] <- "Not recorded"
dat$PEF_init_date_recorded[!is.na(dat$PEF_init_date)] <- "Recorded"
dat$PEF_init_date_recorded <- factor(dat$PEF_init_date_recorded, levels = c("Recorded", "Not recorded"))

dat$PEF_init_time_recorded <- as.character(dat$PEF_init_time_recorded)
dat$PEF_init_time_recorded[!is.na(dat$PEF_init_time)] <- "Recorded"
dat$PEF_init_time_recorded <- factor(dat$PEF_init_time_recorded, levels = c("Recorded", "Not recorded"))

summary(dat$PEF_init_date_recorded)
summary(dat$PEF_init_time_recorded)

colnames(dat)

dat %>% select(contains("PEF")) %>% head(100)

# Now to make the % predicted

dat <- dat %>% mutate(PEF_percent_pred_value = round(ifelse(!is.na(PEF_prev_value), PEF_init_value/PEF_prev_value,
                                                PEF_init_value/PEF_predict_value)*100, 0))

dat <- dat %>% mutate(PEF_percent_pred_calculated = factor(ifelse(is.na(PEF_percent_pred_value), "Not calculated", "Calculated"))) 

summary(dat$PEF_percent_pred_calculated)



summary(dat$PEF_percent_pred_value)
summary(dat$PEF_percent_pred_calculated)


dat %>% filter(PEF_init_recorded == "Recorded" & 
                 (PEF_prev_recorded == "Recorded" | PEF_predict_recorded == "Recorded")) %>% nrow()

dat <- dat %>% mutate(PEF_prev_or_predict_recorded = factor(ifelse(PEF_prev_recorded == "Recorded" | 
                                                              PEF_predict_recorded == "Recorded", 
                                                            "Recorded", "Not recorded"), 
                                                            levels = c("Recorded", "Not recorded")))

dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_recorded, PEF_percent_pred_calculated) %>% table(useNA = "ifany")



dat %>% select(PEF_percent_pred_calculated, PEF_init_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_or_predict_recorded, PEF_init_recorded) %>% table(useNA = "ifany")

dat %>% select(PEF_prev_or_predict_recorded, PEF_percent_pred_calculated) %>% table(useNA = "ifany")

# Create a variable for only those where the initial PEF value was recorded

dat <- dat %>% mutate(PEF_prev_or_predict_recorded_only_PEF_init = PEF_prev_or_predict_recorded)
dat$PEF_prev_or_predict_recorded_only_PEF_init[dat$PEF_init_recorded != "Recorded"] <- NA             

dat %>% select(PEF_prev_or_predict_recorded_only_PEF_init, PEF_init_recorded) %>% table(useNA = "ifany")

# Split into <75% and => 75%

dat <- dat %>% mutate(PEF_percpred_75 = factor(ifelse(PEF_percent_pred_value < 75, "<75%",
                                       ifelse(PEF_percent_pred_value >= 75, ">= 75%", NA))))

# Seems to work
summary(dat$PEF_percpred_75)

# Matches the expected nummber

# Cleaning


dat %>% filter(is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_time)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% nrow()

dat %>% filter(is.na(steroids_admin_DT)) %>% filter(!is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% filter(!is.na(steroids_admin_time)) %>% nrow()


dat %>% select(steroids_admin) %>% table()
dat %>% filter(steroids_admin == "Yes") %>% filter(is.na(steroids_admin_DT)) %>% nrow()
dat %>% filter(!is.na(steroids_admin_date)) %>% select(steroids_admin) %>% table(useNA = "ifany")

dat %>% filter(!is.na(steroids_admin_DT)) %>% select(steroids_admin) %>% table(useNA = "ifany")
dat %>% filter(!is.na(steroids_admin_DT)) %>% filter(steroids_admin != "Yes") 
dat %>% filter(!is.na(steroids_admin_date)) %>% filter(steroids_admin != "Yes")  %>% nrow()
dat %>% filter(!is.na(steroids_admin_time)) %>% filter(steroids_admin != "Yes") %>% nrow()

dat %>% filter(is.na(steroids_admin_DT)) %>% filter(steroids_admin == "Yes") 


nlc("This many people have an steroids administration date but shouldn't have one according to their steroid status:")
dat %>% filter(!is.na(steroids_admin_date)) %>% filter(steroids_admin != "Yes") %>% nrow() %>% nlc()

nlc("This many people have time/date descrepancies for steroids so need to be removed:")
dat %>% filter((is.na(steroids_admin_DT) & steroids_admin == "Yes"))  %>% nrow()


nlc("Need to be removed, to leave this many people:")
dat <- dat %>% filter(!(is.na(steroids_admin_DT) & steroids_admin == "Yes"))


nrow(dat) %>% nlc()




# and then do the same for beta agonists

dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% filter(!is.na(b2a_admin_date)) %>% select(b2a_admin) %>% table(useNA = "ifany") 
dat %>% filter(!is.na(b2a_admin_DT)) %>% select(b2a_admin) %>% table(useNA = "ifany")
dat %>% filter(!is.na(b2a_admin_DT)) %>% filter(b2a_admin != "Yes") 
dat %>% filter(!is.na(b2a_admin_date)) %>% filter(b2a_admin != "Yes") 
dat %>% filter(!is.na(b2a_admin_time)) %>% filter(b2a_admin != "Yes") %>% nrow()


nlc("This many people have an b2a administration date but shouldn't have one according to their beta agonist status:")
dat %>% filter(!is.na(b2a_admin_date)) %>% filter(b2a_admin != "Yes") %>% nrow() %>% nlc()

nlc("Need to be removed, to leave this many people:")
dat <- dat %>% filter(!(!is.na(b2a_admin_date) & b2a_admin != "Yes"))
nrow(dat) %>% nlc()


dat %>% select(b2a_admin) %>% table()
dat %>% filter(!is.na(b2a_admin_date)) %>% select(b2a_admin) %>% table()


dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()




# and then do the same for RSR

dat %>% filter(is.na(RSR_date)) %>% nrow()
dat %>% filter(is.na(RSR_time)) %>% nrow()
dat %>% filter(is.na(RSR_DT)) %>% nrow()
dat %>% select(RSR) %>% table()
dat %>% filter(RSR == "Yes") %>% filter(is.na(RSR_DT)) %>% nrow()
dat %>% filter(!is.na(RSR_date)) %>% select(RSR) %>% table(useNA = "ifany") 
dat %>% filter(!is.na(RSR_DT)) %>% select(RSR) %>% table(useNA = "ifany")
dat %>% filter(!is.na(RSR_DT)) %>% filter(RSR != "Yes") 
dat %>% filter(!is.na(RSR_date)) %>% filter(RSR != "Yes") 
dat %>% filter(!is.na(RSR_time)) %>% filter(RSR != "Yes") %>% nrow()


nlc("This many people have an RSR date but shouldn't have one according to their RSR status:")
dat %>% filter(!is.na(RSR_date)) %>% filter(RSR != "Yes") %>% nrow() %>% nlc()

nlc("Need to be removed, to leave this many people:")
dat <- dat %>% filter(!(!is.na(RSR_date) & RSR != "Yes"))
nrow(dat) %>% nlc()



summary(dat)

nlc("Discharge bundle consistency check - How many patients marked as having no discharge bundle elements,
but actually did have at least 1?")

dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), any_vars(. == 1)) %>% 
  nrow() %>% nlc()

nlc("For reference, this many people had 'none' ticked and truly had none")
dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), all_vars(. == 0)) %>% 
  nrow() %>% nlc()

dat %>% select(discharge_bundle, life_status) %>% table(useNA = "ifany")





# I'm just going to filter these using the study IDs because I'm really stuck trying to invert what I've done using 
# filter and filter_at...

IDOI <- dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), any_vars(. == 1)) %>%
  select(study_ID)

dat <- dat %>% filter(!(study_ID %in% IDOI$study_ID))

nlc("After filtering them out, our new N is:")
nrow(dat) %>% nlc()

table(dat$life_status)



# Was anyone receive anything after being discharged? 

dat %>% filter(discharge_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - steroids_admin_DT < 0) %>% nrow()

dat %>% filter(discharge_DT - PEF_init_DT < 0) %>% select(hosp_code) %>% table() # Not a particular hospital that does this after discharge

nlc("This many people received beta agonists after discharge so need to be removed:")
dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()

dat <- dat %>% filter((discharge_DT - b2a_admin_DT >= 0 ) | is.na(b2a_admin_DT))

nlc("This leaves this many people:")
dat %>% nrow() %>% nlc()


nlc("This many people received their PEF after discharge so need to be removed:")
dat %>% filter(discharge_DT - PEF_init_DT < 0) %>% nrow() %>% nlc() 

dat <- dat %>% filter((discharge_DT - PEF_init_DT >= 0 ) | is.na(PEF_init_DT))

nlc("This leaves this many people:")
dat %>% nrow() %>% nlc()

# Filter out invalid heart rates (0-250) or resp rates (0-80) or oxygen sats (60-100). None present.

dat %>% filter(heart_rate < 0) %>% nrow()
dat %>% filter(heart_rate > 250) %>% nrow()

dat %>% filter(resp_rate < 0) %>% nrow()
dat %>% filter(resp_rate > 80) %>% nrow()

dat %>% filter(oxygen_sat_value < 60) %>% nrow()
dat %>% filter(oxygen_sat_value > 100) %>% nrow()


dat %>% filter(is.na(oxygen_sat_value)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value)) %>% nrow()
dat %>% select(oxygen_sat_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(oxygen_sat_value) & is.na(oxygen_sat_recorded)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value) & !is.na(oxygen_sat_recorded)) %>% nrow()

summary(dat$oxygen_sat_recorded)

str(dat$oxygen_sat_recorded)

dat <- dat %>% mutate(oxygen_sat_recorded = fct_explicit_na(oxygen_sat_recorded, "Recorded"))



# Do we have any duplicates?

nlc("assess whether there are duplicate records. Done based on:
country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date")

nlc("We have this many duplicated records:")

dat %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% duplicated() %>% 
  sum() %>% nlc()

nrow(dat)

dat %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% duplicated() %>% sum()
#  mutate(patient_ID = as.character(patient_ID)) %>% duplicated()  %>% arrange(patient_ID)

dat[duplicated(select(dat, country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)), ] %>% 
  mutate(patient_ID = as.character(patient_ID)) %>% arrange(patient_ID)

dat %>% group_by(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% filter(n()>1) %>% 
  ungroup() %>% as.data.frame() %>% nrow()


dat <- dat %>% arrange(patient_ID, arrival_DT)



dat <- dat[!duplicated(select(dat, country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)), ]


nlc("These are removed to leave this many records:")
nrow(dat) %>% nlc()

summary(dat$life_status)

dat %>% filter(life_status == "Died as inpatient") %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)

# Need to consider this carefully:
#
# nlc("Filter out those with heart rates or respiratory rates of 0, as these likely indicate missing values and suggest that the rest
# of the associated data is less accurate. This many people had a heart rate or resp rate of 0:")
# 
# dat %>% filter(heart_rate == 0 | resp_rate == 0) %>% nrow() %>% nlc()
# nrow(dat)
# dat %>% select(life_status) %>% table()
# 
# dat %>% filter((heart_rate == 0 | resp_rate == 0) & life_status == "Died as inpatient")  %>% glimpse()
# 
# 
# 
# dat <- dat %>% filter(heart_rate > 0 & resp_rate > 0)
# 
# nlc("Our new N is:")
# nrow(dat) %>% nlc()
# 
# dat %>% select(life_status) %>% table()


# Need to classify the oxygen saturation
# Don't need to do this

# head(dat$oxygen_sat_value)
# 
# dat <- dat %>% mutate(oxygen_sat92 = ifelse(oxygen_sat_value < 93, "<=92",
#                                             ifelse(oxygen_sat_value > 92, ">92", NA)))
# 
# 
# table(dat$oxygen_sat92, useNA = "ifany")
# table(dat$oxygen_sat_recorded)



# It's worked

# Also need to create a new variable for the hypoxic children

# table(dat$oxygen_supp, useNA = "ifany")
# 
# dat <- dat %>% mutate(oxygen_supp_hypoxic_only = oxygen_supp)
# dat$oxygen_supp_hypoxic_only[dat$oxygen_sat92 == ">92"] <- NA
# 
# dat %>% select(oxygen_sat92, oxygen_supp_hypoxic_only) %>% table(useNA = "ifany")

# More on steroids
colnames(dat)

# This is what we have to break it down into:

dat <- dat %>% mutate(steroids_1hour = factor(ifelse(arrival_to_steroids_hours >= 1, ">=1 hour",
                                                       ifelse(arrival_to_steroids_hours < 1, "<1 hour", NA))))
summary(dat$steroids_1hour)


dat <- dat %>% mutate(steroids_4hour = factor(ifelse(arrival_to_steroids_hours >= 4, ">=4 hours",
                                                     ifelse(arrival_to_steroids_hours < 4, "<4 hours", NA))))
summary(dat$steroids_4hour)


dat <- dat %>% mutate(PEF_init_1hour = factor(ifelse(arrival_to_PEF_init_hours >= 1, ">=1 hour",
                                                     ifelse(arrival_to_PEF_init_hours < 1, "<1 hour", NA))))
summary(dat$PEF_init_1hour)


dat <- dat %>% mutate(PEF_init_4hour = factor(ifelse(arrival_to_PEF_init_hours >= 4, ">=4 hours",
                                                     ifelse(arrival_to_PEF_init_hours < 4, "<4 hours", NA))))
summary(dat$PEF_init_4hour)


dat <- dat %>% mutate(b2a_1hour = factor(ifelse(arrival_to_b2a_minutes/60 >= 1, ">=1 hour",
                                                     ifelse(arrival_to_b2a_minutes/60 < 1, "<1 hour", NA))))
summary(dat$b2a_1hour)


dat <- dat %>% mutate(b2a_4hour = factor(ifelse(arrival_to_b2a_minutes/60 >= 4, ">=4 hours",
                                                     ifelse(arrival_to_b2a_minutes/60 < 4, "<4 hours", NA))))
summary(dat$b2a_4hour)
summary(dat$arrival_to_b2a_minutes)


dat <- dat %>% mutate(RSR_24hour = factor(ifelse(arrival_to_RSR_hours >=24, ">=24 hours",
                                                ifelse(arrival_to_RSR_hours < 24, "<24 hours", NA))))
summary(dat$RSR_24hour)


# The RSR needs to be further split up into weekdays and weekends
# easiest way to do this is to create a new variable for weekday admission


# # # # NOTE THIS HAS CHANGED NOW THAT DAYTIME/NIGHT TIME AUTOMATICALLY INCLUDED.

# also here, we split into daytime and night time

# discharge weekday/weekend daytime/night time.


colnames(dat)



 
dat <- dat %>% mutate(weekday_admission = factor(ifelse( (arrival_day_of_week == "Friday" & arrival_time_mins_from_midnight >= (17*60)) |
                                                  arrival_day_of_week == "Saturday" |
                                                  arrival_day_of_week == "Sunday" |
                                                  (arrival_day_of_week == "Monday" & arrival_time_mins_from_midnight < (8*60)),
                                                  "Weekend admission", 
                                                  "Weekday admission"),
                                                 levels = c("Weekday admission", "Weekend admission")))

#             "Weekend admission (17:00 Friday to 07:59 Monday)", "Weekday admission (08:00 Monday to 16:59 Friday)"),


dat$daynight_admission <- "Day time"
dat$daynight_admission[dat$arrival_time_mins_from_midnight < (8*60)] <- "Night time"
dat$daynight_admission[dat$arrival_time_mins_from_midnight >= (17*60)] <- "Night time"

dat$daynight_admission <- factor(dat$daynight_admission, levels = c("Day time", "Night time"))


dat$weekday_daynight_admission <- "Weekday day time"
dat$weekday_daynight_admission[dat$weekday_admission == "Weekday admission" & dat$daynight_admission == "Night time"] <- "Weekday night time"
dat$weekday_daynight_admission[dat$weekday_admission == "Weekend admission" & dat$daynight_admission == "Day time"] <- "Weekend day time"
dat$weekday_daynight_admission[dat$weekday_admission == "Weekend admission" & dat$daynight_admission == "Night time"] <- "Weekend night time"

dat$weekday_daynight_admission <- factor(dat$weekday_daynight_admission, levels = c("Weekday day time", "Weekday night time", 
                                                                                    "Weekend day time", "Weekend night time"))
summary(dat$weekday_daynight_admission)

summary(dat$daynight_admission)

table(dat$daynight_admission, dat$weekday_admission, dat$arrival_day_of_week)



# Looks good. Now we break down the RSR variable further
# 

dat$RSR_24hour_weekday <- dat$RSR_24hour
dat$RSR_24hour_weekday[dat$weekday_admission == "Weekend admission"] <- NA

dat$RSR_24hour_weekend <- dat$RSR_24hour
dat$RSR_24hour_weekend[dat$weekday_admission == "Weekday admission"] <- NA

dat %>% select(RSR_24hour, RSR_24hour_weekday) %>% table(useNA = "ifany")
dat %>% select(RSR_24hour, RSR_24hour_weekend) %>% table(useNA = "ifany")
dat %>% select(RSR_24hour_weekday, RSR_24hour_weekend) %>% table(useNA = "ifany")


# dat$steroids_1hour[dat$steroids_pre_arrival == "Yes"] <- NA
summary(dat$steroids_1hour)

# Sorted. For some reason, NA aren't counted as not being <1.

# summary(dat$steroids_pre_arrival)
# summary(dat$arrival_to_steroids_hours)
# summary(dat$arrival_to_steroids_hours_inc_pre_arrival_steroids)
# summary(dat$steroids_admin)
# 
# dat %>% select(steroids_pre_arrival, steroids_1hour, steroids_admin) %>% table(useNA = "ifany")

# And now we sort out the discharge bundle variables, with inappropriate tobacco smoking ones recoded to missing.
# We also have to remove those who transferred to another hospital.


# Make those who don't smoke missing for 'DB_smoke'.

summary(dat$smoke_status)

table(dat$smoke_status, dat$DB_smoke)


# Going to save a copy of this original variable (including those who are not recorded) and will split into smokers and vapers

dat <- dat %>% mutate(DB_smoke_NR_included = DB_smoke)


# Looks like smoking cessation advice is being given to vapers as well
# I'm going to split it into vapers and smokers. 
# those who vape and smoke are going in the smoking section.

dat$DB_vape_cess <- dat$DB_smoke
dat$DB_vape_cess[dat$smoke_status != "Current vaper" & dat$smoke_status != "Never smoked and current vaper" &
  dat$smoke_status != "Ex-smoker and current vaper"] <- NA

dat$DB_smoke[dat$smoke_status != "Current smoker" & dat$smoke_status != "Current smoker and current vaper"] <- NA
summary(dat$DB_vape_cess)
summary(dat$DB_smoke)

table(dat$smoke_status, dat$DB_vape_cess)
table(dat$smoke_status, dat$DB_smoke)

table(dat$smoke_status, dat$DB_smoke)


# dat %>% select(SH_smoke) %>% table(useNA = "ifany")
# dat %>% select(DB_parent_smoke, SH_smoke) %>% table()
# dat %>% select(smoke_status, DB_smoke) %>% table(useNA = "ifany")
# 
# # Does anyone under the age of 11 have smoking status as anything other than missing? No. 
# dat %>% filter(age < 11) %>% select(smoke_status) %>% table(useNA = "ifany")
# dat %>% filter(age < 11) %>% nrow()
# 
# dat %>% filter(age > 10) %>% select(smoke_status) %>% table(useNA = "ifany")
# dat %>% filter(age > 10) %>% nrow()
# 
# summary(dat$DB_smoke)


# For some reason, those with no discharge bundle because they died weren't coded as missing this time, so I've got to do that now.
# But need to keep the originals for the BPT stuff. So we replicate the DB variables.


# This bit was recoding for if we wanted some of those as 0 for calculating the BPT, but we don't

# dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")),  .funs = list(BPT123 = ~(.)))
# 
# # But when we replicate it, the name goes at the end but we want it at the beginning. Therefore we name it 'BPT123' just to identify it,
# # and then use 'rename_at' to paste BPT at the beginning and then remove the string at the end.
# 
# dat <- dat %>% rename_at(.vars = vars(ends_with("_BPT123")), list(~paste0("BPT_", .)))
# dat <- dat %>% rename_at(.vars = vars(starts_with("BPT_")), list(~str_replace(., "_BPT123", "")))
# 
# colnames(dat)
#

summary(dat$life_status)

summary(dat$discharge_bundle)

dat %>% select(starts_with("DB")) %>% summary()

summary(dat$hospital_transfer) # this is transferred in.



dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")),
                         .funs = ~ifelse(life_status == "Died as inpatient", NA, .))


colnames(dat)
summary(dat)



# And now we make all those who aren't current smokers missing for 'DB_smoke':

# dat$DB_smoke[dat$smoke_status != "Current smoker" | is.na(dat$smoke_status)] <- NA
# dat %>% select(smoke_status, DB_smoke) %>% table(useNA = "ifany")
# 
# # And we do the same for parents' smoking status
# 
# dat$DB_parent_smoke[dat$SH_smoke != "Yes"] <- NA
# dat %>% select(SH_smoke, DB_parent_smoke) %>% table(useNA = "ifany")
# 

# # Going to create a new variable for transferred and not transferred, and then recode transferred as missing
# # for the discharge bundle variable. Doing it right at the end because I know I use that variable previously.
# 
# # No missing so doing this is fine. But maybe need to watch out if I re-use this if someone dies.
# 
# dat <- dat %>% mutate(transferred = factor(ifelse(
#                                            discharge_bundle == "Patient transferred to another hospital", "Yes",
#                                     ifelse(discharge_bundle != "Patient transferred to another hospital", "No",
#                                                   NA))))
# 
# # And now we change discharge bundle so that the transferreds are missing
# 
# dat$discharge_bundle[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA
# dat$discharge_bundle <- factor(dat$discharge_bundle)
# 
# str(dat$transferred)
# 
# dat %>% select(transferred, discharge_bundle) %>% table(useNA = "ifany")
# 
# # I also need to create a variable for discharge bundle yes/no for the subanalysis section

summary(dat$discharge_bundle)



dat <- dat %>% mutate(discharge_bundle_yes_no = discharge_bundle)
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Self discharge"] <- "No"
dat$discharge_bundle_yes_no <- factor(dat$discharge_bundle_yes_no, levels = c("No", "Yes"))

summary(dat$discharge_bundle_yes_no)

colnames(dat)               
summary(dat)

dat %>% select(starts_with("DB")) %>% summary()

dat <- dat %>% mutate(gpcprep = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
                                         DB_adherence == 1 & DB_PAAP == 1 & 
                                         (DB_comm_FU_2_days == 1 | DB_spec_review_4_weeks == 1),
                                          1, 0))

# Need to now add the bit about smoking that is only relevant to smokers

dat <- dat %>% mutate(GPC = ifelse(gpcprep == 1 & (DB_smoke == 1 | is.na(DB_smoke)), 1, 0))

dat %>% select(GPC, DB_smoke) %>% table(useNA = "ifany")
dat %>% select(GPC, contains("DB")) %>% filter(GPC == 1) %>%  head(100)


# Okay and now I think we're ready to go!

# I'm also going to put the levels so that they're in the same order as the report.

# Nearly royally f***ed it up here! doing what I've hashed out below changes the NAMES of the underlying
# factor levels, but doesn't change the factor levels itself. Therefore doing this just makes all the values
# wrong!!!

# levels(dat$gender) <- c("Male", "Female", "Transgender", "Other", "Not recorded")
# levels(dat$smoke_status) <- c("Never smoked", "Ex-smoker", "Current smoker", "Ex-smoker and current vaper",
#                               "Never smoked and current vaper", "Not recorded")


# Instead, you need to use the 'factor' command again.

# dat$SH_smoke <- factor(dat$SH_smoke, levels = c("Yes", "No", "Not recorded"))
dat$oxygen_sat_recorded <- factor(dat$oxygen_sat_recorded, levels = c("Recorded", "Not recorded"))


summary(dat$oxygen_sat_measurement_type)

dat$oxygen_sat_measurement_type <- factor(dat$oxygen_sat_measurement_type, levels = c("Yes", "No - room air",
                                                                                      "Not recorded"))

summary(dat$PEF_init_recorded)

# dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded",
#                                                                   "Patient too unwell", "Not recorded"))

dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded",
                                                                  "No - patient unable to do PEF", "No - not done", "Not recorded"))



# dat$steroids_pre_arrival <- factor(dat$steroids_pre_arrival, levels = c("Yes", "Not recorded", "No"))
summary(dat$steroids_admin)

nrow(dat)

# dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered", "Not recorded"))
dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered")) 

summary(dat$b2a_admin)

# dat$b2a_pre_arrival <- factor(dat$b2a_pre_arrival, levels = c("Yes", "Not recorded", "No"))
dat$b2a_admin <- factor(dat$b2a_admin, levels = c("Yes", "Not administered"))

# dat$crit_care_total <- factor(dat$crit_care_total, levels = c("Yes - ICU", "Yes - HDU", 
#                                                               "Yes - HDU,Yes - ICU", "No"))
# dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Parental/carer/self-discharge"))
dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Self discharge", "Patient transferred to another hospital"))
summary(dat$discharge_bundle)

# dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis, 
#                                    levels = c("Yes", "No - not medically indicated", "No - reason not given",
#                                               "Offered but patient/parent/carer declined"))

dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis,
                                   levels = c("Yes", "No", "Not prescribed for medical reasons"))


colnames(dat)
table(dat$inhaled_steroids_dis)
table(dat$oral_steroids_dis, useNA = "ifany")

dat %>% filter(life_status == "Alive") %>% filter(is.na(DB_inhaler)) %>% nrow()

dat %>% filter(is.na(patient_ID)) %>% nrow()
# dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No - not medically indicated", 
#                                 "No - reason not given"))

dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No"))



dat %>% filter(life_status == "Died as inpatient") %>% select(oral_steroids_rescue_history) %>% table()
dat %>% filter(is.na(oral_steroids_rescue_history)) %>% summary()


# oral steroids rescue history should be missing if patient died or transferred

dat$oral_steroids_rescue_history[dat$life_status == "Died as inpatient" | 
                                   dat$discharge_bundle == "Patient transferred to another hospital"] <- NA

dat$oral_steroids_rescue_history <- factor(dat$oral_steroids_rescue_history, levels = c("Yes", "No", 
                                                                                        "Not recorded"))

summary(dat$oral_steroids_rescue_history)

# dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No - not medically indicated", 
#                                                               "Not recorded", 
#                               "Patient/parent/carer declined", "Already being seen in secondary care clinic"))

# no longer variable in dataset

# dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No", "Not recorded", 
#                                                               "Patient declined", "Already being seen in secondary care clinic"))



summary(dat$oxygen_prescribed)
summary(dat$oxygen_admin)

dat$oxygen_prescribed <- factor(dat$oxygen_prescribed, levels = c("Yes", "Yes - but date/time not recorded", "No"))
dat$oxygen_admin <- factor(dat$oxygen_admin, levels = c("Yes", "No"))

# Format of this changed

# dat$oxygen_prescribed <- fct_recode(dat$oxygen_prescribed, `Prescribed and administered` = "Prescribed,Administered",
#                                     `Prescribed and administered` = "Administered,Prescribed",
#                                     `Prescribed only` = "Prescribed", `Administered only` = "Administered")
# 
# dat$oxygen_prescribed <- factor(dat$oxygen_prescribed, levels = c("Prescribed only", "Administered only", 
#                                                                   "Prescribed and administered", "No"))
# 
# summary(dat$oxygen_prescribed)






# - - - - - - - - - - - - - -#
#  Defining asthma severity  #
# - - - - - - - - - - - - - -#

colnames(dat)
summary(dat$PEF_percent_pred_value)
summary(dat$PEF_predict_value)
summary(dat$PEF_prev_value)
summary(dat$PEF_init_value)
summary(dat$PEF_percent_pred_calculated)

dat %>% filter(!is.na(PEF_init_value)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% filter(!is.na(PEF_prev_value) | !is.na(PEF_predict_value)) %>% nrow()
dat %>% filter(!is.na(PEF_percent_pred_value)) %>% nrow()

# this is correct.

# Create a variable for peak flow severity:
# Bear in mind that some people were too unwell for peak flow to be measured - let's see how they
# compare with the other stats used to define severity. Only becomes something to worry about if
# their other stats are all normal.


# Don't need to worry about floating point rounding errors for this 
# But can't use cut because of the definitions the left and the right are both < and >. 

dat <- dat %>% mutate(PEF_percent_pred_sev = NA)



dat$PEF_percent_pred_sev[dat$PEF_percent_pred_value > 50] <- "Moderate"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred_value <= 50] <- "Severe"
dat$PEF_percent_pred_sev[is.na(dat$PEF_percent_pred_value)] <- "Unavailable"
dat$PEF_percent_pred_sev[dat$PEF_init_recorded == "No - patient unable to do PEF"] <- "Severe"
dat$PEF_percent_pred_sev <- factor(dat$PEF_percent_pred_sev)

summary(dat$PEF_percent_pred_sev)
summary(dat$PEF_init_recorded)

dat$resp_rate_sev <- cut(dat$resp_rate, breaks = c(-1, 9, 24, Inf),
                       labels = c("Low", "Normal", "High"))



table(dat$resp_rate_sev)
dat %>% filter(resp_rate_sev == "Low") %>% dplyr::select(resp_rate)
dat %>% filter(is.na(resp_rate_sev)) %>% nrow()
dat %>% filter(is.na(resp_rate)) %>% nrow()


dat$resp_rate[dat$resp_rate_sev == "Low"]
# table(dat$Peakflowonarrival)
# table(dat$Oxygensatrecorded)
# table(dat$Oxygenmeasurement)

dat$heart_rate_sev <- cut(dat$heart_rate, breaks = c(-1, 29, 109, Inf),
                        labels = c("Low", "Normal", "High"))

table(dat$heart_rate_sev, dat$resp_rate_sev, useNA = "ifany")
dat %>% filter(heart_rate_sev == "Low") %>% select(heart_rate, resp_rate)
dat %>% filter(is.na(resp_rate_sev)) %>% nrow()
dat %>% filter(is.na(resp_rate)) %>% nrow()



colnames(dat)

dat$oxygen_sat_sev <- as.character(cut(dat$oxygen_sat_value, breaks = c(-1, 92, 101), right = FALSE,
                    labels = c("Low", "Normal")))
dat$oxygen_sat_sev[is.na(dat$oxygen_sat_value)] <- "Unavailable"
dat$oxygen_sat_sev <- factor(dat$oxygen_sat_sev)
table(dat$oxygen_sat_sev, useNA = "ifany")


dat %>% filter(oxygen_sat_value == 92) %>% select(oxygen_sat_sev) %>% head()


table(dat$oxygen_sat_sev, dat$resp_rate_sev, useNA = "ifany")
table(dat$oxygen_sat_sev, dat$heart_rate_sev, useNA = "ifany")
table(dat$resp_rate_sev, dat$heart_rate_sev, useNA = "ifany")
table(dat$PEF_percent_pred_sev, useNA = "ifany")



# I'm just going to define asthma severity as one thing now, which doesn't account for whether a measurement it missing.
# This one only uses high resp/heart rates for severity.

dat <- dat %>% mutate(asthma_sev = factor(ifelse(PEF_percent_pred_sev == "Severe" |
                                                   resp_rate_sev == "High" |  resp_rate_sev == "Low" | 
                                                   heart_rate_sev == "High" |  heart_rate_sev == "Low" | 
                                              oxygen_sat_sev == "Low", "Severe and Life-threatening", "Moderate"),
                                          levels = c("Moderate", "Severe and Life-threatening")))


summary(dat$asthma_sev)


# There's a new section on BPT, so have to make that

dat$RSR_BPT <- fct_recode(dat$RSR_24hour, `>=24 hours or no RSR` = ">=24 hours") 
dat$RSR_BPT[dat$RSR == "No"] <- ">=24 hours or no RSR"
dat$RSR_BPT[dat$life_status == "Died as inpatient"] <- NA

dat %>% select(RSR_24hour, RSR_BPT) %>% table(useNA = "ifany")

str(dat$DB_adherence)



# !!!! sort this out
summary(dat)
# dat <- dat %>% mutate_at(vars(contains("DB")), BPT = recode())

# Had to do a reverse for the RSR 24 hour thing, because if it is classed as a factor it becomes 'NA' if missing instead of
# registering as 'No'.

########################################
# OLD BPT: 
########################################

dat <- dat %>% mutate(BPT = factor(ifelse(RSR_BPT == "<24 hours" & DB_inhaler == 1 &
                                               DB_maintenance == 1 & DB_PAAP == 1 & discharge_bundle == "Yes" &
                                               (DB_smoke == 1 | is.na(DB_smoke)), "Achieved", "Not achieved")),
                      BPT_optional = factor(ifelse(DB_triggers == 1 & DB_spec_review_4_weeks == 1,
                                                   "Achieved", "Not achieved")))
                      # BPT_all = factor(ifelse(BPT == "Achieved" & BPT_optional == "Achieved",
                      #                         "Achieved", "Not achieved")))


dat <- dat %>% mutate(DB_BPT = ifelse(DB_inhaler == 1 & DB_maintenance == 1 & DB_PAAP == 1 &
                                        discharge_bundle == "Yes" & (DB_smoke == 1 | is.na(DB_smoke)),
                                                     1, 0))


########################################
# NEW BPT
########################################
# dat <- dat %>% mutate(DB_BPT = ifelse(DB_inhaler == 1 & DB_maintenance == 1 & DB_PAAP == 1 &
#                                         discharge_bundle == "Yes" & (DB_smoke == 1 | is.na(DB_smoke)) & DB_spec_review_4_weeks == 1,
#                                                      1, 0))

# The BPT doesn't strictly exclude patients who transferred to another hospital, but there is no way for them to receive the DB so 
# they are excluded

dat$DB_BPT[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA
dat$BPT[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA
dat$BPT_optional[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA
# dat$BPT_all[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA
dat$RSR_BPT[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA







# include for audit post-2023

# dat$BPT <- "Not achieved"
# dat$BPT[dat$DB_BPT == 1 & dat$RSR_BPT == "<24 hours"] <- "Achieved"
# dat$BPT[is.na(dat$DB_BPT)] <- NA
# dat$BPT <- factor(dat$BPT, levels = c("Achieved", "Not achieved"))



# Need a new variable for <1 hour vs all for PEF, beta agonists, and steroids, and <4 hours for steroids

dat <- dat %>% mutate(PEF_init_1hour_all = ">=1 hour or not administered/recorded",
                      b2a_1hour_all = ">=1 hour or not administered/recorded",
                      steroids_1hour_all = ">=1 hour or not administered/recorded",
                      steroids_4hour_all = ">=4 hours or not administered/recorded")

dat$PEF_init_1hour_all[dat$PEF_init_1hour == "<1 hour"] <- "<1 hour"

# Exclude those who were too ill

dat$PEF_init_1hour_all[dat$PEF_init_recorded == "No - patient unable to do PEF"] <- NA

dat$b2a_1hour_all[dat$b2a_1hour == "<1 hour"] <- "<1 hour"
dat$steroids_1hour_all[dat$steroids_1hour == "<1 hour"] <- "<1 hour"

# remove those who had steroids in the previous 24 hours
dat$steroids_1hour[dat$steroids_24hr_prev == "Yes"] <- NA


dat$steroids_4hour_all[dat$steroids_4hour == "<4 hours"] <- "<4 hours"
dat$steroids_4hour[dat$steroids_24hr_prev == "Yes"] <- NA



dat$PEF_init_1hour_all <- factor(dat$PEF_init_1hour_all)
dat$b2a_1hour_all <- factor(dat$b2a_1hour_all)
dat$steroids_1hour_all <- factor(dat$steroids_1hour_all)
dat$steroids_4hour_all <- factor(dat$steroids_4hour_all)


# steroids 4 hour no longer required. However, 1 hour is - will remove those that received steroids before hospital admission from this variable.

summary(dat$steroids_24hr_prev)


# James says that those with b2a in the hour before don't need to be excluded as you can get b2a every 10 minutes
# dat$b2a_1hour[dat$b2a_1hr_prev == "Yes"] <- NA

summary(dat$b2a_1hour)

summary(dat$steroids_1hour)
summary(dat$steroids_4hour)
summary(dat$steroids_4hour_all)

dat %>% select(PEF_init_1hour, PEF_init_1hour_all) %>% table(useNA = "ifany")
dat %>% select(b2a_1hour, b2a_1hour_all) %>% table(useNA = "ifany")
dat %>% select(steroids_1hour, steroids_1hour_all) %>% table(useNA = "ifany")

# Need a new PEF variable where 'too unwell' are excluded

dat <- dat %>% mutate(PEF_init_recorded_unwell_excl = fct_recode(.$PEF_init_recorded, NULL = "No - patient unable to do PEF"))

summary(dat$PEF_init_recorded_unwell_excl)

dat %>% select(PEF_init_recorded, PEF_init_recorded_unwell_excl) %>% table(useNA = "ifany")



# # Need a new variable for hospital follow up or specialist review in just those prescribed over 2 courses of oral steroids
# 
# dat$referred_for_FU_with_2_oral_hist <- dat$referred_for_FU
# dat$referred_for_FU_with_2_oral_hist[dat$oral_steroids_rescue_history != "Yes"] <- NA
# summary(dat$referred_for_FU_with_2_oral_hist)
# dat %>% select(oral_steroids_rescue_history, referred_for_FU_with_2_oral_hist) %>% table(useNA = "ifany")
# 
# summary(dat$DB_comm_FU_2_days)
# summary(dat$DB_spec_review_4_weeks)
# summary(dat$referred_for_FU)
# 
# dat %>% select(DB_comm_FU_2_days, referred_for_FU) %>% table()

summary(dat$steroids_4hour)
summary(dat$steroids_1hour)
table(dat$steroids_1hour_all, useNA = "ifany")
summary(dat$steroids_1hour_all)

dat <- dat %>% mutate(agecat70 = cut(age, breaks = c(-1, 69.5, Inf), labels = c("<70", ">=70")))
dat %>% select(age, agecat70) %>% table(useNA = "ifany")

dat <- dat %>% mutate(agecat70_died = as.character(agecat70))
dat$agecat70_died[dat$life_status == "Alive"] <- NA


dat <- dat %>% mutate(arrival_to_RSR_hours_died = arrival_to_RSR_hours)
dat$arrival_to_RSR_hours_died[dat$life_status == "Alive"] <- NA

dat %>% filter(life_status != "Alive") %>% select(arrival_to_RSR_hours_died)


# Remove the 'day of discharge' of anyone who died.


dat$discharge_day_of_week[dat$life_status != "Alive"] <- NA
summary(dat$discharge_day_of_week)



# Extra variables:

summary(dat$anyIMD)


# length of time until oxygen administration


dat <- dat %>% mutate(arrival_to_oxygen_minutes = difftime(oxygen_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_oxygen_minutes)
dat$arrival_to_oxygen_minutes <- as.integer(dat$arrival_to_oxygen_minutes)

summary(dat$arrival_to_oxygen_minutes)
summary(dat$oxygen_DT)
summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)

dat %>% select(arrival_to_oxygen_minutes) %>% arrange(arrival_to_oxygen_minutes) %>% head(20)

# Make sure that this is has happened:

dat$steroids_1hour[dat$steroids_24hr_prev == "Yes"] <- NA
dat$steroids_4hour[dat$steroids_24hr_prev == "Yes"] <- NA

dat$steroids_1hour_all[dat$steroids_24hr_prev == "Yes"] <- NA
dat$steroids_4hour_all[dat$steroids_24hr_prev == "Yes"] <- NA

# We now sort out the BPT variables that weren't previously sorted out, and the GPC variables.

# Follow-up variable:

dat$DB_FU_any <- dat$DB_comm_FU_2_days
dat$DB_FU_any[dat$DB_spec_review_4_weeks == 1] <- 1

table(dat$DB_FU_any, useNA = "ifany")


# And, we create James' extra variable

summary(dat$arrival_to_oxygen_minutes)
summary(dat$oxygen_prescribed)

summary(dat$oxygen_admin)

dat %>% filter(!is.na(arrival_to_oxygen_minutes)) %>% select(oxygen_prescribed) %>% summary()
# dat %>% filter(!is.na(arrival_to_oxygen_minutes)) %>% select(oxygen_admin) %>% summary()

# According to James, no excuse for no prescription of oxygen.
# So...

dat$oxygen_presc_1hr <- ">= 1 hour or not prescribed or no available time"
dat$oxygen_presc_1hr[dat$arrival_to_oxygen_minutes < 60] <- "<1 hour"
dat$oxygen_presc_1hr <- factor(dat$oxygen_presc_1hr)

summary(dat$oxygen_presc_1hr)

summary(dat$arrival_to_oxygen_minutes)

dat$PEF_b2a_steroids_oxygen_1hr_combo <- "No"
dat$PEF_b2a_steroids_oxygen_1hr_combo[(dat$PEF_init_1hour_all == "<1 hour" |   # PEF
                                        dat$PEF_init_recorded ==  "No - patient unable to do PEF") &
                                        dat$b2a_1hour_all == "<1 hour" & #b2a
                                        (dat$steroids_1hour_all == "<1 hour" | # steroids 
                                           dat$steroids_24hr_prev == "Yes") &
                                        dat$oxygen_presc_1hr == "<1 hour"] <- "All <1 hour"
                                        


dat$PEF_b2a_steroids_oxygen_1hr_combo <- factor(dat$PEF_b2a_steroids_oxygen_1hr_combo)

summary(dat$PEF_b2a_steroids_oxygen_1hr_combo)

dat %>% select(PEF_init_1hour, PEF_init_1hour_all) %>% table(useNA = "ifany")
dat %>% select(PEF_init_1hour, PEF_init_recorded) %>% table(useNA = "ifany")

dat %>% select(PEF_init_1hour_all, PEF_init_recorded) %>% table(useNA = "ifany")


summary(dat$asthma_sev)

summary(dat$DB_BPT)



saveRDS(dat, "C:/Users/aadamson/Documents/Audit_2023_onwards/2022-2023/AA/Data/tidyData/AA_SCC_2022-23_clean_data.RDS")

sink()

