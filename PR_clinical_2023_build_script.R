#-----------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b u i l d   s c r i p t   #

#-----------------------------------------------------------------------------#




library(dplyr)
# library(readstata13)
# library(xlsx)
source("C:/Users/aadamson/Documents/R/My R functions/MySummary.R")
source("C:/Users/aadamson/Documents/R/My R functions/lintestOR.R")
source("C:/Users/aadamson/Documents/R/My R functions/tidyoutput.R")
source("C:/Users/aadamson/Documents/R/My R functions/checkSame.R")


# library(janitor)
# library(officer)
# library(flextable)
library(tidyverse)
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

tablex <- function(x, y, z) { x %>% select(!!y, !!z) %>% table(useNA = "ifany") }

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
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
  
  #   if(nrow(gen) == 0) {return(var_N)}
  
  #  else {
  
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
  
  # }
}







medTable <- function(x, varname, roundno = 0) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  
  # NOTE!!! Medians all rounded to 0dp
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- sprintf(paste0("%.", roundno, "f"), 
                     round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, eng, scot, wal, all), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
  
  ret <- as.data.frame(ret)
  
  return(ret)
}

# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
 # print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
#  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
#  print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
#  print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
#  print(gen.A2)
  
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

WTmed <- function(x, variable, roundno = 0) {
  print(medTable(x, variable, roundno))
  write.table(medTable(x, variable, roundno), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}




WTfreq <- function(x, variable) {
  print(myFreqTable(x, variable))
  write.table(myFreqTable(x, variable), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}






nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}


dat_old <- read.csv("C:/Alex Harley/PR clinical 2021-22/Data/rawData/NACAP-PR-202103-202202-v106-Imperial.csv",
                    header = TRUE, 
                    stringsAsFactors = TRUE, na.strings = c("NA", ""))


dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Data/rawData/PR004-Bristol/PR004-Bristol.csv", header = TRUE, 
                    stringsAsFactors = TRUE, na.strings = c("NA", ""))


colnames(dat_old)
colnames(dat)

checkSame(dat, dat_old) %>% arrange(variable)

#                              New data        Old data
#                     IMD      1   integer     NA      <NA>       0
#                  LSOA11      1    factor     NA      <NA>       0
#         NHS.Number.Valid     NA      <NA>      1   integer       0
# ONSPD_AUG_2021_UK.lsoa11     NA      <NA>      1    factor       0
#             Org.Service      1    factor     NA      <NA>       0
#              ORG.Service     NA      <NA>      1    factor       0
#              Trust.Code      1    factor     NA      <NA>       0

table(dat$LSOA11)
table(dat_old$ONSPD_AUG_2021_UK.lsoa11)
colnames(dat)

table(dat$Org.Service)
table(dat$Trust.Code)
table(dat_old$ORG.Service)

table(dat_old$Trust.Now)
table(dat_old$Trust)

colnames(dat)
colnames(dat_old)

# trust code = Tcode

# LSOA11 related to ONSPD_AUG_2021_UK.lsoa11


# First thing's first - rename column names
# R style guide says variables shouldn't be capitalised.
# NR = not recorded
# init = initial = at assessment
# dis = discharge

# Note - some of the column names have changed since the interim report.


head(dat)

dat <- dat %>% select(study_ID = RowID,
                      patient_ID = PatientID,
                      country = Country,
                      region = Region,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      org_name = OrgName,
                      org_code = Org.Service,
                   #   nhs_number_valid = NHS.Number.Valid,
                      LSOA = LSOA11,
                      age = X.AgeAtAssessment,
                      gender = X1.3.Gender,
                      ethnicity = X1.5.Ethnicity,
                      ref_date = X2.1.Referral.Date,
                      ref_date_NR = X2.1.1.Not.recorded,
                      ref_location = X2.2.Referred.From,
                      assess_date = X2.3.Initial.PR.Assessment.Appointment,
                    #  smoke_status = X3.1.Smoking,
                      FEV1_percpred = X3.1.FEV1...predicted.,
                      FEV1_NR = X3.1x.Not.recorded,
                      FEV1FVC = X3.2.FEV1.FVC.Ratio,
                      FEV1FVC_NR = X3.2x.Not.recorded,
                    #  BMI = X3.4.Patient.s.body.mass.index..BMI.,
                    #  BMI_NR = X3.4.1.Not.recorded,
                      MRC_score_init = X3.3.MRC.Score,
                    #  CVD_history_orig = X3.6.Cardiovascular.Disease,
                    #  musc_skel_history_orig = X3.7.Lower.Limb.Musculoskeletal.Disorder,
                    #  mental_history_orig = X3.8.Mental.Health,
                    #  mental_history_combined_illness = X3.8a.Mental.Health,
                    #  anxiety_bin = X3.8a.Mental.Health...Anxiety,
                    #  depression_bin = X3.8a.Mental.Health...Depression,
                    #  SMI_bin = X3.8a.Mental.Health...Severe.mental.illness,
                      test_type_init = X4.1.Initial.Assessment.Tests,
                      test_value_init = X4.1a.Value.in.metres,
                      prac_test_init = X4.1b.Practice.test.at.initial.assessment,
                      ESWT_at_init = X4.2.ESWT.at.initial.assessment,
                      ESWT_value_init = X4.2a.Value.in.seconds,
                      CRQ_init = X4.3.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_init = X4.3a.Dyspnoea.score,
                      CRQ_fatigue_init = X4.3b.Fatigue.score,
                      CRQ_emotion_init = X4.3c.Emotion.score,
                      CRQ_mastery_init = X4.3d.Mastery.score,
                      CAT_init = X4.4.COPD.Assessment.Test..CAT.,
                      CAT_score_init = X4.4a.Total.CAT.score,
                      enrolled = X5.1.Enrolled.onto.PR.programme.,
                      start_date = X5.1a.Start.Date,
                      centre_based_V1 = X5.2.centre.based.PR.programme..V1.,
                    # Start of V1s
                    #  prog_type = X5.2a.Programme.Type,
                      scheduled_sess_centre_no_V1 = X5.2b.Centre.based.PR.Sessions.Scheduled..V1.,
                      rec_sess_centre_group_no_V1 = X5.2c.a..Group.centre.based.sessions.received..V1.,
                      rec_sess_centre_indiv_no_V1 = X5.2c.b..1.1.centre.based.sessions.received..V1.,
                      home_based_V1 = X5.3.Home.based.PR.programme..V1.,
                      scheduled_sess_home_no_V1 = X5.3a.Home.based.PR.Sessions.Scheduled..V1.,
                      rec_sess_home_in_person_no_V1 = X5.3b.a..Home.based.PR.Sessions...In.Person...V1.,
                      rec_sess_home_video_group_no_V1 = X5.3b.b..Home.based.PR.Sessions...Video.conferencing...group..V1.,
                      rec_sess_home_video_indiv_no_V1 = X5.3b.c..Home.based.PR.Sessions...Video.conferencing...1.1...V1.,
                      rec_sess_home_phone_no_V1 = X5.3b.d..Home.based.PR.Sessions...Phone.calls..V1.,
                      rec_sess_home_other_no_V1 = X5.3b.e..Home.based.PR.Sessions...Other.Digital.Communications..V1.,

                    # Start of V2s

                    centre_based_V2 = X5.2.Supervised.PR.programme...Centre,
                    home_based_V2 = X5.2.Supervised.PR.programme...Home,
                    home_based_in_person_V2 = X5.2a.Home.based.delivery.method...Person,
                    home_based_video_V2 = X5.2a.Home.based.delivery.method...Video,
                    home_based_phone_V2 = X5.2a.Home.based.delivery.method...Phone,
                    home_based_other_V2 = X5.2a.Home.based.delivery.method...Digital,
                    scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED., # I've checked it - it's legit
                    rec_sess_group_V2 = X5.4a.Supervised.PR.group.sessions.received..V2.,
                    rec_sess_indiv_V2 = X5.4b.Supervised.PR.face.to.face.sessions.received..V2., #  this is indiv and not F2F!!!

                      discharge_assess = X6.1.Discharge.assessment.performed.,
                      discharge_date = X6.1a.Discharge.assessment.date,
                      exercise_plan = X6.1b.Discharge.exercise.plan.provided,
                      MRC_score_dis = X7.1.MRC.Score.At.Discharge,
                      test_type_dis = X7.2.Walking.test.at.discharge.assessment.,
                      test_value_dis = X7.2a.Value.in.metres,
                      ESWT_at_dis = X7.3.Did.you.also.record.the.ESWT.at.discharge.,
                      ESWT_value_dis = X7.3a.Value.in.seconds,
                      CRQ_dis = X7.4.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_dis = X7.4a.Dyspnoea.score,
                      CRQ_fatigue_dis = X7.4b.Fatigue.score,
                      CRQ_emotion_dis = X7.4c.Emotion.score,
                      CRQ_mastery_dis = X7.4d.Mastery.score,
                      CAT_dis = X7.5.COPD.Assessment.Test..CAT.,
                      CAT_score_dis = X7.5a.Total.CAT.score,
                      dataset = Dataset)

summary(dat$X5.2.Supervised.PR.programme..V2.)
summary(dat$X5.2.Supervised.PR.programme..MERGED.)
table(dat$home_based_V2)

dat %>% select(contains("V2")) %>% summary()

# old column names

# dat <- dat %>% select(study_ID = RowID,
#                       patient_ID = PatientID,
#                       country = Country, 
#                       region = Region,
#                       trust_code = Tcode.Now,
#                       trust_name = Trust.Now,
#                       org_name = OrgName,
#                       org_code = ORG.Service,     
#                       nhs_number_valid = NHS.Number.Valid,
#                       LSOA = ONSPD_AUG_2021_UK.lsoa11,
#                       age = X.AgeAtAssessment,
#                       gender = X1.3.Gender,
#                       ethnicity = X1.5.Ethnicity,
#                       ref_date = X2.1.Referral.Date,
#                       ref_date_NR = X2.1.1.Not.recorded,
#                       ref_location = X2.2.Referred.From,
#                       assess_date = X2.3.Initial.PR.Assessment.Appointment,
#                     #  smoke_status = X3.1.Smoking, 
#                       FEV1_percpred = X3.1.FEV1...predicted.,
#                       FEV1_NR = X3.1x.Not.recorded,
#                       FEV1FVC = X3.2.FEV1.FVC.Ratio,
#                       FEV1FVC_NR = X3.2x.Not.recorded,
#                     #  BMI = X3.4.Patient.s.body.mass.index..BMI.,
#                     #  BMI_NR = X3.4.1.Not.recorded,
#                       MRC_score_init = X3.3.MRC.Score,
#                     #  CVD_history_orig = X3.6.Cardiovascular.Disease,
#                     #  musc_skel_history_orig = X3.7.Lower.Limb.Musculoskeletal.Disorder,
#                     #  mental_history_orig = X3.8.Mental.Health,
#                     #  mental_history_combined_illness = X3.8a.Mental.Health,
#                     #  anxiety_bin = X3.8a.Mental.Health...Anxiety,
#                     #  depression_bin = X3.8a.Mental.Health...Depression,
#                     #  SMI_bin = X3.8a.Mental.Health...Severe.mental.illness,
#                       test_type_init = X4.1.Initial.Assessment.Tests,
#                       test_value_init = X4.1a.Value.in.metres,
#                       prac_test_init = X4.1b.Practice.test.at.initial.assessment,
#                       ESWT_at_init = X4.2.ESWT.at.initial.assessment,
#                       ESWT_value_init = X4.2a.Value.in.seconds,
#                       CRQ_init = X4.3.Chronic.Respiratory.Questionnaire..CRQ.,
#                       CRQ_dyspnoea_init = X4.3a.Dyspnoea.score,
#                       CRQ_fatigue_init = X4.3b.Fatigue.score,                                                                  
#                       CRQ_emotion_init = X4.3c.Emotion.score,                                                                 
#                       CRQ_mastery_init = X4.3d.Mastery.score,
#                       CAT_init = X4.4.COPD.Assessment.Test..CAT.,
#                       CAT_score_init = X4.4a.Total.CAT.score,
#                       enrolled = X5.1.Enrolled.onto.PR.programme.,
#                       start_date = X5.1a.Start.Date,
#                       centre_based_V1 = X5.2.centre.based.PR.programme..V1.,
#                     # Start of V1s
#                     #  prog_type = X5.2a.Programme.Type,
#                       scheduled_sess_centre_no_V1 = X5.2b.Centre.based.PR.Sessions.Scheduled..V1.,
#                       rec_sess_centre_group_no_V1 = X5.2c.a..Group.centre.based.sessions.received..V1.,
#                       rec_sess_centre_indiv_no_V1 = X5.2c.b..1.1.centre.based.sessions.received..V1.,
#                       home_based_V1 = X5.3.Home.based.PR.programme..V1.,
#                       scheduled_sess_home_no_V1 = X5.3a.Home.based.PR.Sessions.Scheduled..V1.,
#                       rec_sess_home_in_person_no_V1 = X5.3b.a..Home.based.PR.Sessions...In.Person...V1.,
#                       rec_sess_home_video_group_no_V1 = X5.3b.b..Home.based.PR.Sessions...Video.conferencing...group..V1.,
#                       rec_sess_home_video_indiv_no_V1 = X5.3b.c..Home.based.PR.Sessions...Video.conferencing...1.1...V1.,
#                       rec_sess_home_phone_no_V1 = X5.3b.d..Home.based.PR.Sessions...Phone.calls..V1.,
#                       rec_sess_home_other_no_V1 = X5.3b.e..Home.based.PR.Sessions...Other.Digital.Communications..V1.,
#                     
#                     # Start of V2s
#                     
#                     centre_based_V2 = X5.2.Supervised.PR.programme...Centre,
#                     home_based_V2 = X5.2.Supervised.PR.programme...Home,
#                     home_based_in_person_V2 = X5.2a.Home.based.delivery.method...Person,
#                     home_based_video_V2 = X5.2a.Home.based.delivery.method...Video,
#                     home_based_phone_V2 = X5.2a.Home.based.delivery.method...Phone,
#                     home_based_other_V2 = X5.2a.Home.based.delivery.method...Digital,
#                     scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED., # I've checked it - it's legit
#                     rec_sess_group_V2 = X5.4a.Supervised.PR.group.sessions.received..V2.,
#                     rec_sess_indiv_V2 = X5.4b.Supervised.PR.face.to.face.sessions.received..V2., # check this is indiv and not F2F!!!
#                     
#                       discharge_assess = X6.1.Discharge.assessment.performed.,
#                       discharge_date = X6.1a.Discharge.assessment.date,
#                       exercise_plan = X6.1b.Discharge.exercise.plan.provided,
#                       MRC_score_dis = X7.1.MRC.Score.At.Discharge,
#                       test_type_dis = X7.2.Walking.test.at.discharge.assessment.,
#                       test_value_dis = X7.2a.Value.in.metres,
#                       ESWT_at_dis = X7.3.Did.you.also.record.the.ESWT.at.discharge.,
#                       ESWT_value_dis = X7.3a.Value.in.seconds,
#                       CRQ_dis = X7.4.Chronic.Respiratory.Questionnaire..CRQ.,
#                       CRQ_dyspnoea_dis = X7.4a.Dyspnoea.score,
#                       CRQ_fatigue_dis = X7.4b.Fatigue.score,
#                       CRQ_emotion_dis = X7.4c.Emotion.score,
#                       CRQ_mastery_dis = X7.4d.Mastery.score,
#                       CAT_dis = X7.5.COPD.Assessment.Test..CAT.,
#                       CAT_score_dis = X7.5a.Total.CAT.score,
#                       dataset = Dataset) 


# all variables are now from dataset version 2 so no longer need to combine stuff. 

# create the combined variables from V1 and V2

# first, recode the dataset variable.

# dat %>% filter(dataset == "V1A") %>% summary() 
# dat %>% filter(dataset == "V1") %>% summary() 



V2_cols <- dat %>% select(ends_with("_V2")) %>% colnames()


# Make sure that those who aren't enrolled are missing for the values below:

dat$centre_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_V2[dat$enrolled != "Yes"] <- NA

table(dat$enrolled)
table(dat$centre_based_V2)


dat$centre_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_video_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_other_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_phone_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_in_person_V2[dat$enrolled != "Yes"] <- NA
dat$scheduled_sess_V2[dat$enrolled != "Yes"] <- NA
dat$rec_sess_group_V2[dat$enrolled != "Yes"] <- NA
dat$rec_sess_indiv_V2[dat$enrolled != "Yes"] <- NA



dat$home_based_video_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_other_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_phone_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_in_person_V2[dat$home_based_V2 == "0"] <- NA


table(dat$home_based_V2, dat$home_based_other_V2, useNA = "ifany")

dat %>% select(home_based_V2, home_based_other_V2) %>% table(useNA = "ifany")

dat %>% filter(enrolled == "Yes" & is.na(rec_sess_group_V2)) %>% nrow()

dat %>% filter(enrolled == "Yes" & is.na(rec_sess_group_V2)) %>% nrow()

#///////////////////////////////////////////////////////////////////////////////////////#
# Some cleaning: these 5 patients were missing for received sessions instead of '0'
# other strange values for other variables as well.
# therefore, these patients are removed.
#///////////////////////////////////////////////////////////////////////////////////////#

dat <- dat %>% filter(!(enrolled == "Yes" & is.na(rec_sess_group_V2)))


table(dat$X5.2.Supervised.PR.programme..V2.)
summary(dat)

# for scheduled sessions, Tim's merge is fine.
# scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED. 






# I think we have sorted it out.

# So, we remove all the variables we no longer need, and rename the V2 variables to remove the V references.

dat %>% select(ends_with("V2")) %>% colnames()

dat <- dat %>% rename(centre_based = centre_based_V2,
                      home_based = home_based_V2,
                      scheduled_sess = scheduled_sess_V2,
                      rec_sess_group = rec_sess_group_V2,
                      rec_sess_indiv = rec_sess_indiv_V2,
                      home_based_in_person = home_based_in_person_V2,
                      home_based_video = home_based_video_V2,
                      home_based_phone = home_based_phone_V2,
                      home_based_other = home_based_other_V2)

# Remove those still ending in V2, V1, or merge, or temp

dat <- dat %>% select(-ends_with("_V2"), -ends_with("_V1"), -ends_with("MERGED"), -ends_with("temp"))

# Also, we don't need the dataset column

dat <- dat %>% select(-dataset)


# And make sure that anyone who isn't enrolled is missing for these variables:

summary(dat)





dat %>% filter(enrolled != "Yes") %>% summary()
dat %>% summary()

# Phew!

# mainly fine, but have to sort out the question 5 stuff.

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))
                      

# It's strange but these do all have different decimal point levels
head(dat$CAT_score_init, 30) # 0 dp
head(dat$CRQ_fatigue_init, 30) # 2 dp
head(dat$CRQ_dyspnoea_init, 30) # 1 dp
head(dat$CRQ_emotion_init, 30) # 2dp
head(dat$CRQ_mastery_init, 30) # 2dp




# Read in the IMD quintiles
# let's use the joint quintiles seeing as we no longer have scotland

IMDeng_wal <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/IMD/2019_England_and_Wales_Income_Employment_IMD_clean.csv",
                       header = TRUE, stringsAsFactors = FALSE)

summary(IMDeng_wal$new_IMD_quintile)

IMDeng_wal <- IMDeng_wal %>% select(LSOA = LSOA_code_2011, IMD_quintile = new_IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng_wal, by = "LSOA")
summary(dat$IMD_quintile)

dat$IMD_quintile <- as.character(dat$IMD_quintile)
dat$IMD_quintile[is.na(dat$IMD_quintile)] <- "Missing IMD quintile"
dat$IMD_quintile <- factor(dat$IMD_quintile, levels = c("1", "2", "3", "4", "5", "Missing IMD quintile"))



# dat <- dat %>% mutate(anyIMD = factor(ifelse(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Wal) &
#                                                is.na(IMD_quintile_Scot), "No IMD", "IMD present")))

dat <- dat %>% mutate(anyIMD = factor(ifelse(IMD_quintile == "Missing IMD quintile", "No IMD", "IMD present")))


dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)
summary(dat$IMD_quintile)
summary(dat$anyIMD)

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








# relevel gender

dat$gender <- relevel(dat$gender, ref = "Male")


head(dat)

# dat %>% filter(assess_date < "2019-06-01") %>% filter(home_based == "Yes") %>% select(centre_based:discharge_assess)

# # Next thing is to remove the brackets form the study and patient IDs - very unhelpful. 
# 
# dat <- dat %>% mutate(study_ID = str_remove_all(study_ID, "[()]"))
# dat <- dat %>% mutate(patient_ID = str_remove_all(patient_ID, "[()]"))
# 
# # Okay that was easier than I thought. I think the square bracket means 'remove all characters within me'.

#Need to convert dates to dates

dat <- dat %>% mutate(ref_date = as.Date(ref_date, format = "%d/%m/%Y"), 
                      assess_date = as.Date(assess_date, format = "%d/%m/%Y"),
                      start_date = as.Date(start_date, format = "%d/%m/%Y"),
                      discharge_date = as.Date(discharge_date, format = "%d/%m/%Y"))


# Create the 'date to date' variables

dat <- dat %>% mutate(ref_to_start_days = as.numeric(start_date - ref_date),
                      assess_to_start_days = as.numeric(start_date - assess_date),
                      assess_to_discharge_days = as.numeric(discharge_date - assess_date))

# ref to start days needs to be for stable COPD

table(dat$ref_location)

dat <- dat %>% mutate(ref_to_start_days_stable_COPD = ifelse(
    ref_location == "Primary/Community - stable COPD" |
      ref_location == "Secondary Care - stable COPD", 
    ref_to_start_days, NA))

summary(dat$ref_to_start_days_stable_COPD)


# assess to start days probably also needs to be for stable COPD

dat <- dat %>% mutate(assess_to_start_days_stable_COPD = ifelse(
  ref_location == "Primary/Community - stable COPD" |
    ref_location == "Secondary Care - stable COPD", 
  assess_to_start_days, NA))

summary(dat$assess_to_start_days_stable_COPD)


# Make a variable that says whether or not referal date was recorded
                      
dat <- dat %>% mutate(ref_date_rec = as.character(ref_date_NR))
dat$ref_date_rec[is.na(dat$ref_date_rec)] <- "Known"
dat$ref_date_rec <- factor(dat$ref_date_rec)
summary(dat$ref_date_rec)

# We created a more useful variable so we can drop the other one:

dat$ref_date_NR <- NULL



# Make a variable that says whether or not referal date was recorded



dat <- dat %>% mutate(FEV1_percpred_rec = as.character(FEV1_NR),
                      FEV1FVC_rec = as.character(FEV1FVC_NR))

dat$FEV1_percpred_rec[is.na(dat$FEV1_percpred_rec)] <- "Recorded"
dat$FEV1FVC_rec[is.na(dat$FEV1FVC_rec)] <- "Recorded"

dat <- dat %>% mutate(FEV1_percpred_rec = factor(FEV1_percpred_rec),
                      FEV1FVC_rec = factor(FEV1FVC_rec))


dat$FEV1_NR <- NULL
dat$FEV1FVC_NR <- NULL

summary(dat$FEV1_percpred_rec)
summary(dat$FEV1FVC_rec)


# Start date within 90 days for stable COPD

dat <- dat %>% mutate(ninety_day_referral_to_start_for_stable_COPD = ifelse(
                      ref_to_start_days < 90, "<90 days", ">=90 days")) %>%
                        mutate(ninety_day_referral_to_start_for_stable_COPD = factor(ifelse(
                          ref_location == "Primary/Community - stable COPD" |
                            ref_location == "Secondary Care - stable COPD", 
                          ninety_day_referral_to_start_for_stable_COPD, NA)))



dat <- dat %>% mutate(thirty_day_referral_to_start_for_AECOPD = ifelse(
  ref_to_start_days < 30, "<30 days", ">=30 days")) %>%
  mutate(thirty_day_referral_to_start_for_AECOPD = as.factor(ifelse(
    ref_location == "Primary/Community - post treatment for AECOPD" |
      ref_location == "Secondary Care - post admission for AECOPD", 
    thirty_day_referral_to_start_for_AECOPD, NA))) # %>% 

summary(dat$ninety_day_referral_to_start_for_stable_COPD)
summary(dat$thirty_day_referral_to_start_for_AECOPD)
summary(dat$ref_location)


# Sort out tests

# Initial tests

dat %>% select(test_type_init, ESWT_at_init) %>% table(useNA = "ifany")

colnames(dat)

dat <- dat %>% mutate(all_3_test_types_init = ifelse(test_type_init == "6MWT" & ESWT_at_init == "No", "6MWT only",
                                              ifelse(test_type_init == "ISWT" & ESWT_at_init == "No", "ISWT only",
                                                     ifelse(test_type_init == "6MWT" & ESWT_at_init == "Yes", "6MWT and ESWT",
                                                            ifelse(test_type_init == "ISWT" & ESWT_at_init == "Yes", "ISWT and ESWT",
                                              ifelse(test_type_init == "None", "None", "Remote"))))))
table(dat$all_3_test_types_init)

dat$all_3_test_types_init <- factor(dat$all_3_test_types_init, levels = c("6MWT only", "6MWT and ESWT", "ISWT only", "ISWT and ESWT", 
                                                                    "None", "Remote"))

summary(dat$all_3_test_types_init)

# No one only get ESWT this time so don't need to do this:

# dat <- dat %>% mutate(who_only_gets_ESWT_init = ifelse(ESWT_at_init == "No", NA, 
#                                                    ifelse(ESWT_at_init == "Yes" & test_type_init == "None", "Only ESWT",
#                                                           "ESWT and other walking test")))
# table(dat$who_only_gets_ESWT_init)
# 
# dat <- dat %>% mutate(which_other_test_with_ESWT_init = ifelse(ESWT_at_init == "No", NA, 
#                                               ifelse(ESWT_at_init == "Yes" & test_type_init == "ISWT", "ISWT",
#                                               ifelse(ESWT_at_init == "Yes" & test_type_init == "6MWT", "6MWT", NA))))
 
# dat$who_only_gets_ESWT_init <- factor(dat$who_only_gets_ESWT_init)
# dat$which_other_test_with_ESWT_init <- factor(dat$which_other_test_with_ESWT_init)

# summary(dat$who_only_gets_ESWT_init)
# summary(dat$which_other_test_with_ESWT_init)

# table(dat$ESWT_at_init, dat$all_3_test_types_init, useNA = "ifany")

# Test value and practice test variable broken down by test type
dat <- dat %>% mutate(test_value_6MWT_init = ifelse(test_type_init == "6MWT", test_value_init, NA),
                      test_value_ISWT_init = ifelse(test_type_init == "ISWT", test_value_init, NA))

dat <- dat %>% mutate(prac_test_6MWT_init = factor(ifelse(test_type_init == "6MWT",
                                                          as.character(prac_test_init), NA)),
                      prac_test_ISWT_init = factor(ifelse(test_type_init == "ISWT",
                                                          as.character(prac_test_init), NA)))
 
summary(dat$test_value_6MWT_init)
summary(dat$test_value_ISWT_init)

summary(dat$prac_test_6MWT_init)

dat %>% filter(discharge_assess == "Yes") %>%  nrow() # summary()

# Discharge

dat <- dat %>% mutate(all_3_test_types_dis = ifelse(test_type_dis == "6MWT" & (ESWT_at_dis == "No" | is.na(ESWT_at_dis)), "6MWT only",
                                                     ifelse(test_type_dis == "ISWT" & (ESWT_at_dis == "No" | is.na(ESWT_at_dis)), "ISWT only",
                                                     ifelse(test_type_dis == "6MWT" & ESWT_at_dis == "Yes", "6MWT and ESWT",
                                                     ifelse(test_type_dis == "ISWT" & ESWT_at_dis == "Yes", "ISWT and ESWT",
                                                     ifelse(test_type_dis == "None", "None", "Remote"))))))
       
# note that you can only input data at discharge if the patient had data inputted for walking test at initial assessment


dat <- dat %>% mutate(test_value_6MWT_dis = ifelse(test_type_dis == "6MWT", test_value_dis, NA),
                      test_value_ISWT_dis = ifelse(test_type_dis == "ISWT", test_value_dis, NA))







# No practise test info for discharge


# Where are patients enrolled?

table(dat$centre_based, dat$home_based, useNA = "ifany")


dat <- dat %>% mutate(PR_location = NA)
dat$PR_location[dat$centre_based == 1 & dat$home_based == 0] <- "Centre-based"
dat$PR_location[dat$centre_based == 0 & dat$home_based == 1] <- "Home-based"
dat$PR_location[dat$centre_based == 1 & dat$home_based == 1] <- "Both"


dat$PR_location <- factor(dat$PR_location, levels = c("Centre-based", "Home-based", "Both"))



# discharge assess bin

summary(dat$discharge_assess)

dat <- dat %>% mutate(discharge_assess_bin = NA)
dat$discharge_assess_bin[dat$discharge_assess == "Yes"] <- "Yes"
dat$discharge_assess_bin[dat$discharge_assess == "No - DNA" | 
                         dat$discharge_assess == "No - drop-out - health reasons" |
                         dat$discharge_assess == "No - drop-out - patient choice" ] <- "No"

# this is temporarily a character variable in order to make it easier to make the variables
# that are combined with programme type, but at the end I will convert it to a factor

table(dat$discharge_assess, dat$discharge_assess_bin)

# Change the discharge assessment variable...


# Not sure we need this variable...
# 
# 
# dat <- dat %>% rename(discharge_assess_no_reason = discharge_assess)
# dat$discharge_assess_no_reason[dat$discharge_assess_no_reason == "Yes"] <- NA
# dat <- dat %>% mutate(discharge_assess_no_reason = fct_drop(discharge_assess_no_reason))
# 
# table(dat$discharge_assess_no_reason, dat$discharge_assess_bin, useNA = "ifany")

# colnames(dat)
# 
# summary(dat$discharge_assess_bin)
# summary(dat$enrolled)

# Create the variables of discharge assessment by programme type




dat <- dat %>% mutate(discharge_assess_bin_by_centre = ifelse(PR_location == "Centre-based",
                                                            discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_home = ifelse(PR_location == "Home-based",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_both = ifelse(PR_location == "Both",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_both, dat$PR_location, useNA = "ifany")


head(dat)

# Also create the variable exercise plan by programme type
# To do this, I need to convert exercise plan to a characer variable first

dat <- dat %>% mutate(exercise_plan = as.character(exercise_plan))




dat <- dat %>% mutate(exercise_plan_by_centre = ifelse(PR_location == "Centre-based",
                                                              exercise_plan, NA))
table(dat$exercise_plan_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_home = ifelse(PR_location == "Home-based",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_both = ifelse(PR_location == "Both",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_both, dat$PR_location, useNA = "ifany")


dat$PR_location <- factor(dat$PR_location)
summary(dat$PR_location)

dat <- dat %>% mutate(exercise_plan = factor(exercise_plan, levels = c("No", "Yes")))

  

dat <- dat %>% mutate(MRC_score_both = "Both known")
dat$MRC_score_both[dat$MRC_score_init == "Not recorded" | 
                     dat$MRC_score_dis == "Not recorded"] <- "1 or more not known"
dat$MRC_score_both[is.na(dat$MRC_score_init) | is.na(dat$MRC_score_dis)] <- NA


table(dat$MRC_score_both, useNA = "ifany")
table(dat$MRC_score_dis, useNA = "ifany")
table(dat$MRC_score_init, useNA = "ifany")

dat %>% select(MRC_score_init, MRC_score_dis) %>% table()

# Done.

# Now, to create the variable that says whether MRC score has changed.
# to do this safely, need to explicitly give the levels for the MRC score.

levels(dat$MRC_score_init) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")
levels(dat$MRC_score_dis) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")

dat <- dat %>% mutate(MRC_score_init_num = as.numeric(MRC_score_init))
dat$MRC_score_init_num[dat$MRC_score_init_num == 6] <- NA

dat <- dat %>% mutate(MRC_score_dis_num = as.numeric(MRC_score_dis))
dat$MRC_score_dis_num[dat$MRC_score_dis_num == 6] <- NA

# This could be useful
dat <- dat %>% mutate(MRC_change_value = MRC_score_dis_num - MRC_score_init_num)

dat <- dat %>% mutate(MRC_change_factor = NA)
dat$MRC_change_factor[dat$MRC_change_value < 0] <- "Improved"
dat$MRC_change_factor[dat$MRC_change_value == 0] <- "Same"
dat$MRC_change_factor[dat$MRC_change_value > 0] <- "Worse"
dat$MRC_change_factor <- factor(dat$MRC_change_factor)

summary(dat$MRC_score_init_num)
summary(dat$MRC_score_dis_num)
summary(dat$MRC_change_value)

# Create a value for difference in walking tests and helath status questionnaires

dat <- dat %>% mutate(test_value_ISWT_diff = test_value_ISWT_dis - test_value_ISWT_init)
dat <- dat %>% mutate(test_value_6MWT_diff = test_value_6MWT_dis - test_value_6MWT_init)
dat <- dat %>% mutate(test_value_ESWT_diff = ESWT_value_dis - ESWT_value_init)
dat <- dat %>% mutate(CAT_score_diff = CAT_score_dis - CAT_score_init)
dat <- dat %>% mutate(CRQ_dyspnoea_diff = CRQ_dyspnoea_dis - CRQ_dyspnoea_init)
dat <- dat %>% mutate(CRQ_fatigue_diff = CRQ_fatigue_dis - CRQ_fatigue_init)
dat <- dat %>% mutate(CRQ_emotion_diff = CRQ_emotion_dis - CRQ_emotion_init)
dat <- dat %>% mutate(CRQ_mastery_diff = CRQ_mastery_dis - CRQ_mastery_init)


# Create MCID binary values

# MCID is now 35m for ISWT (was 48m)
dat <- dat %>% mutate(MCID_ISWT = NA)
dat$MCID_ISWT[dat$test_value_ISWT_diff < 35] <- "MCID not met"  # changed from 48m
dat$MCID_ISWT[dat$test_value_ISWT_diff >= 35] <- "MCID met"     # changed from 48m
dat$MCID_ISWT <- factor(dat$MCID_ISWT)
summary(dat$MCID_ISWT)

# MCID is 30 for 6MWT
dat <- dat %>% mutate(MCID_6MWT = NA)
dat$MCID_6MWT[dat$test_value_6MWT_diff < 30] <- "MCID not met"
dat$MCID_6MWT[dat$test_value_6MWT_diff >= 30] <- "MCID met"
dat$MCID_6MWT <- factor(dat$MCID_6MWT)
summary(dat$MCID_6MWT)

dat <- dat %>% mutate(MCID_N_ISWT_6MWT = NA)
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_6MWT)] <- 1
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_ISWT)] <- 1


# MCID for CAT is -2
dat <- dat %>% mutate(MCID_CAT = NA)
dat$MCID_CAT[dat$CAT_score_diff <= -2] <- "MCID met"
dat$MCID_CAT[dat$CAT_score_diff > -2] <- "MCID not met"
dat$MCID_CAT <- factor(dat$MCID_CAT)
summary(dat$MCID_CAT)

# MCID for each CRQ is 0.5
dat <- dat %>% mutate(MCID_CRQ_dyspnoea = NA)
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_dyspnoea <- factor(dat$MCID_CRQ_dyspnoea)
summary(dat$MCID_CRQ_dyspnoea)

dat <- dat %>% mutate(MCID_CRQ_fatigue = NA)
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_fatigue <- factor(dat$MCID_CRQ_fatigue)
summary(dat$MCID_CRQ_fatigue)

dat <- dat %>% mutate(MCID_CRQ_emotion = NA)
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_emotion <- factor(dat$MCID_CRQ_emotion)
summary(dat$MCID_CRQ_emotion)

dat <- dat %>% mutate(MCID_CRQ_mastery = NA)
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_mastery <- factor(dat$MCID_CRQ_mastery)
summary(dat$MCID_CRQ_mastery)


dat <- dat %>% mutate(MCID_N_CAT_CRQ = NA)
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CAT)] <- 1
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CRQ_fatigue)] <- 1



dat <- dat %>% mutate(CAT_CRQ_dis_N = NA)
dat$CAT_CRQ_dis_N[!is.na(dat$CAT_dis)] <- 1
dat$CAT_CRQ_dis_N[!is.na(dat$CRQ_dis)] <- 1


dat <- dat %>% mutate(CAT_CRQ_score_diff_N = NA)
dat$CAT_CRQ_score_diff_N[!is.na(dat$CAT_score_diff)] <- 1
dat$CAT_CRQ_score_diff_N[!is.na(dat$CRQ_score_diff)] <- 1


# recode as factors the variables we left before

dat$discharge_assess_bin <- factor(dat$discharge_assess_bin)

# Let's create some benchmarking variables

dat <- dat %>% mutate(BM_start_90 = ifelse(ref_to_start_days_stable_COPD < 90, 1,
                                    ifelse(ref_to_start_days_stable_COPD >= 90, 0, NA)))



# The bit of code below is just to reassure myself that the median for practice test really is 8% with an upper
# quartile of 82%.

# dat %>% group_by(org_code) %>% summarise(practest = sum(BM_prac_test, na.rm = TRUE)/sum(!is.na(BM_prac_test))*100) %>% 
#   arrange(practest) %>% print(., n = 300) 

dat <- dat %>% mutate(BM_prac_test = ifelse(prac_test_init == "No", 0,
                                     ifelse(prac_test_init == "Yes", 1, NA))) 

dat <- dat %>% mutate(BM_discharge_assess = ifelse(discharge_assess_bin == "No", 0,
                                            ifelse(discharge_assess_bin == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_exercise_plan = ifelse(exercise_plan == "No", 0,
                                         ifelse(exercise_plan == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_MCID_exercise = 0)
dat$BM_MCID_exercise[is.na(dat$MCID_6MWT) & is.na(dat$MCID_ISWT)] <- NA
dat$BM_MCID_exercise[dat$MCID_6MWT == "MCID met"] <- 1
dat$BM_MCID_exercise[dat$MCID_ISWT == "MCID met"] <- 1



# table(dat$BM_MCID_exercise, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, dat$MCID_ISWT, useNA = "ifany")
# table(dat$MCID_ISWT, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, useNA = "ifany")


dat <- dat %>% mutate(BM_MCID_CAT_CRQ = 0)
dat$BM_MCID_CAT_CRQ[is.na(dat$MCID_CAT) & is.na(dat$MCID_CRQ_dyspnoea)] <- NA

dat$BM_MCID_CAT_CRQ[dat$MCID_CAT == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_dyspnoea == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_emotion == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_fatigue == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_mastery == "MCID met"] <- 1


table(dat$BM_MCID_CAT_CRQ, dat$MCID_CAT)
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_fatigue, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_emotion, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_dyspnoea, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_mastery, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, useNA = "ifany")


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_exercise_cat = BM_MCID_exercise)
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 0] <- "MCID not met"
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 1] <- "MCID met"
dat$MCID_exercise_cat <- factor(dat$MCID_exercise_cat)
dat$MCID_exercise_cat <- relevel(dat$MCID_exercise_cat, ref = "MCID not met")
summary(dat$MCID_exercise_cat)


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_CAT_CRQ_cat = BM_MCID_CAT_CRQ)
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 0] <- "MCID not met"
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 1] <- "MCID met"
dat$MCID_CAT_CRQ_cat <- factor(dat$MCID_CAT_CRQ_cat)
dat$MCID_CAT_CRQ_cat <- relevel(dat$MCID_CAT_CRQ_cat, ref = "MCID not met")
summary(dat$MCID_CAT_CRQ_cat)



dat <- dat %>% mutate(discharge_assess_bin_by_MRC1_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                          ifelse(MRC_score_init != "Grade 1", NA,
                                                                 as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC2_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 2", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC3_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 3", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC4_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 4", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC5_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 5", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC_NR_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Not recorded", NA,
                                                                               as.character(discharge_assess_bin)))))


dat %>% select(discharge_assess_bin, MRC_score_init) %>% table()

summary(dat$discharge_assess_bin_by_MRC1_init)
summary(dat$discharge_assess_bin_by_MRC2_init)
summary(dat$discharge_assess_bin_by_MRC3_init)
summary(dat$discharge_assess_bin_by_MRC4_init)
summary(dat$discharge_assess_bin_by_MRC5_init)
summary(dat$discharge_assess_bin_by_MRC_NR_init)


# I've checked that all these variables are okay, and correspond to the number of missing values.

summary(dat$ref_location)


# Can split into centre/home-based/both, if required:

# mediSumRound(dat, "scheduled_sess", 0), # good
# # need rec_sess_group and rec_sess indiv.
# # No way to tell where these sessions were based
# mediSumRound(dat, "rec_sess_group", 0),
# mediSumRound(dat, "rec_sess_indiv", 0),

dat$scheduled_sess_by_centre <- dat$scheduled_sess
dat$scheduled_sess_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_group_by_centre <- dat$rec_sess_group
dat$rec_sess_group_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_indiv_by_centre <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_centre[dat$PR_location != "Centre-based"] <- NA

dat$scheduled_sess_by_home <- dat$scheduled_sess
dat$scheduled_sess_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_group_by_home <- dat$rec_sess_group
dat$rec_sess_group_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_indiv_by_home <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_home[dat$PR_location != "Home-based"] <- NA

dat$scheduled_sess_by_both <- dat$scheduled_sess
dat$scheduled_sess_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_group_by_both <- dat$rec_sess_group
dat$rec_sess_group_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_indiv_by_both <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_both[dat$PR_location != "Both"] <- NA


summary(dat$scheduled_sess_by_centre)
summary(dat$rec_sess_group_by_centre)
summary(dat$rec_sess_indiv_by_centre)

summary(dat$scheduled_sess_by_home)
summary(dat$rec_sess_group_by_home)
summary(dat$rec_sess_indiv_by_home)

summary(dat$scheduled_sess_by_both)
summary(dat$rec_sess_group_by_both)
summary(dat$rec_sess_indiv_by_both)

summary(dat$PR_location)







# Do some data cleaning

nlc("Total number of admissions in dataset:")

nrow(dat)

summary(dat$assess_date)
summary(dat$discharge_date)



nrow(dat)

nlc("Now we do some data cleaning. Is there anyone who receives their assessment date before their referral date?")

dat %>% filter(assess_date < ref_date) %>% nrow()
dat <- dat %>% filter(assess_date >= ref_date | is.na(assess_date) | is.na(ref_date)) 

nlc("does anyone receive their start date before their assessment date?")

dat %>% filter(start_date < assess_date) %>% nrow()
dat <- dat %>% filter(start_date >= assess_date | is.na(assess_date) | is.na(start_date)) 

nlc("Is anyone discharged before their start date?")
dat %>% filter(discharge_date < start_date) %>% nrow()
dat <- dat %>% filter(discharge_date >= start_date | is.na(discharge_date) | is.na(start_date)) 


nlc("Is anyone marked as missing something who has that same thing encoded?")

# referral date
dat %>% filter(is.na(ref_date) & ref_date_rec == "Known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()

dat %>% filter(is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Known") %>% nrow()

dat <- dat %>% filter(!(is.na(ref_date) & ref_date_rec == "Known"))
dat <- dat %>% filter(!(!is.na(ref_date) & ref_date_rec == "Not known"))

# This is all fine.

# FEV1 perc pred
dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()

dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()

dat <- dat %>% filter(!(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded"))
dat <- dat %>% filter(!(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded"))

nrow(dat)

# FEV1FVC
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()

dat <- dat %>% filter(!(is.na(FEV1FVC) & FEV1FVC_rec == "Recorded"))
dat <- dat %>% filter(!(!is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded"))

nrow(dat)



# No conflicting records.





dat %>% select(country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
               ref_location, assess_date) %>% nrow()

nlc("assess whether there are duplicate records. Done based on:
country, trust_code, patient_ID, org_code, LSOA, age, gender, ethnicity, ref_date, 
ref_location, assess_date. This many duplicates:")

dat %>% select(country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
               ref_location, assess_date) %>% duplicated() %>% sum() %>% nlc()


nlc("After duplicates removed, we have this many people:")
dat <- dat[!duplicated(select(dat, country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
                               ref_location, assess_date)), ]

nlc(nrow(dat))



# We have a few columns that are character when they should be factor, so we convert them to factor,
# but we leave 'study_ID', 'patient_ID' and 'LSOA' as character.

dat %>% select_if(is.character) %>% colnames()

dat <- dat %>% mutate_if(is.character, factor) %>% 
           mutate_at(c("study_ID", "patient_ID", "LSOA"), as.character)

sink()

saveRDS(dat, "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Data/tidyData/PR_clinical_2022-23_cleaned.RDS")


# done! 

