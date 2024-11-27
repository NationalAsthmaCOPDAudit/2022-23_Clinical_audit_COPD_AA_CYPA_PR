#------------------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   a n a l y s i s    s c r i p t   #
#------------------------------------------------------------------------------------#



library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
source("H:/My R functions/lintestOR.R")
source("H:/My R functions/tidyoutput.R")
source("H:/My R functions/checkSame.R")
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



dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Data/tidyData/PR_clinical_2022-23_cleaned.RDS")



# . . .   t h e   a n a l y s i s   . . . . . . .


psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)



completion_ratio <- data.frame(completion_ratio = round(nrow(filter(dat, discharge_assess_bin == "Yes"))/
                                                          nrow(filter(dat, discharge_assess_bin == "No")), 2))

# I don't know what we're doing for case ascertainment so let's just leave that for now.

# From here, we start off with all, and then re-run it for the individual countries.


# just so that everything works fine, I need to make it so the home-based variables are factors

dat$home_based <- factor(dat$home_based, levels = c("0", "1"))
dat$home_based_in_person <- factor(dat$home_based_in_person, levels = c("0", "1"))
dat$home_based_video <- factor(dat$home_based_video, levels = c("0", "1"))
dat$home_based_phone <- factor(dat$home_based_phone, levels = c("0", "1"))
dat$home_based_other <- factor(dat$home_based_other, levels = c("0", "1"))

flat <- data.frame(org_code = NA)
flat$org_name <- as.character(NA)
flat$trust_code <- as.character(NA)
flat$trust_name <- as.character(NA)
flat$region <- as.character(NA)
flat$country <- as.character("All")


# flat <- data.frame(country = "All", record_N = nrow(dat))

flat <- cbind(flat, mediSumRound(dat, "age", 0), 
              FreqSum(dat, "gender"),
              FreqSum(dat, "IMD_quintile"),
              # FreqSum(dat, "IMD_quintile_Eng"),
              # FreqSum(dat, "IMD_quintile_Wal"),
              # FreqSum(dat, "IMD_quintile_Scot"),
              FreqSum(dat, "anyIMD"),
              FreqSum(dat, "ethnicity"), 
              FreqSum(dat, "ref_location"),
             
              mediSumRound(dat, "ref_to_start_days_stable_COPD", 0),
              FreqSum(dat, "ninety_day_referral_to_start_for_stable_COPD"),
              FreqSum(dat, "thirty_day_referral_to_start_for_AECOPD"),
              mediSumRound(dat, "assess_to_start_days_stable_COPD", 0), 
              FreqSum(dat, "FEV1_percpred_rec"),
              mediSumRound(dat, "FEV1_percpred", 0), 
              FreqSum(dat, "FEV1FVC_rec"),
              mediSumRound(dat, "FEV1FVC", 2), 
              FreqSum(dat, "MRC_score_init"),
              FreqSum(dat, "all_3_test_types_init"),
              mediSumRound(dat, "test_value_ISWT_init", 0), 
              FreqSum(dat, "prac_test_ISWT_init"),
              mediSumRound(dat, "test_value_6MWT_init", 0),
              FreqSum(dat, "prac_test_6MWT_init"),
              mediSumRound(dat, "ESWT_value_init", 0), 
              FreqSum(dat, "CAT_init"),
              FreqSum(dat, "CRQ_init"),
              mediSumRound(dat, "CAT_score_init", 0), 
              mediSumRound(dat, "CRQ_dyspnoea_init", 1), 
              mediSumRound(dat, "CRQ_fatigue_init", 1), # 
              mediSumRound(dat, "CRQ_emotion_init", 1), # 
              mediSumRound(dat, "CRQ_mastery_init", 1),
              FreqSum(dat, "enrolled"),   
              FreqSum(dat, "PR_location"),
              FreqSum(dat, "home_based_in_person"),
              FreqSum(dat, "home_based_video"),
              FreqSum(dat, "home_based_phone"),
              FreqSum(dat, "home_based_other"),
              mediSumRound(dat, "scheduled_sess", 0), 
              mediSumRound(dat, "rec_sess_group", 0),
              mediSumRound(dat, "rec_sess_indiv", 0),
              mediSumRound(dat, "scheduled_sess_by_centre", 0), 
              mediSumRound(dat, "rec_sess_group_by_centre", 0),
              mediSumRound(dat, "rec_sess_indiv_by_centre", 0),
              mediSumRound(dat, "scheduled_sess_by_home", 0), 
              mediSumRound(dat, "rec_sess_group_by_home", 0),
              mediSumRound(dat, "rec_sess_indiv_by_home", 0),
              mediSumRound(dat, "scheduled_sess_by_both", 0), 
              mediSumRound(dat, "rec_sess_group_by_both", 0),
              mediSumRound(dat, "rec_sess_indiv_by_both", 0),
              FreqSum(dat, "discharge_assess"),
              completion_ratio,
              FreqSum(dat, "discharge_assess_bin_by_centre"),
              FreqSum(dat, "discharge_assess_bin_by_home"),
              FreqSum(dat, "discharge_assess_bin_by_both"),
              FreqSum(dat, "exercise_plan"),
              FreqSum(dat, "exercise_plan_by_centre"),
              FreqSum(dat, "exercise_plan_by_home"),
              FreqSum(dat, "exercise_plan_by_both"),
              FreqSum(dat, "discharge_assess_bin_by_MRC1_init"),
              FreqSum(dat, "discharge_assess_bin_by_MRC2_init"),
              FreqSum(dat, "discharge_assess_bin_by_MRC3_init"),
              FreqSum(dat, "discharge_assess_bin_by_MRC4_init"),
              FreqSum(dat, "discharge_assess_bin_by_MRC5_init"),
              FreqSum(dat, "discharge_assess_bin_by_MRC_NR_init"),
              mediSumRound(dat, "assess_to_discharge_days", 0),
              FreqSum(dat, "MRC_score_dis"),
              FreqSum(dat, "MRC_change_factor"),
              FreqSum(dat, "all_3_test_types_dis"),
              mediSumRound(dat, "test_value_ISWT_diff", 1), 
              mediSumRound(dat, "test_value_6MWT_diff", 0), 
              mediSumRound(dat, "test_value_ESWT_diff", 0), 
              FreqSum(dat, "MCID_ISWT"),
              FreqSum(dat, "MCID_6MWT"), 
              FreqSum(dat, "MCID_exercise_cat"),
              FreqSum(dat, "CAT_dis"),
              FreqSum(dat, "CRQ_dis"),
              mediSumRound(dat, "CAT_score_diff", 0),
              mediSumRound(dat, "CRQ_dyspnoea_diff", 1),
              mediSumRound(dat, "CRQ_fatigue_diff", 1),
              mediSumRound(dat, "CRQ_emotion_diff", 1),
              mediSumRound(dat, "CRQ_mastery_diff", 1),
              FreqSum(dat, "MCID_CAT"),
              FreqSum(dat, "MCID_CRQ_dyspnoea"), 
              FreqSum(dat, "MCID_CRQ_fatigue"), 
              FreqSum(dat, "MCID_CRQ_emotion"), 
              FreqSum(dat, "MCID_CRQ_mastery"))



flat.all <- flat

colnames(flat.all)

# To keep the row order the same for the org level, we use the national level one
# as a template to follow so we save it as flat.org as well.

flat.all <- flat

# Need to replace the country column with something else and add another for trust code:

# flat.all <- flat.all[ ,c(1, 1, 2:ncol(flat.all))]
# flat.all <- flat.all %>% rename(org_code = country,
#                                 trust_code = country.1) %>% 
#   mutate(org_code = as.character(org_code),
#          trust_code = as.character(trust_code))
# 
# flat.all$record_N <- NA
# flat.all$record_N <- as.numeric(flat.all$record_N)


dat.save <- dat

#### For country...

for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  
  
  completion_ratio <- data.frame(completion_ratio = round(nrow(filter(dat, discharge_assess_bin == "Yes"))/
                                                            nrow(filter(dat, discharge_assess_bin == "No")), 1))
  
  # I don't know what we're doing for case ascertainment so let's just leave that for now.
  
  # From here, we start off with all, and then re-run it for the individual countries.
  
  flat <- data.frame(org_code = NA)
  flat$org_name <- as.character(NA)
  flat$trust_code <- as.character(NA)
  flat$trust_name <- as.character(NA)
  flat$region <- as.character(NA)
  flat$country <- i
  
  
#  flat <- data.frame(country = i, record_N = nrow(dat))
  
  flat <- cbind(flat, mediSumRound(dat, "age", 0), 
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                FreqSum(dat, "anyIMD"),
                FreqSum(dat, "ethnicity"), 
                FreqSum(dat, "ref_location"),
                mediSumRound(dat, "ref_to_start_days_stable_COPD", 0),
                FreqSum(dat, "ninety_day_referral_to_start_for_stable_COPD"),
                FreqSum(dat, "thirty_day_referral_to_start_for_AECOPD"),
                mediSumRound(dat, "assess_to_start_days_stable_COPD", 0), 
                FreqSum(dat, "FEV1_percpred_rec"),
                mediSumRound(dat, "FEV1_percpred", 0), 
                FreqSum(dat, "FEV1FVC_rec"),
                mediSumRound(dat, "FEV1FVC", 2), 
                FreqSum(dat, "MRC_score_init"),
                FreqSum(dat, "all_3_test_types_init"),
                mediSumRound(dat, "test_value_ISWT_init", 0), 
                FreqSum(dat, "prac_test_ISWT_init"),
                mediSumRound(dat, "test_value_6MWT_init", 0), 
                FreqSum(dat, "prac_test_6MWT_init"),
                mediSumRound(dat, "ESWT_value_init", 0), 
                FreqSum(dat, "CAT_init"),
                FreqSum(dat, "CRQ_init"),
                mediSumRound(dat, "CAT_score_init", 0), 
                mediSumRound(dat, "CRQ_dyspnoea_init", 1), 
                mediSumRound(dat, "CRQ_fatigue_init", 1), # 
                mediSumRound(dat, "CRQ_emotion_init", 1), # 
                mediSumRound(dat, "CRQ_mastery_init", 1),
                FreqSum(dat, "enrolled"),   
                FreqSum(dat, "PR_location"),
                FreqSum(dat, "home_based_in_person"),
                FreqSum(dat, "home_based_video"),
                FreqSum(dat, "home_based_phone"),
                FreqSum(dat, "home_based_other"),
                mediSumRound(dat, "scheduled_sess", 0), 
                mediSumRound(dat, "rec_sess_group", 0),
                mediSumRound(dat, "rec_sess_indiv", 0),
                mediSumRound(dat, "scheduled_sess_by_centre", 0), 
                mediSumRound(dat, "rec_sess_group_by_centre", 0),
                mediSumRound(dat, "rec_sess_indiv_by_centre", 0),
                mediSumRound(dat, "scheduled_sess_by_home", 0), 
                mediSumRound(dat, "rec_sess_group_by_home", 0),
                mediSumRound(dat, "rec_sess_indiv_by_home", 0),
                mediSumRound(dat, "scheduled_sess_by_both", 0), 
                mediSumRound(dat, "rec_sess_group_by_both", 0),
                mediSumRound(dat, "rec_sess_indiv_by_both", 0),
                FreqSum(dat, "discharge_assess"),
                completion_ratio,
                FreqSum(dat, "discharge_assess_bin_by_centre"),
                FreqSum(dat, "discharge_assess_bin_by_home"),
                FreqSum(dat, "discharge_assess_bin_by_both"),
                FreqSum(dat, "exercise_plan"),
                FreqSum(dat, "exercise_plan_by_centre"),
                FreqSum(dat, "exercise_plan_by_home"),
                FreqSum(dat, "exercise_plan_by_both"),
                FreqSum(dat, "discharge_assess_bin_by_MRC1_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC2_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC3_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC4_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC5_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC_NR_init"),
                mediSumRound(dat, "assess_to_discharge_days", 0),
                FreqSum(dat, "MRC_score_dis"),
                FreqSum(dat, "MRC_change_factor"),
                FreqSum(dat, "all_3_test_types_dis"),
                mediSumRound(dat, "test_value_ISWT_diff", 1), 
                mediSumRound(dat, "test_value_6MWT_diff", 0), 
                mediSumRound(dat, "test_value_ESWT_diff", 0), 
                FreqSum(dat, "MCID_ISWT"),
                FreqSum(dat, "MCID_6MWT"), 
                FreqSum(dat, "MCID_exercise_cat"),
                FreqSum(dat, "CAT_dis"),
                FreqSum(dat, "CRQ_dis"),
                mediSumRound(dat, "CAT_score_diff", 0),
                mediSumRound(dat, "CRQ_dyspnoea_diff", 1),
                mediSumRound(dat, "CRQ_fatigue_diff", 1),
                mediSumRound(dat, "CRQ_emotion_diff", 1),
                mediSumRound(dat, "CRQ_mastery_diff", 1),
                FreqSum(dat, "MCID_CAT"),
                FreqSum(dat, "MCID_CRQ_dyspnoea"), 
                FreqSum(dat, "MCID_CRQ_fatigue"), 
                FreqSum(dat, "MCID_CRQ_emotion"), 
                FreqSum(dat, "MCID_CRQ_mastery"))
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}





# And then we get our old dat back:

dat <- dat.save

# Hospital level

#### For organisational...

for (i in unique(dat.save$org_code)) {
  
  dat <- filter(dat.save, org_code == i)
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  
  
  completion_ratio <- data.frame(completion_ratio = round(nrow(filter(dat, discharge_assess_bin == "Yes"))/
                                                            nrow(filter(dat, discharge_assess_bin == "No")), 1))
  
  # I don't know what we're doing for case ascertainment so let's just leave that for now.
  
  # From here, we start off with all, and then re-run it for the individual countries.
  
  flat <- data.frame(org_code = i)
  flat$org_name <- as.character(dat$org_name[1])
  flat$trust_code <- as.character(dat$trust_code[1])
  flat$trust_name <- as.character(dat$trust_name[1])
  flat$region <- as.character(dat$region[1])
  flat$country <- as.character(dat$country[1])
  
  
  flat <- cbind(flat, mediSumRound(dat, "age", 0), 
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                FreqSum(dat, "anyIMD"),
                FreqSum(dat, "ethnicity"), 
                FreqSum(dat, "ref_location"),
                mediSumRound(dat, "ref_to_start_days_stable_COPD", 0),
                FreqSum(dat, "ninety_day_referral_to_start_for_stable_COPD"),
                FreqSum(dat, "thirty_day_referral_to_start_for_AECOPD"),
                mediSumRound(dat, "assess_to_start_days_stable_COPD", 0), 
                FreqSum(dat, "FEV1_percpred_rec"),
                mediSumRound(dat, "FEV1_percpred", 0), 
                FreqSum(dat, "FEV1FVC_rec"),
                mediSumRound(dat, "FEV1FVC", 2), 
                FreqSum(dat, "MRC_score_init"),
                FreqSum(dat, "all_3_test_types_init"),
                mediSumRound(dat, "test_value_ISWT_init", 0), 
                FreqSum(dat, "prac_test_ISWT_init"),
                mediSumRound(dat, "test_value_6MWT_init", 0),
                FreqSum(dat, "prac_test_6MWT_init"),
                mediSumRound(dat, "ESWT_value_init", 0), 
                FreqSum(dat, "CAT_init"),
                FreqSum(dat, "CRQ_init"),
                mediSumRound(dat, "CAT_score_init", 0), 
                mediSumRound(dat, "CRQ_dyspnoea_init", 1), 
                mediSumRound(dat, "CRQ_fatigue_init", 1), # 
                mediSumRound(dat, "CRQ_emotion_init", 1), # 
                mediSumRound(dat, "CRQ_mastery_init", 1),
                FreqSum(dat, "enrolled"),  
                FreqSum(dat, "PR_location"),
                FreqSum(dat, "home_based_in_person"),
                FreqSum(dat, "home_based_video"),
                FreqSum(dat, "home_based_phone"),
                FreqSum(dat, "home_based_other"), 
                mediSumRound(dat, "scheduled_sess", 0), 
                mediSumRound(dat, "rec_sess_group", 0),
                mediSumRound(dat, "rec_sess_indiv", 0),
                mediSumRound(dat, "scheduled_sess_by_centre", 0), 
                mediSumRound(dat, "rec_sess_group_by_centre", 0),
                mediSumRound(dat, "rec_sess_indiv_by_centre", 0),
                mediSumRound(dat, "scheduled_sess_by_home", 0), 
                mediSumRound(dat, "rec_sess_group_by_home", 0),
                mediSumRound(dat, "rec_sess_indiv_by_home", 0),
                mediSumRound(dat, "scheduled_sess_by_both", 0), 
                mediSumRound(dat, "rec_sess_group_by_both", 0),
                mediSumRound(dat, "rec_sess_indiv_by_both", 0),
                FreqSum(dat, "discharge_assess"),
                completion_ratio,
                FreqSum(dat, "discharge_assess_bin_by_centre"),
                FreqSum(dat, "discharge_assess_bin_by_home"),
                FreqSum(dat, "discharge_assess_bin_by_both"),
                FreqSum(dat, "exercise_plan"),
                FreqSum(dat, "exercise_plan_by_centre"),
                FreqSum(dat, "exercise_plan_by_home"),
                FreqSum(dat, "exercise_plan_by_both"),
                FreqSum(dat, "discharge_assess_bin_by_MRC1_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC2_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC3_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC4_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC5_init"),
                FreqSum(dat, "discharge_assess_bin_by_MRC_NR_init"),
                mediSumRound(dat, "assess_to_discharge_days", 0),
                FreqSum(dat, "MRC_score_dis"),
                FreqSum(dat, "MRC_change_factor"),
                FreqSum(dat, "all_3_test_types_dis"),
                mediSumRound(dat, "test_value_ISWT_diff", 1),
                mediSumRound(dat, "test_value_6MWT_diff", 0), 
                mediSumRound(dat, "test_value_ESWT_diff", 0),
                FreqSum(dat, "MCID_ISWT"),
                FreqSum(dat, "MCID_6MWT"), 
                FreqSum(dat, "MCID_exercise_cat"),
                FreqSum(dat, "CAT_dis"),
                FreqSum(dat, "CRQ_dis"),
                mediSumRound(dat, "CAT_score_diff", 0),
                mediSumRound(dat, "CRQ_dyspnoea_diff", 1),
                mediSumRound(dat, "CRQ_fatigue_diff", 1),
                mediSumRound(dat, "CRQ_emotion_diff", 1),
                mediSumRound(dat, "CRQ_mastery_diff", 1),
                FreqSum(dat, "MCID_CAT"),
                FreqSum(dat, "MCID_CRQ_dyspnoea"), 
                FreqSum(dat, "MCID_CRQ_fatigue"), 
                FreqSum(dat, "MCID_CRQ_emotion"), 
                FreqSum(dat, "MCID_CRQ_mastery"))
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

str(flat.all)
ncol(flat.all)
nrow(flat.all)

colnames(flat.all)


# Now we go through and sort out what we want and don't want

flat.all


flat.all <- flat.all %>% select(-ends_with("0_n"), -ends_with("0_perc"),
                                -ends_with("not_met_n"), -ends_with("not_met_perc"),
                                -ends_with("no_n"), -ends_with("no_perc"))

colnames(flat.all)


write.csv(flat.all,
          "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/PR_clinical_2022-23_org_level_data.csv",
          row.names = FALSE)

