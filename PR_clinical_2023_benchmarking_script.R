
#--------------------------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b e n c h m a r k i n g    s c r i p t   #

#--------------------------------------------------------------------------------------------#



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





# glimpse(flat.all)
# 
# write.xlsx(dat, file, sheetName = "Sheet1", 
#            col.names = TRUE, row.names = FALSE)


# sprintf("%.1f", round((BM_prac_test_nume/BM_prac_test_denom)*100,1)),

# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(org_code) %>%
  summarise(trust_code = first(trust_code), 
            cases.audited = n(),
            start_90_denom = sum(!is.na(BM_start_90)),
            start_90_nume = sum(BM_start_90, na.rm = TRUE),
            start_90_perc = (start_90_nume/start_90_denom)*100,
            
            BM_prac_test_denom = sum(!is.na(BM_prac_test)),
            BM_prac_test_nume = sum(BM_prac_test, na.rm = TRUE),
            BM_prac_test_perc = (BM_prac_test_nume/BM_prac_test_denom)*100,
            
            BM_discharge_assess_denom = sum(!is.na(BM_discharge_assess)),
            BM_discharge_assess_nume = sum(BM_discharge_assess, na.rm = TRUE),
            BM_discharge_assess_perc = (BM_discharge_assess_nume/BM_discharge_assess_denom)*100,
            
            BM_exercise_plan_denom = sum(!is.na(BM_exercise_plan)),
            BM_exercise_plan_nume = sum(BM_exercise_plan, na.rm = TRUE),
            BM_exercise_plan_perc = (BM_exercise_plan_nume/BM_exercise_plan_denom)*100,
            
            BM_MCID_exercise_denom = sum(!is.na(BM_MCID_exercise)),
            BM_MCID_exercise_nume = sum(BM_MCID_exercise, na.rm = TRUE),
            BM_MCID_exercise_perc = (BM_MCID_exercise_nume/BM_MCID_exercise_denom)*100,
            
            BM_MCID_CAT_CRQ_denom = sum(!is.na(BM_MCID_CAT_CRQ)),
            BM_MCID_CAT_CRQ_nume = sum(BM_MCID_CAT_CRQ, na.rm = TRUE),
            BM_MCID_CAT_CRQ_perc = (BM_MCID_CAT_CRQ_nume/BM_MCID_CAT_CRQ_denom)*100)

bmk


# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one



quartz1 <- matrix(data = NA, nrow = 3, ncol = 7)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$start_90_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$BM_prac_test_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$BM_discharge_assess_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$BM_exercise_plan_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$BM_MCID_exercise_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 7] <- quantile(bmk$BM_MCID_CAT_CRQ_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "start_90_perc", "BM_prac_test_perc", "BM_discharge_assess_perc", 
                       "BM_exercise_plan_perc", "BM_MCID_exercise_perc", "BM_MCID_CAT_CRQ_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 7)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$start_90_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$BM_prac_test_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$BM_discharge_assess_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$BM_exercise_plan_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$BM_MCID_exercise_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 7] <- sprintf("%.0f", round(quantile(bmk$BM_MCID_CAT_CRQ_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "start_90_perc", "BM_prac_test_perc", "BM_discharge_assess_perc", 
                          "BM_exercise_plan_perc", "BM_MCID_exercise_perc", "BM_MCID_CAT_CRQ_perc") 

quartz_fmt <- as.data.frame(quartz_fmt)


write.csv(quartz_fmt,
          "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/PR_clinical_2022-23_benchmarking_quartiles.csv",
          row.names = FALSE)



# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(start_90_colour_end = ifelse(start_90_denom < 5 | is.na(start_90_denom) == TRUE, "Grey",
                                                   ifelse(start_90_perc < quartz1$start_90_perc[1], "Red",
                                                          ifelse(start_90_perc >= quartz1$start_90_perc[3], "Green", 
                                                                 "Yellow"))),
                      BM_prac_test_colour_end = ifelse(BM_prac_test_denom < 5 | is.na(BM_prac_test_denom) == TRUE, "Grey",
                                                       ifelse(BM_prac_test_perc < quartz1$BM_prac_test_perc[1], "Red",
                                                              ifelse(BM_prac_test_perc >= quartz1$BM_prac_test_perc[3], "Green", 
                                                                     "Yellow"))),
                      BM_discharge_assess_colour_end = ifelse(BM_discharge_assess_denom < 5 | is.na(BM_discharge_assess_denom) == TRUE, "Grey",
                                                              ifelse(BM_discharge_assess_perc < quartz1$BM_discharge_assess_perc[1], "Red",
                                                                     ifelse(BM_discharge_assess_perc >= quartz1$BM_discharge_assess_perc[3], "Green", 
                                                                            "Yellow"))),
                      BM_exercise_plan_colour_end = ifelse(BM_exercise_plan_denom < 5 | is.na(BM_exercise_plan_denom) == TRUE, "Grey",
                                                           ifelse(BM_exercise_plan_perc < quartz1$BM_exercise_plan_perc[1], "Red",
                                                                  ifelse(BM_exercise_plan_perc >= quartz1$BM_exercise_plan_perc[3], "Green", 
                                                                         "Yellow"))),
                      BM_MCID_exercise_colour_end = ifelse(BM_MCID_exercise_denom < 5 | is.na(BM_MCID_exercise_denom) == TRUE, "Grey",
                                                           ifelse(BM_MCID_exercise_perc < quartz1$BM_MCID_exercise_perc[1], "Red",
                                                                  ifelse(BM_MCID_exercise_perc >= quartz1$BM_MCID_exercise_perc[3], "Green", 
                                                                         "Yellow"))),
                      BM_MCID_CAT_CRQ_colour_end = ifelse(BM_MCID_CAT_CRQ_denom < 5 | is.na(BM_MCID_CAT_CRQ_denom) == TRUE, "Grey",
                                                          ifelse(BM_MCID_CAT_CRQ_perc < quartz1$BM_MCID_CAT_CRQ_perc[1], "Red",
                                                                 ifelse(BM_MCID_CAT_CRQ_perc >= quartz1$BM_MCID_CAT_CRQ_perc[3], "Green", 
                                                                        "Yellow"))))






bmk <- bmk %>% add_column(start_90_colour = bmk$start_90_colour_end, .after = "start_90_perc") %>% 
  add_column(BM_prac_test_colour = bmk$BM_prac_test_colour_end, .after = "BM_prac_test_perc") %>%
  add_column(BM_discharge_assess_colour = bmk$BM_discharge_assess_colour_end, .after = "BM_discharge_assess_perc") %>% 
  add_column(BM_exercise_plan_colour = bmk$BM_exercise_plan_colour_end, .after = "BM_exercise_plan_perc") %>% 
  add_column(BM_MCID_exercise_colour = bmk$BM_MCID_exercise_colour_end, .after = "BM_MCID_exercise_perc") %>%
  add_column(BM_MCID_CAT_CRQ_colour = bmk$BM_MCID_CAT_CRQ_colour_end, .after = "BM_MCID_CAT_CRQ_perc") %>%
  select(-start_90_colour_end, -BM_prac_test_colour_end, -BM_discharge_assess_colour_end, -BM_exercise_plan_colour_end,
         -BM_MCID_exercise_colour_end, -BM_MCID_CAT_CRQ_colour_end)





bmk_all <- dat %>%
  summarise(org_code = "National",
            trust_code = "National", 
            cases.audited = n(),
            start_90_denom = sum(!is.na(BM_start_90)),
            start_90_nume = sum(BM_start_90, na.rm = TRUE),
            start_90_perc = round((start_90_nume/start_90_denom)*100, 0),
            
            BM_prac_test_denom = sum(!is.na(BM_prac_test)),
            BM_prac_test_nume = sum(BM_prac_test, na.rm = TRUE),
            BM_prac_test_perc = round((BM_prac_test_nume/BM_prac_test_denom)*100, 0),
            
            BM_discharge_assess_denom = sum(!is.na(BM_discharge_assess)),
            BM_discharge_assess_nume = sum(BM_discharge_assess, na.rm = TRUE),
            BM_discharge_assess_perc = round((BM_discharge_assess_nume/BM_discharge_assess_denom)*100, 0),
            
            BM_exercise_plan_denom = sum(!is.na(BM_exercise_plan)),
            BM_exercise_plan_nume = sum(BM_exercise_plan, na.rm = TRUE),
            BM_exercise_plan_perc = round((BM_exercise_plan_nume/BM_exercise_plan_denom)*100, 0),
            
            BM_MCID_exercise_denom = sum(!is.na(BM_MCID_exercise)),
            BM_MCID_exercise_nume = sum(BM_MCID_exercise, na.rm = TRUE),
            BM_MCID_exercise_perc = round((BM_MCID_exercise_nume/BM_MCID_exercise_denom)*100, 0),
            
            BM_MCID_CAT_CRQ_denom = sum(!is.na(BM_MCID_CAT_CRQ)),
            BM_MCID_CAT_CRQ_nume = sum(BM_MCID_CAT_CRQ, na.rm = TRUE),
            BM_MCID_CAT_CRQ_perc = round((BM_MCID_CAT_CRQ_nume/BM_MCID_CAT_CRQ_denom)*100, 0))

# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

write.csv(bmk, file =
 "C:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Analysis/Output/PR_clinical_2022-23_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!



