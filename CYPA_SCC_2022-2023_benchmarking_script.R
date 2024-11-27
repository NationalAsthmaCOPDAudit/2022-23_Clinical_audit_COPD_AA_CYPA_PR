# Benchmarking CA SCC 2019



library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
source("H:/My R functions/lintestOR.R")
source("H:/My R functions/tidyoutput.R")
source("H:/My R functions/niceN.R")
source("H:/My R functions/niceP.R")
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
  
  ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  # 
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
  # 
  ret <- as.data.frame(ret)
  
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


FreakySum <- function(x, varname) {
  
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



medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  
  # NOTE!!! Medians all rounded to 0dp
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
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
  #  print(varname)
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
  # print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  # print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  # print(gen.A2)
  
  gen.table <- inner_join(gen.A2, gen.E2, by = "Var1") %>% inner_join(gen.S2, by = "Var1") %>%
    inner_join(gen.W2, by = "Var1")
  
  # Changed order to suit what they want. Need to change column names as well.  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
  #   inner_join(gen.A2, by = "Var1")
  
  
  colnames(gen.table) <- c(varname, 
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
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


nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}


# Now let's put this into a function to make it easier

WTmed <- function(x, variable) {
  print(medTable(x, variable))
  write.table(medTable(x, variable), 
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



dat <- readRDS("G:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/tidyData/CYPA_SCC_2022-2023_clean_data.RDS")


# Need to make all the variables binary for this
# Actually it's not too bad, only steroids 1 hour is still not binary.

dat <- dat %>% mutate(BM_steroids_1hour = NA)
dat$BM_steroids_1hour[dat$steroids_1hour == "<1 hour"] <- 1
dat$BM_steroids_1hour[dat$steroids_1hour == ">= 1 hour OR not given OR not recorded"] <- 0
dat$BM_steroids_1hour[dat$age < 6] <- NA

summary(dat$BM_steroids_1hour)

colnames(dat)

# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(hosp_code) %>%
  summarise(hosp_name = first(hosp_name),
            trust_name = first(trust_name),
            cases.audited = n(),
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = (BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100,
            
            DB_smoke_denom = sum(!is.na(DB_smoke)),
            DB_smoke_nume = sum(DB_smoke, na.rm = TRUE),
            DB_smoke_perc = (DB_smoke_nume/DB_smoke_denom)*100,
            
            DB_parent_smoke_denom = sum(!is.na(DB_parent_smoke)),
            DB_parent_smoke_nume = sum(DB_parent_smoke, na.rm = TRUE),
            DB_parent_smoke_perc = (DB_parent_smoke_nume/DB_parent_smoke_denom)*100,
            
            DB_inhaler_denom = sum(!is.na(DB_inhaler)),
            DB_inhaler_nume = sum(DB_inhaler, na.rm = TRUE),
            DB_inhaler_perc = (DB_inhaler_nume/DB_inhaler_denom)*100,
            
            DB_PAAP_denom = sum(!is.na(DB_PAAP)),
            DB_PAAP_nume = sum(DB_PAAP, na.rm = TRUE),
            DB_PAAP_perc = (DB_PAAP_nume/DB_PAAP_denom)*100)
 

bmk
# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one



quartz1 <- matrix(data = NA, nrow = 3, ncol = 6)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$BM_steroids_1hour_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$DB_smoke_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$DB_parent_smoke_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$DB_inhaler_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$DB_PAAP_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "BM_steroids_1hour_perc", "DB_smoke_perc", "DB_parent_smoke_perc", 
                       "DB_inhaler_perc", "DB_PAAP_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 6)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$BM_steroids_1hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$DB_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$DB_parent_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$DB_inhaler_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$DB_PAAP_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "BM_steroids_1hour_perc", "DB_smoke_perc", "DB_parent_smoke_perc", 
                          "DB_inhaler_perc", "DB_PAAP_perc") 

quartz_fmt <- as.data.frame(quartz_fmt)

write.csv(quartz_fmt, file =
  "G:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/CYP_SCC_2022-2023_benchmarking_quartiles.csv",
          row.names = FALSE)


# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(BM_steroids_1hour_colour_end = ifelse(BM_steroids_1hour_denom < 5 | is.na(BM_steroids_1hour_denom) == TRUE, "Grey",
                                                   ifelse(BM_steroids_1hour_perc < quartz1$BM_steroids_1hour_perc[1], "Red",
                                                          ifelse(BM_steroids_1hour_perc >= quartz1$BM_steroids_1hour_perc[3], "Green", 
                                                                 "Yellow"))),
                      DB_smoke_colour_end = ifelse(DB_smoke_denom < 5 | is.na(DB_smoke_denom) == TRUE, "Grey",
                                                       ifelse(DB_smoke_perc < quartz1$DB_smoke_perc[1], "Red",
                                                              ifelse(DB_smoke_perc >= quartz1$DB_smoke_perc[3], "Green", 
                                                                     "Yellow"))),
                      DB_parent_smoke_colour_end = ifelse(DB_parent_smoke_denom < 5 | is.na(DB_parent_smoke_denom) == TRUE, "Grey",
                                                              ifelse(DB_parent_smoke_perc < quartz1$DB_parent_smoke_perc[1], "Red",
                                                                     ifelse(DB_parent_smoke_perc >= quartz1$DB_parent_smoke_perc[3], "Green", 
                                                                            "Yellow"))),
                      DB_inhaler_colour_end = ifelse(DB_inhaler_denom < 5 | is.na(DB_inhaler_denom) == TRUE, "Grey",
                                                           ifelse(DB_inhaler_perc < quartz1$DB_inhaler_perc[1], "Red",
                                                                  ifelse(DB_inhaler_perc >= quartz1$DB_inhaler_perc[3], "Green", 
                                                                         "Yellow"))),
                      DB_PAAP_colour_end = ifelse(DB_PAAP_denom < 5 | is.na(DB_PAAP_denom) == TRUE, "Grey",
                                                           ifelse(DB_PAAP_perc < quartz1$DB_PAAP_perc[1], "Red",
                                                                  ifelse(DB_PAAP_perc >= quartz1$DB_PAAP_perc[3], "Green", 
                                                                         "Yellow"))))





bmk <- bmk %>% add_column(BM_steroids_1hour_colour = bmk$BM_steroids_1hour_colour_end, .after = "BM_steroids_1hour_perc") %>% 
  add_column(DB_smoke_colour = bmk$DB_smoke_colour_end, .after = "DB_smoke_perc") %>%
  add_column(DB_parent_smoke_colour = bmk$DB_parent_smoke_colour_end, .after = "DB_parent_smoke_perc") %>% 
  add_column(DB_inhaler_colour = bmk$DB_inhaler_colour_end, .after = "DB_inhaler_perc") %>% 
  add_column(DB_PAAP_colour = bmk$DB_PAAP_colour_end, .after = "DB_PAAP_perc") %>%
  select(-BM_steroids_1hour_colour_end, -DB_smoke_colour_end, -DB_parent_smoke_colour_end, -DB_inhaler_colour_end,
         -DB_PAAP_colour_end)





bmk_all <- dat %>%
  summarise(hosp_name = "National",
            trust_name = "National", 
            cases.audited = n(),
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = round((BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100, 0),
            
            DB_smoke_denom = sum(!is.na(DB_smoke)),
            DB_smoke_nume = sum(DB_smoke, na.rm = TRUE),
            DB_smoke_perc = round((DB_smoke_nume/DB_smoke_denom)*100, 0),
            
            DB_parent_smoke_denom = sum(!is.na(DB_parent_smoke)),
            DB_parent_smoke_nume = sum(DB_parent_smoke, na.rm = TRUE),
            DB_parent_smoke_perc = round((DB_parent_smoke_nume/DB_parent_smoke_denom)*100, 0),
            
            DB_inhaler_denom = sum(!is.na(DB_inhaler)),
            DB_inhaler_nume = sum(DB_inhaler, na.rm = TRUE),
            DB_inhaler_perc = round((DB_inhaler_nume/DB_inhaler_denom)*100, 0),
            
            DB_PAAP_denom = sum(!is.na(DB_PAAP)),
            DB_PAAP_nume = sum(DB_PAAP, na.rm = TRUE),
            DB_PAAP_perc = round((DB_PAAP_nume/DB_PAAP_denom)*100, 0))

# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

write.csv(bmk, file =
"G:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/CYPA_SCC_2022-2023_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!


# what if we change the CYPA benchmarking for smoking so that everyone has to be given smoking cessation advice if they smoke.

head(bmk)

colnames(bmk)

str(bmk$DB_smoke_perc)

is.nan(bmk$DB_smoke_perc)

bmk$DB_smoke_colour[bmk$DB_smoke_perc == "100"] <- "Green"
bmk$DB_smoke_colour[bmk$DB_smoke_perc != "100" & bmk$DB_smoke_denom != 0 & bmk$hosp_name != "National"] <- "Red"

table(bmk$DB_smoke_colour)
table(bmk$DB_smoke_denom)
colnames(bmk)

write.csv(bmk, file =
            "G:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Analysis/Output/CYPA_SCC_2022-2023_benchmarking_alt_for_smoking.csv",
          row.names = FALSE)
