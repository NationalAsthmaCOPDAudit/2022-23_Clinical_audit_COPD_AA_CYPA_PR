#-----------------------------------------------------------------#
# A d u l t   a s t h m a   s c c   a n l y s i s   s c r i p t   #
#                                                                 #
# Author: Alex Adamson                                            #
#-----------------------------------------------------------------#





library(dplyr)
library(rlang)
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


# My new two by two table:

twoby2 <- function(x, ind, dep) {
  
  indvar1 <- x %>% select(ind) %>% pull()
  indvar1 <- levels(indvar1)[1]
  indvar2 <- x %>% select(ind) %>% pull() 
  indvar2 <- levels(indvar2)[2]
  
  
  dep <- as.character(dep)
  #  print(dep) 
  gen.E <- x %>% filter(UQ(rlang::sym(ind)) == indvar1) %>% select(UQ(rlang::sym(dep))) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  #  print(gen.E2)
  
  
  gen.W <- x %>% filter(UQ(rlang::sym(ind)) == indvar2) %>% select(UQ(rlang::sym(dep))) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  # print(gen.W2)
  
  
  gen.table <- inner_join(gen.E2, gen.W2, by = "Var1") 
  
  # Changed order to suit what they want. Need to change column names as well.  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
  #   inner_join(gen.A2, by = "Var1")
  
  
  colnames(gen.table) <- c(dep, 
                           paste(ind, ": ", indvar1, " (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste(ind, ": ", indvar2, " (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}


twoby2reg <- function(x, ind, dep, title = NULL, obreturn = FALSE, MEM = FALSE) {
  
  tabb <- twoby2(x, ind, dep)
  
  write.table(tabb, file = reporttabs, sep = "\t", append = TRUE, quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  
  # return(tabb)
  
  tabb %>% print()
  
  cat("\n", file=reporttabs, append=TRUE)
  
  
  cat("\n")
  c("levels of dependent var:", levels(x[[dep]])) %>% print()
  c("levels of independent var:", levels(x[[ind]])) %>% print()
  cat("\n")
  
  if(MEM == TRUE) {

  m1form <- paste0(dep, " ~ ", ind, " + (1 | hosp_code)")
  m1 <- glmer(as.formula(m1form), family=binomial(link='logit'), data = x)
  summary(m1) %>% print()
  cat("\n")
  
  
  nlc(title)
  
  # detach("package:lmtest", unload=TRUE)
  
  cat("(Mixed effects) Odds ratio function and output:\n", file=reporttabs, append=TRUE)
  # For some reason, need to use the 'format' function rather than the 'as.character' function
  cat(format(m1form), file=reporttabs, append=TRUE)
  cat("\n\nOR table:\n\n", file=reporttabs, append=TRUE)
  
  m1pres <- tidyoutput(m1, MEM = TRUE, meth = "Wald") %>% rownames_to_column()
  m1pres %>% print()
  
         } else {
           
           m1form <- paste0(dep, " ~ ", ind)
           m1 <- glm(as.formula(m1form), family=binomial(link='logit'), data = x)
           summary(m1) %>% print()
           cat("\n")
           
           
           nlc(title)
           
           # detach("package:lmtest", unload=TRUE)
           
           cat("Odds ratio function and output:\n", file=reporttabs, append=TRUE)
           # For some reason, need to use the 'format' function rather than the 'as.character' function
           cat(format(m1form), file=reporttabs, append=TRUE)
           cat("\n\nOR table:\n\n", file=reporttabs, append=TRUE)
           
           m1pres <- tidyoutput(m1, MEM = FALSE, meth = "Wald") %>% rownames_to_column()
           m1pres %>% print()
         }
           
           
  write.table(m1pres, file = reporttabs, sep = "\t", append = TRUE,
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
  
  RTlist <- list(writtable = tabb,
                 model = m1,
                 tidyoutput = m1pres)
  
  if (obreturn == TRUE) {
    return(RTlist)
  }
  
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
  
  
  # scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  # SN <- sum(scot, na.rm = TRUE)
  # scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  # scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- sum(all, na.rm = TRUE)
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  # ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  ret <- matrix(c(varname, all, eng, wal), nrow = 1, ncol = 4)
  
  
  # colnames(ret) <- c("Variable", 
  #                          paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  colnames(ret) <- c("Variable", 
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
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

mediSumRound <- function(x, variable, roundno = 0) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, median, lo.quart, hi.quart)
  # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno)) # otherwise use 'roundno'
  
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
  
  # NOTE!!! Medians rounded to 0dp by default
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  # scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  # SN <- length(scot[!is.na(scot)])
  # scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  # scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  #  ret <- matrix(c(varname, eng, scot, wal, all), nrow = 1, ncol = 5)
  ret <- matrix(c(varname, eng, wal, all), nrow = 1, ncol = 4)
  
  # colnames(ret) <- c("Variable", 
  #                    paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  colnames(ret) <- c("Variable", 
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
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
  
  # gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  # SN <- nrow(gen.S)
  # gen.S0 <- as.data.frame(table(gen.S[[1]]))
  # gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  # gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  # gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
  #                          trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  # gen.S2 <- select(gen.S2, Var1, Scotland)
  # # print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  # print(gen.A2)
  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>%
  #   inner_join(gen.W2, by = "Var1") %>% inner_join(gen.A2, by = "Var1")
  gen.table <- inner_join(gen.E2, gen.W2, by = "Var1") %>% inner_join(gen.A2, by = "Var1")
  
  # Changed order to suit what they want. Need to change column names as well.  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
  #   inner_join(gen.A2, by = "Var1")
  
  
  # colnames(gen.table) <- c(varname, 
  #                        paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                        paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                        paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                        paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  colnames(gen.table) <- c(varname, 
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
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


nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}


makeFlatNPercInf <- function(subana.N, varname = NULL) {
  
  
  colnames(subana.N) <- gsub(" ", "_", colnames(subana.N))
  rownames(subana.N) <- gsub(" ", "_", rownames(subana.N))
  subana.perc <- round(prop.table(subana.N, 1)*100, 1)  # create the prop table
  # totals <- matrix(margin.table(subana.N, 2), nrow = 1, ncol = 2)
  # colnames(totals) <- paste0(colnames(subana.N), "_N")
  
  
  subana.N_flat <- matrix(subana.N, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.N)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.N)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_N")
  
  colnames(subana.N_flat) <- cols
  subana.N_flat <- as.data.frame(subana.N_flat)
  
  subana.perc_flat <- matrix(subana.perc, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.perc)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.perc)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_perc")
  
  colnames(subana.perc_flat) <- cols
  subana.perc_flat <- as.data.frame(subana.perc_flat)
  
  # subana.flat <- cbind(totals, subana.N_flat, subana.perc_flat)
  subana.flat <- cbind(subana.N_flat, subana.perc_flat)
  
  if (!is.null(varname)) {
    
    colnames(subana.flat) <- paste(varname, colnames(subana.flat), sep = "_")
  }
  
  return(subana.flat)
}





# Now let's put this into a function to make it easier

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







# Okay let's go!

dat <- readRDS("C:/Users/aadamson/Documents/Audit_2023_onwards/2022-2023/AA/Data/tidyData/AA_SCC_2022-23_clean_data.RDS")

summary(dat)



# START OF CSV WRITING


# First up, need to make sure all those binary variables that are currently
# coded as numeric are classed as factors. I think this is just for the IV and DB variables.


dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")), 
                         .funs = ~factor(.)) #%>% str()



# Now we should be fine to get on with what we're doing.

# Second, create our 'psychic' data frame for the medians

psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)

# We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.

admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                   mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                   sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                   se = NA,
                                   lo.quart = round(quantile(admissmeds$admisscount, 
                                                    probs = 0.25, na.rm = TRUE), 0),
                                   median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                   hi.quart = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.75, na.rm = TRUE), 0))

row.names(admissmedsforpsychic) <- "admissions"

psychic <- rbind(psychic, admissmedsforpsychic)

# And again 

BPT_table <- dat %>% 
  mutate(BPT = fct_recode(BPT, `0` = "Not achieved", `1` = "Achieved")) %>%
  mutate(BPT = as.numeric(as.character(BPT))) %>% group_by(hosp_code) %>%
  summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
            BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
            BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))


FreqSum(BPT_table, "BPT_pass")


# makeFlatNPercInf(testtable)



flat <- data.frame(country = "All")

flat <- cbind(flat,
              
             mediSumRound(dat, "age", roundno = 0),

             FreqSum(dat, "hospital_transfer"),
             FreqSum(dat, "first_department"),
             
              
              # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
              # and instead I need to open Excel first and then go on 'import data'
              
              FreqSum(dat, "gender"),
             FreqSum(dat, "IMD_quintile"),
              # FreqSum(dat, "IMD_quintile_Eng"),
              # FreqSum(dat, "IMD_quintile_Scot"),
              # FreqSum(dat, "IMD_quintile_Wal"),
             # FreqSum(dat, "anyIMD"),
             mediSumRound(dat, "admissions", roundno = 0))
             
              

# Now create the 2 hour table and bind it in

admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                rownames(admisstimedow.N)[1:3], "admiss_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
# bind this

flat <- cbind(flat, admisstime_flat)


admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                    rownames(admisstimedow.perc)[1:3], "admiss_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)

# bind this


flat <- cbind(flat, admisstime_flat_perc)

# admisstimedow.N.all <- admisstimedow.N


# # bind these below
# flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
# flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
# flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
# flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
# flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
# flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
# flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]

# bind these below
flat$Weekday_admit_N <- margin.table(admisstimedow.N, 2)[1] +
                        margin.table(admisstimedow.N, 2)[2] +
                        margin.table(admisstimedow.N, 2)[3] +
                        margin.table(admisstimedow.N, 2)[4] +
                        margin.table(admisstimedow.N, 2)[5]

flat$Weekend_admit_N <- margin.table(admisstimedow.N, 2)[6] + margin.table(admisstimedow.N, 2)[7]


# Then carry on as normal:
       
              
              
         
flat <- cbind(flat,
              mediSumRound(dat, "LOS_days_alive"),
              FreqSum(dat, "smoke_status"),
              
        #      FreqSum(dat, "DB_smoke"), # This is the one that gets written
              
              
              mediSumRound(dat, "heart_rate"),
              mediSumRound(dat, "resp_rate"),
              FreqSum(dat, "oxygen_sat_recorded"),
              mediSumRound(dat, "oxygen_sat_value"),
              FreqSum(dat, "oxygen_sat_measurement_type"),
              
              FreqSum(dat, "PEF_init_recorded"),
              mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
              FreqSum(dat, "PEF_init_1hour"),
              FreqSum(dat, "PEF_init_4hour"))

# # PEF
# summary(dat$PEF_init_recorded_unwell_excl)
# 
# 
# admisstimedow.N <- table(dat$arrival2hourtimes[dat$PEF_init_recorded_unwell_excl == "Recorded"], 
#                          dat$arrival_day_of_week[dat$PEF_init_recorded_unwell_excl == "Recorded"])
# rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
# 
# admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
# colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
#                 rownames(admisstimedow.N)[1:12], "admiss_with_PEF_n", sep = "_")
# 
# colnames(admisstime_flat) <- colsss
# admisstime_flat <- as.data.frame(admisstime_flat)
# 
# flat <- cbind(flat, admisstime_flat)
# 
# admisstimedow.N.PEF.denom <- table(dat$arrival2hourtimes[!is.na(dat$PEF_init_recorded_unwell_excl)],
#                                        dat$arrival_day_of_week[!is.na(dat$PEF_init_recorded_unwell_excl)])
# 
# 
# admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.PEF.denom)*100, 1)
# # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
# 
# admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
# colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
#                     rownames(admisstimedow.perc)[1:12], "admiss_with_PEF_perc", sep = "_")
# 
# colnames(admisstime_flat_perc) <- colsssperc
# admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
# flat <- cbind(flat, admisstime_flat_perc)
# 
# 
# 
# flat$Monday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom,2)[1]
# flat$Tuesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[2]
# flat$Wednesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[3]
# flat$Thursday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[4]
# flat$Friday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[5]
# flat$Saturday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[6]
# flat$Sunday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[7]
# 
              
flat <- cbind(flat, 
FreqSum(dat, "PEF_prev_recorded"),
FreqSum(dat, "PEF_predict_recorded"),
FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
mediSumRound(dat, "PEF_percent_pred_value"),
FreqSum(dat, "PEF_percpred_75"),

FreqSum(dat, "RSR"),
mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
FreqSum(dat, "RSR_24hour_weekday"),
FreqSum(dat, "RSR_24hour_weekend"),
FreqSum(dat, "oxygen_prescribed"),
FreqSum(dat, "oxygen_admin"),
mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
FreqSum(dat, "oxygen_presc_1hr"),

FreqSum(dat, "steroids_admin"),
mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
FreqSum(dat, "steroids_24hr_prev"),
FreqSum(dat, "steroids_1hour"),
FreqSum(dat, "steroids_4hour"))
              
# # steroids 4 hours
# summary(dat$steroids_4hour)
# 
# 
# admisstimedow.N <- table(dat$arrival2hourtimes[dat$steroids_4hour == "<4 hours"], 
#                          dat$arrival_day_of_week[dat$steroids_4hour == "<4 hours"])
# rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
# 
# admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
# colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
#                 rownames(admisstimedow.N)[1:12], "admiss_with_4hour_steroids_n", sep = "_")
# 
# colnames(admisstime_flat) <- colsss
# admisstime_flat <- as.data.frame(admisstime_flat)
# 
# flat <- cbind(flat, admisstime_flat)
# 
# admisstimedow.N.steroid.denom <- table(dat$arrival2hourtimes[!is.na(dat$steroids_4hour)],
#                                        dat$arrival_day_of_week[!is.na(dat$steroids_4hour)])
# 
# 
# admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
# # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
# 
# admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
# colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
#                     rownames(admisstimedow.perc)[1:12], "admiss_with_4hour_steroids_perc", sep = "_")
# 
# colnames(admisstime_flat_perc) <- colsssperc
# admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
# flat <- cbind(flat, admisstime_flat_perc)
# 
# 
# 
# flat$Monday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
# flat$Tuesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
# flat$Wednesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
# flat$Thursday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
# flat$Friday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
# flat$Saturday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
# flat$Sunday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]


flat <- cbind(flat,
FreqSum(dat, "b2a_admin"),
mediSumRound(dat, "arrival_to_b2a_minutes"),
FreqSum(dat, "b2a_1hr_prev"),
FreqSum(dat, "b2a_1hour"),
FreqSum(dat, "b2a_4hour"),
FreqSum(dat, "discharge_day_of_week"),
FreqSum(dat, "discharge_bundle"),

makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),

FreqSum(dat, "DB_inhaler"),
FreqSum(dat, "DB_maintenance"),
FreqSum(dat, "DB_adherence"),
FreqSum(dat, "DB_PAAP"),
FreqSum(dat, "DB_triggers"),
FreqSum(dat, "DB_comm_FU_2_days"),
FreqSum(dat, "DB_spec_review_4_weeks"),
FreqSum(dat, "DB_FU_any"),
FreqSum(dat, "DB_smoke"),
FreqSum(dat, "DB_vape_cess"),
FreqSum(dat,"DB_none"),
# FreqSum(dat, "GPC"),
FreqSum(dat, "inhaled_steroids_dis"),
FreqSum(dat, "oral_steroids_dis"),
FreqSum(dat, "oral_steroids_rescue_history"),
# FreqSum(dat, "referred_for_FU"),
# FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
FreqSum(dat, "RSR_BPT"),
FreqSum(dat, "DB_BPT"),
FreqSum(dat, "BPT"),

# FreqSum(dat, "BPT_mandatory"),
# FreqSum(dat, "BPT_all"),
FreqSum(BPT_table, "BPT_pass"),

# Then we do first hour of care

FreqSum(dat, "PEF_init_1hour_all"),
FreqSum(dat, "b2a_1hour_all"),
FreqSum(dat, "steroids_1hour_all"),
FreqSum(dat, "asthma_sev"),
FreqSum(dat, "life_status"),
FreqSum(dat, "PEF_b2a_steroids_oxygen_1hr_combo"),
FreqSum(dat, "BPT_optional"))






flat.all <- flat
dat.save <- dat

# # # # now for countries

for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)

  
  
  
  # Now we should be fine to get on with what we're doing.
  
  # Second, create our 'psychic' data frame for the medians
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  # And again 
  
  # BPT_table <- dat %>% filter(life_status == "Alive") %>%
  #   mutate(BPT_mandatory = fct_recode(BPT_mandatory, `0` = "Not achieved", `1` = "Achieved")) %>%
  #   mutate(BPT_mandatory = as.numeric(as.character(BPT_mandatory))) %>% group_by(hosp_code) %>%
  #   summarise(BPT_nume = sum(BPT), country = first(country),
  #             BPT_denom = n(), BPT_perc = BPT_nume/BPT_denom,
  #             BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))

  BPT_table <- dat %>% filter(life_status == "Alive") %>%
    mutate(BPT = fct_recode(BPT, `0` = "Not achieved", `1` = "Achieved")) %>%
    mutate(BPT = as.numeric(as.character(BPT))) %>% group_by(hosp_code) %>%
    summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
              BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
              BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  
  
  FreqSum(BPT_table, "BPT_pass")

  
  # makeFlatNPercInf(testtable)
  
  
  
  flat <- data.frame(country = i)
  
  

  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                FreqSum(dat, "hospital_transfer"),
                FreqSum(dat, "first_department"),
                
                
                # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
                # and instead I need to open Excel first and then go on 'import data'
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                # FreqSum(dat, "anyIMD"),
                mediSumRound(dat, "admissions", roundno = 0))
  
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                  rownames(admisstimedow.N)[1:3], "admiss_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                      rownames(admisstimedow.perc)[1:3], "admiss_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # bind these below
  flat$Weekday_admit_N <- margin.table(admisstimedow.N, 2)[1] +
    margin.table(admisstimedow.N, 2)[2] +
    margin.table(admisstimedow.N, 2)[3] +
    margin.table(admisstimedow.N, 2)[4] +
    margin.table(admisstimedow.N, 2)[5]
  
  flat$Weekend_admit_N <- margin.table(admisstimedow.N, 2)[6] + margin.table(admisstimedow.N, 2)[7]
  
  
  # Then carry on as normal:
  
  
  
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days_alive"),
                FreqSum(dat, "smoke_status"),
                
                #      FreqSum(dat, "DB_smoke"), # This is the one that gets written
                
                
                mediSumRound(dat, "heart_rate"),
                mediSumRound(dat, "resp_rate"),
                FreqSum(dat, "oxygen_sat_recorded"),
                mediSumRound(dat, "oxygen_sat_value"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
                FreqSum(dat, "PEF_init_1hour"),
                FreqSum(dat, "PEF_init_4hour"))
  
  # # PEF
  # summary(dat$PEF_init_recorded_unwell_excl)
  # 
  # 
  # admisstimedow.N <- table(dat$arrival2hourtimes[dat$PEF_init_recorded_unwell_excl == "Recorded"], 
  #                          dat$arrival_day_of_week[dat$PEF_init_recorded_unwell_excl == "Recorded"])
  # rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  # colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
  #                 rownames(admisstimedow.N)[1:12], "admiss_with_PEF_n", sep = "_")
  # 
  # colnames(admisstime_flat) <- colsss
  # admisstime_flat <- as.data.frame(admisstime_flat)
  # 
  # flat <- cbind(flat, admisstime_flat)
  # 
  # admisstimedow.N.PEF.denom <- table(dat$arrival2hourtimes[!is.na(dat$PEF_init_recorded_unwell_excl)],
  #                                        dat$arrival_day_of_week[!is.na(dat$PEF_init_recorded_unwell_excl)])
  # 
  # 
  # admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.PEF.denom)*100, 1)
  # # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  # colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
  #                     rownames(admisstimedow.perc)[1:12], "admiss_with_PEF_perc", sep = "_")
  # 
  # colnames(admisstime_flat_perc) <- colsssperc
  # admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  # flat <- cbind(flat, admisstime_flat_perc)
  # 
  # 
  # 
  # flat$Monday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom,2)[1]
  # flat$Tuesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[2]
  # flat$Wednesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[3]
  # flat$Thursday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[4]
  # flat$Friday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[5]
  # flat$Saturday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[6]
  # flat$Sunday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[7]
  # 
  
  flat <- cbind(flat, 
                FreqSum(dat, "PEF_prev_recorded"),
                FreqSum(dat, "PEF_predict_recorded"),
                FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
                mediSumRound(dat, "PEF_percent_pred_value"),
                FreqSum(dat, "PEF_percpred_75"),
                
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                FreqSum(dat, "RSR_24hour_weekday"),
                FreqSum(dat, "RSR_24hour_weekend"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_admin"),
                mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
                FreqSum(dat, "oxygen_presc_1hr"),
                
                FreqSum(dat, "steroids_admin"),
                mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
                FreqSum(dat, "steroids_24hr_prev"),
                FreqSum(dat, "steroids_1hour"),
                FreqSum(dat, "steroids_4hour"))
  
  # # steroids 4 hours
  # summary(dat$steroids_4hour)
  # 
  # 
  # admisstimedow.N <- table(dat$arrival2hourtimes[dat$steroids_4hour == "<4 hours"], 
  #                          dat$arrival_day_of_week[dat$steroids_4hour == "<4 hours"])
  # rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  # colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
  #                 rownames(admisstimedow.N)[1:12], "admiss_with_4hour_steroids_n", sep = "_")
  # 
  # colnames(admisstime_flat) <- colsss
  # admisstime_flat <- as.data.frame(admisstime_flat)
  # 
  # flat <- cbind(flat, admisstime_flat)
  # 
  # admisstimedow.N.steroid.denom <- table(dat$arrival2hourtimes[!is.na(dat$steroids_4hour)],
  #                                        dat$arrival_day_of_week[!is.na(dat$steroids_4hour)])
  # 
  # 
  # admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
  # # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  # colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
  #                     rownames(admisstimedow.perc)[1:12], "admiss_with_4hour_steroids_perc", sep = "_")
  # 
  # colnames(admisstime_flat_perc) <- colsssperc
  # admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  # flat <- cbind(flat, admisstime_flat_perc)
  # 
  # 
  # 
  # flat$Monday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
  # flat$Tuesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
  # flat$Wednesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
  # flat$Thursday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
  # flat$Friday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
  # flat$Saturday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
  # flat$Sunday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]
  
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_admin"),
                mediSumRound(dat, "arrival_to_b2a_minutes"),
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "b2a_4hour"),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_spec_review_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_vape_cess"),
                FreqSum(dat,"DB_none"),
                # FreqSum(dat, "GPC"),
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                # FreqSum(dat, "referred_for_FU"),
                # FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
                FreqSum(dat, "RSR_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "BPT"),
                
            #    FreqSum(dat, "BPT_mandatory"),
             #   FreqSum(dat, "BPT_all"),
               FreqSum(BPT_table, "BPT_pass"),
                
                # Then we do first hour of care
                
                FreqSum(dat, "PEF_init_1hour_all"),
                FreqSum(dat, "b2a_1hour_all"),
                FreqSum(dat, "steroids_1hour_all"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "life_status"),
                FreqSum(dat, "PEF_b2a_steroids_oxygen_1hr_combo"),
            FreqSum(dat, "BPT_optional"))
  
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save
str(flat.all)



# # # # now for hospitals

unique(dat$hosp_code)

str(flat.all)

for (i in unique(dat.save$hosp_code)) {
  
  dat <- filter(dat.save, hosp_code == i)
  
  
  flat <- data.frame(hosp_code = i)
  flat$hosp_name <- as.character(dat$hosp_name[1])
  flat$trust_code <- as.character(dat$trust_code[1])
  flat$trust_name <- as.character(dat$trust_name[1])
  flat$region <- as.character(dat$region[1])
  flat$country <- as.character(dat$country[1])
  flat$record_N <- nrow(dat)
  
  # Now we should be fine to get on with what we're doing.
  
  # Second, create our 'psychic' data frame for the medians
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  # And again 
  
  # BPT_table <- dat %>% filter(life_status == "Alive") %>% 
  #   mutate(BPT_mandatory = fct_recode(BPT_mandatory, `0` = "Not achieved", `1` = "Achieved")) %>% 
  #   mutate(BPT_mandatory = as.numeric(as.character(BPT_mandatory))) %>% group_by(hosp_code) %>% 
  #   summarise(BPT_nume = sum(BPT_mandatory), country = first(country), 
  #             BPT_denom = n(), BPT_perc = BPT_nume/BPT_denom,
  #             BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  # 
  # FreqSum(BPT_table, "BPT_pass")
  # 
  
  BPT_table <- dat %>% filter(life_status == "Alive") %>%
    mutate(BPT = fct_recode(BPT, `0` = "Not achieved", `1` = "Achieved")) %>%
    mutate(BPT = as.numeric(as.character(BPT))) %>% group_by(hosp_code) %>%
    summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
              BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
              BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  
  # makeFlatNPercInf(testtable)
  
  
  
 # flat <- data.frame(country = i)
  
  

  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                FreqSum(dat, "hospital_transfer"),
                FreqSum(dat, "first_department"),
                
                
                # Need to use tab - delimited - it's fine but means that I can't just open it immediately by double clicking it,
                # and instead I need to open Excel first and then go on 'import data'
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                # FreqSum(dat, "anyIMD"),
                mediSumRound(dat, "admissions", roundno = 0))
  
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                  rownames(admisstimedow.N)[1:3], "admiss_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                      rownames(admisstimedow.perc)[1:3], "admiss_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # bind these below
  flat$Weekday_admit_N <- margin.table(admisstimedow.N, 2)[1] +
    margin.table(admisstimedow.N, 2)[2] +
    margin.table(admisstimedow.N, 2)[3] +
    margin.table(admisstimedow.N, 2)[4] +
    margin.table(admisstimedow.N, 2)[5]
  
  flat$Weekend_admit_N <- margin.table(admisstimedow.N, 2)[6] + margin.table(admisstimedow.N, 2)[7]
  
  
  # Then carry on as normal:
  
  
  
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days_alive"),
                FreqSum(dat, "smoke_status"),
                
                #      FreqSum(dat, "DB_smoke"), # This is the one that gets written
                
                
                mediSumRound(dat, "heart_rate"),
                mediSumRound(dat, "resp_rate"),
                FreqSum(dat, "oxygen_sat_recorded"),
                mediSumRound(dat, "oxygen_sat_value"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
                FreqSum(dat, "PEF_init_1hour"),
                FreqSum(dat, "PEF_init_4hour"))
  
  # # PEF
  # summary(dat$PEF_init_recorded_unwell_excl)
  # 
  # 
  # admisstimedow.N <- table(dat$arrival2hourtimes[dat$PEF_init_recorded_unwell_excl == "Recorded"], 
  #                          dat$arrival_day_of_week[dat$PEF_init_recorded_unwell_excl == "Recorded"])
  # rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  # colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
  #                 rownames(admisstimedow.N)[1:12], "admiss_with_PEF_n", sep = "_")
  # 
  # colnames(admisstime_flat) <- colsss
  # admisstime_flat <- as.data.frame(admisstime_flat)
  # 
  # flat <- cbind(flat, admisstime_flat)
  # 
  # admisstimedow.N.PEF.denom <- table(dat$arrival2hourtimes[!is.na(dat$PEF_init_recorded_unwell_excl)],
  #                                        dat$arrival_day_of_week[!is.na(dat$PEF_init_recorded_unwell_excl)])
  # 
  # 
  # admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.PEF.denom)*100, 1)
  # # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  # colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
  #                     rownames(admisstimedow.perc)[1:12], "admiss_with_PEF_perc", sep = "_")
  # 
  # colnames(admisstime_flat_perc) <- colsssperc
  # admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  # flat <- cbind(flat, admisstime_flat_perc)
  # 
  # 
  # 
  # flat$Monday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom,2)[1]
  # flat$Tuesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[2]
  # flat$Wednesday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[3]
  # flat$Thursday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[4]
  # flat$Friday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[5]
  # flat$Saturday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[6]
  # flat$Sunday_admit_for_PEF_denom_N <- margin.table(admisstimedow.N.PEF.denom, 2)[7]
  # 
  
  flat <- cbind(flat, 
                FreqSum(dat, "PEF_prev_recorded"),
                FreqSum(dat, "PEF_predict_recorded"),
                FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
                mediSumRound(dat, "PEF_percent_pred_value"),
                FreqSum(dat, "PEF_percpred_75"),
                
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                FreqSum(dat, "RSR_24hour_weekday"),
                FreqSum(dat, "RSR_24hour_weekend"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_admin"),
                mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
                FreqSum(dat, "oxygen_presc_1hr"),
                
                FreqSum(dat, "steroids_admin"),
                mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
                FreqSum(dat, "steroids_24hr_prev"),
                FreqSum(dat, "steroids_1hour"),
                FreqSum(dat, "steroids_4hour"))
  
  # # steroids 4 hours
  # summary(dat$steroids_4hour)
  # 
  # 
  # admisstimedow.N <- table(dat$arrival2hourtimes[dat$steroids_4hour == "<4 hours"], 
  #                          dat$arrival_day_of_week[dat$steroids_4hour == "<4 hours"])
  # rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  # colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
  #                 rownames(admisstimedow.N)[1:12], "admiss_with_4hour_steroids_n", sep = "_")
  # 
  # colnames(admisstime_flat) <- colsss
  # admisstime_flat <- as.data.frame(admisstime_flat)
  # 
  # flat <- cbind(flat, admisstime_flat)
  # 
  # admisstimedow.N.steroid.denom <- table(dat$arrival2hourtimes[!is.na(dat$steroids_4hour)],
  #                                        dat$arrival_day_of_week[!is.na(dat$steroids_4hour)])
  # 
  # 
  # admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.steroid.denom)*100, 1)
  # # rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  # 
  # admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
  # colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
  #                     rownames(admisstimedow.perc)[1:12], "admiss_with_4hour_steroids_perc", sep = "_")
  # 
  # colnames(admisstime_flat_perc) <- colsssperc
  # admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  # flat <- cbind(flat, admisstime_flat_perc)
  # 
  # 
  # 
  # flat$Monday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom,2)[1]
  # flat$Tuesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[2]
  # flat$Wednesday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[3]
  # flat$Thursday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[4]
  # flat$Friday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[5]
  # flat$Saturday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[6]
  # flat$Sunday_admit_for_4hour_steroids_denom_N <- margin.table(admisstimedow.N.steroid.denom, 2)[7]
  
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_admin"),
                mediSumRound(dat, "arrival_to_b2a_minutes"),
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "b2a_4hour"),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_spec_review_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_vape_cess"),
                FreqSum(dat,"DB_none"),
                # FreqSum(dat, "GPC"),
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                # FreqSum(dat, "referred_for_FU"),
                # FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
                FreqSum(dat, "RSR_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "BPT"),
                
              #  FreqSum(dat, "BPT_mandatory"),
              #  FreqSum(dat, "BPT_all"),
                FreqSum(BPT_table, "BPT_pass"),
                
                # Then we do first hour of care
                
                FreqSum(dat, "PEF_init_1hour_all"),
                FreqSum(dat, "b2a_1hour_all"),
                FreqSum(dat, "steroids_1hour_all"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "life_status"),
                FreqSum(dat, "PEF_b2a_steroids_oxygen_1hr_combo"),
              FreqSum(dat, "BPT_optional"))
  
  
  
  
  
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

str(flat.all)


str(flat.all)
dat <- dat.save

head(flat.all$country)

colnames(flat.all)

nrow(flat.all)

# change to appropriate order and remove unnecessary 'median admissions' columns and heat map columns,
flat.all <- flat.all %>% select(hosp_code:record_N, country:`BPT_optional_Not_achieved_perc`) %>% 
  select(-admissions_N, -admissions_median, -admissions_lo.quart, -admissions_hi.quart, -PEF_b2a_steroids_oxygen_1hr_combo_No_n,
         -PEF_b2a_steroids_oxygen_1hr_combo_No_perc)

# # remove the 'all', 'england', 'wales', and 'scotland' rows
# 
# summary(flat.all$record_N)
# 
# 
# 
# # they are the ones without a 'record_N' variable.
# 
# flat.all <- flat.all %>% filter(!is.na(record_N))

nrow(flat.all)
length(unique(flat.all$hosp_code))
length(unique(dat.save$hosp_code))

# All seems legit! Let's write the table


flat.all$record_N <- flat.all$age_N

write.csv(flat.all,
 "C:/Users/aadamson/Documents/Audit_2023_onwards/2022-2023/AA/Analysis/Output/AA_SCC_2022-2023_hospital_level_data.csv",
          row.names = FALSE)

