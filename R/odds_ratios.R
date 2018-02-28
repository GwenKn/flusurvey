##### ODDS RATIO CALCS

###****** CRUDE ***##############################################################################
# following https://www.r-bloggers.com/computing-odds-ratios-in-r/

### locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
datap <- "~/Documents/Flusurvey/data/"

### functions
orwp.ma <- function(datas,category, base){
  #
  #  Compute the odds ratio between category and medication.antibiotic
  
  if (base == 0){ #if only 2 levels 
    w <- which(colnames(datas) == category)
    tt <- table(datas[,w], datas$medication.antibiotic)
  } else {
    
    
  }
  
  
  #    n00 = number of cases where x = 0 and y = 0 ## x = medication.antibiotic 
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  n00 <- tt[1] 
  n01 <- tt[2]  
  n10 <- tt[3] 
  n11 <- tt[4] 
  
  #
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  alpha = 0.05 # 95% CI
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
  oframe
}

orwp.ma(3920,288,1692,2224,alpha = 0.05)

orwp.ma(dabx,"gender",0)

### data
setwd(datap)
dabx <- readRDS("dabx.rds")

table(dabx$medication.antibiotic,dabx$gender)
