##### ODDS RATIO CALCS

###****** CRUDE ***##############################################################################
# following https://www.r-bloggers.com/computing-odds-ratios-in-r/

### locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
datap <- "~/Documents/Flusurvey/data/"

### functions
cr_orwp.ma <- function(datas,category, base,sentence = 0){
  # datas = data
  # category = which column / grouping
  # base = number of unique entries - 2, to a min of 1. 
  # sentence = whether to output a sentence describing the OR
  
  #  Compute the odds ratio between category and medication.antibiotic
  w <- which(colnames(datas) == category)
  tt0 <- table(datas[,w], datas$medication.antibiotic)
  
  for(i in 1:base){
    
    tt <- tt0[c(1,i+1),]
    ###             abx // no abx
    #exposed        n00     n01
    #not exposed    n10     n11
    # OR : (n00 / n10) / (n01 / n11)
    
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
    
    # print out definition
    if(sentence == 1){
      print(c(paste("For ", category,", the odds of taking an antibiotic is ",round(OR,2)," higher if the participant is ", 
                    rownames(tt)[2], " compared to being ", rownames(tt)[1],sep="")))
    }
    
    # Output
    if(i == 1){
      oframe <- data.frame(OR = OR, LowerCI = ORlo, UpperCI = ORhi, alpha = alpha,cat = category, denom = rownames(tt)[1], compar = rownames(tt)[2])
    } else { 
      oframe2 <- data.frame(OR = OR, LowerCI = ORlo, UpperCI = ORhi, alpha = alpha,cat = category,denom = rownames(tt)[1], compar = rownames(tt)[2] )
      oframe <- rbind(oframe, oframe2)
      
    }
    
  }
  
  return(oframe)
  
}

# OLD: orwp.ma(3920,288,1692,2224,alpha = 0.05)

### data
setwd(datap)
#dabx <- readRDS("dabx.rds")

table(dabx$medication.antibiotic,dabx$season)

# age, influenza like illness (ILI) with fever, influenza vaccine received this year, 
# whether a participant visited a medical service during this episode, 
# gender, frequent contact with children or elderly, and underlying health issue (e.g. diabetes).

oos<-rbind(
  cr_orwp.ma(dabx,"agegroup",3,1),
  cr_orwp.ma(dabx,"ili.fever",1,1),
  cr_orwp.ma(dabx,"vaccine.this.year",1,1),
  cr_orwp.ma(dabx,"visit.medical.service.no",1,1),
  cr_orwp.ma(dabx,"gender",1,1),
  cr_orwp.ma(dabx,"frequent.contact.children",1,1),
  cr_orwp.ma(dabx,"frequent.contact.elderly",1,1),
  cr_orwp.ma(dabx,"norisk",1,1))

oom <-rbind(cr_orwp.ma(dabx,"season",5,1),
            cr_orwp.ma(dabx,"region",13,1)) # v wide


####*** ADJUSTED ***################################################################################################################################
#model_ma <- glm(medication.antibiotic ~.,family=binomial(link='logit'),data=train) ## in abx_uni_stats.R

# too slow
#library(oddsratio)
#or_glm(data = train, model = model_ma) # too slow?

oo_a <- as.data.frame(exp(cbind("OR" = coef(model_ma), confint.default(model_ma, level = 0.95))))
colnames(oo_a) <- c("OR","lci","uci")

# confint.default does Wald confidence limits whilst confint() function produces the profile-likelihood limits
# neither wrong, latter perhaps better but using WALD for crude so keep at .default

# NICE output: https://stats.idre.ucla.edu/r/dae/logit-regression/

###**** Latex output ***########################################################################################################################################################################
table_or <- c()
rownames(oo_a) <- gsub("_", " ", rownames(oo_a))

saveRDS(oos, "oo_unad.rds")


oom$denom <- gsub("_", " ", oom$denom)
oom$compar <- gsub("_", " ", oom$compar)


pv <- coef(summary(model_ma))[,'Pr(>|z|)']
pv <- as.numeric(pv)
star <- matrix("",1,length(pv))
for(j in 1:length(pv)){
  if(pv[j]<0.05){star[j]<- "*"}
  if(pv[j]<0.03){star[j]<- "**"}
  if(pv[j]<0.01){star[j]<- "***"} # overwrites
}

oo_a <- cbind(oo_a, t(rbind(pv, star)))
saveRDS(oo_a, "oo_adju.rds")

for(i in 1:length(oos[,1])){
  gg <- grep(paste(oos$cat[i],oos$compar[i],sep=""),rownames(oo_a), fixed=TRUE)
  
  table_or <- rbind(table_or,
                    paste(oos$cat[i], " & & &", "\\",sep=""),
                    paste(oos$denom[i], " & 1 & 1 & \\",sep=""),
                    paste(oos$compar[i], " & ", 
                          round(oos$OR[i],2)," (", round(oos$LowerCI[i],2),",", round(oos$UpperCI[i],2),") & ",
                          round(oo_a$OR[gg],2)," (", round(oo_a$lci[gg],2),",",round(oo_a$uci[gg],2),") & ",
                          round(pv[gg],4), star[gg], "\\",sep="")
  )
}

uu<-unique(oom$cat)
for(i in 1:length(uu)){
  gg <- grep(uu[i],rownames(oo_a))
  ggm <- which(oom$cat == uu[i])
  
  table_or <- rbind(table_or,
                    paste(uu[i], " & & &", "\\",sep=""),
                    paste(oom$denom[ggm[1]], " & 1 & 1 & \\",sep=""))
  
  for(j in 1:length(gg)){
    ggj <- grep(paste(uu[i],oom[ggm[j],"compar"],sep=""),rownames(oo_a), fixed=TRUE)
    
    table_or <- rbind(table_or,
                      paste(oom[ggm[j],"compar"], " & ", 
                            round(oom$OR[ggm[j]],2)," (", round(oom$LowerCI[ggm[j]],2),",", round(oom$UpperCI[ggm[j]],2),") & ",
                            round(oo_a$OR[ggj],2)," (", round(oo_a$lci[ggj],2),",",round(oo_a$uci[ggj],2),") & ",
                            round(pv[ggj],4), star[ggj], "\\",sep="")
    )
  }
}


write.csv(table_or,"table_or.csv")

