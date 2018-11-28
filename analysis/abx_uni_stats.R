### Univariate statistical analysis

## TODO: 
# Add in health score variable 


# guided by: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
### Admin
# libraries
library('flusurvey'); library(reshape2)
library('ggplot2')
library('cowplot')
library(Amelia)
library(texreg)

theme_set(theme_bw(base_size=24))

# locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
datap <- "~/Documents/Flusurvey/data/"

# data
setwd(datap)
data.raw <- readRDS("btt_abx.rds")

### Cleaning
# which parameters? 
data <- data.raw[,c("season","gender","ili","ili.fever","region","vaccine.this.year",
                    "visit.medical.service.no","highest.education","frequent.contact.children",
                    "frequent.contact.elderly","norisk","medication.antibiotic","agegroup")]
data$hs <- (data.raw$min.health.score - data.raw$baseline.health.score)/data.raw$baseline.health.score

sapply(data,function(x) sum(is.na(x)))
#missmap(data, main = "Missing values vs observed")

# remove highest.education as many missing and ili 
# dabx <- data.raw[,c("season","gender","ili.fever","region","vaccine.this.year",
#                     "visit.medical.service.no","frequent.contact.children",
#                     "frequent.contact.elderly","norisk","medication.antibiotic","agegroup")]

dabx <- data.raw[,c("gender","ili.fever","vaccine.this.year",
                    "visit.medical.service.no","frequent.contact.children",
                    "frequent.contact.elderly","norisk","medication.antibiotic","agegroup")]
sapply(dabx,function(x) sum(is.na(x))) # no NAs

#w<-which(is.na(dabx$region))
#dabx <- dabx[-w,] # remove the 33 with missing regions
#missmap(dabx, main = "Missing values vs observed")
dabx$vaccine.this.year <- droplevels(dabx$vaccine.this.year) # removes "don't know" 
saveRDS(dabx,"dabx.rds")

is.factor(dabx$gender)
contrasts(dabx$gender)
is.factor(dabx$medication.antibiotic)
contrasts(dabx$medication.antibiotic)

# train and test
train <- dabx[1:34000,]
test <- dabx[34001:(dim(dabx)[1]),] # Last ~4,000 into test
100*dim(test)[1]/dim(dabx)[1] # ~ 10%

#### check shouldn\t pic train / test randomly 
## Seem to be relatively evenly spread so ok to just train on last set
# plot(seq(1,dim(train)[1],1),train$season)
# plot(seq(1,dim(train)[1],1),train$age)
# plot(seq(1,dim(train)[1],1),train$gender)
# plot(seq(1,dim(train)[1],1),train$ili.fever)
# plot(seq(1,dim(train)[1],1),train$region)
# plot(seq(1,dim(train)[1],1),train$vaccine.this.year)
# plot(seq(1,dim(train)[1],1),train$visit.medical.service.no)
# plot(seq(1,dim(train)[1],1),train$frequent.contact.children)
# plot(seq(1,dim(train)[1],1),train$frequent.contact.elderly)
# plot(seq(1,dim(train)[1],1),train$norisk)


###### MODEL
model_ma <- glm(medication.antibiotic ~.,family=binomial(link='logit'),data=train)

summary(model_ma)
# SIGNIFICANT: AGE / ILI.FEVER / VACCINETHISYEARNO / VISITMEDICAL /  NORISK
# MARGINAL SIGNIFICANCE: FREQCONTACTCHILDREN / GENDER => both drop out with less than 32000 in training set
# Age: unit increase in age increases log odds by 0.009
# ILI.FEVER: having fever increases the log odds by 1.3
# VACCINETHISYEAR: not having the vaccine this year decreases the log odds by 0.29 (=> don't have vaccine less likely to have abx)
# VISITMEDICAL: not visiting a medical service decreases the log odds by 3.3
# NORISK: not having an underlying risk decreases the log odds by 0.54 (got underlying issue => take more antibiotics)

# FREQCONTACTCHILDREN: having contact increases the log odds by 0.19
# GENDER: being female increaes the log odds by 0.15

anova(model_ma,test="Chisq")
# visit.medical.service.no and ili.fever have big impact on reducing deviance

####### How well does it fit? 
fitted.results <- predict(model_ma,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

aa <- as.numeric(test$medication.antibiotic) - 1 # need the -1 to go from true being 2

misClasificError <- mean(fitted.results != aa)
print(paste('Accuracy',1-misClasificError)) # 89%! pretty good?!

