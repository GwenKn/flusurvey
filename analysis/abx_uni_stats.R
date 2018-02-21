### Univariate statistical analysis

# guided by: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
### Admin
# libraries
library('flusurvey')
library('ggplot2')
library('cowplot')
library(Amelia)

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
data <- data.raw[,c("season","age","gender","ili","ili.fever","region","vaccine.this.year",
                    "visit.medical.service.no","highest.education","frequent.contact.children",
                    "frequent.contact.elderly","norisk")]

sapply(data,function(x) sum(is.na(x)))
missmap(data, main = "Missing values vs observed")
# remove highest.education as many missing and ili 
dabx <- data.raw[,c("season","age","gender","ili.fever","region","vaccine.this.year",
                    "visit.medical.service.no","frequent.contact.children",
                    "frequent.contact.elderly","norisk","medication.antibiotic")]
missmap(dabx, main = "Missing values vs observed")

is.factor(dabx$gender)
contrasts(dabx$gender)
is.factor(dabx$medication.antibiotic)
contrasts(dabx$medication.antibiotic)

# train and test
train <- dabx[1:30000,]
test <- dabx[30001:(dim(dabx)[1]),]

model <- glm(medication.antibiotic ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model,test="Chisq")

# How well does it fit? 
fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- length(fitted.results != test$medication.antibiotic)
print(paste('Accuracy',1-misClasificError))
