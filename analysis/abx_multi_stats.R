#### Multivariate statistical analysis

## TODO: 
# Add in health score variable 


# guided by: https://rcompanion.org/rcompanion/e_07.html
# https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/


### Admin
# libraries
library('flusurvey'); library(reshape2)
library('ggplot2')
library('cowplot')
library(Amelia)
library(texreg)
library(PerformanceAnalytics)
library(car)
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)

theme_set(theme_bw(base_size=24))

# locations
home <- "~/Documents/Flusurvey/"
plots <- "~/Documents/Flusurvey/plots/"
datap <- "~/Documents/Flusurvey/data/"

# data
setwd(datap)
dabx <- readRDS("dabx.rds")

chart.Correlation(dabx,method="spearman",histogram=TRUE,pch=16)

### Multivariable model
model.null = glm(medication.antibiotic ~ 1,data=dabx,
                 family = binomial(link="logit")
)

model.full = glm(medication.antibiotic ~  season + age + gender + ili.fever + region + vaccine.this.year + 
                                      visit.medical.service.no + frequent.contact.children + frequent.contact.elderly + norisk,                   
                 data=dabx,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=Data)

# model.final = model.full?
model.final = glm(medication.antibiotic ~ season + age + gender + ili.fever + region + vaccine.this.year + 
                    visit.medical.service.no + frequent.contact.children + frequent.contact.elderly + norisk,
                  data=dabx,
                  family = binomial(link="logit"),
                  na.action(na.omit)
)

summary(model.final)

Anova(model.final, type="II", test="Wald")

summary(model.final)
summary(model.final)$deviance / summary(model.final)$df.residual # if < 1.5 then not overdispersed (good)

####**** Random effects ***######
setwd(datap)
data.raw <- readRDS("btt_abx.rds")

# remove highest.education as many missing and ili 
rdabx <- data.raw[,c("season","age","gender","ili.fever","region","vaccine.this.year",
                    "visit.medical.service.no","frequent.contact.children",
                    "frequent.contact.elderly","norisk","medication.antibiotic","participant_id")]
w<-which(is.na(rdabx$region))
rdabx <- rdabx[-w,] # remove the 33 with missing regions
saveRDS(rdabx,"rdabx.rds")

# model 
model.r = glmer(medication.antibiotic ~ season + age + gender + ili.fever + region + vaccine.this.year + 
                    visit.medical.service.no + frequent.contact.children + frequent.contact.elderly + norisk + (1 | participant_id),
                  data=rdabx,
                  family = binomial(link="logit"),
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 10)

glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
        (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
      nAGQ = 10)
