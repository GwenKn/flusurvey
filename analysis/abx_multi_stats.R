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
## agex, influenza like illness (ILI) with feverx, influenza vaccine received this yearx, 
# whether a participant visited a medical service during this episodex, 
# genderx, frequent contact with childrenx or elderlyx and underlying health issue (e.g. diabetes).

model.final = glm(medication.antibiotic ~ age + gender + ili.fever  + vaccine.this.year + 
                    visit.medical.service.no + frequent.contact.children + 
                    frequent.contact.elderly + norisk,
                  data=dabx,
                  family = binomial(link="logit"),
                  na.action(na.omit)
)

summary(model.final)

Anova(model.final, type="II", test="Wald")

summary(model.final)
summary(model.final)$deviance / summary(model.final)$df.residual # if < 1.5 then not overdispersed (good)

####**** Random effects ***######
# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
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

model.r = glmer(medication.antibiotic ~  age + gender + ili.fever + 
                  vaccine.this.year + 
                    visit.medical.service.no + frequent.contact.children + 
                  frequent.contact.elderly + norisk + (1 | participant_id),
                  data=rdabx,
                  family = binomial(link="logit"),
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 10)

# scale
rdabx$age <-scale(rdabx$age)

model.final.glmer1 = glmer(medication.antibiotic ~ age + gender + ili.fever + 
                             vaccine.this.year + 
                            visit.medical.service.no + 
                             frequent.contact.children + frequent.contact.elderly + 
                            norisk + (1 | participant_id),
                          data=rdabx,
                          family = binomial(link="logit"),
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 10)

print(model.final.glmer1, corr = FALSE)


#### *** OUTPUT
#### ***table of OR (exponentail) estimates with 95% CI
se <- sqrt(diag(vcov(model.final.glmer1)))

tab <- exp(cbind(Est = fixef(model.final.glmer1), 
              LL = fixef(model.final.glmer1) - 1.96 * se, 
              UL = fixef(model.final.glmer1) + 1.96 * se))
# These are the OR taking into account individual level too

# increase number of iterations 
model.final.glmer2 = glmer(medication.antibiotic ~ age + gender + ili.fever + 
                             vaccine.this.year + 
                             visit.medical.service.no + 
                             frequent.contact.children + frequent.contact.elderly + 
                             norisk + (1 | participant_id),
                           data=rdabx,
                           family = binomial(link="logit"),
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 20)

print(model.final.glmer2, corr = FALSE) # V V V SIMILAR TO GLMER1
summary(model.final.glmer2)

se <- sqrt(diag(vcov(model.final.glmer2)))
# table of OR (exponentail) estimates with 95% CI
tab <- cbind(exp(cbind(Est = fixef(model.final.glmer2), 
                 LL = fixef(model.final.glmer2) - 1.96 * se, 
                 UL = fixef(model.final.glmer2) + 1.96 * se)), p = summary(model.final.glmer2)$coefficients[,4])
# These are the OR taking into account individual level too
