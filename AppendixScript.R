library(magrittr)
library(tidyverse)
library(stats)
library(corrplot)
library(ISwR)

# Loading data, keep df unaltered for original plotting.
df = read.csv("data/project_data.csv") %>%
  dplyr::select(c(2,3,9,16,18:20, 24, 42, 46, 58, 70, 75, 77, 85:87)) %>%
  rename(
    BSS = Berlin.Sleepiness.Scale,
    AIS = Athens.Insomnia.Scale,
    PSQI = Pittsburgh.Sleep.Quality.Index.Score,
    ESS = Epworth.Sleepiness.Scale,
    Time.From.Transplant = Time.from.transplant,
    Recurrence = Recurrence.of.disease,
    Fibrosis = Any.fibrosis,
    PCS = SF36.PCS,
    MCS = SF36.MCS,
    Rejection.Graft.Dysfunction = Rejection.graft.dysfunction
  )

# Restructuring the Liver.Diagnosis Column
df$Liver.Diagnosis <- as.factor(
  ifelse(df$Liver.Diagnosis == 1, "Hep C",
         ifelse(df$Liver.Diagnosis == 2, "Hep B",
                ifelse(df$Liver.Diagnosis == 3, "PSC/PBC/AHA",
                       ifelse(df$Liver.Diagnosis == 4, "Alcohol", "other")))))

# Data Cleaning and Exploration
################################################################################

# Inspecting numerical data:
## ESS outlier shouldn't exist, ESS scores capped at 24, entry should be removed.
## AIS outlier is acceptable as it falls within the score range.
par(mfrow=c(4,2), mar=c(1,2,1,2))
for (i in colnames(df[, !names(df) %in% c("Gender",
                                          "Liver.Diagnosis",
                                          "Rejection.Graft.Dysfunction",
                                          "Recurrence",
                                          "Fibrosis",
                                          "Renal.Failure",
                                          "Depression",
                                          "Corticoid",
                                          "BSS")])){
  boxplot(df[,c(i)], main = i)
  
}
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

# Inspecting categorical and binary data:
par(mfrow=c(3,3), mar=c(3,2,3,2))
for (i in c("Gender",
            "Rejection.Graft.Dysfunction",
            "Recurrence",
            "Fibrosis",
            "Renal.Failure",
            "Depression",
            "Corticoid",
            "BSS"
)){
  hist(df[,c(i)], main = i, breaks = 2)
}
# Separate call for categorical data.
barplot(table(df$Liver.Diagnosis), main = "Liver.Diagnosis")
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
## Can exclude Renal.Failure as its limiting group is only a population of 4;
## unable to support it as a predictor as m/15 ~= 0.
length(df$Renal.Failure[df$Renal.Failure == 1])
## Fibrosis is limited in use with advanced models at it is only compatible
## with models composed of up to two predictors; 2 < m/15 < 3.
length(df$Fibrosis[df$Fibrosis == 1])

## Can exclude PSQI as a feature since its correlation with AIS is high (>80).
## Would be favourable given 85 of 268 entries have a PSQI of NA.
## Can attempt to remove entries with NA scores for BSS, ESS, and AIS since
## number of NA entries is low.
summary(df[,c("BSS", "ESS", "PSQI", "AIS")])
corrplot(cor(df[!is.na(df$PSQI),c("BSS", "ESS", "PSQI", "AIS")], 
             use = "pairwise"), method = "number")

# Removing critical NA entries.
df2 = df[!is.na(df$ESS) &
           !is.na(df$BSS) &
           !is.na(df$AIS),]
## Only lost 19 entries.
dim(df2) 

# Removing entry with invalid ESS score.
df2 = df2[df2$ESS <= 24,]

## Still strong correlation between PSQI and AIS.
summary(df2[,c("BSS", "ESS", "PSQI", "AIS")])
corrplot(cor(df[!is.na(df2$PSQI),c("BSS", "ESS", "PSQI", "AIS")], 
             use = "pairwise"), method = "number")

# Inspecting numerical data post-cleaning:
par(mfrow=c(4,2), mar=c(1,2,1,2))
for (i in colnames(df2[, !names(df2) %in% c("Gender",
                                            "Liver.Diagnosis",
                                            "Rejection.Graft.Dysfunction",
                                            "Recurrence",
                                            "Fibrosis",
                                            "Renal.Failure",
                                            "Depression",
                                            "Corticoid",
                                            "BSS")])){
  boxplot(df2[,c(i)], main = i)
  
}
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))


# Inspecting categorical and binary data post-cleaning:
# Representing binary data as histograms.
par(mfrow=c(3,3), mar=c(3,2,3,2))
for (i in c("Gender",
            "Rejection.Graft.Dysfunction",
            "Recurrence",
            "Fibrosis",
            "Renal.Failure",
            "Depression",
            "Corticoid",
            "BSS"
)){
  hist(df2[,c(i)], main = i, breaks = 2)
}
# Separate barplot for categorical data.
barplot(table(df2$Liver.Diagnosis), main = "Liver.Diagnosis")
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## Feature limit by each parameter's limiting group.
length(df2$Gender[df2$Gender == 1])/15
length(df2$Rejection.Graft.Dysfunction[df2$Rejection.Graft.Dysfunction == 1])/15
length(df2$Recurrence[df2$Recurrence == 1])/15
length(df2$Fibrosis[df2$Fibrosis == 1])/15
length(df2$Renal.Failure[df2$Renal.Failure == 1])/15
length(df2$Depression[df2$Depression == 1])/15
length(df2$Corticoid[df2$Corticoid == 1])/15
length(df2$Liver.Diagnosis[df2$Liver.Diagnosis == "Hep B"])/15
length(df2$BSS[df2$BSS == 1])/15
nrow(df2)/15

# Copy of dataset for manipulation.
df3 <- df %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(BMI))

# Prevalence
################################################################################

## Creating binary columns for sleep scales

df$ESS_binary <- ifelse(df$ESS >10, 1, 0)
df$PSQI_binary <- ifelse(df$PSQI >5, 1, 0)
df$AIS_binary <- ifelse(df$AIS > 5, 1, 0)
df$Sleep_disturbance <- ifelse(df$ESS | df$PSQI | df$AIS | df$BSS, 1, 0)

## Calculation of prevalence of sleep disturbance from each scale
summary(df$ESS_binary) # mean = 0.2669, 26.69% prevalence
summary(df$PSQI_binary) # mean = 0.5464, 54.64% prevalence
summary(df$AIS_binary) # mean = 0.5534, 55.34% prevalence
summary(df$BSS) # mean = 0.3895, 38.95% prevalence

summary(df$Sleep_disturbance) 
#' mean = 1, 100% of participants had some sort of sleep 
#' disturbance judged by clinically accepted thresholds, 
#' 6 people did not answer any of the sleep scales


# Modeling BSS, AIS, and ESS
################################################################################

# PSQI excluded

## Log Regression Model with BSS as the response variable
BSS_lm <- glm(BSS~Gender+Age+BMI+Time.From.Transplant+Liver.Diagnosis+
                Recurrence+Rejection.Graft.Dysfunction+Fibrosis+Renal.Failure+
                Depression+Corticoid, data = df, family = binomial)
summary(BSS_lm) # Intercept = 0.00183, BMI p = 5.7e-0.9
# n = 262
# m = 262*0.3895 = 102
# predictors < 102/15
# p < 6
BSS_lm2 <- glm(BSS~BMI, data = df, family = binomial)            # AIC = 284.97
BSS_lm3 <- glm(BSS~BMI+Recurrence, data = df, family = binomial) # AIC = 284.41*
BSS_lm4 <- glm(BSS~BMI+Recurrence+Liver.Diagnosis, data = df, 
               family = binomial)                                # AIC = 289.82

anova(BSS_lm2, BSS_lm3, test = "Chisq") # p = 0.1098
anova(BSS_lm2, BSS_lm4, test = "Chisq") # p = 0.3979
anova(BSS_lm3, BSS_lm4, test = "Chisq") # p = 0.6281
# BSS_lm3 is the optimal model to use, df = 2

## Linear model with AIS as response variable
AIS_lm <- glm(AIS~Gender+Age+BMI+Time.From.Transplant+Liver.Diagnosis+
                Recurrence+Rejection.Graft.Dysfunction+Fibrosis+Renal.Failure+
                Depression+Corticoid, data = df3)

summary(AIS_lm) 
# Intercept p = 0.0061, Age p = 0.0311, Recurrence p = 0.0260, Corticoid = 0.0175
# n = 262
# m = 117
# p < 7

AIS_lm2 <- glm(AIS~Corticoid, data = df3)                        # AIC = 923.45
AIS_lm3 <- glm(AIS~Corticoid + Recurrence, data = df3)           # AIC = 921.03
AIS_lm4 <- glm(AIS~Corticoid + Recurrence + Age, data = df3)     # AIC = 913.76*
AIS_lm5 <- glm(AIS~Corticoid + Recurrence + Age + Gender, 
               data = df3)                                       # AIC = 913.76
AIS_lm6 <- glm(AIS~Corticoid + Recurrence + Age + Gender + Time.From.Transplant, 
               data = df3)                                       # AIC = 915.37
AIS_lm7 <- glm(AIS~Corticoid + Recurrence + Age + Time.From.Transplant, 
               data = df3)                                       # AIC = 915.65
AIS_lm8 <- glm(AIS~Corticoid + Recurrence + Age + Gender + Time.From.Transplant 
               + BMI, data = df3)                                # AIC = 915.05* 
AIS_lm9 <- glm(AIS~Corticoid + Recurrence + Age + Gender + Time.From.Transplant 
               + BMI + Depression, data = df3)                   # AIC = 915.81
AIS_lm10 <- glm(AIS~Corticoid + Recurrence + Age + Gender + Time.From.Transplant 
                + Depression, data = df3)                        # AIC = 915.11

anova(AIS_lm9, AIS_lm2, test = "Chisq") # p = 0.002935 **
anova(AIS_lm9, AIS_lm3, test = "Chisq") # p = 0.009699 **
anova(AIS_lm9, AIS_lm4, test = "Chisq") # p = 0.2193
anova(AIS_lm4, AIS_lm2, test = "Chisq") # p = 0.0009341 ***
anova(AIS_lm4, AIS_lm3, test = "Chisq") # p = 0.00228 **
anova(AIS_lm4, AIS_lm5, test = "Chisq") # p = 0.1635
anova(AIS_lm4, AIS_lm8, test = "Chisq") # p = 0.2073
anova(AIS_lm4, AIS_lm9, test = "Chisq") # p = 0.2193
#' Linear model 4 is the optimal model - it has the lowest AIC and anova shows
#' p < 0.05 against simpler models and none of the more complex models have a 
#' p < 0.05 against model 4
#' 
# Linear model 4 has df = 3



# Linear model with ESS as response variable
ESS_lm12 <- lm(ESS ~ Gender+Age+BMI+Time.From.Transplant+
                 Liver.Diagnosis+Recurrence+Rejection.Graft.Dysfunction+
                 Fibrosis+Renal.Failure+Depression+Corticoid, data = df3)
summary(ESS_lm12)
ESS_lm1 <- lm(ESS ~ Gender, data = df3)
ESS_lm2 <- lm(ESS ~Gender + Age, data = df3)
anova(ESS_lm1, ESS_lm2)
AIC(ESS_lm1)
AIC(ESS_lm2)
#Model 1 is better

df3 <- df2 %>%
  filter(!is.na(BMI))
ESS_lm1 <- lm(ESS ~ Gender, data = df3)
ESS_lm3 <- lm(ESS~Gender + BMI, data = df3)
anova(ESS_lm1, ESS_lm3)
AIC(ESS_lm1)
AIC(ESS_lm3)
#Model 1 is better

ESS_lm4 <- lm(ESS~Gender+Time.From.Transplant, data = df3)
anova(ESS_lm1, ESS_lm4)
AIC(ESS_lm1)
AIC(ESS_lm4)
#Model 1 is bstter

ESS_lm5 <- lm(ESS~Gender + Liver.Diagnosis, data = df3)
anova(ESS_lm1,ESS_lm5)
AIC(ESS_lm5)
#Model 1 is better

ESS_lm6 <- lm(ESS~Gender + Recurrence, data = df3)
anova(ESS_lm1, ESS_lm6)
AIC(ESS_lm6)
#1342.351 < 1342.717, model 6 is better

ESS_lm7 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction, 
              data = df3)
anova(ESS_lm6, ESS_lm7)
AIC(ESS_lm7)
#p-value < 0.05, indicating lm7 fits the data better, no need to perform AIC.

ESS_lm8 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + 
                Fibrosis, data = df3)
anova(ESS_lm7, ESS_lm8)
AIC(ESS_lm7)
AIC(ESS_lm8)
#Model 7 is better

ESS_lm9 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + 
                Renal.Failure, data = df3)
anova(ESS_lm7, ESS_lm9)
AIC(ESS_lm7)
AIC(ESS_lm9)
#Model 7 is better

ESS_lm10 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + 
                 Depression, data = df3)
anova(ESS_lm7, ESS_lm10)
AIC(ESS_lm10)
#Model 7 is better

ESS_lm11 <- lm(ESS~Gender + Recurrence + Rejection.Graft.Dysfunction + 
                 Corticoid, data = df3)
anova(ESS_lm7, ESS_lm11)
AIC(ESS_lm11)
#1334.463 < 1334.9, model 11 is better

# Modeling MCS and PCS
################################################################################

## AIS and ESS support up to 16 features, BSS supports up to 6.
## All 3 compatible for use in a more advanced model.

plot(df2[,c("MCS", "PCS", "AIS", "ESS", "BSS")])
corrplot(cor(df2[,c("MCS", "PCS", "AIS", "ESS", "BSS")], 
             use = "pairwise"), method = "number")


# Generating models with all parameters.
MCS3 = lm(MCS ~ BSS + AIS + ESS, data= df2)
PCS3 = lm(PCS ~ BSS + AIS + ESS, data= df2)

# Assessing MCS3.
summary(MCS3)
deviance(MCS3)
AIC(MCS3)
## BSS is incompatible as a predictor, high deviance and AIC scores.

# Creating simplified model without BSS.
MCS2 = lm(MCS ~ AIS + ESS, data= df2)

# Assessing MCS2.
summary(MCS2)
deviance(MCS2)
AIC(MCS2)
anova(MCS2, MCS3)
## AIC decreased.
## Simpler model fits data better.

# Checking simplest models.
MCSA = lm(MCS ~ AIS, data = df2)
MCSE = lm(MCS ~ ESS, data = df2)

# Assessing MCSA.
summary(MCSA)
deviance(MCSA)
AIC(MCSA)
anova(MCSA, MCS2)
## AIC increased.
## More complex model fits data better.

# Assessing MCSE.
summary(MCSE)
deviance(MCSE)
AIC(MCSE)
anova(MCSE, MCS2)
## AIC increased.
## More complex model fits data better.

## MCS2 fits data the best

# Assessing PCS3.
summary(PCS3)
deviance(PCS3)
AIC(PCS3)
## All predictors have significant beta-coefficients.

# Generating simpler models.
PCS.AE = lm(PCS ~ AIS + ESS, data = df2)
PCS.AB = lm(PCS ~ AIS + BSS, data = df2)
PCS.EB = lm(PCS ~ ESS + BSS, data = df2)

# Assessing PCS.AB.
summary(PCS.AB)
deviance(PCS.AB)
AIC(PCS.AB)
anova(PCS.AB, PCS3)
## AIC increased.
## More complex model fits data better.

# Assessing PCS.EB.
summary(PCS.EB)
deviance(PCS.EB)
AIC(PCS.EB)
anova(PCS.EB, PCS3)
## AIC increased.
## More complex model fits data better.

# Assessing PCS.AE.
summary(PCS.AE)
deviance(PCS.AE)
AIC(PCS.AE)
anova(PCS.AE, PCS3)
## AIC decreased.
## ... More complex model fits data better.

# Checking simplest models versus PCS.AE.
PCSA = lm(PCS ~ AIS, data = df2)
PCSE = lm(PCS ~ ESS, data = df2)

# Assessing PCSA.
summary(PCSA)
deviance(PCSA)
AIC(PCSA)
anova(PCSA, PCS.AE)
## AIC increased.
## More complex model fits data better.

# Assessing PCSE.
summary(PCSE)
deviance(PCSE)
AIC(PCSE)
anova(PCSE, PCS.AE)
## AIC increased.
## More complex model fits data better.