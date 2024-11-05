rm(list=ls())
wd <- dirname(rstudioapi::getSourceEditorContext()$path) # Set the working directiory wherever you save this R script
setwd(wd)
library(quantmod)
library(writexl)
library(readxl)
library(progress)
library(dplyr)
library(lfe)
library(lmtest)
library(texreg)
library(quantreg)
library(glmnet)
library(estimatr)
library(car)

data <- read_xlsx("SMI_Weekly_Sentiments_v6.xlsx")

data$month <- format(as.Date(data$Date, format="%Y-%m-%d"), "%m")

# SwissFinBERT

ols_swissfinbert <- lm(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_swissfinbert)

rob_swissfinbert <- lm_robust(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_swissfinbert)

olsame_swissfinbert <- lm(data$return ~ data$Sentiment_SwissFinBERT + data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(olsame_swissfinbert)

q10_swissfinbert <- rq(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_swissfinbert)

q25_swissfinbert <- rq(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_swissfinbert)

med_swissfinbert <- rq(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_swissfinbert)

q75_swissfinbert <- rq(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_swissfinbert)

q90_swissfinbert <- rq(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.90)
summary(q90_swissfinbert)


fe_swissfinbert <- felm(data$return ~ data$Sentiment_SwissFinBERT_t1 + data$Sentiment_SwissFinBERT_t2 + data$Sentiment_SwissFinBERT_t3 + data$Sentiment_SwissFinBERT_t4 + data$Sentiment_SwissFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) +as.factor(data$month) | data$Ticker)
summary(fe_swissfinbert)


# GFinBERT

ols_gfinbert <- lm(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_gfinbert)

rob_gfinbert <- lm_robust(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_gfinbert)

olsame_gfinbert <- lm(data$return ~ data$Sentiment_GFinBERT + data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(olsame_gfinbert)

q10_gfinbert <- rq(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_gfinbert)

q25_gfinbert <- rq(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_gfinbert)

med_gfinbert <- rq(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_gfinbert)

q75_gfinbert <- rq(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_gfinbert)

q90_gfinbert <- rq(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.90)
summary(q90_gfinbert)

fe_gfinbert <- felm(data$return ~ data$Sentiment_GFinBERT_t1 + data$Sentiment_GFinBERT_t2 + data$Sentiment_GFinBERT_t3 + data$Sentiment_GFinBERT_t4 + data$Sentiment_GFinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month) | data$Ticker)
summary(fe_gfinbert)


# GSentBERT

ols_gsentbert <- lm(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_gsentbert)

rob_gsentbert <- lm_robust(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_gsentbert)

fe_gsentbert <- felm(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month) | data$Ticker)
summary(fe_gsentbert)

q10_gsentbert <- rq(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_gsentbert)

q25_gsentbert <- rq(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_gsentbert)

med_gsentbert <- rq(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_gsentbert)

q75_gsentbert <- rq(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_gsentbert)

q90_gsentbert <- rq(data$return ~ data$Sentiment_GSentBERT_t1 + data$Sentiment_GSentBERT_t2 + data$Sentiment_GSentBERT_t3 + data$Sentiment_GSentBERT_t4 + data$Sentiment_GSentBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.90)
summary(q90_gsentbert)

# FinBERT

ols_finbert <- lm(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_finbert)

rob_finbert <- lm_robust(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_finbert)

fe_finbert <- felm(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month) | data$Ticker)
summary(fe_finbert)

q10_finbert <- rq(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_finbert)

q25_finbert <- rq(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_finbert)

med_finbert <- rq(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_finbert)

q75_finbert <- rq(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_finbert)

q90_finbert <- rq(data$return ~ data$Sentiment_FinBERT_t1 + data$Sentiment_FinBERT_t2 + data$Sentiment_FinBERT_t3 + data$Sentiment_FinBERT_t4 + data$Sentiment_FinBERT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.90)
summary(q90_finbert)

# GPT

ols_gpt <- lm(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_gpt)

rob_gpt <- lm_robust(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_gpt)

fe_gpt <- felm(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month) | data$Ticker)
summary(fe_gpt)

q10_gpt <- rq(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_gpt)

q25_gpt <- rq(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_gpt)

med_gpt <- rq(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_gpt)

q75_gpt <- rq(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_gpt)

q90_gpt <- rq(data$return ~ data$Sentiment_GPT_t1 + data$Sentiment_GPT_t2 + data$Sentiment_GPT_t3 + data$Sentiment_GPT_t4 + data$Sentiment_GPT_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.90)
summary(q90_gpt)

# Gemini

ols_gemini <- lm(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(ols_gemini)

rob_gemini <- lm_robust(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month))
summary(rob_gemini)

fe_gemini <- felm(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month) | data$Ticker)
summary(fe_gemini)

q10_gemini <- rq(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.1)
summary(q10_gemini)

q25_gemini <- rq(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.25)
summary(q25_gemini)

med_gemini <- rq(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.5)
summary(med_gemini)

q75_gemini <- rq(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.75)
summary(q75_gemini)

q90_gemini <- rq(data$return ~ data$Sentiment_Gemini_t1 + data$Sentiment_Gemini_t2 + data$Sentiment_Gemini_t3 + data$Sentiment_Gemini_t4 + data$Sentiment_Gemini_t5 + data$return_t1  + data$return_t2 + data$return_t3 + data$return_t4 + data$return_t5 + I(data$return_t1**2)+ I(data$return_t2**2)+ I(data$return_t3**2)+ I(data$return_t4**2)+ I(data$return_t5**2) + as.factor(data$month), tau = 0.9)
summary(q90_gemini)


##### TEST

library(tseries)

#Heteroskedasticity with Breush Pagan Test

bptest(ols_swissfinbert)
bptest(ols_gfinbert)
bptest(ols_gsentbert)
bptest(ols_finbert)
bptest(ols_gpt)
bptest(ols_gemini)


# Autocorrelation  with Durbin-Watson Test

dwtest(ols_swissfinbert)
dwtest(ols_gfinbert)
dwtest(ols_gsentbert)
dwtest(ols_finbert)
dwtest(ols_gpt)
dwtest(ols_gemini)

# Stationarity adf

adf.test(data$return)



### LaTeX Output

# Rename coefficients in ols models
names(ols_swissfinbert$coefficients) <- c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(ols_gfinbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(ols_gsentbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(ols_finbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(ols_gpt$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(ols_gemini$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")



models <- list(ols_swissfinbert, ols_gfinbert, ols_gsentbert, ols_finbert, ols_gpt, ols_gemini)


texreg(models,
       digits = 3,
       custom.model.names = c("SwissFinBERT", "GFinBERT","GSentBERT" ,"FinBERT", "GPT", "Gemini"))

# Rename coefficients in ols models --> robust se
names(rob_swissfinbert$coefficients) <- c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(rob_gfinbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(rob_gsentbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(rob_finbert$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(rob_gpt$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(rob_gemini$coefficients) <-     c("Intercept", "SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")



models <- list(rob_swissfinbert, rob_gfinbert, rob_gsentbert, rob_finbert, rob_gpt, rob_gemini)


texreg(models,
       digits = 3,
       custom.model.names = c("SwissFinBERT", "GFinBERT","GSentBERT" ,"FinBERT", "GPT", "Gemini"))

# Rename coefficients in fe models
names(fe_swissfinbert$coefficients) <- c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(fe_gfinbert$coefficients) <-     c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(fe_gsentbert$coefficients) <-     c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(fe_finbert$coefficients) <-     c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(fe_gpt$coefficients) <-     c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(fe_gemini$coefficients) <-     c("SentL1", "SentL2", "SentL3", "SentL4", "SentL5", "RL1", "RL2", "RL3", "RL4", "RL5", "R^2L1", "R^2L2", "R^2L3", "R^2L4", "R^2L5", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")




femodels <- list(fe_swissfinbert, fe_gfinbert, fe_gsentbert, fe_finbert, fe_gpt, fe_gemini)



texreg(femodels,
       digits = 3,
       custom.model.names = c("SwissFinBERT", "GFinBERT","GSentBERT" ,"FinBERT", "GPT", "Gemini"))

# Quantiles SwissFinBERT

models <- list(q10_swissfinbert, q25_swissfinbert, med_swissfinbert, q75_swissfinbert, q90_swissfinbert)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))

# Quantiles GFinBERT

models <- list(q10_gfinbert, q25_gfinbert, med_gfinbert, q75_gfinbert, q90_gfinbert)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))

# Quantiles GSentBERT

models <- list(q10_gsentbert, q25_gsentbert, med_gsentbert, q75_gsentbert, q90_gsentbert)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))

# Quantiles FinBERT

models <- list(q10_finbert, q25_finbert, med_finbert, q75_finbert, q90_finbert)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))

# Quantiles GPT

models <- list(q10_gpt, q25_gpt, med_gpt, q75_gpt, q90_gpt)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))

# Quantiles Gemini

models <- list(q10_gemini, q25_gemini, med_gemini, q75_gemini, q90_gemini)

texreg(models,
       digits = 3,
       custom.model.names = c("10", "25", "Median", "75", "90"))


#####################
# Correlation Matrix#
#####################

corrmatr_finbert <- cov2cor(vcov(ols_finbert))
corrmatr_finbert

write_xlsx(as.data.frame(corrmatr_swissfinbert), "VCOV_SwissFinBERT_OLS.xlsx")

############################
# Predictions SwissFinBERT #
############################

data$predictions <- predict(ols_swissfinbert, data.frame(data$Sentiment_SwissFinBERT_t1, data$Sentiment_SwissFinBERT_t2, data$Sentiment_SwissFinBERT_t3, data$Sentiment_SwissFinBERT_t4, data$Sentiment_SwissFinBERT_t5, data$return_t1, data$return_t2, data$return_t3, data$return_t4, data$return_t5, data$return_t1, data$return_t2, data$return_t3, data$return_t4, data$return_t5, data$month))
data$predictions[is.na(data$predictions)] <- 0
min(data$predictions)
max(data$predictions)

write_xlsx(data, "Backtest_data.xlsx")

