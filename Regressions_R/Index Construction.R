rm(list=ls())
wd <- dirname(rstudioapi::getSourceEditorContext()$path) # Set the working directiory wherever you save this R script
setwd(wd)
library(quantmod)
library(writexl)
library(readxl)
library(progress)
library(dplyr)
library(lfe)


# Import Data

sentiment <- read_xlsx("Master_Dataset_vCompact5.xlsx")
smi_weekly_returns <- read_xlsx("SMI_Weekly_Returns.xlsx")


# Matching Sentiment form GFinBERT Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$GFinBERT_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$GFinBERT_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$GFinBERT_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_GFinBERT <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)

# Matching Sentiment form FinBERT Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$FinBERT_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$FinBERT_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$FinBERT_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_FinBERT <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)

# Matching Sentiment form GSentBERT Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$GSentBERT_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$GSentBERT_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$GSentBERT_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_GSentBERT <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)

# Matching Sentiment form GPT Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$GPT3_5_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$GPT3_5_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$GPT3_5_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_GPT <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)


# Matching Sentiment form Gemini Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$Gemini_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$Gemini_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$Gemini_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_Gemini <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)

# Matching Sentiment form SwissFinBERT Weekly
smi_weekly_returns$Positive <- 0
smi_weekly_returns$Neutral <- 0
smi_weekly_returns$Negative <- 0

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in seq(nrow(smi_weekly_returns))) {
  filtered_df <- sentiment[as.Date(sentiment$Date) <= (as.Date(smi_weekly_returns$Date[i])) & as.Date(sentiment$Date) >= (as.Date(smi_weekly_returns$Date[i])-7), ]
  smi_weekly_returns$Positive[i] <- sum(filtered_df$SwissFinBERT_label == 2)
  smi_weekly_returns$Neutral[i] <- sum(filtered_df$SwissFinBERT_label == 1)
  smi_weekly_returns$Negative[i] <- sum(filtered_df$SwissFinBERT_label == 0)
  pb$tick()
  Sys.sleep((1/10000))
}

smi_weekly_returns$Sentiment_SwissFinBERT <- (smi_weekly_returns$Positive - smi_weekly_returns$Negative) / (smi_weekly_returns$Positive + smi_weekly_returns$Neutral + smi_weekly_returns$Negative)

# Lags

smi_weekly_returns$Sentiment_GFinBERT_t1 <- 0
smi_weekly_returns$Sentiment_GFinBERT_t2 <- 0
smi_weekly_returns$Sentiment_GFinBERT_t3 <- 0
smi_weekly_returns$Sentiment_GFinBERT_t4 <- 0
smi_weekly_returns$Sentiment_GFinBERT_t5 <- 0

smi_weekly_returns$Sentiment_FinBERT_t1 <-0
smi_weekly_returns$Sentiment_FinBERT_t2 <-0
smi_weekly_returns$Sentiment_FinBERT_t3 <-0
smi_weekly_returns$Sentiment_FinBERT_t4 <-0
smi_weekly_returns$Sentiment_FinBERT_t5 <-0

smi_weekly_returns$Sentiment_GSentBERT_t1 <- 0
smi_weekly_returns$Sentiment_GSentBERT_t2 <- 0
smi_weekly_returns$Sentiment_GSentBERT_t3 <- 0
smi_weekly_returns$Sentiment_GSentBERT_t4 <- 0
smi_weekly_returns$Sentiment_GSentBERT_t5 <- 0

smi_weekly_returns$Sentiment_GPT_t1 <- 0
smi_weekly_returns$Sentiment_GPT_t2 <- 0
smi_weekly_returns$Sentiment_GPT_t3 <- 0
smi_weekly_returns$Sentiment_GPT_t4 <- 0
smi_weekly_returns$Sentiment_GPT_t5 <- 0

smi_weekly_returns$Sentiment_Gemini_t1 <- 0
smi_weekly_returns$Sentiment_Gemini_t2 <- 0
smi_weekly_returns$Sentiment_Gemini_t3 <- 0
smi_weekly_returns$Sentiment_Gemini_t4 <- 0
smi_weekly_returns$Sentiment_Gemini_t5 <- 0

smi_weekly_returns$Sentiment_SwissFinBERT_t1 <- 0
smi_weekly_returns$Sentiment_SwissFinBERT_t2 <- 0
smi_weekly_returns$Sentiment_SwissFinBERT_t3 <- 0
smi_weekly_returns$Sentiment_SwissFinBERT_t4 <- 0
smi_weekly_returns$Sentiment_SwissFinBERT_t5 <- 0

#GFinBERT
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GFinBERT_t1[i] <- smi_weekly_returns$Sentiment_GFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GFinBERT_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GFinBERT_t2[i] <- smi_weekly_returns$Sentiment_GFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GFinBERT_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GFinBERT_t3[i] <- smi_weekly_returns$Sentiment_GFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GFinBERT_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GFinBERT_t4[i] <- smi_weekly_returns$Sentiment_GFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GFinBERT_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GFinBERT_t5[i] <- smi_weekly_returns$Sentiment_GFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GFinBERT_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#FinBERT
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_FinBERT_t1[i] <- smi_weekly_returns$Sentiment_FinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_FinBERT_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_FinBERT_t2[i] <- smi_weekly_returns$Sentiment_FinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_FinBERT_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_FinBERT_t3[i] <- smi_weekly_returns$Sentiment_FinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_FinBERT_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_FinBERT_t4[i] <- smi_weekly_returns$Sentiment_FinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_FinBERT_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_FinBERT_t5[i] <- smi_weekly_returns$Sentiment_FinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_FinBERT_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#GSentBERT
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GSentBERT_t1[i] <- smi_weekly_returns$Sentiment_GSentBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GSentBERT_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GSentBERT_t2[i] <- smi_weekly_returns$Sentiment_GSentBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GSentBERT_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GSentBERT_t3[i] <- smi_weekly_returns$Sentiment_GSentBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GSentBERT_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GSentBERT_t4[i] <- smi_weekly_returns$Sentiment_GSentBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GSentBERT_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GSentBERT_t5[i] <- smi_weekly_returns$Sentiment_GSentBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GSentBERT_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#GPT
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GPT_t1[i] <- smi_weekly_returns$Sentiment_GPT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GPT_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GPT_t2[i] <- smi_weekly_returns$Sentiment_GPT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GPT_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GPT_t3[i] <- smi_weekly_returns$Sentiment_GPT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GPT_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GPT_t4[i] <- smi_weekly_returns$Sentiment_GPT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GPT_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_GPT_t5[i] <- smi_weekly_returns$Sentiment_GPT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_GPT_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#Gemini
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_Gemini_t1[i] <- smi_weekly_returns$Sentiment_Gemini[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_Gemini_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_Gemini_t2[i] <- smi_weekly_returns$Sentiment_Gemini[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_Gemini_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_Gemini_t3[i] <- smi_weekly_returns$Sentiment_Gemini[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_Gemini_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_Gemini_t4[i] <- smi_weekly_returns$Sentiment_Gemini[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_Gemini_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_Gemini_t5[i] <- smi_weekly_returns$Sentiment_Gemini[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_Gemini_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#SwissFinBERT
pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_SwissFinBERT_t1[i] <- smi_weekly_returns$Sentiment_SwissFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_SwissFinBERT_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_SwissFinBERT_t2[i] <- smi_weekly_returns$Sentiment_SwissFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_SwissFinBERT_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_SwissFinBERT_t3[i] <- smi_weekly_returns$Sentiment_SwissFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_SwissFinBERT_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_SwissFinBERT_t4[i] <- smi_weekly_returns$Sentiment_SwissFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_SwissFinBERT_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$Sentiment_SwissFinBERT_t5[i] <- smi_weekly_returns$Sentiment_SwissFinBERT[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$Sentiment_SwissFinBERT_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

#Returns

smi_weekly_returns$return_t1 <- 0
smi_weekly_returns$return_t2 <- 0
smi_weekly_returns$return_t3 <- 0
smi_weekly_returns$return_t4 <- 0
smi_weekly_returns$return_t5 <- 0


pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t1[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 1)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t1[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t2[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t2[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 2)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t2[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t3[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 3)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t3[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t4[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 4)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t4[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}

pb <- progress_bar$new(total = nrow(smi_weekly_returns))
for (i in 1:nrow(smi_weekly_returns)) {
  tryCatch({
    smi_weekly_returns$return_t5[i] <- smi_weekly_returns$return[
      smi_weekly_returns$Ticker == smi_weekly_returns$Ticker[i] & 
        smi_weekly_returns$t == (smi_weekly_returns$t[i] - 5)
    ]
  }, error = function(e) {
    smi_weekly_returns$return_t5[i] <- NA
  })
  pb$tick()
  Sys.sleep((1/10000))
}


# Export Excel

write_xlsx(smi_weekly_returns, "SMI_Weekly_Sentiments_v6.xlsx")


