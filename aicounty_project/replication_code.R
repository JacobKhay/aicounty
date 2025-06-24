# Load required libraries
library(tidyverse)
library(readr)
library(dplyr)

# Read data
data_ai <- read_csv("data.csv")

# Preprocessing
data_ai <- data_ai %>% ungroup() %>%
  mutate(
    degshare = (udeg + mdeg) / Employed,
    stemshare = (ustemdeg + mstemdeg) / (udeg + mdeg),
    stemshare2 = ustemdeg / udeg
  )

data_ai <- data_ai %>%
  mutate(
    stemshare = replace_na(stemshare, 0),
    stemshare2 = replace_na(stemshare2, 0),
    tightness = nads / Unemployed,
    hpi_ch = hpi_ch / 100
  )

# ✅ Calculate logpop BEFORE the zscores block
data_ai <- data_ai %>% mutate(logpop = log(pop))

# zscores
data_ai_z <- data_ai %>% filter(emp != 0) %>%
  mutate(
    share_bac = scale(share_bac),
    share_black = scale(share_black),
    share_poverty = scale(share_poverty),
    logpop = scale(logpop),
    hpi_ch = scale(hpi_ch),
    tightness = scale(tightness),
    unrate = scale(unrate),
    degshare = scale(degshare),
    stemshare = scale(stemshare),
    TurnOvrS = scale(TurnOvrS)
  )
