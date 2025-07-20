# Load required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(fixest)
library(modelsummary)

# Read data
data_ai <- read_csv("data.csv")

# Preprocessing and variable creation
data_ai <- data_ai %>% ungroup() %>%
  mutate(
    degshare = (udeg + mdeg) / Employed,
    stemshare = (ustemdeg + mstemdeg) / (udeg + mdeg),
    stemshare2 = ustemdeg / udeg,
    stemshare = replace_na(stemshare, 0),
    stemshare2 = replace_na(stemshare2, 0),
    tightness = nads / Unemployed,
    hpi_ch = hpi_ch / 100,
    logpop = log(pop),
    logincome = log(medhhincome),
    pat_intensity = n_inventors / Employed,
    patai_intensity = ai_patents / n_patents,
    large_firms = 1 - (small + medium) / est,
    information_intensity = information_emp / emp,
    manuf_intensity = manuf_emp / emp,
    ai_intensity = ai / nads,
    lads = log(1 + nads),
    state_year = paste0(state, Year)

  )
data_ai <- data_ai %>%
  mutate(state_year = paste0(state, Year))

# Z-score version of the dataset
data_ai_z <- data_ai %>%
  filter(emp != 0) %>%
  mutate(
    share_bac = scale(share_bac),
    share_black = scale(share_black),
    share_poverty = scale(share_poverty),
    logpop = scale(logpop),
    hpi_ch = scale(hpi_ch),
    logincome = scale(logincome),
    tightness = scale(tightness),
    unrate = scale(unrate),
    pat_intensity = scale(pat_intensity),
    patai_intensity = scale(patai_intensity),
    degshare = scale(degshare),
    stemshare = scale(stemshare),
    large_firms = scale(large_firms),
    information_intensity = scale(information_intensity),
    manuf_intensity = scale(manuf_intensity),
    TurnOvrS = scale(TurnOvrS),
    ai_intensity = ai_intensity * 100
  )

# Column (1) - Demographics
est_demog_no = fixest::feols(
  ai_intensity ~ share_bac + share_black + share_poverty + logpop + hpi_ch + logincome + tightness | Year + COUNTY_FIPS,
  data = data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                               tightness, pat_intensity, patai_intensity, degshare, stemshare,
                               large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

# Column (2) - Innovation
est_innovation_no = fixest::feols(
  ai_intensity ~ pat_intensity + patai_intensity + degshare + stemshare | Year + COUNTY_FIPS,
  data = data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                               tightness, pat_intensity, patai_intensity, degshare, stemshare,
                               large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

# Column (3) - Industry
est_industry_no = fixest::feols(
  ai_intensity ~ large_firms + information_intensity + manuf_intensity + TurnOvrS | Year + COUNTY_FIPS,
  data = data_ai_z %>% drop_na(share_bac, share_black, share_poverty, logpop, hpi_ch, logincome,
                               tightness, pat_intensity, patai_intensity, degshare, stemshare,
                               large_firms, information_intensity, manuf_intensity, TurnOvrS),
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

# Column (4) - All Controls
est_all = fixest::feols(
  ai_intensity ~ share_bac + share_black + share_poverty + logpop + hpi_ch + logincome + tightness +
    pat_intensity + patai_intensity + degshare + stemshare + large_firms +
    information_intensity + manuf_intensity + TurnOvrS | Year + COUNTY_FIPS,
  data = data_ai_z,
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

# Column (5) - All Controls + State×Year FE
est_all_large = fixest::feols(
  ai_intensity ~ share_bac + share_black + share_poverty + logpop + hpi_ch + logincome + tightness +
    pat_intensity + patai_intensity + degshare + stemshare + large_firms +
    information_intensity + manuf_intensity + TurnOvrS | state_year + COUNTY_FIPS,
  data = data_ai_z,
  cluster = "COUNTY_FIPS",
  weights = ~lads
)

# Optional: render the table
modelsummary(
  list(
    "Demographics" = est_demog_no,
    "Innovation" = est_innovation_no,
    "Industry" = est_industry_no,
    "All Controls" = est_all,
    "All + State×Year FE" = est_all_large
  ),
  stars = TRUE,
  coef_omit = "Intercept"
)
