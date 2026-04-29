


library(dplyr)
library(readxl)
library(ggplot2)
library(readr)
library(car)
library(randomForest)
library(stringi)
library(caret)
library(car)
library(stringr)
library(psych)
library(kableExtra)
library(broom)
library(tidyverse)
library(psych)
library(janitor)




### Documented Returns from Pakistan and Iran  

df_raw <- read_excel("~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/raw/afghanistan-baseline-assessment-district-round15.xlsx", col_names = FALSE)


df_raw <- df_raw |> 
  slice(-1, -2)

names(df_raw) <- c(
  "adm1code",
  "province",
  "adm2code",
  "district",
  "returnees_pak_irn_documented_total",
  "returnees_pak_irn_documented_2012_2018",
  "returnees_pak_irn_documented_2019",
  "returnees_pak_irn_documented_2020",
  "returnees_pak_irn_documented_2021",
  "returnees_pak_irn_documented_2022",
  "percent_2012_2018",
  "percent_2019",
  "percent_2020",
  "percent_2021",
  "percent_2022"
)

df_raw <- df_raw[-1, ]

df_raw <- df_raw |> 
  filter(!is.na(district))



####### Loading the Round 14 of the Assessment and appending the values for 2016, 2017, and 2018

df_14 <- read_excel("~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/raw/afghanistan-baseline-assessment-district-round-14_DEC-31-2021.xlsx", col_names = FALSE)


names(df_14) <- as.character(df_14[3,])
df_14 <- df_14[-1,]
df_14 <- df_14[-1,]
df_14 <- df_14[-1,]
head(df_14)

df_14 <- df_14 |> 
  filter(!is.na(District))




#### Merging the years into the master df_raw dataset

df_raw <- df_raw %>%
  left_join(
    df_14 %>%
      select(
        District,
        `Returnees PAK IRN Documented 2016`,
        `Returnees PAK IRN Documented 2017`,
        `Returnees PAK IRN Documented 2018`,
        `Returnees PAK IRN Documented 2012_2015`
      ),
    by = c("district" = "District")
  )


df_raw %>%
  count(district) %>%
  filter(n > 1)    ########################## The following districts are duplicate:  Arghandab, Baharak, Fayzabad, Jaghatu, Jani Khel, Muqur, Tagab.

#### Removingt the duplicate districts
df_raw <- df_raw %>%
  distinct(adm2code, .keep_all = TRUE)


df_raw <- df_raw[, - c(6,10, 11, 12, 13, 14)]


df_raw$returnees_pak_irn_documented_total <- as.numeric(df_raw$returnees_pak_irn_documented_total) 
df_raw$returnees_pak_irn_documented_2019 <- as.numeric(df_raw$returnees_pak_irn_documented_2019) 
df_raw$returnees_pak_irn_documented_2020 <- as.numeric(df_raw$returnees_pak_irn_documented_2020) 
df_raw$returnees_pak_irn_documented_2021 <- as.numeric(df_raw$returnees_pak_irn_documented_2021) 
df_raw$`Returnees PAK IRN Documented 2016` <- as.numeric(df_raw$`Returnees PAK IRN Documented 2016`) 
df_raw$`Returnees PAK IRN Documented 2017` <- as.numeric(df_raw$`Returnees PAK IRN Documented 2017`) 
df_raw$`Returnees PAK IRN Documented 2018` <- as.numeric(df_raw$`Returnees PAK IRN Documented 2018`) 
df_raw$`Returnees PAK IRN Documented 2012_2015` <- as.numeric(df_raw$`Returnees PAK IRN Documented 2012_2015`) 



colnames(df_raw) <- c("adm1code", "province", "adm2code", "district", "returnee_total_incomplete", "returnees_2019", "returnees_2020", "returnees_2021", "percent_2022", "returnees_2016", "returnees_2017", "returnees_2018", "returnees_2012-2015")

df_raw <- df_raw[, -c(5,9)]




################################################################### Province Level Concentration ##################################################################

df_long <- df_raw %>%
  pivot_longer(
    cols = c(
      `returnees_2012-2015`,
      returnees_2016,
      returnees_2017,
      returnees_2018,
      returnees_2019,
      returnees_2020,
      returnees_2021
    ),
    names_to = "year_period",
    values_to = "returnees"
  ) %>%
  mutate(
    returnees = as.numeric(returnees)
  )


province_returns <- df_long %>%
  group_by(province) %>%
  summarise(
    total_returnees = sum(returnees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_returnees))


district_returns <- df_long %>%
  group_by(province, district) %>%
  summarise(total_returnees = sum(returnees, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_returnees))


national_total <- sum(province_returns$total_returnees, na.rm = TRUE)

top10_province <- province_returns %>%
  slice_max(total_returnees, n = 10)

top10_summary <- top10_province %>%
  summarise(
    top10_total = sum(total_returnees),
    national_total = national_total,
    top10_share = top10_total / national_total
  )

top10_summarynational_total <- sum(province_returns$total_returnees, na.rm = TRUE)

top10_province <- province_returns %>%
  slice_max(total_returnees, n = 10)

top10_summary <- top10_province %>%
  summarise(
    top10_total = sum(total_returnees),
    national_total = national_total,
    top10_share = top10_total / national_total
  )

top10_summary




district_returns <- df_long %>%
  group_by(province, district) %>%
  summarise(
    total_returnees = sum(returnees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_returnees))

national_total_district <- sum(district_returns$total_returnees, na.rm = TRUE)

top10_district <- district_returns %>%
  slice_max(total_returnees, n = 10)

top10_district_summary <- top10_district %>%
  summarise(
    top10_total = sum(total_returnees),
    national_total = national_total_district,
    top10_share = top10_total / national_total
  )

top10_district_summary


write.csv(df_raw, "~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/processed/documented_returns_master.csv", row.names = FALSE)

