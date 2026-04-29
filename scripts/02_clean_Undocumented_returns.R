


### Undocumented Return from Pakistan and Iran 





df_raw_und <- read_excel("~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/raw/afghanistan-baseline-assessment-district-round15.xlsx", sheet = 2, col_names = FALSE)


df_raw_und <- df_raw_und |> 
  slice(-1, -2)

names(df_raw_und) <- c(
  "ADM1Code",
  "Province",
  "ADM2Code",
  "District",
  "returnees_pak_irn_undocumented_total",
  "returnees_pak_irn_undocumented_2012_2018",
  "returnees_pak_irn_undocumented_2019",
  "returnees_pak_irn_undocumented_2020",
  "returnees_pak_irn_undocumented_2021",
  "returnees_pak_irn_undocumented_2022",
  "percent_2012_2018",
  "percent_2019",
  "percent_2020",
  "percent_2021",
  "percent_2022"
)

df_raw_und <- df_raw_und[-1, ]

df_raw_und <- df_raw_und |> 
  filter(!is.na(District))



####### Loading the Round 14 of the Assessment and appending the values for 2016, 2017, and 2018

df_14_und <- read_excel("~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/raw/afghanistan-baseline-assessment-district-round-14_DEC-31-2021.xlsx",sheet = 2, col_names = FALSE)


names(df_14_und) <- as.character(df_14_und[3,])
df_14_und <- df_14_und[-1,]
df_14_und <- df_14_und[-1,]
df_14_und <- df_14_und[-1,]
head(df_14_und)

df_14_und <- df_14_und |> 
  filter(!is.na(District))




#### Merging the years into the master df_raw dataset
df_raw_und <- df_raw_und %>%
  left_join(
    df_14_und %>%
      select(
        District,
        `Returnees PAK IRN Undocumented 2016`,
        `Returnees PAK IRN Undocumented 2017`,
        `Returnees PAK IRN Undocumented 2018`,
        `Returnees PAK IRN Undocumented 2012_2015`
      ),
    by = c("District" = "District")
  )

 

#### Removingt the duplicate districts
df_raw_und <- df_raw_und %>%
  distinct(ADM2Code, .keep_all = TRUE)





df_raw_und$returnees_pak_irn_undocumented_total <- as.numeric(df_raw_und$returnees_pak_irn_undocumented_total)
df_raw_und$returnees_pak_irn_undocumented_2019 <- as.numeric(df_raw_und$returnees_pak_irn_undocumented_2019)
df_raw_und$returnees_pak_irn_undocumented_2020 <- as.numeric(df_raw_und$returnees_pak_irn_undocumented_2020)
df_raw_und$returnees_pak_irn_undocumented_2021 <- as.numeric(df_raw_und$returnees_pak_irn_undocumented_2021)
df_raw_und$`Returnees PAK IRN Undocumented 2016` <- as.numeric(df_raw_und$`Returnees PAK IRN Undocumented 2016`)
df_raw_und$`Returnees PAK IRN Undocumented 2017` <- as.numeric(df_raw_und$`Returnees PAK IRN Undocumented 2017`)
df_raw_und$`Returnees PAK IRN Undocumented 2018` <- as.numeric(df_raw_und$`Returnees PAK IRN Undocumented 2018`)
df_raw_und$`Returnees PAK IRN Undocumented 2012_2015` <- as.numeric(df_raw_und$`Returnees PAK IRN Undocumented 2012_2015`)

df_raw_und <- df_raw_und[, -c(5,6,11,12,13,14,15)]

colnames(df_raw_und) <- c(
  "adm1code",
  "province",
  "adm2code",
  "district",
  "returnees_2019",
  "returnees_2020",
  "returnees_2021",
  "percent_2022",
  "returnees_2016",
  "returnees_2017",
  "returnees_2018",
  "returnees_2012-2015"
)


df_long_und <- df_raw_und %>%
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


province_returns_und <- df_long_und %>%
  group_by(province) %>%
  summarise(
    total_returnees = sum(returnees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_returnees))


district_returns_und <- df_long_und %>%
  group_by(province, district) %>%
  summarise(
    total_returnees = sum(returnees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_returnees))


national_total_und <- sum(province_returns_und$total_returnees, na.rm = TRUE)

top10_province_und <- province_returns_und %>%
  slice_max(total_returnees, n = 10)

top10_summary_und <- top10_province_und %>%
  summarise(
    top10_total = sum(total_returnees),
    national_total = national_total_und,
    top10_share = top10_total / national_total_und
  )

top10_summary_und


national_total_district_und <- sum(district_returns_und$total_returnees, na.rm = TRUE)

top10_district_und <- district_returns_und %>%
  slice_max(total_returnees, n = 10)

top10_district_summary_und <- top10_district_und %>%
  summarise(
    top10_total = sum(total_returnees),
    national_total = national_total_district_und,
    top10_share = top10_total / national_total_district_und
  )

top10_district_summary_und

write.csv(df_raw_und, "~/Afghanistan Data Research Lab/Afghanistan_Return_Reintegration_Observatory/data/processed/Undocumented_returns_master.csv", row.names = FALSE)


