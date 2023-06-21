# ETL pipelines

#### READ IN RAW DATA ####

wgm18 <- read.csv('data/wgm18.csv')
nrow(wgm18)

wgm20 <- read.csv('data/wgm20.csv')
nrow(wgm20)

# read in vdem data
vdem <- read.csv('data/vdem-v12.csv')
nrow(vdem)

#### 2018 DATA ####

# clean, transform, mutate
wgm18df <-
  wgm18 %>%
   # replace all missing values 97, 98, 99 == NA
  mutate_at(
    vars(c(6, 8:12, 19:20, 48:50)),
    recode,
    '1' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '97' = 0,
    '98' = 0,
    '99' = 0) %>% 
  # recode variables for restricted trust index
  mutate_at(
    vars(Q11C, Q13, Q14A),
    recode,
    '1' = 4,
    '2' = 3,
    '3' = 2,
    '4' = 1,
    '97' = 0,
    '98' = 0,
    '99' = 0) %>% 
  mutate(
    sci_over_reli = ifelse(Q30 == 1, 1, 0),
    scieduc = ifelse(Q5C  == 1, 3,
                     ifelse(Q5B == 1, 2,
                            ifelse(Q5A == 1, 1, 0))),
    obj_sciknow = ifelse(Q3 == 1 & Q4 == 2, 1, 0),
    # recode zeros to NAs
    trust_index = ifelse(WGM_Index == 0, NA, WGM_Index),
    # construct restricted index
    trust_index_cond = case_when(Q11C == 0 ~ 0,
                                 Q13 == 0 ~ 0,
                                 Q14A == 0 ~ 0,
                                 TRUE ~ (Q11C + Q13 + Q14A) / 3),
    trust_index_cond = ifelse(trust_index_cond == 0, NA, trust_index_cond)
  ) %>%
  rename(
    id = WP5,
    natwgt = wgt,
    popwgt = PROJWT,
    trust_gov = Q11B,
    regions = Regions_Report,
    sci_benefit = ViewOfScience,
    subj_sciknow = Q1,
    educ = Education,
    income_feel = Subjective_Income,
    urban_rural = Urban_Rural,
    country_income = WBI,
    income = Household_Income,
    age = Age,
    gender = Gender,
    employ = EMP_2010
  ) %>%
  select(
    id,
    natwgt,
    popwgt,
    regions,
    trust_gov,
    trust_index,
    trust_index_cond,
    scieduc,
    sci_over_reli,
    subj_sciknow,
    obj_sciknow,
    sci_benefit,
    educ,
    urban_rural,
    income_feel,
    country_income, 
    income,
    age,
    gender,
    employ
  )
summary(wgm18df)

# manually recode 98 & 99 to NA values - not needed
summary(wgm18df)
# wgm18df[, 4:14][wgm18df[, 4:14] == 98 | wgm18df[, 4:14] == 99] <- 0

# add country var
countrylist <- read.csv('data/countrylist18.csv', header = TRUE)
wgm18df <- left_join(wgm18df, countrylist, by = 'id')

# refactor regions var
wgm18df$regions <- factor(
  wgm18df$regions,
  levels = c(0:18),
  labels = c(
    "Not assigned",
    "Eastern Africa",
    "Central Africa",
    "North Africa",
    "Southern Africa",
    "Western Africa",
    "Central America/Mexico",
    "Northern America",
    "South America",
    "Central Asia",
    "East Asia",
    "Southeast Asia",
    "South Asia",
    "Middle East",
    "Eastern Europe",
    "Northern Europe",
    "Southern Europe",
    "Western Europe",
    "Aus/NZ"
  )
)

# merge w/ vdem measures
vdem18 <- vdem %>%
  filter(year == 2018) %>%
  select(country_name,
         v2x_polyarchy, v2x_libdem,
         v2x_partipdem) %>%
  rename(
    country = country_name,
    elecdem = v2x_polyarchy,
    libdem = v2x_libdem,
    partipdem = v2x_partipdem
  )

vdem18$country <- recode(
  vdem18$country,
  "United States of America" = "United States",
  "Palestine/Gaza" = "Palestinian Territories",
  "Democratic Republic of Vietnam" = "Vietnam",
  "Burma/Myanmar" = "Myanmar",
  "Republic of the Congo"  = "Republic of Congo",
  "North Macedonia" = "Macedonia"
)
summary(vdem18)

# compare differences in countries
setdiff(wgm18df$country, vdem18$country)

# merge dfs
data2018 <- merge(wgm18df, vdem18, by = "country")
head(data2018)

# write to file
write.csv(data2018, "data/data2018.csv", row.names=FALSE)

#### 2020 DATA ####

colnames(wgm20)

# clean up the data
wgm20df <-
  wgm20 %>%
  mutate_at(
    vars(W1:W5B, W6, W7C:W10),
    recode,
    '1' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '99' = 0,
    '0' = 0
  ) %>%
  mutate_at(
    vars(Education, Subjective_Income),
    recode,
    '1' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '5' = 0,
    '6' = 0,
    '99' = 0,
  ) %>%
  # recode variables for restricted trust index
  mutate_at(
    vars(W5C, W7A, W7B),
    recode,
    '1' = 4,
    '2' = 3,
    '3' = 2,
    '4' = 1,
    '99' = 0,
    '0' = 0) %>% 
  rename(
    country = COUNTRYNEW,
    natwgt = WGT,
    popwgt = PROJWT,
    regions = Global11Regions,
    subj_sciknow = W1,
    scieduc = W3,
    trust_gov = W5B,
    leaders_value = W7C,
    trust_science = W6,
    educ = Education,
    income_feel = Subjective_Income,
    country_income = wbi,
    age = Age,
    gender = Gender,
    income = Household_Income,
    employ = EMP_2010
  ) %>%
  mutate(
    sci_benefit = ((W8 + W9) / 2),
    sci_over_reli = ifelse(W30 == 1, 1, 0),
    # recode trust index
    trust_index = case_when(W5C == 0 ~ 0,
                            W7A == 0 ~ 0,
                            W7B == 0 ~ 0,
                            TRUE ~ (W5C + W7A + W7B) / 3),
    trust_index = ifelse(trust_index == 0, NA, trust_index),
  ) %>%
  select(
    country,
    natwgt,
    popwgt,
    regions,
    leaders_value,
    trust_gov,
    trust_science,
    trust_index,
    scieduc,
    subj_sciknow,
    sci_benefit,
    sci_over_reli,
    educ,
    income_feel,
    country_income,
    age, gender, income, employ
  )
summary(wgm20df)

# refactor regions var
wgm20df$regions <- factor(
  wgm20df$regions,
  levels = c(1:11),
  labels = c(
    "Western Europe",
    "Eastern Europe",
    "Russia/Caucasus/Central Asia",
    "Aus/NZ",
    "East Asia",
    "Southeast Asia",
    "South Asia",
    "Latin America",
    "Northern America",
    "Middle East/North Africa",
    "Sub-Saharan Africa"
  )
)

# merge w/ vdem measures
vdem20 <- vdem %>%
  filter(year == 2020) %>%
  select(country_name,
         v2x_polyarchy, v2x_libdem,
         v2x_partipdem) %>%
  rename(
    country = country_name,
    elecdem = v2x_polyarchy,
    libdem = v2x_libdem,
    partipdem = v2x_partipdem
  )

# recode countries for matching
vdem20$country <- recode(
  vdem20$country,
  "United States of America" = "United States",
  "Palestine/Gaza" = "Palestinian Territories",
  "Democratic Republic of Vietnam" = "Vietnam",
  "Burma/Myanmar" = "Myanmar",
  "Republic of the Congo"  = "Congo Brazzaville",
  "North Macedonia" = "North Macedonia",
  "Bosnia and Herzegovina" = "Bosnia Herzegovina"
)

# compare differences in countries
setdiff(wgm20df$country, vdem20$country)

# merge dfs
data2020 <- merge(wgm20df, vdem20, by = "country")
colnames(data2020)

# write to file
write.csv(data2020, "data/data2020.csv", row.names=FALSE)

