# further analysis

install.packages("multiwaycov")
install.packages("modelsummary")
install.packages("stargazer")

mypackages <- c("tidyverse")
lapply(mypackages, library())

# load packages
library(tidyverse)
library(ggplot2)
library(texreg)
library(gghighlight)
library(ggeffects)
library(ggrepel)
library(ggforce)
library(plm)
library(data.table)
library(effects)
library(broom)
library(lme4)
library(ggeffects)
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(modelsummary)
library(texreg)
library(stargazer)

###### Advanced models ######

# is the data normally distributed?
hist(data2018$trust_index_cond)
hist(data2020$trust_index)

#### data processing ####

# level of science education
scieduc_func <- function(dat, x, y) {
  case_when(x >= mean(y, na.rm = T)  ~ "high",
            TRUE ~ "low")  %>%
    factor(levels = c('high', 'low'))
}

# function to recode elecdem vdem indicator
elecdem_score <- function(x) {
  case_when(
    x > 0.80 & x < 1.00 ~ "full democracy",
    x > 0.60 & x <= 0.80 ~ "flawed democracy",
    x > 0.40 & x <= 0.60 ~ "hybrid regime",
    #x > 0.00 & x <= 0.40 ~ "authoritarian",
    TRUE ~ "authoritarian"
  )  %>%
    factor(levels = c(
      'full democracy',
      'flawed democracy',
      'hybrid regime',
      'authoritarian'
    ))
}

# global mean
mean(data2018$scieduc, na.rm = T)

# create science educ measures
dat2018 <- data2018 %>%
  filter(!is.na(scieduc)) %>%
  group_by(country) %>%
  mutate(scieduc_countrymean = mean(scieduc, na.rm = TRUE)) %>%
  ungroup() %>%
  # new columns for individual level compared to country mean and global mean
  mutate(
    scieduc_level_country = scieduc_func(data2018, scieduc, scieduc_countrymean),
    scieduc_globalmean = mean(scieduc, na.rm = TRUE),
    scieduc_level_global = scieduc_func(data2018, scieduc, scieduc_globalmean),
    elecdem_fct = elecdem_score(elecdem)
  )

# country-income interval variable scaled from WBI classification
# 1=Low income, 2=Lower Middle income, 3=Upper Middle income, 4=High income

table(data2020$trust_index)
table(is.na(data2018$trust_index_cond))
str(dat2018$scieduc_level_country)

# 2020 interaction model
dat2020 <- data2020 %>%
  filter(!is.na(scieduc)) %>%
  group_by(country) %>%
  mutate(scieduc_countrymean = mean(scieduc, na.rm = TRUE)) %>%
  ungroup() %>%
  # new columns for individual level compared to country mean and global mean
  mutate(
    scieduc_level_country = scieduc_func(data2020, scieduc, scieduc_countrymean),
    scieduc_globalmean = mean(scieduc, na.rm = TRUE),
    scieduc_level_global = scieduc_func(data2020, scieduc, scieduc_globalmean),
    elecdem_fct = elecdem_score(elecdem)
  )

nrow(dat2020)
nrow(data2020)
str(dat2020$scieduc_level_country)


### --------------------------

# Fit simple interaction model

mod1_simple <- lm(
  trust_index_cond ~
    elecdem * scieduc_level_country
  + country_income +
    subj_sciknow +
    educ +
    trust_gov,
  data = dat2018
)
summary(mod1_simple)

mod2_simple <- lm(
  trust_index ~
    elecdem * scieduc_level_country
  + country_income +
    subj_sciknow +
    educ +
    trust_gov,
  data = dat2020
)
summary(mod2_simple)

# Cluster standard errors at the country level
mod1_simple_clse <- coeftest(
  mod1_simple,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)

mod2_simple_clse <- coeftest(
  mod2_simple,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)

# model summary
screenreg(list(mod1_simple, mod1_simple_clse, mod2_simple, mod2_simple_clse),
          digits = 3)

# html tables
htmlreg(
  list(mod1_simple, mod1_simple_clse, mod2_simple, mod2_simple_clse),
  booktabs = T,
  dcolumn = T,
  single.row = F
)

### categorical interactions ####

# full model 2018
mod1_int_full <- lm(
  trust_index_cond ~
    # interaction
    elecdem_fct * scieduc_level_country
  # country-level+country_income +
  # individual-level
  subj_sciknow +
    educ +
    trust_gov,
  data = dat2018
) # got rid of income_pers & sci_over_reli
summary(mod1_int_full)

# full model 2020
mod2_int_full <- lm(trust_index ~
                      elecdem_fct * scieduc_level_country
                    # country-level+country_income +
                    # individual-level
                    subj_sciknow +
                      educ +
                      trust_gov,
                    data = dat2020)
summary(mod2_int_full)

# Cluster standard errors at the country level
mod1_full_clse <- coeftest(
  mod1_int_full,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)
mod2_full_clse <- coeftest(
  mod2_int_full,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)

screenreg(list(mod1_int_full, mod1_full_clse, mod2_int_full, mod2_full_clse),
          digits = 3)

# html tables
htmlreg(
  list(mod1_int_full, mod1_full_clse, mod2_int_full, mod2_full_clse),
  booktabs = T,
  dcolumn = T,
  single.row = T
)

### relevant predictors only ####

# full model 2018
mod1_int_full <- lm(
  trust_index_cond ~
    # interaction
    elecdem_fct * scieduc_level_country
  # country-level+country_income +
  # individual-level
  subj_sciknow +
    educ +
    trust_gov,
  data = dat2018
) # got rid of income_pers & sci_over_reli
summary(mod1_int_full)

# full model 2020
mod2_int_full <- lm(trust_index ~
                      elecdem_fct * scieduc_level_country
                    # country-level+country_income +
                    # individual-level
                    subj_sciknow +
                      educ +
                      trust_gov,
                    data = dat2020)
summary(mod2_int_full)

# Cluster standard errors at the country level
mod1_full_clse <- coeftest(
  mod1_int_full,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)
mod2_full_clse <- coeftest(
  mod2_int_full,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)

screenreg(list(mod1_int_full, mod1_full_clse, mod2_int_full, mod2_full_clse),
          digits = 3)

# html tables
htmlreg(
  list(mod1_int_full, mod1_full_clse, mod2_int_full, mod2_full_clse),
  booktabs = T,
  dcolumn = T,
  single.row = T
)

### limited model terms #####
colnames(dat2018)

# full model 2018

mod_a <- lm(
  trust_index_cond ~
    scieduc_level_country * elecdem +
    country_income +
    educ + income,
  data = dat2018,
  contrasts = 
)
summary(mod_a)

mod_a_high <- lm(
  trust_index_cond ~
    scieduc_level_countryh * elecdem +
    country_income +
    educ + income,
  data = dat2018,
  contrasts = 
)
summary(mod_a_high)

# full model 2020
mod_b <- lm(
  trust_index ~
    scieduc_level_countryh * elecdem +
    country_income +
    educ + income,
  data = dat2020
)
summary(mod_b)


formul <-
  (
    trust_index_cond ~ scieduc_level_countryh*elecdem_fct
    + country_income + educ + income
  )

mod_c <- lm(formul, dat2018)
summary(mod_c)

mod_d <- lm(formul, dat2020)
summary(mod_d)

# Cluster standard errors at the country level
library(sandwich)
library(stargazer)

cls_se <-
  list(sqrt(diag(
    vcovCL(mod_a, type = "HC1", cluster = ~ country)
  )),
  sqrt(diag(
    vcovCL(mod_b, type = "HC1", cluster = ~ country)
  )),
  sqrt(diag(
    vcovCL(mod_c, type = "HC1", cluster = ~ country)
  )),
  sqrt(diag(
    vcovCL(mod_d, type = "HC1", cluster = ~ country)
  )))

# table for comparison
library(stargazer)

stargazer(
  list(mod_a, mod_b, mod_c, mod_d),
  header = FALSE,
  se = cls_se,
  type = "text"
)

# some testing!

# Fit linear regression model
model <- lm(trust_index_cond ~
              scieduc_level_country * elecdem +
              country_income +
              educ + income,
            data = dat2018)

str(dat2018$scieduc_level_country)

# Cluster standard errors by country
library(lmtest)
lmtest::coeftest(
  model,
  vcov = vcovCL,
  type = "HC1",
  cluster = ~ country
)

# Create dataset for plotting
dataset_plot <- data.frame(scieduc_level_country = as.factor(rep(c("high", "low"), 101)), 
                           elecdem = seq(0, 1, by = 0.01),
                           country_income=mean(dat2018$country_income, na.rm=T),
                           educ=mean(dat2018$educ, na.rm=T),
                           income=mean(dat2018$income, na.rm=T))

# Add predicted values and marginal effects to dataset for plotting
dataset_plot$predicted_values <- predict(model, newdata = dataset_plot)
dataset_plot$marginal_effect <- coef(model)[2] + coef(model)[3] * dataset_plot$elecdem
head(dataset_plot)
table(dataset_plot$marginal_effect,dataset_plot$scieduc_level_country)

# Plot marginal effect of science education on trust
ggplot(dataset_plot, aes(x = elecdem, y = marginal_effect, group = factor(scieduc_level_country), 
                         color = factor(scieduc_level_country))) +
  geom_line() +
  xlab("Democracy Score") +
  ylab("Marginal Effect of Science Education on Trust") +
  ggtitle("Conditional Marginal Effect of Science Education on Trust at Different Democracy Scores")



# tables for SE comparison

# Add an extra row with the error names
se_info <-
  tibble(term = "Standard errors", "Regular", "Robust", "Clustered")

modelsummary(
  mod1_int_full,
  # Specify how to robustify/cluster the model
  vcov = list("iid", "robust", function(x)
    vcovCL(x, cluster = ~ country)),
  # Get rid of other coefficients and goodness-of-fit (gof) stats
  gof_omit = ".*",
  add_rows = se_info
)

# basic model interpretation --- FIX THIS SECTION!!
# intercept = mean elecdem and scieduchigh, then trust level = 1.36252
# if mean elecdem + scieduc_levellow = 1.36252 -0.25775 = 1.10477

# interaction effect = elecdem*scieduc(low)
# every 1-unit increase in elecdem,
# scieduc high increases trust by 0.272,
# while scieduc low increases trust by 0.272 + 0.523 = 0.795


# anova for reference --- RELEVANCE?
aov1 <-
  aov(trust_index_cond ~ (elecdem * scieduc_level_country), data = dat2018)
summary(aov1)
plot(aov1) # check plots for heteroskedastic errors?



#### hierarchical models ####

colnames(dat2018)

model <- lmer(
  trust_index_cond ~
    elecdem + scieduc_level_country +
    (1 | country) +
    country_income +
    subj_sciknow +
    trust_gov +
    educ,
  data = dat2018,
  control = lmerControl(optimizer = "bobyqa")
)
summary(model)

model1b <- lmer(
  trust_index_cond ~
    elecdem_fct + scieduc_level_country +
    (1 | country) +
    country_income +
    subj_sciknow +
    trust_gov +
    educ,
  data = dat2018,
  control = lmerControl(optimizer = "bobyqa")
)
summary(model1b)


model2 <- lmer(
  trust_index ~
    elecdem + scieduc_level_country +
    (1 | country) +
    country_income +
    subj_sciknow +
    trust_gov +
    educ,
  data = dat2020,
  control = lmerControl(optimizer = "bobyqa")
)
summary(model2)


model2b <- lmer(
  trust_index ~
    elecdem_fct + scieduc_level_country +
    (1 | country) +
    country_income +
    subj_sciknow +
    trust_gov +
    educ,
  data = dat2020,
  control = lmerControl(optimizer = "bobyqa")
)
summary(model2b)

screenreg(list(model, model1b, model2, model2b))


htmlreg(
  list(model, model1b, model2, model2b),
  booktabs = T,
  dcolumn = T,
  single.row = T,
  digits = 3
)

# Multilevel modeling is a statistical technique that allows you to account for the
# hierarchical structure of the data, such as when observations are nested within
# groups (e.g., countries in this case). In the example above, the random effect of
# "country" allows for the estimated intercepts (i.e., means) of the trust levels to
# vary across countries. This allows the model to account for the fact that the trust
# levels of different countries may be influenced by different factors, such as
# cultural or historical differences, and may not be directly comparable to one another.

# #### Full dataset #####
#
# # fixed effects within model
# dat2018fe <- pdata.frame(dat2018, index = c("country"))
# model <-
#   plm(
#     trust_index_cond ~  elecdem * scieduc_level_country + factor(country),
#     model = 'within',
#     data = dat2018fe
#   )
# summary(model)
# # clustered standard errors
# coeftest(model, vcovHC(model, type = 'HC0', cluster = 'group'))
