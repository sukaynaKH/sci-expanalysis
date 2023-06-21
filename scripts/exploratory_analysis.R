# set working directory
getwd()
setwd("/home/sukayna/Drive/readings/reading_notes/projects/exploratory-analysis")

# install libraries
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("texreg")
# install.packages("ggrepel")
# install.packages("gghighlight")
# install.packages("effects")

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
library(sandwich)
library(broom)
library(lmtest)
library(lme4)
library(ggeffects)

# read in cleaned data
data2018 <- read.csv('data/data2018.csv', header = T)
colnames(data2018)

data2020 <- read.csv('data/data2020.csv')
nrow(data2020)

write.csv(dat2018, 'data/cleaned_dat2018.csv')
write.csv(dat2020, 'data/cleaned_dat2020.csv')


#### 2018 ####

##### 2018 plots #####

# plot1 - avg trust index score across regions

# tidy data structure
dat <- data2018 %>%
  select(regions, trust_index, trust_index_cond) %>%
  filter(regions != "Not assigned") %>%
  group_by(regions) %>%
  mutate(
    trust_index = mean(trust_index, na.rm = TRUE),
    trust_index_cond = mean(trust_index_cond, na.rm = TRUE)
  ) %>%
  distinct() %>%
  ungroup() %>%
  as.data.table() %>%
  melt(id = "regions",
       measure = c("trust_index", "trust_index_cond"))

p1 <- dat %>%
  ggplot() +
  geom_bar(
    aes(x = regions, y = value, fill = variable),
    position = "dodge",
    stat = "identity",
    #fill = "#f68060",
    alpha = .6,
    width = .9
  ) +
  coord_flip() +
  xlab("") +
  theme_minimal() +
  theme(aspect.ratio = 3 / 5) +
  labs(title = "Average trust index score by region in 2018") +
  geom_text(aes(
    x = regions,
    y = value,
    label = paste0("(", round(value, 2), ")")
  ), 
  size = 2.5, check_overlap = T, 
  nudge_y = -0.08, nudge_x=-0.05) +
  scale_y_continuous(
    name = "",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) + expand_limits(y = c(1, 3.5)) +
  labs(caption = "Note: Original trust index scores indicated by extended blue bars.
          Restricted trust score (using 3 items) indicated by orange bars.") +
  theme(
    legend.position = "none",
    plot.caption = element_text(vjust = 5, hjust = 0.1),
    plot.caption.position = "panel"
  ) +
  scale_fill_manual(values = c("#60a5f6", "#f68060"))
p1

# plot 2 - avg trust index given electoral democracy for all countries
data2018$country <-
  recode(data2018$country, "United Arab Emirates" = "UAE")

# data2018 %>%
#   group_by(country) %>%
#   mutate(avgtrust = mean(trust_index, na.rm = TRUE)) %>%
#   select(country, regions, elecdem, avgtrust) %>%
#   arrange(elecdem) %>%
#   ggplot(aes(x = elecdem, y = avgtrust, label = country)) +
#   geom_point(size = .75) +
#   geom_text(
#     aes(label = ifelse(elecdem < 0.25 & avgtrust > 3.00,
#                        country, '')),
#     size = 3,
#     check_overlap = T,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   geom_text(
#     aes(label = ifelse(elecdem > 0.9 & avgtrust > 3.00, country, '')),
#     size = 3,
#     check_overlap = T,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   scale_y_continuous(name = "trust in scientists index (avg)") +
#   scale_x_continuous(name = "electoral democracy index") +
#   gghighlight(elecdem < 0.25 & avgtrust > 3.00) +
#   theme_minimal()

# plot 3 - avg trust index given liberal democracy for all countries
# data2018 %>%
#   group_by(country) %>%
#   mutate(avgtrust = mean(trust_index, na.rm = TRUE)) %>%
#   select(country, regions, libdem, avgtrust) %>%
#   arrange(libdem) %>%
#   ggplot(aes(x = libdem, y = avgtrust, label = country)) +
#   geom_point(size = .75) +
#   geom_text(
#     aes(label = ifelse(
#       libdem < 0.2 & avgtrust > 3.00, as.character(country), ''
#     )),
#     size = 3,
#     check_overlap = T,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   geom_text(aes(label = ifelse(
#     libdem > 0.85 & avgtrust > 3.00, as.character(country), ''
#   )),
#   size = 3, nudge_y = 0.02) +
#   scale_y_continuous(name = "trust in scientists index (avg)") +
#   scale_x_continuous(name = "liberal democracy index") +
#   gghighlight(libdem < 0.2 & avgtrust > 3.00) +
#   theme_minimal()

# plot 4 - edu by region
data2018 %>%
  filter(
    regions != "Not assigned"
    & regions != "Eastern Africa"
    & regions != "Western Africa"
    & regions != "Central Africa"
    & regions != "Northern Europe"
    & regions != "Southern Europe"
    & regions != "North Africa"
  ) %>%
  drop_na() %>%
  ggplot(aes(factor(scieduc), trust_index_cond, color = factor(scieduc))) +
  geom_boxplot(width = 0.8, outlier.size = .5) +
  facet_wrap(~ regions, ncol = 4) +
  #gghighlight(calculate_per_facet = TRUE, trust_index_cond > 2.5) +
  scale_y_continuous(
    name = "trust in scientists index",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_x_discrete(
    name = "level of science education",
    breaks = c(0, 1, 2, 3),
    labels = c(
      "0" = "none",
      "1" = "primary",
      "2" = "secondary",
      "3" = "college"
    )
  ) +
  theme_minimal() +
  #labs(caption = "Note: Boxplot regions with trust index score > 2.5 are highlighted.") +
  theme(
    legend.position = "none",
    axis.text.x.bottom = element_text(size = 6),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/scieduc2018.png", width = 8.5)

# plot 5 - obj/subj knowledge + trust

# How much do you, personally, know about science?
# Do you know a lot, some, not much, or nothing at all?
# 1=A lot, 2=Some, 3=Not much, 4=Nothing at all, 98=(DK), 99=(Refused)

dat2018 %>%
  filter(regions != "Not assigned", subj_sciknow > 0) %>% 
  drop_na() %>%
  ggplot(aes(factor(obj_sciknow), trust_index_cond, fill = factor(subj_sciknow))) +
  geom_boxplot(width = .7, outlier.size = .3) +
  facet_wrap(~ regions, ncol = 6) +
  gghighlight(calculate_per_facet = TRUE, trust_index_cond > 2.5) +
  scale_x_discrete(name = "objective knowledge of science",
                   labels = c("1" = "yes",
                              "0" = "no")) +
  scale_y_continuous(
    name = "trust in scientists index",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_fill_manual(
    labels = c("a lot", "some", "not\nmuch", "nothing"),
    values = c("pink", "lightblue", "lightgreen", "lightyellow")
  ) +
  labs(fill = "perceived knowledge\nof science") +
  theme_minimal() +
  labs(caption = "Note: Boxplot regions with trust index score > 2.5 (medium) are highlighted.") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/persciknow2018.png")

##### Models #####

#install.packages("corrplot")
library(corrplot)

colnames(data2018)

corrdat <- data2018[, ][, c(6:20)] %>% na.omit()
corrdat <- cor(corrdat)
head(round(corrdat, 2))

corrplot(
  corrdat,
  method = "square",
  addCoef.col = 'grey66',
  tl.srt = 45,
  tl.cex = 0.85,
  number.cex = 0.8,
  tl.col = 'black',
  cl.length = 5,
  cl.align = "l"
)

# linear model - electoral democracy index
elecmodel <-
  lm(
    trust_index ~
      # country-level
      elecdem + wbi +
      # individual-level
      scieduc + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov +
      educ + income_pers,
    data = data2018
  )
summary(elecmodel)

# linear model - electoral democracy index restricted

table(data2018$scieduc, data2018$trust_index_cond)

elecmodelcond <-
  lm(
    trust_index_cond ~
      # country-level
      elecdem + wbi +
      # individual-level
      scieduc + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov +
      educ + income_pers,
    data = data2018
  )
summary(elecmodelcond)

# linear model - liberal democracy index
libmodel <-
  lm(
    trust_index ~
      # country-level
      libdem + wbi +
      # individual-level
      scieduc + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov +
      educ + income_pers,
    data = data2018
  )
summary(libmodel)

# linear model - liberal democracy index restricted
libmodelcond <-
  lm(
    trust_index_cond ~
      # country-level
      libdem + wbi +
      # individual-level
      scieduc + obj_sciknow + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov +
      educ + urban_rural + income_pers,
    data = data2018
  )
summary(libmodelcond)

screenreg(list(elecmodel, elecmodelcond, libmodel, libmodelcond))

# # country fixed effects models
# library(plm)
# 
# data2018fe <-
#   pdata.frame(data2018, index = c("country"), drop.index = TRUE)
# head(attr(data2018fe, "index"))
# 
# femodel <- plm(
#   trust_index ~
#     # country-level
#     elecdem + wbi +
#     # individual-level
#     scieduc + obj_sciknow + subj_sciknow + sci_benefit +
#     sci_over_reli + trust_gov +
#     educ + urban_rural + income_pers,
#   data = data2018fe,
#   model = "within"
# )
# summary(femodel)


#### 2020 dataset ####

##### 2020 plots #####

# plot1 - avg trust index score across regions
p2 <- data2020 %>%
  group_by(regions) %>%
  summarise(across(trust_index, mean, na.rm = TRUE)) %>%
  # mutate(name = fct_reorder(regions, desc(trust_index))) %>%
  ggplot(aes(x = regions, y = trust_index)) +
  geom_bar(
    stat = "identity",
    fill = "#f68060",
    alpha = .6,
    width = .6
  ) +
  coord_flip() +
  xlab("") +
  theme_minimal() +
  theme(aspect.ratio = 2 / 5) +
  labs(title = "Average trust index score by region in 2020") +
  geom_text(aes(label = paste0("(", round(trust_index, 2), ")")),
            size = 2.5, nudge_y = -0.15) +
  scale_y_continuous(
    name = "trust in scientists index (avg)",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) + expand_limits(y = c(1, 3.5))

library(gtable)
library(grid)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g)
ggsave("images_pres/avgtrustbyregion.png", plot = g)

# plot 2 - avg trust index given electoral democracy for all countries

# data2020 %>%
#   group_by(country) %>%
#   mutate(avgtrust = mean(trust_index)) %>%
#   select(country, regions, elecdem, avgtrust) %>%
#   arrange(elecdem) %>%
#   ggplot(aes(x = elecdem, y = avgtrust, label = country)) +
#   geom_point(size = .75) +
#   geom_text(
#     aes(label = ifelse(elecdem < 0.25 & avgtrust >= 1.5,
#                        country, '')),
#     size = 3,
#     check_overlap = T,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   geom_text(
#     aes(label = ifelse(
#       elecdem > 0.75 & avgtrust >= 1.55, as.character(country), ''
#     )),
#     size = 3,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   scale_y_continuous(name = "trust in scientists index (avg)") +
#   scale_x_continuous(name = "electoral democracy index") +
#   gghighlight(elecdem < 0.25 & avgtrust > 1.5) +
#   theme_minimal()

# plot 3 - avg trust index given liberal democracy for all countries

# data2020 %>%
#   group_by(country) %>%
#   mutate(avgtrust = mean(trust_index, na.rm = TRUE)) %>%
#   select(country, regions, libdem, avgtrust) %>%
#   arrange(libdem) %>%
#   ggplot(aes(x = libdem, y = avgtrust, label = country)) +
#   geom_point(size = .75) +
#   geom_text(
#     aes(label = ifelse(libdem < 0.25 & avgtrust >= 1.50, country, '')),
#     size = 3,
#     check_overlap = T,
#     nudge_y = 0.02,
#     nudge_x = -0.01
#   ) +
#   geom_text(aes(label = ifelse(libdem > 0.75 &
#                                  avgtrust >= 1.50, country, '')),
#             size = 3, nudge_y = 0.02) +
#   scale_y_continuous(name = "trust in scientists index (avg)") +
#   scale_x_continuous(name = "liberal democracy index") +
#   gghighlight(libdem < 0.25 & avgtrust >= 1.50) +
#   theme_minimal()

# plot 4 - sciedu by region

data2020 %>%
  #filter(scieduc > 0 & trust_index > 0) %>%
  drop_na() %>%
  ggplot(aes(factor(scieduc), trust_index, color = factor(scieduc))) +
  geom_boxplot(width = 0.8, outlier.size = .5) +
  facet_wrap(~ regions, ncol = 4) +
  gghighlight(calculate_per_facet = TRUE, trust_index > 2.5) +
  scale_y_continuous(
    name = "trust in scientists index",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_x_discrete(
    name = "level of science education",
    breaks = c(0, 1, 2, 3),
    labels = c(
      "0" = "none",
      "1" = "primary",
      "2" = "secondary",
      "3" = "college"
    )
  ) +
  theme_minimal() +
  labs(caption = "Note: Boxplot regions with trust index score > 2.5 are highlighted.") +
  theme(
    legend.position = "none",
    axis.text.x.bottom = element_text(size = 6),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/scieduc2020.png", width = 8.5)

# plot 5 - subj knowledge + trust

# How much do you, personally, know about science?
# Do you know a lot, some, not much, or nothing at all?
# 1=A lot, 2=Some, 3=Not much, 4=Nothing at all, 98=(DK), 99=(Refused)

data2020 %>%
  filter(regions != "Not assigned", subj_sciknow > 0) %>%
  drop_na() %>%
  ggplot(aes(factor(subj_sciknow), trust_index, fill = factor(subj_sciknow))) +
  geom_boxplot(width = .7, outlier.size = .3) +
  facet_wrap(~ regions, ncol = 4) +
  gghighlight(calculate_per_facet = TRUE, trust_index > 2.5) +
  scale_y_continuous(
    name = "trust in scientists index",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_fill_manual(
    breaks = c(1, 2, 3, 4),
    labels = c(
      "1" = "a lot",
      "2" = "some",
      "3" = "not\nmuch",
      "4" = "nothing"
    ),
    values = c("pink", "lightblue", "lightgreen", "lightyellow")
  ) +
  scale_x_discrete(
    name = "perceived knowledge of science",
    breaks = c(1, 2, 3, 4),
    labels = c(
      "1" = "a lot",
      "2" = "some",
      "3" = "not\nmuch",
      "4" = "nothing"
    )
  ) +
  theme_minimal() +
  labs(caption = "Note: Boxplot regions with trust index score > 2.5 are highlighted.") +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/persciknow2020.png")


##### Models #####

colnames(data2020)
#data2020 <- data2020[,-1]

corrdat <- data2020[, c(5:18)] %>% na.omit()
corrdat <- cor(corrdat)
head(round(corrdat, 2))

library(corrplot)

corrplot(
  corrdat,
  method = "square",
  addCoef.col = 'grey66',
  tl.srt = 45,
  tl.cex = 0.85,
  number.cex = 0.8,
  tl.col = 'black',
  cl.length = 5,
  cl.align = "l"
)

# linear model - electoral democracy index
elecmodel20 <-
  lm(
    trust_index ~
      # country-level
      elecdem + country_income +
      # individual-level
      scieduc + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov + 
      educ + income_pers,
    data = data2020
  )
summary(elecmodel20)

# linear model - liberal democracy index
libmodel20 <-
  lm(
    trust_index ~
      # country-level
      libdem + country_income +
      # individual-level
      scieduc + subj_sciknow + sci_benefit +
      sci_over_reli + trust_gov + 
      educ + income_pers,
    data = data2020
  )
summary(libmodel20)

screenreg(list(elecmodel, elecmodelcond, elecmodel20))

htmlreg(
  list(elecmodel, elecmodelcond, elecmodel20),
  booktabs = T,
  dcolumn = T,
  single.row = T
)

# # simple country fixed effects model
# mod <-
#   lm(trust_index_cond ~ elecdem + country - 1, data = data2018)
# screenreg(list(mod, mod20))
#
# # simple country-effects model
# mod20 <- lm(trust_index ~ elecdem + country - 1, data = data2020)
# summary(mod20)
# 
# plotreg(
#   list(mod, mod20),
#   theme = theme_minimal(),
#   custom.coef.map = list(
#     "countryChina" =
#       "China",
#     "countrySaudi Arabia" =
#       "Saudi Arabia",
#     "countryRussia" =
#       "Russia",
#     "countryIran" =
#       "Iran",
#     "countryIndia" =
#       "India",
#     "countryUzbekistan" =
#       "Uzbekistan",
#     "countryNigeria" =
#       "Nigeria",
#     "countryMyanmar" =
#       "Myanmar",
#     "countryPoland" =
#       "Poland",
#     "countryDenmark" =
#       "Denmark",
#     "countryGermany" =
#       "Germany",
#     "countryHungary" =
#       "Hungary"
#   ),
#   ci.force = TRUE,
#   custom.model.names = c("2018", "2020"),
#   signif.light = "gray66",
#   signif.dark = c("red", "blue", "green", "orange", "purple", "yellow"),
#   type = "forest"
# )
# ggsave("images_pres/countrymods.png")

# # country fixed effects models
# data2020fe <- pdata.frame(data2020, index = c("country"))
# 
# femodel20 <- plm(
#   trust_index ~
#     # country-level
#     libdem + country_income +
#     # individual-level
#     scieduc + subj_sciknow + sci_benefit +
#     sci_over_reli + trust_gov + leaders_value +
#     educ + income_pers,
#   data = data2020fe,
#   model = "within"
# )
# summary(femodel20)

#### Further analysis ####

# How does the education level vary by regime type?
# - graphical by region type
# - interaction models

#authoritarian regime < hybrid regime < flawed democracy < full democracy

# function to recode elecdem vdem indicator
elecdem_score <- function(x) {
  case_when(
    x > 0.80 & x < 1.00 ~ "full democracy",
    x > 0.60 & x <= 0.80 ~ "flawed democracy",
    x > 0.40 & x <= 0.60 ~ "hybrid regime",
    #x > 0.00 & x <= 0.40 ~ "authoritarian",
    TRUE ~ "authoritarian")  %>% 
    factor(levels=c('full democracy', 'flawed democracy', 
                    'hybrid regime', 'authoritarian'))
}

##### 2018 data #####

data2018 <- data2018 %>% 
  mutate(elecdem_fct = elecdem_score(elecdem))
  
dat2018 %>%
  filter(
    regions != "Not assigned"
    & regions != "Eastern Africa"
    & regions != "South Asia"
    & regions != "East Asia"
    & regions != "Central Africa"
    #& regions != "Southern Africa"
    & regions != "Northern Europe"
    #& regions != "Southern Europe"
    & regions != "North Africa"
  ) %>%
  drop_na() %>%
  ggplot(aes(factor(scieduc), trust_index_cond, color = elecdem_fct)) +
  geom_boxplot(width = 0.8, outlier.size = .5) +
  facet_wrap(~ as.factor(regions), ncol = 2) +
  #gghighlight(calculate_per_facet = TRUE, trust_index > 2.5) +
  scale_y_continuous(
    name = "trust in scientists index (restricted index)",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_x_discrete(
    name = "level of science education",
    breaks = c(0, 1, 2, 3),
    labels = c(
      "0" = "none",
      "1" = "primary",
      "2" = "secondary",
      "3" = "college"
    )
  ) +
  scale_color_viridis_d(
    name = "electoral democracy index",
    option = "turbo"
  ) +
  theme_minimal() +
  labs(title = "Level of science education and trust across regime types in 2018",
       caption = "Note: Boxplots indicate science education levels across different regime types across major world regions.") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/demsciedu2018.png")

##### 2020 data ######

data2020 <- data2020 %>% 
  mutate(elecdem_fct = elecdem_score(elecdem))

dat2020 %>%
  filter(regions != "Not assigned") %>%
  drop_na() %>%
  ggplot(aes(factor(scieduc), trust_index, color=elecdem_fct)) +
  geom_boxplot(width = 0.8, outlier.size = .5) +
  facet_wrap(~ as.factor(regions), ncol = 2) +
  #gghighlight(calculate_per_facet = TRUE, trust_index > 2.5) +
  scale_y_continuous(
    name = "trust in scientists index (restricted index)",
    breaks = c(1, 2.5, 3.5),
    labels = c("1" = "low",
               "2.5" = "med",
               "3.5" = "high")
  ) +
  scale_x_discrete(
    name = "level of science education",
    breaks = c(0, 1, 2, 3),
    labels = c(
      "0" = "none",
      "1" = "primary",
      "2" = "secondary",
      "3" = "college"
    )
  ) +
  scale_color_viridis_d(
    name = "electoral democracy index",
    option = "turbo", direction = -1
  ) +
  theme_minimal() +
  labs(title = "Level of science education and trust across regime types in 2020",
       caption = "Note: Boxplots indicate science education levels across different regime types across major world regions.") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel"
  )
ggsave("images_pres/demsciedu2020.png")


# how science education and trust vary by regime type

# Most basic bubble plot

scieduc_lvl <- function(x) {
  case_when(
    x >= 2.55 & x <= 3.00 ~ "college",
    x >= 1.55 & x <= 2.45  ~ "secondary",
    x >= 0.55 & x <= 1.45 ~ "primary",
    TRUE ~ "none")  %>%
    factor(levels=c('college', 'secondary', 'primary', 'none'))
}

dat18 <- dat2018 %>%
  group_by(country) %>%
  mutate(
    educm = mean(educ, na.rm = TRUE),
    scieducm = mean(scieduc, na.rm = TRUE),
    trust_indexmcon = mean(trust_index_cond, na.rm = TRUE),
    trust_indexm = mean(trust_index, na.rm = TRUE),
    regionsm = ifelse(
      grepl("Europe", regions),
      "Europe",
      ifelse(
        grepl("Africa", regions),
        "Africa",
        ifelse(
          grepl("Asia", regions),
          "Asia",
          ifelse(
            grepl("Central America", regions),
            "Central/South America",
            ifelse(
              grepl("South America", regions),
              "Central/South America",
              as.character(regions)
            )
          )
        )
      )
    )
  ) %>%
  select(country,
         scieducm,
         trust_indexm,
         trust_indexmcon,
         elecdem,
         elecdem_fct,
         regions,
         regionsm) %>%
  distinct() %>%
  arrange(desc(elecdem)) %>% 
  mutate(
    text = paste(
      "Country: ",
      country,
      "\nRegion: ",
      regions,
      "\nElectoral Democracy Index: ",
      elecdem,
      "\nTrust in Scientists (mean): ",
      round(trust_indexmcon,2),
      "\nScience Education Level (mean): ",
      round(scieducm,2),
      sep = ""
    )
  )

p1 <- dat18 %>%
  filter(regions != "Not assigned") %>%
  #arrange(desc(elecdem)) %>% 
  ggplot(aes(
    x = scieducm,
    y = trust_indexmcon,
    size = desc(elecdem),
    color = elecdem_fct
  )) +
  geom_point(aes(text=text), alpha = 0.6) +
  scale_size(range = c(10, 0.1), name = "electoral democracy index") +
  scale_color_viridis_d(
    name = "",
    option = "magma",
    begin = 0.01,
    end = 0.75,
    direction = -1
  ) +
  theme_minimal() +
  labs(title="How do trust level and science education vary by regime type in 2018?") +
  theme(legend.position = "right") +
  scale_y_continuous(name = "mean trust in scientists (restricted index)") +
  scale_x_continuous(name = "mean level of science education")

p1
ggsave("images_pres/bubbleplot2018b.png")


# turn ggplot interactive with plot
library(plotly)
library(viridis)

pp1 <- ggplotly(p1, tooltip = "text")
pp1

# save the widget
library(htmlwidgets)
saveWidget(pp1, file=paste0( getwd(), "/images_pres/widgets/ggplotlybubblechart18.html"), 
           selfcontained = FALSE)

# 2020 data
dat20 <- dat2020 %>%
  group_by(country) %>%
  mutate(
    educm = mean(educ, na.rm = TRUE),
    scieducm = mean(scieduc, na.rm = TRUE),
    trust_indexm = mean(trust_index, na.rm = TRUE),
    regionsm = ifelse(
      grepl("Europe", regions),
      "Europe",
      ifelse(
        grepl("Africa", regions),
        "Africa",
        ifelse(
          grepl("Asia", regions),
          "Asia",
          ifelse(
            grepl("Central America", regions),
            "Central/South America",
            ifelse(
              grepl("South America", regions),
              "Central/South America",
              as.character(regions)
            )
          )
        )
      )
    )
  ) %>%
  select(country,
         scieducm,
         trust_indexm,
         elecdem,
         elecdem_fct,
         regions,
         regionsm) %>%
  distinct() %>%
  arrange(desc(elecdem)) %>% 
  mutate(
    text = paste(
      "Country: ",
      country,
      "\nRegion: ",
      regions,
      "\nElectoral Democracy Index: ",
      elecdem,
      "\nTrust in Scientists (mean): ",
      round(trust_indexm,2),
      "\nScience Education Level (mean): ",
      round(scieducm,2),
      sep = ""
    )
  )

p2 <- dat20 %>%
  filter(regions != "Not assigned") %>%
  arrange(desc(elecdem)) %>% 
  ggplot(aes(
    x = scieducm,
    y = trust_indexm,
    size = desc(elecdem),
    color = elecdem_fct
  )) +
  geom_point(aes(text=text), alpha = 0.6) +
  scale_size(range = c(10, 0.1), name = "electoral democracy index") +
  scale_color_viridis_d(
    name = "",
    option = "viridis",
    begin = 0.01,
    end = 0.75,
    direction = -1,
  ) +
  theme_minimal() +
  labs(title="How do trust level and science education vary by regime type in 2020?") +
  theme(legend.position = "right") +
  scale_y_continuous(name = "mean trust in scientists (restricted index)") +
  scale_x_continuous(name = "mean level of science education")

p2
ggsave("images_pres/bubbleplot2020.png")


# turn ggplot interactive with plot
library(plotly)
library(viridis)

pp2 <- ggplotly(p2, tooltip = "text")
pp2

# save the widget
library(htmlwidgets)
saveWidget(pp2, file=paste0( getwd(), "/images_pres/widgets/ggplotlybubblechart20.html"),
           selfcontained = FALSE)
