# ECR application graphs

# set working directory
getwd()
setwd(
  "/home/sukayna/Drive/PhD/my_vault/projects/science-perceptions-survey/exploratory_analysis"
)

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
data2018 <-
  read.csv(
    "~/Drive/PhD/my_vault/projects/science-perceptions-survey/exploratory_analysis/data/cleaned_dat2018.csv",
    header = T
  )
colnames(data2018)

data2020 <-
  read.csv(
    "~/Drive/PhD/my_vault/projects/science-perceptions-survey/exploratory_analysis/data/cleaned_dat2020.csv",
    header = T
  )
colnames(data2020)

# data processing

dat20 <- data2020 %>%
  mutate_at(
    vars(leaders_value, trust_science),
    recode,
    '1' = 4,
    '2' = 3,
    '3' = 2,
    '4' = 1,
    '0' = 0
  ) %>%
  group_by(country) %>%
  mutate(
    trust_sciencem = mean(trust_science, na.rm = TRUE),
    trust_indexm = mean(trust_index, na.rm = TRUE),
    leaders_valuem = mean(leaders_value, na.rm = TRUE),
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
  select(
    country,
    trust_sciencem,
    trust_indexm,
    leaders_valuem,
    elecdem,
    elecdem_fct,
    regionsm
  ) %>%
  distinct() %>%
  arrange(desc(elecdem)) %>%
  mutate(
    txt = paste(
      "Country: ",
      country,
      "\nRegion: ",
      regionsm,
      "\nElectoral Democracy Index: ",
      elecdem,
      "\nTrust in Science (mean): ",
      round(trust_sciencem, 2),
      "\nTrust in Scientists (mean): ",
      round(trust_indexm, 2),
      "\nLeaders value Science (mean): ",
      round(leaders_valuem, 2),
      sep = ""
    )
  )

colnames(dat20)

# 1) scatterplot of avg. trust in scientists across regimes

p1 <- dat20 %>%
  filter(regionsm != "Not assigned") %>%
  #arrange(desc(elecdem)) %>%
  ggplot(aes(x = elecdem,
             y = trust_indexm,
             color = elecdem_fct)) +
  geom_point(size = 2.5, alpha = 0.65) +
  geom_smooth(method = "lm", se=TRUE, 
              color="gray45", 
              size=.5, alpha=.2,
              formula = y ~ x) +
  scale_color_viridis_d(
    name = "",
    option = "magma",
    begin = 0.01,
    end = 0.70,
    direction = 1
  ) +
  theme_light() +
  labs(
    title = "Trust in Scientists Across Regimes",
    subtitle = "Aggregated trust in scientists index for 113 countries
       (Wellcome Trust 2020)",
    caption = "*Electoral democracy score (V-Dem, 2020)"
  ) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(),
    plot.title = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 13
    ),
    plot.subtitle = element_text(
      lineheight = 1,
      family = "Helvetica",
      face = "italic",
      size = 9
    ),
    plot.caption = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 9,
      hjust = 0
    ),
    plot.caption.position = "panel",
    axis.title.x = element_text(family = "Helvetica", size = 9),
    axis.text.y = element_text(family = "Helvetica", size = 8),
    axis.text.x = element_text(family = "Helvetica", size = 8),
    axis.title.y = element_text(family = "Helvetica", size = 9),
    legend.title = element_text(family = "Helvetica", size = 9)
  ) +
  scale_y_continuous(name = "trust in scientists (mean)") +
  scale_x_continuous(name = "electoral democracy index") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  annotate("text",  label = "higher trust",
           x = 0,
           y = 3.1,
           hjust = 0,
           vjust = -1,
           size = 3,
           angle = 90
  ) + 
  annotate("segment", x = 0, y = 3.1, xend = 0, yend = 3.5,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))
  ) +
  annotate("text",  label = "lower trust",
           x = 0,
           y = 2.7,
           hjust = 0,
           vjust = -1,
           size = 3,
           angle = 90
  ) +
  annotate("segment", x = 0, y = 2.87, xend = 0, yend = 2.47,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) 
p1

ggsave("images_ecr/trustscientists2020.png")


# 2) do leaders value science

p2 <- dat20 %>%
  filter(regionsm != "Not assigned") %>%
  #arrange(desc(elecdem)) %>%
  ggplot(aes(x = elecdem,
             y = leaders_valuem,
             color = elecdem_fct)) +
  geom_point(size = 2.5, alpha = 0.65) +
  geom_smooth(method = "lm", se=TRUE, 
              color="gray45", 
              size=.5, alpha=.2,
              formula = y ~ x) +
  expand_limits(y = c(1.3, 3.8)) +
  scale_color_viridis_d(
    name = "",
    option = "magma",
    begin = 0.01,
    end = 0.70,
    direction = 1,
    na.value = "grey50"
  ) +
  theme_light() +
  labs(
    title = "Do National Leaders Value Scientists?",
    subtitle = "In general, how much do you think the leaders in the national government
       value the opinions and expertise of scientists? (Wellcome Trust 2020)",
    caption = "*Not asked in Tajikistan, Saudi Arabia, United Arab Emirates"
  ) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"), 
    plot.title = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 13
    ),
    plot.subtitle = element_text(
      lineheight = 1,
      family = "Helvetica",
      face = "italic",
      size = 9
    ),
    plot.caption = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 9,
      hjust = -0
    ),
    plot.caption.position = "panel",
    axis.title.x = element_text(family = "Helvetica", size = 9),
    axis.text.y = element_text(family = "Helvetica", size = 8),
    axis.text.x = element_text(family = "Helvetica", size = 8),
    axis.title.y = element_text(family = "Helvetica", size = 9),
    legend.title = element_text(family = "Helvetica", size = 9)
  ) +
  scale_y_continuous(name = "whether leaders value scientists (mean)") +
  scale_x_continuous(name = "electoral democracy index") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  annotate("text",  label = "value more",
    x = 0,
    y = 3,
    hjust = 0,
    vjust = -1,
    size = 3,
    angle = 90
  ) + 
  annotate("segment", x = 0, y = 3, xend = 0, yend = 3.6,
                arrow = arrow(type = "closed", length = unit(0.02, "npc"))
           ) +
  annotate("text",  label = "value less",
           x = 0,
           y = 1.8,
           hjust = 0,
           vjust = -1,
           size = 3,
           angle = 90
  ) +
  annotate("segment", x = 0, y = 2.4, xend = 0, yend = 1.8,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))

p2

ggsave("images_ecr/lmleadersvalue2020.pdf")

# Q: In general, how much do you think the leaders in the national government
# value the opinions and expertise of scientists?
# A lot, some, not much, or not at all?

# 3) scatterplot of avg. trust in science

p3 <- dat20 %>%
  filter(regionsm != "Not assigned") %>%
  ggplot(aes(x = elecdem,
             y = trust_sciencem,
             color = elecdem_fct)) +
  geom_point(size = 2.5, alpha = 0.65) +
  geom_smooth(method = "lm", se=TRUE, 
              color="gray45", 
              size=.5, alpha=.2,
              formula = y ~ x) +
  expand_limits(y = c(1.3, 3.8)) +
  scale_color_viridis_d(
    name = "",
    option = "magma",
    begin = 0.01,
    end = 0.70,
    direction = 1
  ) +
  theme_light() +
  labs(
    title = "Trust in Science Across Regimes",
    subtitle = "In general, would you say that you trust science
       a lot, some, not much, or not at all? (Wellcome Trust 2020)",
    caption = "*Single survey item fielded in 113 countries"
  ) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(),
    plot.title = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 13
    ),
    plot.subtitle = element_text(
      lineheight = 1,
      family = "Helvetica",
      face = "italic",
      size = 9
    ),
    plot.caption = element_text(
      lineheight = 1,
      family = "Helvetica",
      size = 9,
      hjust = 0
    ),
    plot.caption.position = "panel",
    axis.title.x = element_text(family = "Helvetica", size = 9),
    axis.text.y = element_text(family = "Helvetica", size = 8),
    axis.text.x = element_text(family = "Helvetica", size = 8),
    axis.title.y = element_text(family = "Helvetica", size = 9),
    legend.title = element_text(family = "Helvetica", size = 9)
  ) +
  scale_y_continuous(name = "trust in scientists (mean)") +
  scale_x_continuous(name = "electoral democracy index") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  annotate("text",  label = "higher trust",
           x = 0,
           y = 3,
           hjust = 0,
           vjust = -1,
           size = 3,
           angle = 90
  ) + 
  annotate("segment", x = 0, y = 3, xend = 0, yend = 3.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))
  ) +
  annotate("text",  label = "lower trust",
           x = 0,
           y = 1.8,
           hjust = 0,
           vjust = -1,
           size = 3,
           angle = 90
  ) +
  annotate("segment", x = 0, y = 2.4, xend = 0, yend = 1.8,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))

p3

ggsave("images_ecr/lmtrustscience2020.pdf")


# grid layout

p2
p3

library(gridExtra)
grid.arrange(p3, p2, nrow = 1)


#ggsave("images_ecr/trustplots.png")
