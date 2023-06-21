

# # plot models - predicted values
# libdem_pred <- predict(
#   libmodel,
#   newdata = data.frame(
#     trust_index = 1:4,
#     libdem = mean(data2018$libdem),
#     sciedu = mean(data2018$sciedu),
#     obj_sciknow = mean(data2018$obj_sciknow),
#     subj_sciknow = mean(data2018$subj_sciknow),
#     ViewOfScience = mean(data2018$ViewOfScience),
#     sci_over_reli = mean(data2018$sci_over_reli),
#     trust_gov = mean(data2018$trust_gov),
#     Education = mean(data2018$Education),
#     Urban_Rural = mean(data2018$Urban_Rural),
#     Subjective_Income = mean(data2018$Subjective_Income),
#     se = TRUE
#   )
# )
#
# libdem_pred
#
# ggplot(cbind(libdem = 0:1, as.data.frame(libdem_pred[1:2])), aes(libdem, libmodel)) +
#   geom_ribbon(aes(ymin = libmodel - 1.96 * se.fit, ymax = libmodel + 1.96 *
#                     se.fit),
#               alpha = 0.15) +
#   geom_line(linewidth = 1) +
#   ggtitle('Predicted values of libdem')


##### elecdem plots #####

combined1 <- data2018 %>%
  group_by(country) %>%
  mutate(avgtrust = mean(trust_index_cond, na.rm = T),
         year = "2018") %>%
  select(country, elecdem, avgtrust) %>%
  arrange(elecdem) %>% distinct() %>%
  ungroup()

combined2 <- data2020 %>%
  group_by(country) %>%
  mutate(avgtrust = mean(trust_index, na.rm = T)) %>%
  select(country, elecdem, avgtrust) %>%
  arrange(elecdem) %>% distinct() %>%
  ungroup()

combined1$year <- rep(2018, length(nrow(data2018)))
combined2$year <- rep(2020, length(nrow(data2020)))

table(is.na(combined1))
table(is.na(combined2))

# 2018 highlight elecdem
ggplot(combined1, aes(x = elecdem, y = avgtrust, label = country)) +
  geom_point(size = .75, color = "red") +
  # geom_point(data = combined2,
  #            color = "blue",
  #            size = .75) +
  geom_text_repel(
    aes(label = ifelse(elecdem < 0.25 & avgtrust > 2.5, country, '')),
    size = 3,
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 50000
  ) +
  geom_text_repel(
    aes(label = ifelse(elecdem > 0.85 & avgtrust > 2.5, country, '')),
    size = 3,
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 50000
  ) +
  scale_y_continuous(name = "trust in scientists index (avg)") +
  scale_x_continuous(name = "electoral democracy index") +
  gghighlight(avgtrust > 2.5) +
  theme_minimal() +
  labs(caption = "Note: Only countries with electoral democracy scores < 0.25 (autocratic) and > 0.85 (democractic) with 
       avg trust scores > 2.5 (medium) in 2018 are annotated.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "panel")

ggsave("images_pres/trustelecdem2018.png")


# 2020 highlight elecdem
ggplot(combined2, aes(x = elecdem, y = avgtrust, label = country)) +
  geom_point(size = .75, color = "blue") +
  # geom_point(data = combined1,
  #            color = "red",
  #            size = .75) +
  geom_text_repel( 
    aes(label = ifelse(elecdem < 0.25 & avgtrust > 2.5, country, '')),
    size = 3,
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 5000
  ) +
  geom_text_repel(
    aes(label = ifelse(elecdem > 0.85 & avgtrust > 2.5, country, '')),
    size = 3,
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 5000
  ) +
  scale_y_continuous(name = "trust in scientists index (avg)") +
  scale_x_continuous(name = "electoral democracy index") +
  gghighlight(avgtrust > 2.5) +
  labs(caption = "Note: Only countries with electoral democracy scores < 0.25 (autocratic) and > 0.85 (democractic) 
       with avg trust scores > 2.5 (medium) in 2020 are annotated.") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "panel")

ggsave("images_pres/trustelecdem2020.png")


#### libdem plots ####

combined1 <- data2018 %>%
  group_by(country) %>%
  mutate(avgtrust = mean(trust_index_cond, na.rm = T),
         year = "2018") %>%
  select(country, libdem, avgtrust) %>%
  arrange(libdem) %>% distinct() %>%
  ungroup()

combined2 <- data2020 %>%
  group_by(country) %>%
  mutate(avgtrust = mean(trust_index, na.rm = T)) %>%
  select(country, libdem, avgtrust) %>%
  arrange(libdem) %>% distinct() %>%
  ungroup()

combined1$year <- rep(2018, length(nrow(data2018)))
combined2$year <- rep(2020, length(nrow(data2020)))

table(is.na(combined1))
table(is.na(combined2))


# 2018 highlight libdem
ggplot(combined1, aes(x = libdem, y = avgtrust, label = country)) +
  geom_point(size = .75, color = "red") +
  # geom_point(data = combined2,
  #            color = "blue",
  #            size = .75) +
  geom_text_repel(
    aes(label = ifelse(libdem < 0.25 & avgtrust > 2.5,
                       country, '')),
    size = 3,
    #color = "red",
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 5000
  ) +
  geom_text_repel(
    aes(label = ifelse(libdem > 0.85 & avgtrust > 2.5, country, '')),
    size = 3,
    #color = "red",
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 5000
  ) +
  scale_y_continuous(name = "trust in scientists index (avg)") +
  scale_x_continuous(name = "liberal democracy index") +
  gghighlight(avgtrust > 2.5) +
  theme_minimal() +
  labs(caption = "Note: Only countries with liberal democracy scores < 0.25 (autocratic) and > 0.85 (democractic)
    and avg trust scores > 2.5 in 2018 are annotated.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "panel")

ggsave("images_pres/modtrustlibdem2018.png")


# 2020 highlight libdem
ggplot(combined2, aes(x = libdem, y = avgtrust, label = country)) +
  geom_point(size = .75, color = "blue") +
  # geom_point(data = combined1,
  #            color = "red",
  #            size = .75) +
  geom_text_repel(
    aes(label = ifelse(libdem < 0.25 & avgtrust > 2.5, country, '')),
    size = 3,
    # color = "blue",
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 200
  ) +
  geom_text_repel(
    aes(label = ifelse(libdem > 0.85 & avgtrust > 2.5, country, '')),
    size = 3,
    # color = "blue",
    nudge_y = 0.02,
    nudge_x = -0.01,
    max.overlaps = 200
  ) +
  scale_y_continuous(name = "trust in scientists index (avg)") +
  scale_x_continuous(name = "liberal democracy index") +
  gghighlight(avgtrust > 2.5) +
  theme_minimal() +
  labs(caption = "Note: Countries with electoral democracy scores < 0.25 (autocratic) and > 0.89 (democractic)
       with avg trust scores > 2.5 in 2020 are annotated.") +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "panel")

ggsave("images_pres/modtrustlibdem2020.png")

##############

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

dat <- data2020 %>%
  ungroup() %>% 
  select(trust_gov, leaders_value, trust_science) %>%
  filter(!trust_gov == 0,!leaders_value == 0,!trust_science == 0)

dat$trust_gov <- factor(
  dat$trust_gov,
  levels = c(1:4),
  labels = c("A lot", "Some", "Not much", "None")
)

dat$trust_science <- factor(
  dat$trust_science,
  levels = c(1:4),
  labels = c("A lot", "Some", "Not much", "None")
)

dat$leaders_value <- factor(
  dat$leaders_value,
  levels = c(1:4),
  labels = c("A lot", "Some", "Not much", "None")
)

dat <- dat %>% 
  rename("Trust in government" = trust_gov, 
         "Trust in science" = trust_science,
         "Leaders value science" = leaders_value) %>% 
  ggsankey::make_long("Trust in government",
            "Leaders value science", 
            "Trust in science") 

# sankey diagram
ggplot(
  dat,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = factor(node),
    label = node)
  ) +
  geom_sankey(
    flow.alpha = 0.4,
    linewidth = 0.8,
    width = .03,
    #smooth = 5,
    node.color = "white",
    show.legend = FALSE
  ) + 
  geom_sankey_label(
    size = 3, 
    color = "black",
    fill = "white",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(colour="black", size=12, hjust=0.5, vjust=-2),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="black", size=10, vjust=6),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title="% of respondents with different levels of trust in government and in science") + 
  scale_fill_viridis_d(option = "turbo")

ggsave("images_pres/leadersvalue2020.png")

