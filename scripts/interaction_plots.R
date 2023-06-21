# plotting
# descriptive stats tables

#install.packages("gtsummary")
library(gtsummary)

dat2018 %>% select(
  -c(
    id,
    natwgt,
    popwgt,
    country,
    regions,
    scieduc_resp,
    obj_sciknow,
    wbi,
    scieduc_level_global,
    scieduc_level,
    scieduc_globalmean,
    trust_index,
    sci_over_reli,
    income_pers,
    scieduc_countrymean,
    urban_rural,
    sci_benefit,
    libdem,
    partipdem
  )
) %>%
  tbl_summary(by = elecdem_fct) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  add_overall() %>%
  as_gt() %>%
  gt::gtsave(filename = "table1.html")



# plot effects
#install.packages("effects")
library(effects)

plot(allEffects(mod1_simple)[5],
     multiline = TRUE,
     ci.style = "bands",
     main = "Interaction plot for 2018 model (full)")

plot(allEffects(mod2_simple)[5],
     multiline = TRUE,
     ci.style = "bands",
     main = "Interaction plot for 2020 model (core)")


#install.packages("interactions")
library(interactions)

mod1_int <-
  lm(trust_index_cond ~ elecdem_fct * scieduc_level_country, dat2018)

plot(
  allEffects(mod1_int),
  multiline = TRUE,
  ci.style = "bands",
  xlab = "Electoral democracy index",
  ylab = "Trust index (restricted)",
  main = "Interaction plot for 2018 model (core)"
)

cat_plot(
  mod1_int,
  pred = scieduc_level_country,
  modx = elecdem_fct,
  geom = "line",
  interval = TRUE,
  legend.main = "Electoral democracy index",
  x.label = "Science education",
  y.label = "Trust index (restricted)",
  main = "Two-way interaction for 2018 core model (regime_type as moderator)"
)

plot(
  allEffects(mod1_int_full)[5],
  multiline = TRUE,
  xlab = "Electoral democracy index",
  ylab = "Trust index (restricted)",
  main = "Interaction plot for 2018 model (full)"
)

cat_plot(
  mod1_int_full,
  pred = scieduc_level_country,
  modx = elecdem_fct,
  geom = "line",
  interval = TRUE,
  legend.main = "Electoral democracy index",
  x.label = "Science education",
  y.label = "Trust index (restricted)",
  main = "Two-way interaction for 2018 full model (regime_type as moderator)"
)

dev.off()

library(png)
library(grid)
library(gridExtra)

getwd()

plot1 <- readPNG('intplot1.png')
plot2 <- readPNG('intplot2.png')
plot3 <- readPNG('intplot3.png')
plot4 <- readPNG('intplot4.png')

grid.arrange(rasterGrob(plot1), rasterGrob(plot2),
             ncol = 2)
grid.arrange(rasterGrob(plot3), rasterGrob(plot4),
             ncol = 2)


# Plot interaction effect
# Fit linear regression model
summary(mod1_int)

# Predict trust levels and confidence intervals for each observation
predictions <- predict(mod1_int, dat2018, interval = "confidence")

# Add predictions and confidence intervals to data frame
dat <- cbind(dat2018, predictions)

# Plot interaction effect
ggplot(dat,
       aes(x = elecdem, y = fit, color = scieduc_level_country)) +
  geom_smooth(method = "lm") +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Democracy Score", y = "Trust Level", color = "Science Education Level")

# 2020 interaction plots

plot(
  allEffects(mod2_int),
  multiline = TRUE,
  ci.style = "bands",
  xlab = "Electoral democracy index",
  ylab = "Trust index (restricted)",
  main = "Interaction plot for 2020 model (core)"
)

cat_plot(
  mod2_int,
  pred = scieduc_level_country,
  modx = elecdem_fct,
  geom = "line",
  interval = TRUE,
  legend.main = "Electoral democracy index",
  x.label = "Science education",
  y.label = "Trust index (restricted)",
  main = "Two-way interaction for 2020 core model (regime_type as moderator)"
)

plot(
  allEffects(mod2_int_full)[5],
  multiline = TRUE,
  ci.style = "bands",
  xlab = "Electoral democracy index",
  ylab = "Trust index (restricted)",
  main = "Interaction plot for 2020 model (full)"
)

cat_plot(
  mod2_int_full,
  pred = scieduc_level_country,
  modx = elecdem_fct,
  geom = "line",
  interval = TRUE,
  legend.main = "Electoral democracy index",
  x.label = "Science education",
  y.label = "Trust index (restricted)",
  main = "Two-way interaction for 2020 full model (regime_type as moderator)"
)

dev.off()

library(png)
library(grid)
library(gridExtra)

plot1 <- readPNG('images_pres/intplot1b.png')
plot2 <- readPNG('images_pres/intplot2b.png')
plot3 <- readPNG('images_pres/intplot3b.png')
plot4 <- readPNG('images_pres/intplot4b.png')

grid.arrange(rasterGrob(plot1), rasterGrob(plot2),
             ncol = 2)
grid.arrange(rasterGrob(plot3), rasterGrob(plot4),
             ncol = 2)

### marginal Effects plots

table(dat2018$scieduc_level_country)

#install.packages("interplot")
#install.packages("marginaleffects")
library(marginaleffects)
library(interplot)

#dat2020$scieduc_level_countryh <- relevel(dat2020$scieduc_level_country, ref=2)
# m <-lm(trust_index_cond ~ elecdem * scieduc_level_countryh, data=dat2018)
# interplot(m, var1="elecdem", var2="scieduc_level_countryh")
#
# df <- marginaleffects(mod_a,
#                 newdata = datagrid(scieduc_level_country=c("high")),
#                 vcov = sandwich::vcovCL(mod_a, type = "HC1", cluster = ~ country))
# plot(df)

# problems with displaying both curves for factor scieduc

p1 <- plot_cme(
  mod_a_high,
  effect = "scieduc_level_countryh",
  condition = "elecdem",
  vcov = sandwich::vcovCL(mod_a, type = "HC1", cluster = ~ country)) +
  geom_rug(aes(x = elecdem), data = dat2018) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  theme_minimal() +
  labs(title = "Conditional marginal effect of science education in 2018") +
  xlab("electoral democracy index") +
  ylab("marginal effect of science education (baseline=low) on trust index")

#caption="Note: y-axis shows effect on trust of change in science education from baseline 'low' to 'high'")

p2 <- plot_cme(
  mod_b,
  effect = "scieduc_level_countryh",
  condition = "elecdem",
  vcov = sandwich::vcovCL(mod_a, type = "HC1", cluster = ~ country)) +
  geom_rug(aes(x = elecdem), data = dat2020) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  theme_minimal() +
  labs(title = "Conditional marginal effect of science education in 2020") +
  xlab("electoral democracy index") +
  ylab("marginal effect of science education (baseline=low) on trust index")

library(gridExtra)
grid.arrange(p1, p2,ncol = 2)

# categorical variable
p3 <- plot_cme(
  mod_c,
  effect = "scieduc_level_countryh",
  condition = "elecdem_fct",
  vcov = sandwich::vcovCL(mod_c, type = "HC1", cluster = ~ country))+
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  theme_minimal() +
  labs(title = "Conditional marginal effect of science education in 2018") +
  xlab("regime type") +
  ylab("marginal effect of science education (baseline=low) on trust index")


p4 <- plot_cme(
  mod_d,
  effect = "scieduc_level_countryh",
  condition = "elecdem_fct",
  vcov = sandwich::vcovCL(mod_d, type = "HC1", cluster = ~ country)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  theme_minimal() +
  labs(title = "Conditional marginal effect of science education in 2020") +
  xlab("regime type") +
  ylab("marginal effect of science education (baseline=low) on trust index")

library(gridExtra)
grid.arrange(p3, p4, ncol = 2)
# ggsave("images_pres/cme_categorical.png")

