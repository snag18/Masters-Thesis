
# Bayesian Regression and Prediction in Multilevel Moels for Democracy, Misinformation, Polarization 
# Using V-Dem Dataset 12 and Digital Society Project (DSP) Dataset
# Hyp: > Misinformation + > Polarizartion -- < Democracy and vice versa

# packages
library(rmarkdown)
library(tinytex)
library(rstan)
library(rstanarm)
library(plotly)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidybayes)
library(broom.mixed)
library(modelsummary)
options(mc.cores = parallel::detectCores())

render("Bayesian_Reg_VDem_DSP.Rmd", "pdf_document")

# datasets
vdem_main <- read.csv("V-Dem-CY-Full+Others-v12.csv")
vdemdsp_main <- read.csv("DSP-Dataset-v2.csv")

#filtering 
vdem <- vdem_main %>%
  filter(country_name %in% c("Brazil", "Chile", "Egypt", "Germany", "Hungary", "India", "Poland", "Saudi Arabia", "Sri Lanka", "Turkey", "Sweden", "United Kingdom", "United States of America")) %>%
  filter(year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) %>%
  select(country_name, year, v2x_regime, v2cacamps)

dsp <- dsp_main %>%
  filter(country_name %in% c("Brazil", "Chile", "Egypt", "Germany", "Hungary", "India", "Poland", "Saudi Arabia", "Sri Lanka", "Turkey", "Sweden", "United Kingdom", "United States of America")) %>%
  filter(year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) %>%
  select(country_name, year, v2smpolsoc, v2smpolhate, v2smgovdom, v2smpardom)
  
country <- vdem$country_name
id <- vdem$country_id
year <- vdem$year

# USA, UK, Sweden, Germany, Chile, Hungary, Poland, Brazil, Turkey, India, Sri Lanka, Saudi Arabia, Egypt
# 2009 - 2019 (start of internet + end of dsp data + control for pandemic)
# 0 - CA, 1 - EA, 2 - ED, 3 - LD

# Measure Democracy - vdem 
# from other indies Regimes of the World (RoW) index as DV
# Regimes of the World (v2x_regime) - Closed Autocracy, Electoral Autocracy, Electoral Democracy, Liberal Democracy
RoW <- vdem$v2x_regime # Regimes of the World - 0 Closed Autocracy, 1 Electoral Autocracy, 2 Electoral Democracy, 3 Liberal Democracy
polpl <- vdem$v2cacamps #Political polarization

# Measure Polarisation - DSP Social Cleavages 
# Polarisation of Society (v2smpolsoc) as DV2
# Political Parties Hate Speech (v2smpolhate) IV2
polsoc <- dsp$v2smpolsoc # Polarization of Society
polhs <- dsp$v2smpolhate # Political Parties Hate Speech

# Measure Disinformation - DSP Coordinated Information Operations
# Government Dissemination of False Information Domestic (v2smgovdom) as IV3
# Party Dissemination of False Information Domestic (v2smpardom) as IV4
disgov <- dsp$v2smgovdom # Government dissemination of false information domestic
disparty <- dsp$v2smpardom # Party dissemination of false information domestic

# Less liberal democracy in closed and electoral autocracies
# More liberal democracy in electoral and liberal democracies
# More Polarizarion in Closed Autocracies and Electoral Autocracies
# Less Polarization in  Electoral Democracies and Liberal Democracies
# More Polarization means more Hate Speech, more Government and Party Disinformation
# More Polarized Countries are Closed and Electoral Autocracies

### Data Exploration with Summary Statistics 

## Summary Statistics
summary(vdem)
summary(dsp) 

## Plotting
# Countries in Specific Regimes
fig1 <- vdem %>%
  ggplot(aes(RoW, country, col = country, size = RoW, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Regimes of the World", y = "Country")
fig1

country_regimes <- ggplotly(fig1, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
country_regimes
htmlwidgets::saveWidget(country_regimes, "country_regimes.html")
# trend of countries moving towards autocracy

# Political Polarization
fig2 <- vdem %>%
  ggplot(aes(polpl, country, col = country, size = polpl, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Polarization", y = "Country")
fig2

country_pol <- ggplotly(fig2, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
country_pol
htmlwidgets::saveWidget(country_pol, "country_pol.html")
# countries got more polarized between 2009 and 2019

# Political Polarization in Specific Regimes
fig3 <- vdem %>%
  ggplot(aes(RoW, polpl, col = country, size = RoW, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Regimes", y = "Polarization")
fig3

regime_pol <- ggplotly(fig3, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
regime_pol
htmlwidgets::saveWidget(regime_pol, "regime_pol.html")
# countries got more polarized between 2009 and 2019 and as electoral and liberal democracies moved towards electoral autocracies polarization increased

# Societal Polarization in Countries
fig4 <- dsp %>%
  ggplot(aes(polsoc, country, col = country, size = polsoc, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Polarization", y = "Country")
fig4

polarization <- ggplotly(fig4, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
polarization
htmlwidgets::saveWidget(polarization, "polarization.html")

# Hate Speech in Countries
fig5 <- dsp %>%
  ggplot(aes(polhs, country, col = country, size = polhs, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Hate Speech", y = "Country")
fig5

hate_speech <- ggplotly(fig5, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
hate_speech
htmlwidgets::saveWidget(hate_speech, "hate_speech.html")

# Government Disinformation
fig6 <- dsp %>%
  ggplot(aes(disgov, country, col = country, size = disgov, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Government Disinformation", y = "Country")
fig6

gov_disinfo <- ggplotly(fig6, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
gov_disinfo
htmlwidgets::saveWidget(gov_disinfo, "gov_disinfo.html")

# Party Disinformation
fig7 <- dsp %>%
  ggplot(aes(disparty, country, col = country, size = disparty, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Party Disinformation", y = "Country")
fig7

par_disinfo <- ggplotly(fig7, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
par_disinfo
htmlwidgets::saveWidget(par_disinfo, "par_disinfo.html")

# Societal Polarization and Hate Speech
fig8 <- dsp %>%
  ggplot(aes(polsoc, polhs, col = country, size = polsoc, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Polarization", y = "Hate Speech")
fig8

pol_hate <- ggplotly(fig8, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
pol_hate
htmlwidgets::saveWidget(pol_hate, "pol_hate.html")

# Societal Polarization and Government Disinformation
fig9 <- dsp %>%
  ggplot(aes(polsoc, disgov, col = country, size = polsoc, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Polarization", y = "Government Disinformation")
fig9

pol_gov_disinfo <- ggplotly(fig9, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
pol_gov_disinfo
htmlwidgets::saveWidget(pol_gov_disinfo, "pol_gov_disinfo.html")

# Societal Polarization and Party Disinformation
fig10<- dsp %>%
  ggplot(aes(polsoc, disparty, col = country, size = polsoc, label = country)) +
  geom_point() +
  facet_grid(. ~ year) +
  labs(x = "Polarization", y = "Party Disinformation")
fig10

pol_par_disinfo <- ggplotly(fig10, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
pol_par_disinfo
htmlwidgets::saveWidget(pol_par_disinfo, "pol_par_disinfo.html")

## Bayesian Regression and Inference with Multilevel Models

## Single Predictor Models
# Political Polarization is more in Electoral Autocracies and the Democracies
lm1 <- stan_glm(RoW ~ polpl, vdem, family = gaussian)
summary(lm1)

tidy_coef <- tidy(lm1)
model_intercept1 <- tidy_coef$estimate[1]
model_slope1 <- tidy_coef$estimate[2]
draws1 <- spread_draws(lm1, `(Intercept)`, polpl)

fig11 <- ggplot(vdem, aes(x = RoW, y = polpl)) +
  labs(x = "Regimes", y = "Polarization") +
  geom_point() +
  geom_abline(data = draws1, aes(intercept = `(Intercept)`, slope = polpl),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept1, slope = model_slope1)
fig11

# with country factor
lm2 <- stan_glm(RoW ~ polpl + factor(country), vdem, family = gaussian)
summary(lm2)

tidy_coef <- tidy(lm2)
model_intercept2 <- tidy_coef$estimate[1]
model_slope2 <- tidy_coef$estimate[2]
draws2 <- spread_draws(lm2, `(Intercept)`, polpl)

fig12 <- ggplot(vdem, aes(x = RoW, y = polpl)) +
  labs(x = "Regimes", y = "Polarization") +
  geom_point() +
  geom_abline(data = draws2, aes(intercept = `(Intercept)`, slope = polpl),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept2, slope = model_slope2)
fig12

# compare both models
loo1 <- loo(lm1)
loo2 <- loo(lm2)
loo_compare(loo1, loo2)
# lm2 is better due to factorizing by country 

# Societal Polarization and Hate Speech move in the same direction
lm3 <- stan_glm(polsoc ~ polhs, dsp, family = gaussian)
summary(lm3)

tidy_coef <- tidy(lm3)
model_intercept3 <- tidy_coef$estimate[1]
model_slope3 <- tidy_coef$estimate[2]
draws3 <- spread_draws(lm3, `(Intercept)`, polhs)

fig13 <- ggplot(dsp, aes(x = polsoc, y = polhs)) +
  labs(x = "Polarization", y = "Hate Speech") +
  geom_point() +
  geom_abline(data = draws3, aes(intercept = `(Intercept)`, slope = polhs),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept3, slope = model_slope3)
fig13

# with country factor 
lm4 <- stan_glm(polsoc ~ polhs + factor(country), dsp, family = gaussian)
summary(lm4)

tidy_coef <- tidy(lm4)
model_intercept4 <- tidy_coef$estimate[1]
model_slope4 <- tidy_coef$estimate[2]
draws4 <- spread_draws(lm4, `(Intercept)`, polhs)

fig14 <- ggplot(dsp, aes(x = polsoc, y = polhs)) +
  labs(x = "Polarization", y = "Hate Speech") +
  geom_point() +
  geom_abline(data = draws4, aes(intercept = `(Intercept)`, slope = polhs),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept4, slope = model_slope4)
fig14

# compare both models
loo3 <- loo(lm3)
loo4 <- loo(lm4)
loo_compare(loo3, loo4)
#lm4 is the better model due to factorization by country

# Societal Polarization and Government Disinformation move in different direction but my very little margin
lm5 <- stan_glm(polsoc ~ disgov, dsp, family = gaussian)
summary(lm5)

tidy_coef <- tidy(lm5)
model_intercept5 <- tidy_coef$estimate[1]
model_slope5 <- tidy_coef$estimate[2]
draws5 <- spread_draws(lm5, `(Intercept)`, disgov)

fig15 <- ggplot(dsp, aes(x = polsoc, y = disgov)) +
  labs(x = "Polarization", y = "Government Disinformation") +
  geom_point() +
  geom_abline(data = draws5, aes(intercept = `(Intercept)`, slope = disgov),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept5, slope = model_slope5)
fig15

lm6 <- stan_glm(polsoc ~ disgov + factor(country), dsp, family = gaussian)
summary(lm6)

tidy_coef <- tidy(lm6)
model_intercept6 <- tidy_coef$estimate[1]
model_slope6 <- tidy_coef$estimate[2]
draws6 <- spread_draws(lm6, `(Intercept)`, disgov)

fig16 <- ggplot(dsp, aes(x = polsoc, y = disgov)) +
  labs(x = "Polarization", y = "Government Disinformation") +
  geom_point() +
  geom_abline(data = draws6, aes(intercept = `(Intercept)`, slope = disgov),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept6, slope = model_slope6)
fig16

# compare both models
loo5 <- loo(lm5)
loo6 <- loo(lm6)
loo_compare(loo5, loo6)
# lm6 is the better model due to factorization by country 

# Societal Polarization and Party Disinformation move in different direction but my very little margin
lm7 <- stan_glm(polsoc ~ disparty, dsp, family = gaussian)
summary(lm7)

tidy_coef <- tidy(lm7)
model_intercept7 <- tidy_coef$estimate[1]
model_slope7 <- tidy_coef$estimate[2]
draws7 <- spread_draws(lm7, `(Intercept)`, disparty)

fig17 <- ggplot(dsp, aes(x = polsoc, y = disparty)) +
  labs(x = "Polarization", y = "Party Disinformation") +
  geom_point() +
  geom_abline(data = draws7, aes(intercept = `(Intercept)`, slope = disparty),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept7, slope = model_slope7)
fig17

# with country factor
lm8 <- stan_glm(polsoc ~ disparty + factor(country), dsp, family = gaussian)
summary(lm8)

tidy_coef <- tidy(lm8)
model_intercept8 <- tidy_coef$estimate[1]
model_slope8 <- tidy_coef$estimate[2]
draws8 <- spread_draws(lm8, `(Intercept)`, disparty)

fig18 <- ggplot(dsp, aes(x = polsoc, y = disparty)) +
  labs(x = "Polarization", y = "Party Disinformation") +
  geom_point() +
  geom_abline(data = draws8, aes(intercept = `(Intercept)`, slope = disparty),    
              size = 0.2, alpha = 0.1, color = "red") +  
  geom_abline(intercept = model_intercept8, slope = model_slope8)
fig18

# compare both models 
loo7 <- loo(lm7)
loo8 <- loo(lm8)
loo_compare(loo7, loo8)
# lm8 is better due to country factor

# summarizing the better models
models <- list(lm4, lm6, lm8)
modelsummary(models, statistic = "conf.int")

# comparing better models 
loo_compare(loo4, loo6, loo8)
# lm4 is best model, lm6 is an okay model, lm8 is the worst model
# political hate speech is the best predictor of polarization that disinformation in general
# government disinformation is a better predictor of polarization that party dissemination
# government disinformation is the main hypothesis subject and gets an average score

## Multi-predictor Model 
lm9 <- stan_glm(polsoc ~ polhs + disgov + disparty, dsp, family = gaussian)
summary(lm9)

lm10 <- stan_glm(polsoc ~ polhs + disgov + disparty + factor(country), dsp, family = gaussian)
summary(lm10)

# compare both models
loo9 <- loo(lm9)
loo10 <- loo(lm10)
loo_compare(loo9, loo10)
# lm10 is better model due to factor country 

# compare best models 
loo_compare(loo4, loo6, loo10)
# lm10 is better model due to having all factors

## Bayesian Prediction for Best Model (predicing for lm4 and lm6)
# predictor percentage goes down when uncertainty is added 
# point predictor for lm4
new <- data.frame(polhs=2.0)

a_hat <- coef(lm4)[1]
b_hat <- coef(lm4)[2]
y_point_pred <- a_hat + b_hat*new
y_point_pred

# linear predictor with uncertainly for lm4
sims <- as.matrix(lm4)
a <- sims[,1]
b <- sims[,2]
y_linpred <- a + b*new
y_linpred

# point predictor for lm6
new1 <- data.frame(disgov=2.0)

a_hat1 <- coef(lm6)[1]
b_hat1 <- coef(lm6)[2]
y_point_pred1 <- a_hat1 + b_hat1*new1
y_point_pred1

# linear precitor with uncertainty for lm6
sims1 <- as.matrix(lm6)
a1 <- sims1[,1]
b1 <- sims1[,2]
y_linpred1 <- a1 + b1*new1
y_linpred1

