library(tidyverse)
library(plm)
library(lme4)
library(lmtest)
library(lmerTest)
library(knitr)
library(car)
library(kableExtra)
library(stats)
rm(list = ls())
df = read.csv2("C:/Öga Pub/Daten Update2/data set/data_pub_8.csv",fileEncoding="UTF-8-BOM")

Sys.setenv(LANGUAGE="en")
## Panel data creation
df = pdata.frame(df, index = c("id", "t"))

str(df)
view(df)
## Data transformations


df$First_time_applicants_log = log(df$First_time_applicants)
#df$Caloric_supply
df$Caloric_supply_log = log(df$Caloric_supply)
df$GDPpc_log = log(df$GDPpc)
df$Distance_1000 = df$Distance / 1000
df$Population_size_million = df$Population_size / 1e6

df$Caloric_supply_log = df$Caloric_supply - mean(df$Caloric_supply_log, na.rm = TRUE)
df$GDPpc_log = df$GDPpc_log - mean(df$GDPpc_log, na.rm = TRUE)
df$Distance_1000 = df$Distance_1000 - mean(df$Distance_1000, na.rm = TRUE)
df$Population_size_million = df$Population_size_million - mean(df$Population_size_million, na.rm = TRUE)

#Correlation between PM/PR and FTAs see file "Untitled"
#Linear model FTAs and logFoodPrices w/o country effects

v=1:12
#v_lagged=Lagged(v)

FTAsCSs = lm(First_time_applicants_log~
               Caloric_supply,
             data=df,
             effect="time")
FTAsCSs

kable(summary(FTAsCSs)$coef, digits = 10) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#Including country effects
#Calculated with country effects
FTAsCSCountries = plm(First_time_applicants~
                  Caloric_supply_log, 
                data=df,
                effect="individual")

FTAsCSCountries


kable(summary(FTAsCSCountries)$coef, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#Actual model

  FullModel=lm(df$First_time_applicants_log~
                Caloric_supply_log +
                Population_size_million +
                Age_group +
                GDPpc_log +
                Unemployment_rate +
                Conflict +
                Disaster +
                Political_restriction,
              data=df,
              effect="individual")
  
              #model="within",
              
  kable(summary(FullModel)$coef, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

  #Attempt to display FTAS, CS per year
  FTAs_CS_Year= lm(df$First_time_applicants_log~df$Caloric_supply_logy)
  
  kable(summary(FTAs_CS_Year)$coef, digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
 
  
#Table: FTAs ~ CS per year  
  coef.table = data.frame(t = sort(unique(df$t)), estimate = NA, pvalue = NA) 
  
  for(i in 1:nrow(coef.table)){
    
    fit = lm(First_time_applicants_log~Caloric_supply_log, data = subset(df, t == coef.table$t[i]))
    
    coef.table[i,2] = summary(fit)$coefficients[2,3]
    
    coef.table[i,3] = summary(fit)$coefficients[2,4] 
    }
  
  
  
## ============================================================================
## Pooling
## ============================================================================

fit_pooling = plm(First_time_applicants ~
                    Caloric_supply +
                    Population_size_million +
                    Age_group +
                    GDPpc_log +
                    Unemployment_rate +
                    Conflict +
                    Disaster +
                    Political_restriction,
                  data = df,
                  model = "pooling")


kable(summary(fit_pooling)$coef, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

bptest(fit_pooling)

par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
qqPlot(resid(fit_pooling))

coefci(fit_pooling)


## ============================================================================
## Within
## ============================================================================

fit_within = plm(First_time_applicants_log ~
                   Caloric_supply_log +
                   Population_size_million +
                   Age_group +
                   GDPpc_log +
                   Unemployment_rate +
                   Conflict +
                   Disaster +
                   Political_restriction,
                 data = df,
                 model = "within",
                 effect = "individual")


kable(summary(fit_within)$coef, digits = 5) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

bptest(fit_within)

par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
qqPlot(resid(fit_within))

coefci(fit_within)

## ============================================================================
## Compare Pooling vs. Within
## ============================================================================

## First variant: Using plm's pFtest:
## F-Test.
## Hypothesis:
## H0: factor(id) in fit_within is _not_ relevant
## vs.
## H1: factor(id) in fit_within is relevant

pFtest(fit_within, fit_pooling)

## p < 0.001 ==> Reject H0, factor(id) seems to be relevant
## ==> Within model wins

## Second variant: Using anova function: Refit models:

fit_pooling_lm = lm(First_time_applicants_log ~
                      Caloric_supply_log +
                      Population_size_million +
                      Age_group +
                      GDPpc_log +
                      Unemployment_rate +
                      Conflict +
                      Disaster +
                      Political_restriction,
                    data = df)

fit_within_lm = lm(First_time_applicants_log ~
                     Caloric_supply_log +
                     Population_size_million +
                     Age_group +
                     GDPpc_log +
                     Unemployment_rate +
                     Conflict +
                     Disaster +
                     Political_restriction +
                     factor(id),
                   data = df)


summary(fit_within_lm)$r.squared
summary(fit_within_lm)$adj.r.squared
summary(fit_within_lm)$fstatistic

## Compare pooling and within again:
anova(fit_pooling_lm, fit_within_lm)

## p-value: 2.2e-16
## p-value < 0.05
## ==> Within model wins

## ============================================================================
## Random
## ============================================================================

fit_random = plm(First_time_applicants_log ~
                   Caloric_supply +
                   Population_size_million +
                   Age_group +
                   GDPpc_log +
                   Unemployment_rate +
                   Conflict +
                   Disaster +
                   Political_restriction,
                 data = df,
                 model = "random",
                 effect = "individual")


kable(summary(fit_random)$coef, digits = 5) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

bptest(fit_random)

par(mar = c(3, 3, 2, 1), mgp = c(2, .7, 0))
qqPlot(resid(fit_random))

summary(fit_random)$r.squared
summary(fit_random)$adj.r.squared
summary(fit_random)$fstatistic
coefci(fit_random)

## Hypothesis:
## H0: Preferred model is random effects.
## So if the p-value is smaller than 0.05, reject H0.
## Within model Consistent Inefficient and random model Consistent Efficient
## vs.
## H1: Within model Consistent and Random model Inconsistent
phtest(fit_within, fit_random) ## Works!

## p-value: 0.0005997
## p-value > 0.05 ==> cannot reject H0
## ==> random model wins over within model

## ============================================================================
## Variable selection for winning model (within model)
## ============================================================================

## First: refit model using lmer


fit_random_lmer = lmer(First_time_applicants_log ~
                         Caloric_supply_log +
                         Population_size_million +
                         Age_group +
                         GDPpc_log +
                         Unemployment_rate +
                         Conflict +
                         Disaster +
                         Political_restriction +
                         (1 | id),
                       data = df)

## Results are comparable

##Step wise variable selection of random model using AIC
#step_random = lmerTest::step(update(fit_random_lmer, data = na.omit(df)))
#fit_random_aic = get_model(step_random)

#kable(summary(fit_random_aic)$coef, digits = 3) %>%
#  kable_styling(bootstrap_options = c("striped", "hover"))

#summary(fit_random_lmer)$r.squared
#summary(fit_random_lmer)$adj.r.squared
#summary(fit_random_lmer)$
#  coefci(fit_random_lmer)  





## ============================================================================
## Comparison reduced and full model
## ============================================================================

## Extract results from reduced model

ci_reduced = confint(fit_random_aic, parm = "beta_")
res_reduced = tibble(
  Predictor = names(fixef(fit_random_aic)),
  Effect = fixef(fit_random_aic),
  Lower = ci_reduced[,1],
  Upper = ci_reduced[,2],
  pvalue = summary(fit_random_aic)$coef[,5],
  Type = "reduced"
)

#ci_reduced = confint(fit_random_aic, parm = "beta_")
#res_reduced = tibble(
# Predictor = names(fixef(fit_random_aic)),
#Effect = fixef(fit_random_aic),
#Lower = ci_reduced[,1],
#Upper = ci_reduced[,2],
#pvalue = summary(fit_random_aic)$coef[,5],
#Type = "reduced"
#)

## Extract results from full model
ci_full = confint(fit_random_lmer, parm = "beta_")
res_full = tibble(
  Predictor = names(fixef(fit_random_lmer)),
  Effect = fixef(fit_random_lmer),
  Lower = ci_full[,1],
  Upper = ci_full[,2],
  pvalue = summary(fit_random_lmer)$coef[,5],
  Type = "full"
)


#ci_full = confint(fit_random_lmer, parm = "beta_")
#res_full = tibble(
#  Predictor = names(fixef(fit_random_lmer)),
#  Effect = fixef(fit_random_lmer),
#  Lower = ci_full[,1],
#  Upper = ci_full[,2],
#  pvalue = summary(fit_random_lmer)$coef[,5],
#  Type = "full"
#)

## Only full model
res = bind_rows(res_full,
                res_reduced)
res$effect_type = "Predictor"
res$effect_type[grepl(pattern = "factor", x = res$Predictor)] = "individual"

p = ggplot(res %>%
             filter(Predictor != "(Intercept)",
                    Type == "full"),
           aes(x = Predictor, y = Effect, ymin = Lower, ymax = Upper))

p + geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
  geom_pointrange(position = position_dodge(width = .8)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Point estimate and 95% uncertainty interval", x = NULL)

## Only reduced model
p = ggplot(res %>%
             filter(Predictor != "(Intercept)",
                    Type == "reduced"),
           aes(x = Predictor, y = Effect, ymin = Lower, ymax = Upper))

p + geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
  geom_pointrange(position = position_dodge(width = .8)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Point estimate and 95% uncertainty interval", x = NULL)

## Plot without distance
p = ggplot(res %>%
             filter(Predictor != "(Intercept)"),
           aes(x = Predictor, y = Effect, ymin = Lower, ymax = Upper, colour = Type))
p + geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
  geom_pointrange(position = position_dodge(width = .8)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Point estimate and 95% uncertainty interval", x = NULL)

## Nearly no differences between reduced and full model visible.
## Table
res %>%
  filter(effect_type == "Predictor") %>%
  dplyr::select(Type, Predictor, Effect, Lower, Upper, pvalue) %>%
  mutate(Sign = ifelse(pvalue < 0.05, "*", "")) %>%
  kable(digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## ============================================================================
## Check model assumptions for reduced model
## ============================================================================

## First, refit reduced within model using plm
fit_random_aic_plm = plm(First_time_applicants_log ~
                           Caloric_supply_log +
                           Population_size_million +
                           Unemployment_rate  +
                           Political_restriction,
                         data = df,
                         model = "random",
                         effect = "individual")


## Using the Studentized residuals as these values show no correltaion.
r = rstudent(fit_random_aic)

## ===================================
## Normal distributed error components
## ===================================

qqPlot(r)

## ===================================
## Outlier analysis
## ===================================

## First: Cook's distance.
plot(cooks.distance(fit_random_aic))

## ==> No suspecious values. The mximum is at

na.omit(df)[which.max(cooks.distance(fit_random_aic)),]

## You may want to delete this observtion and try again.

IM = influence(fit_random_aic)

## Conducting DFBETAS
dfbetas(IM) %>%
  as_tibble %>%
  dplyr::select(-contains("Intercept")) %>%
  mutate(indx = 1:length(r)) %>%
  gather(-indx, key = "Predictor", value = "value") %>%
  ggplot(aes(x = indx, y = value)) +
  geom_point() +
  facet_wrap(~ Predictor)

## ==> Effect on standardized regression coefficients is ok.


## ====================================
## Plot of residuals against predictors
## ====================================

## First: Graphs to check if there is a trend in the error components
## along the predictors.

na.omit(df) %>%
  as_tibble() %>%
  mutate(r = r) %>%
  dplyr::select(r, Unemployment_rate) %>%
  gather(-r, key = "Predictor", value = "value") %>%
  ggplot(aes(x = value, y = r)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ Predictor, scales = "free_x")

## No trend visible. However, I need to watch out for this gap. I should divide the observations
## into two classes: Class 1 with Unemployment_rate < 15, Class 2 with Unemployment_rate > 15

## ===============================================
## Checks for heterogenity and serial correlations
## ===============================================

## Test for heteroskedasticity
## Breusch-Pagan test against heteroskedasticity.
bptest(fit_random_aic_plm)
## ==> p < 0.05, low indication of heteroskedasticity.
## ==> heteroskedasticity seems to be an issue

## Test for serial correlation
## Breusch-Godfrey of serial correlation:
pbgtest(fit_random_aic_plm)
## ==> p < 0.001, strong indication of serial correlation in idiosyncratic errors

## Wooldridge Test for AR(1) Errors can be tested for within models onlÃ¶y

#Arellano estimator, because of serial correlation
## Regression coefficients
coeftest(fit_random_aic_plm, vcov. = vcovHC(fit_random_aic_plm, method = "arellano"))


kable(summary(fit_random_aic)$coef, digits = 5) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## Confidence intervals
coefci(fit_random_aic_plm, vcov. = vcovHC(fit_random_aic_plm, method = "arellano"))


## Results are comparable

step_within = lmerTest::step(update(fit_within_lmer, data = na.omit(df)))
fit_within_aic = get_model(step_within)

kable(summary(fit_within_aic)$coef, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

coefci(fit_within_lmer)
summary(fit_within_lmer)$r.squared
summary(fit_within_lmer)$adj.r.squared
summary(fit_within_lmer)$fstatistic

## ============================================================================
## Figures for individual and temporal effects
## ============================================================================


#res = res %>%
  #left_join(df %>% group_by(id) %>% summarize(Country = unique(Country), Distance = unique(Distance)))
view(df$Country)
## Individual effect 
df$Country = factor(df$Country, levels = sort(as.character(df$Countries)))

## Individual effect (country)
re <- ranef(fit_within_aic, condVar = TRUE)
res <- tibble(
  eff = re$id$`(Intercept)`,
  se = attr(re$id, "postVar")[,,],
  lower = eff - 2 * se,
  upper = eff + 2 * se,
  id = factor(row.names(re$id), levels = levels(df$id))
)

# <- res %>%
 # left_join(df %>% group_by(id) %>% summarize(Country = unique(Country), Distance = unique(Distance)))

res <- res %>%
  left_join(df %>% dplyr::select(Country, Distance_km_1000, id))

## Individual effect 
ggplot(res, aes(x = reorder(Country, Distance), y = eff, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
  geom_pointrange() +
  coord_flip() +
  theme_bw()

## Correlation between estimated country specific effects and distance
ggplot(res, aes(x = reorder(Country, Distance_km_1000), y = eff, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = 2, colour = grey(.5)) +
  geom_pointrange() +
  coord_flip() +
  theme_bw()


geom_pointrange(res, aes(x=df$Country), y = df$Distance, ymin = 1, ymax = 2) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 16)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 1, l = -1)))

  view(df)
## It seems that countries with longer distance have  + effects. Let's check this
## with correlation:
#cor.test(res$z_scores, res$Distance)
## Negative correlation of effect with distance, i.e. with more distance the effect
## decreases. However, no significant correlation is found.

#ggplot(df, aes(x= Caloric_supply_log, y=First_time_applicants_log, col=t)) + geom_point()+ geom_smooth(method = lm)
  str(df)
