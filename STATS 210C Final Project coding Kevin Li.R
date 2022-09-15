# setwd("C:/Users/pilow/OneDrive/Desktop/stats210c/dataset")


library(tableone)
library(naniar)
library(tidyverse)
library(nlme)
library(lme4)
library(geepack)
library(lmtest)
library(MuMIn)
library(multcomp)

beta_carotene <- read.csv("bcarotene_210C.csv")
beta_carotene$dose <- as.factor(beta_carotene$dose)

noDose0_3 <- subset(beta_carotene, month <= 3)
Dose3_9 <- subset(beta_carotene, month >= 3 & month <= 9)
noDose9_15 <- subset(beta_carotene, month >= 9)

covars <- c("bcarot", "vite", "age", "male", "bmi", 
            "chol")
catvars <- c( "male")

tab03 <- CreateTableOne(data = noDose0_3, vars = covars, factorVars = catvars,strata = "dose")
print(tab03, formatOptions = list(big.mark = ","))

tab39 <- CreateTableOne(data = noDose9_15, vars = covars, factorVars = catvars,strata = "dose")
print(tab39, formatOptions = list(big.mark = ","))

tab915 <- CreateTableOne(data = Dose3_9, vars = covars, factorVars = catvars,strata = "dose")
print(tab915, formatOptions = list(big.mark = ","))

dput(names(beta_carotene))

length(unique(beta_carotene$id))

tab3 <- CreateTableOne(data = beta_carotene, vars = covars, factorVars = catvars,strata = "dose")
print(tab3, formatOptions = list(big.mark = ","))

which(is.na(beta_carotene))
beta_carotene$NACount <- apply(beta_carotene, 1, function(x) sum(is.na(x)))
subset(beta_carotene, NACount > 0)

head(beta_carotene)
beta <- subset(beta_carotene, select = -c(age,male, bmi, chol))
wide_beta <- beta %>%
  pivot_wider(names_from = c("month"), values_from = c("bcarot", "vite"))
head(wide_beta)


n_miss(beta_carotene) #num of missing
n_complete(beta_carotene) #num of complete
miss_var_summary(beta_carotene)
miss_case_summary(beta_carotene)
miss_var_table(beta_carotene)
miss_case_table(beta_carotene)

#great for time seies data, good for balanced data
miss_var_span(beta_carotene, var = bcarot, span_every = 12)
miss_var_span(beta_carotene, var = vite, span_every = 12)

#returns runs of missingness, great for pattern of missingness
miss_var_run(beta_carotene, bcarot)
miss_var_run(beta_carotene, vite)

#summaries with group by
beta_carotene %>%
  group_by(dose) %>%
  miss_var_summary()
names(beta_carotene)
#bird's eye view of data
vis_miss(beta_carotene)
vis_miss(beta_carotene, cluster = TRUE)

#look at missings in variables and cases
gg_miss_var(beta_carotene)
gg_miss_case(beta_carotene)
gg_miss_var(beta_carotene, facet = dose)
names(beta_carotene)

gg_miss_var(wide_beta, facet = dose)

#visualing misisngness patterns
gg_miss_upset(beta_carotene)

#visualiing factors of missingness
gg_miss_fct(x = beta_carotene, fct = month)

#visualiing spans of missingness
gg_miss_span(beta_carotene, bcarot, span_every = 100)
gg_miss_span(beta_carotene, vite, span_every = 100)




noDose0_3 <- subset(beta_carotene, month <= 3)
Dose3_9 <- subset(beta_carotene, month >= 3 & month <= 9)
noDose9_15 <- subset(beta_carotene, month >= 9)

noDose9_15 %>% 
  ggplot(aes(x = month, y = bcarot, group = id, color = dose)) +
  geom_point() +
  geom_line()

Dose3_9 %>% 
  ggplot(aes(x = month, y = bcarot, group = id, color = dose)) +
  geom_point() +
  geom_line()

noDose0_3 %>% 
  ggplot(aes(x = month, y = bcarot, group = id, color = dose)) +
  geom_point() +
  geom_line()



head(beta_carotene)
summary(beta_carotene)
length(unique(beta_carotene$id))

long_beta_beta <- beta_carotene %>% select(-c("vite", "age", "male", "bmi", "chol"))
long_beta_vite <- beta_carotene %>% select(-c("bcarot", "age", "male", "bmi", "chol"))
head(long_beta_beta)
head(long_beta_vite)


wide_beta_beta <- long_beta_beta %>%
  pivot_wider(names_from = "month", values_from = "bcarot")
wide_beta_beta <- data.frame(wide_beta_beta)
wide_beta_beta

id_nobs <- beta_carotene %>%
  group_by(id) %>%
  summarize(nobs = n())
table(id_nobs$nobs)

beta_carotene %>% 
  ggplot(aes(x = month, y = bcarot, group = id, color = dose)) +
  geom_point() +
  geom_line()

names(wide_beta_beta)
mean_carotene_month <- wide_beta_beta %>%
  group_by(dose) %>% 
  summarize(X0 = mean(X0, na.rm = TRUE),
            X1 = mean(X1, na.rm = TRUE),
            X2 = mean(X2, na.rm = TRUE),
            X3 = mean(X3, na.rm = TRUE),
            X4 = mean(X4, na.rm = TRUE),
            X5 = mean(X5, na.rm = TRUE),
            X6 = mean(X6, na.rm = TRUE),
            X7 = mean(X7, na.rm = TRUE),
            X8 = mean(X8, na.rm = TRUE),
            X9 = mean(X9, na.rm = TRUE),
            X10 = mean(X10, na.rm = TRUE),
            X11 = mean(X11, na.rm = TRUE),
            X12 = mean(X12, na.rm = TRUE),
            X13 = mean(X13, na.rm = TRUE),
            X14 = mean(X14, na.rm = TRUE),
            X15 = mean(X15, na.rm = TRUE))

mean_carotene_month_long <- mean_carotene_month %>%
  pivot_longer(-c(dose), names_to = "month", values_to = "bcarot")

mean_carotene_month_long$monthNum <- case_when(
  mean_carotene_month_long$month == "X0" ~ 0,
  mean_carotene_month_long$month == "X1" ~ 1,
  mean_carotene_month_long$month == "X2" ~ 2,
  mean_carotene_month_long$month == "X3" ~ 3,
  mean_carotene_month_long$month == "X4" ~ 4,
  mean_carotene_month_long$month == "X5" ~ 5,
  mean_carotene_month_long$month == "X6" ~ 6,
  mean_carotene_month_long$month == "X7" ~ 7,
  mean_carotene_month_long$month == "X8" ~ 8,
  mean_carotene_month_long$month == "X9" ~ 9,
  mean_carotene_month_long$month == "X10" ~ 10,
  mean_carotene_month_long$month == "X11" ~ 11,
  mean_carotene_month_long$month == "X12" ~ 12,
  mean_carotene_month_long$month == "X13" ~ 13,
  mean_carotene_month_long$month == "X14" ~ 14,
  mean_carotene_month_long$month == "X15" ~ 15
)

mean_carotene_month_long$dose <- as.factor(mean_carotene_month_long$dose)
mean_carotene_month_long %>% 
  ggplot(aes(x = monthNum, y = bcarot, group = dose, color = dose)) +
  geom_point() +
  geom_line()

#3, 4, 10, 12 splines
head(beta_carotene)
beta_carotene$dose <- as.factor(beta_carotene$dose)
beta_carotene$spline_month3 <- (beta_carotene$month - 3) * as.numeric(beta_carotene$month >= 3)
beta_carotene$spline_month4 <- (beta_carotene$month - 4) * as.numeric(beta_carotene$month >= 4)
beta_carotene$spline_month10 <- (beta_carotene$month - 10) * as.numeric(beta_carotene$month >= 10)
beta_carotene$spline_month12 <- (beta_carotene$month - 12) * as.numeric(beta_carotene$month >= 12)

head(beta_carotene)

gee_spline <- geeglm(bcarot ~ dose * 
                       (month + spline_month3 + 
                          spline_month4 + spline_month10 + 
                          spline_month12),
                     data = beta_carotene, id = id,
                     corstr = "exch", waves = month)

summary(gee_spline)
anova(gee_spline)
QIC(gee_spline)

noDose9_15 <- subset(beta_carotene, month >= 9)
noDose9_15$spline_month11 <- (noDose9_15$month - 11) * as.numeric(noDose9_15$month >= 11)
noDose9_15$spline_month13 <- (noDose9_15$month - 13) * as.numeric(noDose9_15$month >= 13)

gee_spline915 <- geeglm(bcarot ~ dose * (month + spline_month11 + spline_month13),
                     data = noDose9_15, id = id,
                     corstr = "exch", waves = month)
summary(gee_spline915)
anova(gee_spline915)

summary(glht(gee_spline915, linfct = c("dose15 + dose15:month + dose15:spline_month11 + dose15:spline_month13 == 0",
                                       "dose30 + dose30:month + dose30:spline_month11 + dose30:spline_month13 == 0",
                                       "dose45 + dose45:month + dose45:spline_month11 + dose45:spline_month13 == 0",
                                       "dose60 + dose60:month + dose60:spline_month11 + dose60:spline_month13 == 0")))

head(noDose9_15)
noDose9_15$fitted_outcome <- predict(gee_spline915, newdata = noDose9_15)
noDose9_15 %>%
  ggplot(aes(x = month, y = bcarot, group = id, color = dose)) +
  geom_point() + 
  geom_line()
noDose9_15 %>%
  ggplot(aes(x = month, y = fitted_outcome, group = id, color = dose)) +
  geom_point() + 
  geom_line()

beta_carotene$fitted_outcome <- predict(gee_spline, newdata = beta_carotene)
beta_carotene %>%
  ggplot(aes(x = month, y = fitted_outcome, group = id, color = dose)) +
  geom_point() + 
  geom_line()
head(beta_carotene)

head(Dose3_9)
gee_spline39 <- geeglm(bcarot ~ dose * (month + spline_month11 + spline_month13),
                        data = noDose9_15, id = id,
                        corstr = "exch", waves = month)
Dose39_model <- lm(bcarot ~ month *(age + male + bmi + chol), data = Dose3_9)
summary(Dose39_model)
anova(Dose39_model)

#Scietntific questions:
#1. giant many knots linear spline (add slopes then compare them)
#2. model only months 10-15 (compare slopes)
#3. generic parametric model including age, gender,
#BMI, and cholesterol. 