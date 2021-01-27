
# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(fitdistrplus)

# ONE EVENT TYPE, ONE EVENT PER SUBJECT -----------------------------------

options(show.signif.stars = F)

cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lung)

print(cfit1) #Short summary

summary(cfit1, digits = 3) #Longer one

tidy(cfit1) # Prettier one

anova(cfit1)

cfit1a <- coxph(Surv(time, status) ~ age + sex + wt.loss, 
                         data = lung,
                         na.action = na.omit)

cfit1b <- coxph(Surv(time, status) ~ age + sex + wt.loss, 
                         data = lung,
                         na.action = na.exclude)

r1 <- residuals(cfit1a)

r2 <- residuals(cfit1b)

length(r1)

length(r2)

# Strata data

cfit2 <- coxph(Surv(time, status) ~ age + sex + wt.loss + strata(inst), 
               data = lung)

round(cbind(simple = coef(cfit1), stratified = coef(cfit2)), 4)


dummy <- expand.grid(age = c(50, 60), sex = 1, wt.loss = 5)

csurv1 <- survfit(cfit1, newdata = dummy)

csurv2 <- survfit(cfit2, newdata = dummy)

plot(csurv1, col = 1:2, xscale = 365.25, xlab = "Years", ylab = "Survival")

dummy2 <- data.frame(age = c(50, 60), sex = 1:2, wt.loss = 5, inst = c(6, 11))

csurv3 <- survfit(cfit2, newdata = dummy2)

zp1 <- cox.zph(cfit1)

plot(zp1[2], resid = F)

abline(coef(cfit1)[2], 0, lty = 3)

# Fit age with a spline

cfit3 <- coxph(Surv(time, status) ~ pspline(age) + sex + wt.loss, lung)

print(cfit3, digits = 2)

cfit4 <- update(cfit1, . ~ . + age*sex)

anova(cfit1, cfit3, cfit4)

# Predict data

pred <- 
        tibble(csurv1$time, csurv1$n.event) %>% 
        rename("ttf" = 'csurv1$time', "freq" = "csurv1$n.event") %>% 
        filter(freq > 0) %>% 
        mutate(freq = as.integer(freq)) %>% 
        uncount(freq)

# Fit a distribution to predicted data

plotdist(pred$ttf, histo = T, demp = T)

descdist(pred$ttf, boot = 1000)

fw <- fitdist(pred$ttf, "weibull")

fg <- fitdist(pred$ttf, "gamma")

fl <- fitdist(pred$ttf, "lnorm")

par(mfrow = c(2, 2))

plot.legend <- c("Weibull", "gamma", "lognormal")

denscomp(list(fw, fg, fl), legendtext = plot.legend)

qqcomp(list(fw, fg, fl), legendtext = plot.legend)

cdfcomp(list(fw, fg, fl), legendtext = plot.legend)

ppcomp(list(fw, fg, fl), legendtext = plot.legend)

gofstat(list(fw, fg, fl), fitnames = c("Weibull", "Gamma", "Lognormal"))

summary(fw)

plot(fw)

# # ACESSING VALIDITY OF COX MODEL ----------------------------------------

resCox <- cfit1

resCox

# Proportional Hazard assumption

test.ph <- cox.zph(resCox)

test.ph

ggcoxzph(test.ph)

# Influential observations

ggcoxdiagnostics(resCox, type = "dfbeta", linear.predictions = F)

# Outliers

ggcoxdiagnostics(resCox, type = "deviance", linear.predictions = F)

# Testing non linearity

ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)
