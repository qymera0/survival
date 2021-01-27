
# PACKAGES --------------------------------------------------------------

library(survival)
library(survminer)
library(tidyverse)
library(broom)

# INTRODUCTION ----------------------------------------------------------

# Creating survival objects

df <- lung

Surv(lung$time, lung$status)


# # Kaplan-Meier ----------------------------------------------------------

# Overall survival curve

f1 <- survfit(Surv(time, status) ~ 1, data = lung)

names(f1)

# Kaplan-Meier plot - base R

plot(
        f1,
        xlab = "Days",
        ylab = "Overall survival probability"
)

# Kaplan-Meier plot - ggsurvplot

ggsurvplot(
        fit = f1,
        xlab = "Days",
        ylab = "Overall survival probability"
)

# Estimating the x-year survival

summary(f1, times = 365.25)

# Median survival time

f1

# Comparing suvival times between groups

f2 <- survdiff(Surv(time, status) ~ sex, data = lung)

f2

# Extract p-value from Survdiff

1 - pchisq(f2$chisq, length(f2$n) - 1)

# Cox regression model

f3 <- coxph(Surv(time, status) ~ sex, data = lung)

f3

tidy(f3,
     exp = TRUE)

# Hazard ratios


