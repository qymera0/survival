
# LOAD PACKAGES -----------------------------------------------------------

library(flexsurv)

# STANDARD PARAMETRIC SURVIVAL --------------------------------------------

fs1 <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'weibull')

fs1

# COVARIATES AT ANCILLARY PARAMETERS --------------------------------------

fs2 <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'gengamma')

fs3 <- flexsurvreg(Surv(recyrs, censrec) ~ group, 
                   anc = list(sigma = ~ group),
                   data = bc, dist = 'gengamma')

fs2

fs3

fs2 <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'weibull')

fs3 <- flexsurvreg(Surv(recyrs, censrec) ~ group, 
                   anc = list(shape = ~ group),
                   data = bc, dist = 'weibull')
plot(fs2)

plot(fs3, type = "cumhaz")

summary.flexsurvreg(fs2)

# EXAMPLE WITH LUNG DATA --------------------------------------------------

lung1 <- flexsurvreg(Surv(time, status) ~ age + sex + wt.loss, 
                     data = lung, dist = 'weibull')

dummy <- expand.grid(age = c(50, 60), sex = 1, wt.loss = 5)

times <- seq(1:2000)

predSuvr <- predict(lung1, newdata = dummy, times = times, type = 'survival')




