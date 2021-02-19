library(tidyverse)
library(haven)
library(lubridate)
library(forecast)
library(lmtest)
library(sandwich)
library(car)

filpat <- "C:\\Users\\Lui Yu Sen\\Google Drive\\NTU_study materials\\Economics\\HE3021 Intermediate Econometrics\\Week 5\\HE3021-Week-5-Tutorial-4\\rawdata\\Okun.dta"
dat <- read_dta(filpat)
dat <- mutate(dat, year = as.character(year))
dat <- mutate(dat, year = paste0(year,c("0101"),sep=""))
dat <- mutate(dat, year = ymd(year))
dat <- mutate(dat, f_pcrgdp = (pcrgdp - 3 + 2*cunem))
dat <- mutate(dat, constantvalue = rep(1, 47))
dat <- mutate(dat, residuals_lag1 = c(NA, model_a$residuals[1:46]))
dat <- mutate(dat, residuals_lag0 = c(model_a$residuals))

dat_a <- ts(dat)
model_a <- tslm(pcrgdp ~ cunem, dat_a)
summary(model_a)

# b
critregionb <- -1.8909 + c(-1,1)*qt(0.975, 44)*0.1820 # x_bar is in, so dont reject
t_statb <- (-1.8909 - (-2))/(0.1820/(46**0.5)) # >1.96, reject

# c
critregionc <- 3.3444 + c(-1,1)*qt(0.975, 44)*0.1627 # reject
t_statc <- (3.3444 - (3))/(0.1627) # > 1.96, reject

# d
dat <- mutate(dat, residualsqrf_pcrgdp = (f_pcrgdp - colMeans(dat[5], na.rm = TRUE))**2)
SSR_f_pcrgdp <- sum(dat[6], na.rm = TRUE)
# residuals_ur <- as.vector(model_a$residuals)
# residuals_ur <- residuals_ur**2
# SSR_ur <- sum(residuals_ur, na.rm = TRUE)
# F_testd <- ((SSR_f_pcrgdp - SSR_ur)/2)/(SSR_ur/(44))
# F_p <- pf(q = F_testd, df1 = 2, df2 = 46) # bigger than 0.1, don't reject


model_restricted <- tslm(f_pcrgdp~constantvalue, data = ts(dat))
residuals_restricted <- as.vector(model_restricted$residuals)
residuals_restricted <- residuals_restricted[!is.na(residuals_restricted)]
residuals_restricted <- residuals_restricted**2
SSR_restricted <- sum(residuals_restricted)

residuals_unrestricted <- as.vector(model_a$residuals)
residuals_unrestricted <- residuals_unrestricted[!is.na(residuals_unrestricted)]
residuals_unrestricted <- residuals_unrestricted**2
SSR_unrestricted <- sum(residuals_unrestricted)

F_stat <- ((SSR_restricted - SSR_unrestricted)/2)/(SSR_unrestricted/(44))
F_pvalue <- pf(q = F_stat, df1 = 2, df2 = 44, lower.tail = FALSE) # bigger than 0.1, don't reject
linearHypothesis(model_a,c("cunem=-2","(Intercept)=3"), test = "F")
# e
summary(tslm(residuals_lag1~cunem, data = ts(dat)))
summary(tslm(residuals_lag0~cunem, data = ts(dat)))

# f
serialcorr_test6 <- bgtest(model_a, order = 6, data = dat_a)
serialcorr_test <- c()
for (n in 1:44){
    serialcorr_test <- c(serialcorr_test, bgtest(model_a, order = n, data = dat_a)$p.value)
}
between(0.05, range(serialcorr_test)[1], range(serialcorr_test)[2]) # false, so no serial correlation. All null hypotheses of no serial corr not rejected.

# g
dat <- mutate(dat, residuals_squared = (as.vector(model_a$residuals))**2)
heterotestmodel <- tslm(residuals_squared~cunem, ts(dat))
heterotestpvalue <- 0.04469
0.04469 < 0.05 # thus heteroscedastic

# h 
dat <- mutate(dat, mean_pcrgdp = rep(mean(pcrgdp), 47))
model_hrestricted <- tslm(pcrgdp ~ mean_pcrgdp, data = ts(dat))
