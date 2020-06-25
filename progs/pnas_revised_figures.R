## figures for resubmission of revision to PNAS
library(data.table)

## fig1 -- 4 panel figure of rates observed and normalized (code given here)
## fig2 -- scatter plot of gompertz slopes across countries (see "calculations_for_text.R")
## fig3 -- impact on all-cause mortality in US (code given here)
## fig4 -- comparative epidemics (see  "hiv_and_other_new.R")



## 1 -- 4 panel figure of rates observed and normalized

dt = fread("../data/cleaned/harmonize_covid_deaths.csv")
nMx.mat_unadjusted = dt[, xtabs(nMx ~ x + Country)]
## adjust for age structure in the open interval
source("get_open_age_group_adjustments.R")
## assure order theta_pretty
my_theta = theta_pretty[colnames(nMx.mat_unadjusted)]
nMx.mat_adj = nMx.mat_unadjusted
nMx.mat_adj["80",] = my_theta * nMx.mat_unadjusted["80", names(my_theta)]
nMx.mat = nMx.mat_adj


nMx_norm.mat = prop.table(nMx.mat, 2)






## Now plot the results
my_cex_axis = .9
my_x = seq(0, 80, 10)
age10 = my_x
##
nMx_norm_bar = apply(nMx_norm.mat, 1, mean, na.rm = T)
b = 1/9 ## gompertz slope
a = nMx_norm_bar["70"] * exp((1/9) * (-70)) ## intercept to match at age 70
axis_fun <- function()
    {
axis(2); box()
axis(1, at = age10, labels = my_labels, cex.axis = my_cex_axis)
}

## fig1
pdf("../text_and_figs/fig1_age_specific_covid_rates.pdf",
     height = 10, width = 10)
my_col = c("black", "red", "blue", "orange", "darkgreen", "purple", "green", "pink")
my_pch = c(15, 17, 19, 21, 22, 23, 24, 25)
my_labels = c(paste(age10[-length(age10)], age10[-1]-1, sep = "-"), "80+")
##
par(mfcol = c(2,2))
## observed
matplot(my_x, nMx.mat, type = 'b',
        col = my_col, pch = my_pch, lty = 1,
        xlab = "Age", ylab = "Rate",
        axes = F)
axis_fun()
legend("topleft", colnames(nMx_norm.mat),
       col = my_col, pch = my_pch, lty = -1,
       bty = "n")
title("(a) Unnormalized Covid-19 death rates")
## observed log
matplot(my_x, log(nMx.mat), type = 'b', col = my_col, pch = my_pch, lty = 1,
        xlab = "Age", ylab = "log(Rate)",
        axes = F)
axis_fun()
title("logarithmic scale")
## normalized
matplot(my_x, nMx_norm.mat, log = '', type = 'p', col = my_col, pch = my_pch,
        xlab = "Age", ylab = "Normalized Rate",
        axes = F)
axis_fun()
legend("topleft", c(colnames(nMx_norm.mat),
                    "Average",
                    "Exponential: rate = 0.11"),
       col = c(my_col, "black", "grey"),
       pch = c(my_pch, -1,-1),
       lty = c(rep(-1, length(my_col)), 1, 2),
       lwd = c(rep(1, length(my_col)), 2, 2),
     , bty = "n")
## legend("topleft", colnames(nMx_norm.mat), col = my_col, pch = my_pch, bty = "n")
title("(b) Normalized Covid-19 death rates")
lines(my_x, a * exp(my_x * (1/9)), col = "grey", lwd = 3, lty = 2)
lines(my_x, nMx_norm_bar, lwd = 2)
## log normalized
matplot(my_x, log(nMx_norm.mat), type = 'p', col = my_col, pch = my_pch,
        xlab = "Age", ylab = "log(Normalized Rate)",
        axes = F)
axis_fun()
## legend("topleft", colnames(nMx_norm.mat), col = my_col, pch = my_pch, bty = "n")
title("logarithmic scale")
lines(my_x, log(a * exp(my_x * (1/9))), col = "grey", lwd = 3, lty = 2)
lines(my_x, log(nMx_norm_bar), lwd = 2)
dev.off()
system("open ../text_and_figs/fig1_age_specific_covid_rates.pdf")


## for fig2 see the file:
## "calculations_for_text.R"


## fig3 -- impact on all-cause mortality

######################
## Now do US figure ##
######################

#######################################################
## now draw US mortality, scaled to 1 million deaths ##
#######################################################
names(nMx_norm_bar) = seq(0, 80, 10)
nMx.norm.ave = nMx_norm_bar

## use 1x1 so that we can do easy calcs of life expectancy etc
dt <- fread("../data/raw/USA.Mx_1x1.txt")
dt <- dt[Year == 2017]
x <- 0:110
Mx_base <- dt$Total
Mx_covid_ave <- exp(approx(x = names(nMx.norm.ave), y = log(nMx.norm.ave), xout = x)$y)
## plot(x, Mx_covid_ave, log = 'y')
##  plot(x, Mx_covid_ave, log = '')
## now assume beta = 1/10 after age 80 (consider using 1/9)
beta = 1/9
xx <- (81:110) - 80
Mx_covid_ave[x %in% 81:110] <- Mx_covid_ave[x == 80] * exp(beta * xx)


## now scale to 1 million deaths

million = 10^6
thousand = 1000
D <- 1 * million

dt <- fread("../data/raw/USA.Exposures_1x1.txt")
dt <- dt[Year == 2017]
Kx <- dt$Total
sum(Kx)/million ## 325 ...
sum(Mx_base * Kx, na.rm = T)/million ## [1] 2.813514

Mx_covid_ave[is.nan(Mx_covid_ave)] <- 0
tot <- sum(Mx_covid_ave * Kx, na.rm = T)
Mx_covid_scaled = Mx_covid_ave * D/tot
Mx_base_and_covid = Mx_base + Mx_covid_scaled
fwrite(data.table(x, Mx_base_and_covid, Mx_base, Mx_covid_scaled),
       "../data/cleaned/normalized_nMx_out.csv")
age <- 0:110
s <- age %in% 0:90

## now make pdf figure
pdf("../text_and_figs/fig3_us_rates.pdf", width = 8, height = 8)
par(mfrow = c(1,1))
age <- 0:110
s <- age %in% 0:90
plot(age[s], Mx_base[s], type = "l", col = "blue", ylim = c(0, .2),
     lwd = 4,
     yaxs = "i",
     xaxs = "i",
     ylab = "Age-specific mortality rates",
     xlab = "Age")
axis(1, at = seq(0, 100, 10))
lines(age[s], Mx_covid_scaled[s], col = "black", lwd = 2, lty = 1)
lines(age[s], Mx_base_and_covid[s], col = "blue", lwd = 2, lty = 2)
title("Estimated U.S. death rates, by age")
legend("topleft",
       c("Baseline + Covid-19",
         "Baseline",
         "Covid-19 only (normalized average, scaled to 1 million deaths)"),
       lty = c(2,1,1),
       lwd = c(2,4,2),
       bty = "n",
       col = c("blue","blue","black"))
dev.off()
system("open ../text_and_figs/fig3_us_rates.pdf")

## not necessary: to convert to png
## system("sips -s format png ../figures/pnas_fig_2.pdf --out ../figures/pnas_fig_2.png")


## for fig4 see the file:
## "hiv_and_other_new.R"



## done


