################################################ 
# Plot US Rates (Figure 3)
################################################ 

# We plot the estimated age-specific mortality in the United State in 2020
# for the scenario of 1 million additional Covid-19 deaths. 


# init --------------------------------------------------------------------

library(data.table)
source("get_open_age_group_adjustments.R")


# data --------------------------------------------------------------------
usa_Mx_1x1 <- fread("../data/raw/hmd_life_tables/USA.Mx_1x1.txt")
harmonize_covdeaths = fread("../data/cleaned/harmonize_covid_deaths.csv")
usa_exposure_1x1 <- fread("../data/raw/hmd_exposures/USA.Exposures_1x1.txt")


# calculations -----------------------------------------------------------------

nMx.mat_unadjusted = harmonize_covdeaths[, xtabs(nMx ~ x + Country)]
## adjust for age structure in the open interval

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



names(nMx_norm_bar) = seq(0, 80, 10)
nMx.norm.ave = nMx_norm_bar

## use 1x1 so that we can do easy calcs of life expectancy etc
usa_Mx_1x1 <- usa_Mx_1x1[Year == 2017]
x <- 0:110
Mx_base <- usa_Mx_1x1$Total
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

usa_exposure_1x1 <- usa_exposure_1x1[Year == 2017]
Kx <- usa_exposure_1x1$Total
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

