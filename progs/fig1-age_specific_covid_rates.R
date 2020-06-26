################################################ 
# Plot Age-Specific Covid Rates (Figure 1)
################################################ 

# We plot the unnormalized and normalized age-specific death rates 
# across regions. 4 panel figures of rates observed and normalized. 
# Bottom two panels are log-adjusted. 


# init --------------------------------------------------------------------

library(data.table)

## get adjustments for age structure
source("get_open_age_group_adjustments.R")



# data --------------------------------------------------------------------


dt <-  fread("../data/cleaned/harmonize_covid_deaths.csv")


# calculations ------------------------------------------------------------


nMx.mat_unadjusted = dt[, xtabs(nMx ~ x + Country)]
## adjust for age structure in the open interval
## assure order theta_pretty
my_theta = theta_pretty[colnames(nMx.mat_unadjusted)]
nMx.mat_adj = nMx.mat_unadjusted
nMx.mat_adj["80",] = my_theta * nMx.mat_unadjusted["80", names(my_theta)]
nMx.mat = nMx.mat_adj

nMx_norm.mat = prop.table(nMx.mat, 2)

my_cex_axis = .9
my_x = seq(0, 80, 10)
age10 = my_x

nMx_norm_bar = apply(nMx_norm.mat, 1, mean, na.rm = T)
b = 1/9 ## gompertz slope
a = nMx_norm_bar["70"] * exp((1/9) * (-70)) ## intercept to match at age 70

axis_fun <- function()
{
  axis(2); box()
  axis(1, at = age10, labels = my_labels, cex.axis = my_cex_axis)
}

# Plot Figure 1 —————————————————————————————————————————————————

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

## View plot
system("open ../text_and_figs/fig1_age_specific_covid_rates.pdf")

