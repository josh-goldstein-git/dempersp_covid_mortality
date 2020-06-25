## bar plot of epidemic deaths in comparitive perspective
## we add a dashed line at 250k

library(data.table)
thousand = 1000
million = 10^6
billion = thousand*million


pop.vec <- c("1920" = 106*million,
             "1990" = 249*million,
             "2010" = 309*million,
             "2020" = 330*million)
cdr_all.vec <- c("1920" = 13.0/1000, ## Hist statistics of 1789- ...
             "1990" = 8.6/1000, ## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5754931/
             "2010" = 8.0/1000, ## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5754931/
             "2020" = 9.1/1000)
D_all.vec <- pop.vec * cdr_all.vec

D_hiv = 675 * thousand ## "todaysepidemic-508.pdf" ## exposure period 1985-2013
D_covid = 1 * million
D_opioids = 770 * thousand
D_flu = 675 * thousand

## get e.dagger by period

dt <- fread("~/Documents/hmd/hmd_statistics/lt_both/bltper_1x1/USA.bltper_1x1.txt")
radix = 10^5
dt[, e.dagger := sum(dx * ex)/radix, by = Year]
dt[, e0 := round(ex[Age == "0"]), by = Year]
dt[, e.dagger.hat := ex[Age == e0], by = Year] ## aprox

e.dagger_all_cause = dt[Age == 0 & Year %in% c(1933, 1990, 2010, 2017)]$e.dagger
e.dagger_all_cause.hat = dt[Age == 0 & Year %in% c(1933, 1990, 2010, 2017)]$e.dagger.hat
## a bit smaller!!


## get mean ages of death

## hiv
df <- read.table("../data/raw/hiv_deaths.txt", header = T)
sum(df$Dx)
## [1] 323934, correct
x <- df$x
n <- c(diff(x), 2.5)
Dx <- df$Dx
mean_age_of_hiv_death = sum((x + n/2) * Dx)/sum(Dx)
mad_hiv = mean_age_of_hiv_death
## [1] 39.68522

## opioids ## source: Opioid Overdose Deaths by AGe groups 2010
perc.by.age <- c(12, 22, 23, 28, 15)/100
age <- c(20, 30, 40, 50, 65) ## guessed mid-intervals
sum(perc.by.age*age)/sum(perc.by.age)
## [1] 41.95


mad_by_cause <- c("flu" = 30, ## rounding up from 28 peak in Gagnon et al.
                  "hiv" = 40, ## rounded up from 39.7
                  "opioids" = 42,
                  "covid" = 80)

e.dagger_by_cause.hat <- c("flu" = dt[Age == round(mad_by_cause["flu"]) & Year == 1933]$ex,
                       "hiv" = dt[Age == round(mad_by_cause["hiv"]) & Year == 1990]$ex,
                       "opioids" = dt[Age == round(mad_by_cause["opioids"]) & Year == 2010]$ex,
                       "covid" = dt[Age == round(mad_by_cause["covid"]) & Year == 2017]$ex)


## measures
D_cause.vec <- c(D_flu, D_hiv, D_opioids, D_covid)
cdr_cause.vec <- 1000*D_cause.vec / pop.vec

py_lost_ratio <- D_cause.vec * e.dagger_by_cause.hat / (D_all.vec * e.dagger_all_cause.hat)

cbind(D_cause.vec, e.dagger_by_cause.hat, D_all.vec, e.dagger_all_cause.hat, py_lost_ratio)
##         D_cause.vec e.dagger_by_cause.hat D_all.vec e.dagger_all_cause.hat
## flu          675000                 38.80   1378000                  15.10
## hiv          675000                 38.03   2141400                  11.00
## opioids      770000                 31.60   2472000                   9.79
## covid       1000000                  9.57   3003000                  10.15
##         py_lost_ratio
## flu         1.2586626
## hiv         1.0897820
## opioids     1.0054180
## covid       0.3139717


## figure (original)

mycol = rev(c("grey", "darkgreen", "blue", "red"))
#### mynames <- c("Covid-19 ???", "Opioids (10 years)", "HIV (20 years)", "Spanish Flu")
mynames <- c("Spanish Flu (1918)", "HIV (1985-2013)",  "Opioids (1999-2018)", "Covid-19 ???")
par(mfrow = c(3,1), mar = c(3,4,4,2) + 1)## , mar = c(5,4,4,2) + 1)
bp <- barplot(D_cause.vec/thousand, ylim = c(0, 1.2*thousand), ylab = "Thousands",
              col = mycol,
              names.arg = mynames,
              cex.names = 1.2, cex.axis = 1.2)
text(x = bp, y = D_cause.vec/thousand, round(D_cause.vec/thousand, 2), pos = 3)
title("Epidemic Deaths")
barplot(cdr_cause.vec, ylim = c(0, 8), ylab = "per 1000",
        col = mycol, names.arg = mynames,
        cex.names = 1.2, cex.axis = 1.2)
text(x = bp, y = cdr_cause.vec, round(cdr_cause.vec), pos = 3)
title("Crude death rate of epidemic: Epidemic deaths / Pop")
barplot(py_lost_ratio , ylim = c(0, 1.5), ylab = "ratio",
        col = mycol,
        names.arg = mynames,
        cex.names = 1.2, cex.axis = 1.2)
title("Life years lost, relative to non-epidemic mortality")
text(x = bp, y = py_lost_ratio, round(py_lost_ratio, 1), pos = 3)
## dev.off()
## system("open hiv_plus_2.pdf")


## new figure with 6 bars


n <- c(1,1,1, 29, 1,20)
D6 <- c(D_covid, D_flu, D_hiv, D_hiv, D_opioids, D_opioids)/n
##
cdr6 = c(D_covid/pop.vec["2020"],
         D_flu/pop.vec["1920"],
       D_hiv/pop.vec["1990"],
       D_hiv/pop.vec["1990"],
       D_opioids/pop.vec["2010"],
       D_opioids/pop.vec["2010"])/n
py6 = c(py_lost_ratio["covid"],
        py_lost_ratio["flu"],
       py_lost_ratio["hiv"],
       py_lost_ratio["hiv"],
       py_lost_ratio["opioids"],
       py_lost_ratio["opioids"])/n
names(py6) <- NULL
names(cdr6) <- NULL
##
mynames <- c("Spanish Flu (1918)", "HIV (1985-2013)",  "Opioids (1999-2018)", "Covid-19 ???")
col6 = c("grey", "red",  rep("seagreen",2), rep("lightblue", 2))
space6 = c(1,1,1,.1,1,.1)
width6 = c(rep(.5,2), rep(.5, 4))
##


plotfun <- function(x, ..., cex1 = .8, cex2 = .7)
{
    bp <- barplot(x,
              ## ylim = c(0, 1.2*million/thousand),
              col = col6,
              width = width6,
              space = space6,
              ...)
mtext(at = c(bp[1], bp[2], mean(bp[3:4]), mean(bp[5:6])),
      side = 1,
      line = 3,
      cex = cex1,
      c("Covid-19 (2020)", "Spanish Flu (1918)", "HIV (1985-2013)",  "Opioids (1999-2018)"))
mtext(at = bp,
      side = 1,
      cex = cex2,
      line = 1,
      c("", "", "All years", "Per year", "All years", "Per year"))
    return(bp)
}



par(mfrow = c(3,1))
##
bp = plotfun(D6/1000,
        main = "Epidemic deaths (in thousands)",
        ylim = c(0, 1500),
        axes = F,
        cex.main = 1.5)
text(x = bp, y = D6/thousand, round(D6/1000, 0), pos = 3)

##
bp = plotfun(cdr6*1000,
             main = "Epidemic deaths / Population size (per thousand)",
             ylim = c(0, 10),
             axes = F,
             cex.main = 1.5)
## didn't do: add a "?" after covid number
text(x = bp, y = cdr6 * thousand, round(cdr6*1000, 1), pos = 3)
##
bp = plotfun(py6,
        main = "Life years lost, relative to non-epidemic mortality",
        ylim = c(0, 1.5),
        axes = F,
        cex.main = 1.5)
text(x = bp, y = py6, round(py6, 2), pos = 3)



########### dashed line at 250k
pdf("../text_and_figs/fig4_hiv_plus_new_dash.pdf")
par(mfrow = c(3,1))
##
bp = plotfun(D6/1000,
        main = "Epidemic deaths (in thousands)",
        ylim = c(0, 1500),
        axes = F,
        cex.main = 1.5)
text(x = bp, y = D6/thousand, round(D6/1000, 0), pos = 3)
w <- .5
segments(x0 = bp[1]-w/2, x1 = bp[1]+w/2, y0 = 250, lty = 2)
text(x = bp[1], y = D6[1]/1000*250/1000, round(D6[1]/1000*250/1000, 2), pos = 3,
     cex = .8)
##
bp = plotfun(cdr6*1000,
             main = "Epidemic deaths / Population size (per thousand)",
             ylim = c(0, 10),
             axes = F,
             cex.main = 1.5)
## didn't do: add a "?" after covid number
text(x = bp, y = cdr6 * thousand, round(cdr6*1000, 1), pos = 3)
segments(x0 = bp[1]-w/2, x1 = bp[1]+w/2, y0 = cdr6[1]*1000 * 250/1000, lty = 2)
text(x = bp[1], y = cdr6[1]*1000*250/1000, round(cdr6[1]*1000*250/1000, 1), pos = 3,
          cex = .8)
##
bp = plotfun(py6,
        main = "Life years lost, relative to non-epidemic mortality",
        ylim = c(0, 1.5),
        axes = F,
        cex.main = 1.5)
text(x = bp, y = py6, round(py6, 2), pos = 3)
segments(x0 = bp[1]-w/2, x1 = bp[1]+w/2, y0 = py6[1] * 250/1000, lty = 2)
text(x = bp[1], y = py6[1]*250/1000, round(py6[1]*250/1000, 2), pos = 3,
          cex = .8)
 dev.off()
 system("open ../text_and_figs/hiv_plus_new_dash.pdf")
## system("sips -s format png ../figures/hiv_plus_new_dash.pdf --out ../figures/hiv_plus_new_dash.png")
