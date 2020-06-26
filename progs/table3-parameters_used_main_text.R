################################################ 
# Parameters Used to Calculate Main Text Figure 4
# epidemics (Table 3)
################################################ 

# The list of parameters used to calculate the mortality of 
# Covid-19 and compare to past U.S. epidemics.

# init --------------------------------------------------------------------

library(data.table)

# data --------------------------------------------------------------------

USA_bltper_1x1 <- fread("../data/raw/USA_bltper_1x1.txt")


# calculations for table --------------------------------------------------

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

radix = 10^5
USA_bltper_1x1[, e.dagger := sum(dx * ex)/radix, by = Year]
USA_bltper_1x1[, e0 := round(ex[Age == "0"]), by = Year]
USA_bltper_1x1[, e.dagger.hat := ex[Age == e0], by = Year] ## aprox

e.dagger_all_cause = USA_bltper_1x1[Age == 0 & Year %in% c(1933, 1990, 2010, 2017)]$e.dagger
e.dagger_all_cause.hat = USA_bltper_1x1[Age == 0 & Year %in% c(1933, 1990, 2010, 2017)]$e.dagger.hat
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

e.dagger_by_cause.hat <- c("flu" = USA_bltper_1x1[Age == round(mad_by_cause["flu"]) & Year == 1933]$ex,
                           "hiv" = USA_bltper_1x1[Age == round(mad_by_cause["hiv"]) & Year == 1990]$ex,
                           "opioids" = USA_bltper_1x1[Age == round(mad_by_cause["opioids"]) & Year == 2010]$ex,
                           "covid" = USA_bltper_1x1[Age == round(mad_by_cause["covid"]) & Year == 2017]$ex)
## measures
D_cause.vec <- c(D_flu, D_hiv, D_opioids, D_covid)
cdr_cause.vec <- 1000*D_cause.vec / pop.vec

py_lost_ratio <- D_cause.vec * e.dagger_by_cause.hat / (D_all.vec * e.dagger_all_cause.hat)

table3 <- as.data.frame(cbind("Deaths (1,000s)" = D_cause.vec/1000,
                "$x_e$" = round(mad_by_cause, 1), 
                "$e({x}_e)$" = round(e.dagger_by_cause.hat, 1),
                "Reference year" = c("1920 (1933)", "1990", "2010", "2017"),
                "Deaths (millions)" = round(D_all.vec/1000000, 1),
                "$e(x)$" = round(e.dagger_all_cause.hat, 1)))

table3

##           Deaths (1,000s) ${x}_e$ $e{x}_e)$ Reference year Deaths (millions) $e(bar{x})$
## flu       "675"           "30"       "38.8"        "1920 (1933)"  "1.378"           "15.1"     
## hiv       "675"           "40"       "38.03"       "1990"         "2.141"           "11"       
## opioids   "770"           "42"       "38.81"       "2010"         "2.472"           "9.79"     
## covid     "1000"          "80"       "9.57"        "2017"         "3.003"           "10.15" 


# Create latex table ----------------------------------------------------------

## library packages to create latex table 
library(magrittr)
library(kableExtra)

# must add "\bar" for x (e.g., instead of $x_e$, should be $\bar{x}_e$)

kable(table3, "latex", booktabs = T, escape = F) %>% 
  add_header_above(c(" ", "Epidemic" = 3, "All-cause" = 3))

