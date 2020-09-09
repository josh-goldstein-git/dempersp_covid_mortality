## recalculate relative risk ratios of covid:all-cause by sex for PNAS correction

## source for covid:
## "Deaths-Age-Sex_Covid-19_USA_21_07.xlsx"
## downloaded from https://dc-covid.site.ined.fr/en/data/united-states-of-america on Sept 8, 2020

## source for all-cause:
## HMD

## (1) Get ratio of covid ASMR by sex
library(data.table)
df <- read.table("../data/raw/INED_covid_usa_by_sex.tsv", header = T)
dt <- as.data.table(df)
##
dt[, nMx_m := m_covid / m_pop]
dt[, nMx_f := f_covid / f_pop]
dt[, covid_mf_ratio := round(nMx_m / nMx_f, 6)]

## midAge

dt[, midAge := c(.5, 3, seq(10, 80, 10), 90)]

## (2) now do ratio in HMD, by 1 year age groups (for graphical comparison)
## (We can compare to mid-point of age groups above)

hmd <- fread("../data/raw/hmd_life_tables/USA.Mx_1x1.txt")
hmd <- hmd[Year == 2017]
hmd[, x := as.numeric(Age)]
hmd[Age == "110+", x := 110]
hmd[, mf_ratio := Male/Female]


## plot
hmd[, plot(x, mf_ratio, type = "l")]
dt[, lines(midAge, covid_mf_ratio, col = "red")]


## table
hmd_mf_ratio = hmd[, approx(x = x, y = mf_ratio, xout = dt$midAge)$y ]
my_tab = cbind(dt[, .(midAge, covid_mf_ratio, hmd_mf_ratio)])
print(my_tab)

my_tab[, plot(midAge, covid_mf_ratio, type = "l", col = "red",
              ylab = "mf_ratio")]
my_tab[, lines(midAge, hmd_mf_ratio)]
legend("topright", c("Covid", "All Causes"), col = c("red", "black"),
       lty = c(1,1))
title("Covid has higher male:female mortality")
