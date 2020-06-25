## this file is a grab-bag of calculations for the text
## as well as the code to produce figure 2 scatter plot of gompertz slopes

library(data.table)
## 1 -- % of Covid19 deaths in USA that are above age 70

dt = fread("../data/cleaned/harmonize_covid_deaths.csv")
nDx_prop = dt[Country == "USA", prop.table(nDx)]
names(nDx_prop) = dt[Country == "USA"]$x
print("Proportion deaths over age 70")
print(sum(nDx_prop[c("70", "80")]))
## [1] 0.7002877

## 2 -- Gompertz slopes

tmp = dt[, .("b" = log(nMx[x == 80]/ nMx[x == 40])/40 ),
         by = Country]
print("Gompertz slopes from 40-49 group to 80+ group")
print(tmp[, .(Country, b = round(b, 3))])

##    Country     b
## 1: England 0.110
## 2:  France 0.107
## 3: Germany 0.121
## 4:   Italy 0.128
## 5:   Korea 0.122
## 6:   Spain 0.120
## 7:     USA 0.092
## 8:   Wuhan 0.099


## let's get Gompertz slopes for abridged life table

code.vec = c("GBRTENW", "FRATNP", "DEUTNP", "ITA", "KOR", "ESP", "USA")
result.mat = matrix(NA, length(code.vec), 2)
rownames(result.mat) = code.vec
colnames(result.mat) = c("Year", "b")
for (i in 1:length(code.vec))
{
    this_code = code.vec[i]
    print(this_code)
    this_file = paste0("~/Documents/hmd/hmd_statistics/death_rates/Mx_5x1/",
                      this_code, ".Mx_5x1.txt")
    dt = fread(this_file, na.string = ".")
    out = dt[Year == max(Year),
             .("Year" =  max(Year),
               "b" = log(Total[Age == "85-89"]/
                         Total[Age == "45-49"])/40 )]
    result.mat[i,] = c("Year" = out$Year, "b" = round(out$b,3))
}

##         Year     b
## GBRTENW 2016 0.099
## FRATNP  2017 0.093
## DEUTNP  2017 0.103
## ITA     2014 0.106
## KOR     2018 0.101
## ESP     2016 0.102
## USA     2017 0.086

##    Country     b
## 1: England 0.110
## 2:  France 0.107
## 3: Germany 0.121
## 4:   Italy 0.128
## 5:   Korea 0.122
## 6:   Spain 0.120
## 7:     USA 0.092
## 8:   Wuhan 0.099

pdf("../figures/gompertz_scatter.pdf")
par(mfrow = c(1,1))
plot(result.mat[,"b"], tmp$b[-nrow(tmp)],
     ylab = "Rate of increase with age from COVID-19 mortality",
     xlab = "Rate of increase with age from all causes mortality",
     xlim = c(.08, .12),
     pch = 19,
     ylim = c(.09, .13))
## text(result.mat[,"b"], tmp$b[-nrow(tmp)], rownames(result.mat),
##      pos = 3)
mypos = rep(1, nrow(result.mat))
names(mypos) = tmp$Country[-nrow(tmp)]
mypos["Spain"] = 3
mypos["Germany"] = 4
mypos["Korea"] = 1
myadj = rep(.5, length(mypos))
names(myadj) = names(mypos)
myadj["Spain"] = 0
text(result.mat[,"b"], tmp$b[-nrow(tmp)], tmp$Country[-nrow(tmp)],
      pos = mypos)
dev.off()
system("open ../figures/gompertz_scatter.pdf")

#####################
## temporary aging ##
#####################

beta = .1
million = 10^6
covid_death.vec = c(.125, .250, .500, 1, 2) * million
normal <- normal_deaths_in_three_months <- (3 * million) / 4
death_ratio = (covid_death.vec + normal) / normal
aging = log(death_ratio)/beta

cbind(covid_death.vec, aging = round(aging,1))
##      covid_death.vec aging
## [1,]          125000   1.5
## [2,]          250000   2.9
## [3,]          500000   5.1
## [4,]         1000000   8.5
## [5,]         2000000  13.0

####################################################
## life expectancy effect and entropy calculation ##
####################################################

## Note for entropy calculation we can figure out the
## proportional change in mortality that produces 1 million deaths
## OR we can say what proportional change 1 million deaths represents
## I like the latter because more general.

get.e0 <- function(mx)
{
    Hx <- cumsum(mx)
    lx <- c(1, exp(-Hx))
    lxpn <- c(lx[-1], 0)
    Lx <- (lx + lxpn)/2
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx/lx
    e0 <- ex[1]
    e0
}
get.H <- function(mx)
{
    Hx <- cumsum(mx)
    lx <- c(1, exp(-Hx))
    lxpn <- c(lx[-1], 0)
    Lx <- (lx + lxpn)/2
    H = -sum(Lx * log(Lx))/sum(Lx)
    return(H)
}


dt = fread(       "../data/cleaned/normalized_nMx_out.csv")
Mx_base = dt$Mx_base
Mx_base_and_covid = dt$Mx_base_and_covid
Mx_covid_scaled = dt$Mx_covid_scaled
e0.with.covid = get.e0(Mx_base_and_covid) ## [1] 75.91916
e0.without.covid = get.e0(Mx_base) ## [1] 78.85569
(e0.drop.1million = e0.without.covid - e0.with.covid)
## [1] 2.936527


## with 250k deaths we have 1/4 of covid mort
Mx_base_and_covid_250k = Mx_base + Mx_covid_scaled / 4
e0.with.covid.250k = get.e0(Mx_base_and_covid_250k)
##
(e0.drop.250k = e0.without.covid - e0.with.covid.250k)
## [1] 0.8416148

## ratio

e0.drop.250k / e0.drop.1million
## [1] 0.2866021 -- this is proportion of 1 million drop

.25 * e0.drop.1million
## [1] 0.7341318

## let's visualize linearity / concavity
million = 10^6
deaths = seq(-2000, 2000, 100) * 1000
adj.factor = deaths/ (1 * million)
e0.with.covid.vec = NULL
for (i in 1:length(deaths))
    e0.with.covid.vec[i] = get.e0(Mx_base + Mx_covid_scaled * adj.factor[i])

plot(deaths, e0.with.covid.vec)

## 2 million
Mx_base_and_covid_2million = Mx_base + Mx_covid_scaled *2
e0.with.covid.2million = get.e0(Mx_base_and_covid_2million)
##
(e0.drop.2million = e0.without.covid - e0.with.covid.2million)
## [1] 5.077957



###########################
## now we can do entropy ##
###########################

## 1 million deaths reprsents a 35.7% increase ove 2.8 million
## these are disproportionately at older ages (so loss of e0 is less)


H = get.H(Mx_base) ## [1] 0.1535208

H = .15
d.e0.hat = 1/3 * H * e0.without.covid
## [1] 3.942784

## New text on entropy

## Important point is that effect of 1 million Covid1-9 is smaller than would be the case if the increase in death rates were perfectly proporitonal to mortality at all ages. In the proportional case, we can use mathematical models of the life talbe to approximate the effect. Keyfitz found ... , which would suggest a decline of ...

## Note we're not using 2020 pop to get scaling factor!!!!


## we could scale the pop up
dt <- fread("../data/raw/USA.Exposures_1x1.txt")
dt <- dt[Year == 2017]
Kx <- dt$Total

N2020 = 332.6 *million
N2017 = sum(Kx) ## [1] 325109738
N2020/N2017
## [1] 1.023039
3/2.8 ## [1] 1.071429
## I'm not getting 3 million deaths


###################
## PYL remaining ##
###################

## 1) 330 million people with 44.4 years of remaining life expectancy?
## 330 * 44.4
## [1] 14652
## 14.8
## ##

## So we take 2017 pop and get remaining life expectancy

dt <- fread("../data/raw/USA.Exposures_1x1.txt")
dt <- dt[Year == 2017]
Kx <- dt$Total
names(Kx) = dt$Age
sum(Kx)/million ## 325 ...
## scale up to 330 million

Kx.star = Kx * million * 330/sum(Kx)

## cohort ex

dt = fread("../data/cleaned/cohort_ex_period_2020.csv")
dt = dt[x <= 110]
ex = dt$ex
( ave.ex = sum(ex * Kx.star)/sum(Kx.star) )
## [1] 45.81366
tot.ex = sum(ex * Kx.star)/million
## [1] 14894.47
## or 14.9 billion

## loss of life expectancy
qx = 1 - exp(-Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)

( tot.ex.after.covid = sum(ex * Kx.after.covid)/million )
## [1] 15106.79

print(tot.ex - tot.ex.after.covid)
## [1] 11.71966 million


(tot.ex - tot.ex.after.covid)/tot.ex
## [1] 0.0007751864

1000 * (tot.ex - tot.ex.after.covid)/tot.ex
## [1] 0.7751864

### 2 million
## loss of life expectancy
qx = 1 - exp(-2*Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)

( tot.ex.after.covid = sum(ex * Kx.after.covid)/million )
## [1] 15106.79
## [1] 15095.25
print(tot.ex - tot.ex.after.covid)
## [1] 11.71966 million
## [1] 23.25563

(tot.ex - tot.ex.after.covid)/tot.ex
## [1] 0.0007751864
## [1] 0.001538222 = 1/650
1000 * (tot.ex - tot.ex.after.covid)/tot.ex
## [1] 0.7751864
## [1] 1.538222

######### 250k
## loss of life expectancy
qx = 1 - exp(-.25*Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)

( tot.ex.after.covid = sum(ex * Kx.after.covid)/million )
## [1] 15106.79
## [1] 15115.56
print(tot.ex - tot.ex.after.covid)
## [1] 11.71966 million
## [1] 2.947625
(tot.ex - tot.ex.after.covid)/tot.ex
## [1] 0.0007751864
## [1] 0.000194968= 1/5129



#############################################
## age specific patterns of future ex loss ##
#############################################

## we have ex and kx.
## we want kx_post
## then remaining life lost is
## ex*(1 - (kx_post / kx)

## 1 milllion deaths
qx = 1 - exp(-1*Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)
kx = Kx.star
kx_post = Kx.after.covid
lost.ex = ex*(1 - (kx_post / kx))
par(mfrow = c(1,1))
plot(0:110, lost.ex)
## lost.ex["80"]
##       80
## 0.200724

## 2 milllion deaths
qx = 1 - exp(-2*Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)
kx = Kx.star
kx_post = Kx.after.covid
lost.ex = ex*(1 - (kx_post / kx))
par(mfrow = c(1,1))
plot(0:110, lost.ex)
lost.ex["80"]
##       80
## 0.3971228

## 250k
qx = 1 - exp(-(1/4)*Mx_covid_scaled)
Kx.after.covid = Kx.star * (1-qx)
kx = Kx.star
kx_post = Kx.after.covid
lost.ex = ex*(1 - (kx_post / kx))
plot(lost.ex)
lost.ex.hat = ex*Mx_covid_scaled
lines(lost.ex)

par(mfrow = c(1,1))
plot(0:110, lost.ex)
lost.ex["80"]
##       80
## 0.05059168



## average years of life expectancy lost from covid


#### sex ratio

dt = fread("../data/raw/USA.Mx_1x1.txt")
dt[Age == "110+", Age := 110]
dt[, x := as.numeric(Age)]
dt[Year == 2017, plot(x, Male/Female)]
abline(h = seq(1, 2, .25), col = "grey")
points(c(45, 60, 70, 80, 90),
       c(1.44, 1.37, 1.34, 1.26, 1.12),
       pch = 19)
