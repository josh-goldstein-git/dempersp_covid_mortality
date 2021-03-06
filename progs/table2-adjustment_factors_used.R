################################################ 
# Adjustment Factors Used for Open Age Interval
# (Table 2)
################################################ 

# Table of adjustment factors that were multiplied 
# by the observed age-specific mortality rates from 
# Covid-19 for the open interval age 80+. 

# init --------------------------------------------------------------------
library(xtable)
library(data.table)

# data --------------------------------------------------------------------

## USA
expos_usa = fread("../data/raw/hmd_exposures/USA.Exposures_1x1.txt")
mort_usa = fread("../data/raw/hmd_life_tables/USA.Mx_1x1.txt")

## Italy
expos_ita = fread("../data/raw/hmd_exposures/ITA.Exposures_1x1.txt", na.string = ".")
mort_ita = fread("../data/raw/hmd_life_tables/ITA.Mx_1x1.txt", na.string = ".")

## Korea
expos_kor = fread("../data/raw/hmd_exposures/KOR.Exposures_1x1.txt")
mort_kor = fread("../data/raw/hmd_life_tables/KOR.Mx_1x1.txt")


mort_wuhan = fread("../data/raw/Wuhan_Kx.csv")

## my_country.vec = c("EnglandWales", "France", "Germany", "Italy", "Korea", "Spain", "USA", "Wuhan")


# calculations ------------------------------------------------------------

Age2x <- function(Age)
{
  Age[Age == "110+"] <- "110"
  x = as.numeric(Age)
  return(x)
}

expos_usa[, x := Age2x(Age)]
mort_usa[, x := Age2x(Age)]
##
Mx_80p = mort_usa[Year == 2017 & x >= 80]$Total
names(Mx_80p) = mort_usa[Year == 2017 & x >= 80]$x
Kx_80p = expos_usa[Year == 2017 & x >= 80]$Total
cdr_80p = sum(Mx_80p * Kx_80p)/sum(Kx_80p)
## [1] 0.09874335

## Now let's do Korea
expos_kor[, x := Age2x(Age)]
mort_kor[, x := Age2x(Age)]
##
Mx_80p = mort_kor[Year == 2017 & x >= 80]$Total
names(Mx_80p) = mort_kor[Year == 2017 & x >= 80]$x
Kx_80p = expos_kor[Year == 2017 & x >= 80]$Total
kor_cdr_80p = sum(Mx_80p * Kx_80p)/sum(Kx_80p)
## [1] 0.08345613
## now get indirectly standardized
sMx_80p = mort_usa[Year == 2017 & x >= 80]$Total
names(sMx_80p) = mort_usa[Year == 2017 & x >= 80]$x
s_kor_cdr_80p = sum(sMx_80p * Kx_80p)/sum(Kx_80p)
## [1] 0.08394175
## a tiny bit higher, which means that the age structure of standard (USA) is a tiny bit older
## our adjusted rate would be multiply observed Korean Covid19 rate 80+ and multiply by adjustment factor
adj_factor_kor = s_kor_cdr_80p / kor_cdr_80p
## [1] 1.005819

## now let's do Italy
expos_ita[, x := Age2x(Age)]
mort_ita[, x := Age2x(Age)]


Mx_80p = mort_ita[Year == 2014 & x >= 80]$Total
names(Mx_80p) = mort_ita[Year == 2014 & x >= 80]$x
Kx_80p = expos_ita[Year == 2014 & x >= 80]$Total
ita_cdr_80p = sum(Mx_80p * Kx_80p)/sum(Kx_80p)
## [1] 0.09278272
## now get indirectly standardized

sMx_80p = mort_usa[Year == 2017 & x >= 80]$Total
names(sMx_80p) = mort_usa[Year == 2017 & x >= 80]$x
s_ita_cdr_80p = sum(sMx_80p * Kx_80p)/sum(Kx_80p)
## [1] 0.09231215

adj_factor_ita = s_ita_cdr_80p / ita_cdr_80p
## [1] 0.9949282

### wuhan

wuhan_Kx = mort_wuhan$"Population Size"
names(wuhan_Kx) = mort_wuhan$Age

wuhan_Kx_80p = wuhan_Kx[paste(80:99)]

## now we calculate the adjustment factor as deaths from wuhan vs deaths from US
## with standard schedule

s = names(wuhan_Kx_80p)
usa_Kx_80p = expos_usa[Year == 2017 & x >= 80]$Total
names(usa_Kx_80p) = expos_usa[Year == 2017 & x >= 80]$x

s_cdr_wuhan = sum(wuhan_Kx_80p * sMx_80p[s]) / sum(wuhan_Kx_80p)
s_cdr_usa =   sum(usa_Kx_80p[s] * sMx_80p[s]) / sum(usa_Kx_80p[s])

s_cdr_wuhan/s_cdr_usa
## [1] 0.8605059

theta_wuhan = s_cdr_usa/s_cdr_wuhan
## [1] 1.162107

## so wuhan's cdr is depressed by about 14% and needs to be inflated to be comparable

s = paste(80:99)
par(mfrow = c(1,1))
code.vec = c("GBRTENW", "FRATNP", "DEUTNP", "ITA", "KOR", "ESP", "USA")
## expos_ita = fread("~/Documents/hmd/hmd_statistics/exposures/Exposures_1x1/ITA.Exposures_1x1.txt", na.string = ".")
theta.vec = rep(NA, length(code.vec))
names(theta.vec) = code.vec
for (i in 1:length(code.vec))
{
  this_code = code.vec[i]
  this_filename = paste0("../data/raw/hmd_exposures/", this_code, ".Exposures_1x1.txt")
  this_dt = fread(this_filename, na.string = ".")
  this_dt[, x := Age2x(Age)]
  this_Kx = this_dt[Year == max(Year) & x >= 80]$Total
  names(this_Kx) = this_dt[Year == max(Year) & x >= 80]$x
  this_cx = prop.table(this_Kx[s])
  ##     s = names(this_Kx)
  ##
  usa_cx = prop.table(usa_Kx_80p[s])
  if(i == 1)
    plot(names(usa_cx), usa_cx, ylim = c(0, .17))
  ##
  this_stan_cdr = sum(this_cx * sMx_80p[s])
  usa_stan_cdr  = sum(usa_cx  * sMx_80p[s])
  theta = usa_stan_cdr/this_stan_cdr
  theta.vec[i] = theta
  lines(names(this_cx), this_cx, col = i, lty = i)
}
legend("topright", code.vec, col = seq(code.vec), lty = seq(code.vec))
lines(names(wuhan_Kx_80p), prop.table(wuhan_Kx_80p), type = "l", col = "red")

theta_all.vec = c(theta.vec, "WUHAN" = theta_wuhan)
theta_pretty = theta_all.vec
names(theta_pretty) = c("England", "France", "Germany",
                        "Italy", "Korea",   "Spain",
                        "USA", "Wuhan")

theta_pretty <- round(theta_pretty, 3)

theta_pretty <- data.frame(as.list(theta_pretty[order(theta_pretty)]))

# create latex table ------------------------------------------------------

print(xtable(theta_pretty, digits = 3), include.rownames=FALSE, hline.after = c(0,1))


