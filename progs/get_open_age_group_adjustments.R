## I think this supercedes "age_structure_adjustment.R"
## need to check and if so, delete the other file

## produce the theta for adjustment


## 0 -- standard

my_age_range = paste(80:99)
Age2x <- function(Age)
{
    Age[Age == "110+"] <- "110"
    x = as.numeric(Age)
    return(x)
}
mort_usa = fread("../data/raw/USA.Mx_1x1.txt")
mort_usa[, x := Age2x(Age)]
usa_Mx_80p = mort_usa[Year == 2017 & x >= 80]$Total
names(usa_Mx_80p) = mort_usa[Year == 2017 & x >= 80]$x
stan_Mx_80p = usa_Mx_80p[my_age_range] ## for Wuhan

expos_usa = fread("../data/raw/hmd_exposures/USA.Exposures_1x1.txt")
expos_usa[, x := Age2x(Age)]
usa_Kx_80p = expos_usa[Year == 2017 & x >= 80]$Total
names(usa_Kx_80p) = expos_usa[Year == 2017 & x >= 80]$x
stan_cx = prop.table(usa_Kx_80p[my_age_range])

## 1-- from HMD
s = my_age_range
par(mfrow = c(1,1))
code.vec = c("GBRTENW", "FRATNP", "DEUTNP", "ITA", "KOR", "ESP", "USA")
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
    if(i == 1)
        plot(names(stan_cx), stan_cx, ylim = c(0, .17))
    ##
    this_stan_cdr = sum(this_cx * stan_Mx_80p[s])
    stan_cdr  = sum(stan_cx  * stan_Mx_80p[s])
    theta = stan_cdr/this_stan_cdr
    theta.vec[i] = theta
    lines(names(this_cx), this_cx, col = i, lty = i)
}
legend("topright", code.vec, col = seq(code.vec), lty = seq(code.vec))


## 2 -- from Wuhan data

wuhan.dt = fread("../data/raw/Wuhan_Kx.csv")
wuhan_Kx = wuhan.dt$"Population Size"
names(wuhan_Kx) = wuhan.dt$Age

wuhan_Kx_80p = wuhan_Kx[paste(my_age_range)]

## now we calculate the adjustment factor as deaths from wuhan vs deaths from US
## with standard schedule

stan_cdr_wuhan = sum(wuhan_Kx_80p * stan_Mx_80p) / sum(wuhan_Kx_80p)

stan_cdr_usa =   sum(stan_cx * stan_Mx_80p)

theta_wuhan = stan_cdr_usa / stan_cdr_wuhan
## [1] 1.162107

lines(names(wuhan_Kx_80p), prop.table(wuhan_Kx_80p), type = "l", col = "red")

## 3 -- put together


theta_all.vec = c(theta.vec, "WUHAN" = theta_wuhan)
theta_pretty = theta_all.vec
names_old = names(theta_all.vec)
names_new = rep(NA, length(names_old))
names_new[names_old == "GBRTENW"] <- "England"
names_new[names_old == "FRATNP"] <- "France"
names_new[names_old == "DEUTNP"] <- "Germany"
names_new[names_old == "ITA"] <- "Italy"
names_new[names_old == "KOR"] <- "Korea"
names_new[names_old == "ESP"] <- "Spain"
names_new[names_old == "USA"] <- "USA"
names_new[names_old == "WUHAN"] <- "Wuhan"
names(theta_pretty) = names_new

## sort(theta_pretty)
##    France       USA   England     Spain     Italy   Germany     Korea     Wuhan
## 0.9967053 1.0000000 1.0362666 1.0512126 1.0669473 1.0984380 1.1602161 1.1621071

