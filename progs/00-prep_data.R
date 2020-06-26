#########################
# Produce cleaned data  #
#########################

# This script (1) loads the INED Covid mortality rates and harmonizes them
# and (2) wrangles the raw SSA cohort ex data. It needs to be run once. 
# 


# init --------------------------------------------------------------------
library(data.table)


# harmonize INED rates ----------------------------------------------------

## We harmonize the INED rates
## 1 -- include Wuhan rates
## 2 -- redistribute US rates from 0, 5, 15, ..., 85+ --> 0, 10, ..., 80+
## 3 -- regroup so all countries have  80+ open interval
## 4 -- output the cleaned data

## from INED

dt = fread("../data/raw/INED_covid_mortality_data_combined.csv")
dt[, V6 := NULL]
dt[, nDx := gsub(",", "", nDx)]
dt[, nKx := gsub(",", "", nKx)]
dt[, nDx := as.numeric(nDx)]
dt[, nKx := as.numeric(nKx)]
## 1 -- include Wuhan rates
wuhan.df = read.csv("../data/raw/Wuhan_covid_mortality.csv")
wuhan.dt = data.table(Country = "Wuhan",
                      x = seq(0, 80, 10),
                      n = NA,
                      nDx = wuhan.df$Deaths[1:9],
                      nKx = wuhan.df$Population.Size[1:9])
dt = rbind(dt, wuhan.dt)


## change name of EnglandWales
dt[Country == "EnglandWales", Country := "England"]
## select included countries
my_country.vec = c("England", "France", "Germany", "Italy", "Korea", "Spain", "USA", "Wuhan")
dt = dt[Country %in% my_country.vec]

## inspect death totals and age range
dt[, sum(nDx), by = Country]
dt[, .(min(x), max(x)), by = Country]

## 2 -- redistribute US rates from 0, 5, 15, ..., 85+ --> 0, 10, ..., 80+

## We use 80+ as the cut-off because then we don't have to do any
## extrapolation, only redistribution for USA
## We take half from each group and redistirbution to either side.
max_age = 199
age10 = seq(0, 80, 10)
n10 = c(diff(age10), 10)
nDx = dt[Country == "USA"]$nDx
x = dt[Country == "USA"]$x
nn = length(nDx)
nDxpn = c(nDx[-1], 0)
nDx10.hat = c(nDx[1] + nDx[2]/2, ## 0-5 & 5-15 --> 0-10
              c((nDx[2:(nn-2)] + nDx[3:(nn-1)])/2), ## 5-15...65-75 --> 10-20...70-80
              nDx[nn-1]/2 + nDx[nn]) ## 75-85 & 85+ --> 80+
plot(age10 + n10/2, nDx10.hat, col = "green", pch = 19)


## now let's get USA 10 year age groups (from HMD)
Age2x <- function(Age)
{
  Age[Age == "110+"] <- "110"
  x = as.numeric(Age)
  return(x)
}
expos_usa = fread("../data/raw/hmd_exposures/USA.Exposures_1x1.txt")
expos_usa[, x := Age2x(Age)]
expos_usa[, x10 := cut(x, c(age10, max_age), include.lowest = TRUE, right = FALSE)]
nKx10 = expos_usa[Year == 2017, xtabs(Total ~ x10)]

## now let's put back into "dt" (this is a bit ugly)
usa_tmp.dt = data.table(Country = "USA10",
                        x = seq(0, 80, 10),
                        n = NA,
                        nDx = nDx10.hat,
                        nKx = unclass(nKx10))
dt = rbind(dt, usa_tmp.dt)
dt[, sum(nKx), by = Country]
## 7:          USA 327167434
## 8:        Wuhan   8535001
## 9:        USA10 325109738
## missing 2 million people, maybe because of date? (Yes, INED has 2018)
dt[, sum(nDx), by = Country]
## deaths match
## 7:          USA 67790
## 9:        USA10 67790
## no
dt = dt[Country != "USA"]
dt[Country == "USA10", Country := "USA"]

## 3 -- standardize all countries to 80+

## now fix the other countries so we have 80+ as open interval for everyone
age10 = seq(0, 80, 10)
max_age = 199
dt[, x10 := cut(x, c(age10, max_age), include.lowest = TRUE, right = FALSE)]

nDx.mat = dt[, xtabs(nDx ~ x10 + Country)]
nKx.mat = dt[, xtabs(nKx ~ x10 + Country)]

nDx.dt = as.data.table(nDx.mat)
setnames(x = nDx.dt, old = "N", new = "nDx")
nKx.dt = as.data.table(nKx.mat)
setnames(x = nKx.dt, old = "N", new = "nKx")
dt10 = merge(nDx.dt, nKx.dt, by = c("Country", "x10"))
dt10[, nMx := nDx/nKx]
## clean age labels
dt10[, x := gsub("\\[", "", x10)]
dt10[, x := gsub(",.*$", "", x)]
dt10[, x10 := NULL]

## 4 -- output the cleaned data

fwrite(dt10, "../data/cleaned/harmonize_covid_deaths.csv")

# Wrangle raw SSA cohort ex data ------------------------------------------

## extract both sex cohort ex for 2020 from
## SSA files
## step 1: combine files into a single file
## cat coh5024.h20 coh2500.220 coh0175.220 > coh_ssa.txt

x <- scan("../data/raw/coh_ssa.txt", character(), sep = "\n")
## each "block" begins with a "  0 ..."
zero_loc = grep("^  0", x)
## break into blocks
my_list = vector("list", length(zero_loc) -1)
for (i in 2:length(zero_loc))
{
  my_list[[i-1]] = x[-8 + (zero_loc[i-1]:
                             zero_loc[i])]
}

## get sex and cohort from blocks before stripping this info
get_sex = function(my_block)
{
  sex = NA
  header = grep("born", my_block, value = TRUE)[1]
  if(grepl("females born", header))
    sex = "f";
  if(grepl(" males born", header))
    sex = "m"
  return(sex)
}
get_cohort = function(my_block)
{
  cohort = NA
  header = grep("born", my_block, value = TRUE)[1]
  header.vec = unlist(strsplit(header, " "))
  cohort = header.vec[length(header.vec)]
  return(cohort)
}
##
sex.vec = unlist(lapply(my_list, get_sex))
cohort.vec = unlist(lapply(my_list, get_cohort))

## now transform each block into a lifetable

extract_lt <- function(my_block) {
  ## get rid of starting spaces
  my_block = gsub("^\\s+", "", my_block)
  ## get only rows that start with a number
  lt_raw = my_block[grepl("^[0-9]", my_block)]
  ## split on any number of spaces
  lt_raw = strsplit(lt_raw, "\\s+")
  ## make matrix
  out.mat = do.call(rbind, lt_raw)
  ## convert to numeric
  mode(out.mat) <- "numeric"
  ## add colnames w/o parenthesis
  my_names = strsplit(my_block[grepl("q\\(x\\)", my_block)][1], "\\s+")[[1]]
  my_names = gsub("\\(", "", my_names)
  my_names = gsub("\\)", "", my_names)
  colnames(out.mat) = my_names
  return(out.mat)
}

lt_list = lapply(my_list, extract_lt)
## make a big data table (with sex and cohort vars)
## create a period variable
## extract period == 2020
names(lt_list) = paste0(sex.vec," ", cohort.vec)
lt_as_dt_list = lapply(lt_list, as.data.table)
dt = rbindlist(lt_as_dt_list, idcol = TRUE)
dt[, sex := substr(.id, 1,1)]
dt[, cohort := substr(.id, 3,6)]
dt[, period := as.numeric(cohort) + as.numeric(x)]

## save entire file as csv

dt[x == 0, plot(cohort, Nx)]
dt[period == 2015, plot(x, Nx)]

dt_full = dt[, .(sex, cohort, x, qx, lx, dx, Lx, Tx, ex)]


## average out men and women
dt_out = dt[period == 2020, .(ex = mean(ex), period = mean(period)), by = .(x,cohort)][order(x)]

## visualize
dt_out[, plot(x, ex)]

## save
fwrite(dt_out, "../data/cleaned/cohort_ex_period_2020.csv")
fwrite(dt_full, file = "../data/cleaned/ssa_cohort_lifetables_2020.csv")

print("cohort_ex_period_2020.csv created")










