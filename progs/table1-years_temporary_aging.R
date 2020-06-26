################################################ 
# Years of "temporary aging" for hypothetical
# epidemics (Table 1)
################################################ 

# Table of years of temporary aging for hypothetical 
# epidemics concentrated in a three month period


# init --------------------------------------------------------------------
library(xtable)
library(data.table)

# create table 1 ----------------------------------------------------------

beta = .1
million = 10^6
covid_death.vec = c(.125, .250, .500, 1, 2) * million
normal <- normal_deaths_in_three_months <- (3 * million) / 4
death_ratio = (covid_death.vec + normal) / normal
aging = log(death_ratio)/beta

table1 <- data.frame(covid_death.vec = covid_death.vec/1000, aging = round(aging,1))


covid_death.vec

table1 <- table1[order(-table1$aging),]

names(table1) <- c("Deaths (1,000s)", "Temporary Aging (years)")


## Deaths (1,000s) Temporary Aging (years)
##       2000                    13.0
##       1000                     8.5
##        500                     5.1
##        250                     2.9
##        125                     1.5


# latex plot --------------------------------------------------------------

print(xtable(table1, digits=c(0, 0,1)), include.rownames=FALSE, hline.after = c(0,5))


