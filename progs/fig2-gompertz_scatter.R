################################################ 
# Plot Gompertz Hazards (Figure 2)
################################################ 

# We plot the relationship between exponential rates of increase with age of all-cause 
# mortality and COVID-19 mortality for countries in the Human Mortality Database.


# init --------------------------------------------------------------------

library(data.table)

# data --------------------------------------------------------------------

dt = fread("../data/cleaned/harmonize_covid_deaths.csv")

# calculate Gompertz slopes ————————————————————————————————————

nDx_prop = dt[Country == "USA", prop.table(nDx)]
names(nDx_prop) = dt[Country == "USA"]$x
print("Proportion deaths over age 70")
print(sum(nDx_prop[c("70", "80")]))

tmp = dt[, .("b" = log(nMx[x == 80]/ nMx[x == 40])/40 ),
         by = Country]
print("Gompertz slopes from 40-49 group to 80+ group")
print(tmp[, .(Country, b = round(b, 3))])

## let's get Gompertz slopes for abridged life table

code.vec = c("GBRTENW", "FRATNP", "DEUTNP", "ITA", "KOR", "ESP", "USA")
result.mat = matrix(NA, length(code.vec), 2)
rownames(result.mat) = code.vec
colnames(result.mat) = c("Year", "b")
for (i in 1:length(code.vec))
{
  this_code = code.vec[i]
  print(this_code)
  this_file = paste0("../data/raw/hmd_life_tables/",
                     this_code, "_Mx_5x1.txt")
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

# Plot Gompertz Hazards ————————————————————————————————————

pdf("../text_and_figs/fig2_gompertz_scatter.pdf")
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
system("open ../text_and_figs/fig2_gompertz_scatter.pdf")

