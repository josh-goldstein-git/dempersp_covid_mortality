## this is data wrangling file for raw SSA cohort ex data.

## extract both sex cohort ex for 2020 from
## SSA files

## step 1: combine files into a single file
## cat coh5024.h20 coh2500.220 coh0175.220 > coh_ssa.txt

library(data.table)
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
fwrite(dt_full, file = "../data/cleaned/ssa_cohort_lifetables_2020.csv")


## average out men and women
dt_out = dt[period == 2020, .(ex = mean(ex), period = mean(period)), by = .(x,cohort)][order(x)]

## visualize
dt_out[, plot(x, ex)]

## save
fwrite(dt_out, "../data/cleaned/cohort_ex_period_2020.csv")

print("cohort_ex_period_2020.csv created")


