## Prepare Env ##
#####################
install.packages("dplyr")
install.packages("tidyr")
install.packages('fitdistrplus')
install.packages('logspline')
install.packages('mc2d')
install.packages('tidyverse')

library(dplyr)
library(tidyr)
library(fitdistrplus)
library(logspline)
library(mc2d)
library(tidyverse)
#####################



## Read File ##
#####################
setwd("G:/My Drive/BACKUP/USD School Folder Senior Year/2021 Spring Semester/Senior Design/Senior Design Project") # Defaults script's directory to working directory
getwd()
filename = "test.csv" # Replace test.csv with the values you want to fit
data <- read.csv(filename)
data
#####################




## Filter Data if Necessary ## 
#####################
#data <- subset(data, COLUMN1 == value1 &  COLUMN2 == value2)
#data
#####################




ValueUnits = "mins" # Place the unit of the values, default = "mins"
values = as.numeric(data$Numbers, units=ValueUnits)  # Place the name of the column that have the values you want to fit 
values
hist(values)
length(values)
boxplot(values)

# Remove outliers from data using quantile values
lower_limit = quantile(values)[2]-IQR(values)*1.5
upper_limit = quantile(values)[4]+IQR(values)*1.5
clean_values = values[values >= lower_limit & values <=upper_limit]

hist(clean_values)

options(show.error.messages = TRUE)# TRUE IF YOU WANT TO SEE ERRORS OR FALSE TO NOT SEE IT
try(Random.Triangular <- fitdist(clean_values, "triang", method="mge", start = list(min=min(clean_values), mode=mode(clean_values),max=max(clean_values)), gof="KS"), silent = TRUE)
#try(Random.Weibull <- fitdist(clean_values, "weibull",lower = c(0, 0))) # Weibuull is too much fitting - Use as last resort
try(Random.Normal <- fitdist(clean_values, "norm"))
try(Random.Gamma <- fitdist(clean_values, "gamma",lower = c(0, 0)))
try(Random.Lognormal <- fitdist(clean_values, "lnorm"))
#try(Random.Poisson <- fitdist(clean_values, "pois"))
try(Random.Exponential <- fitdist(clean_values, "exp"), silent=TRUE)
try(Random.LogLogistic <- fitdist(clean_values, "logis"))

### ABS INCLUDE ALL ## # Don't recommend ABS
fits = c('Random.Triangular','Random.Weibull','Random.Normal','Random.Lognormal','Random.Gamma','Random.Poisson','Random.LogLogistic')
aic = c(abs(Random.Triangular$aic),abs(Random.Weibull$aic),abs(Random.Normal$aic),abs(Random.Lognormal$aic),abs(Random.Gamma$aic),abs(Random.Poisson$aic),abs(Random.LogLogistic$aic))
######################

### ABS EXCLUDE POISSON ## # Don't recommend ABS
fits = c('Random.Triangular','Random.Weibull','Random.Normal','Random.Lognormal','Random.Gamma','Random.LogLogistic')
aic = c(abs(Random.Triangular$aic),abs(Random.Weibull$aic),abs(Random.Normal$aic),abs(Random.Lognormal$aic),abs(Random.Gamma$aic),abs(Random.LogLogistic$aic))
######################

### ABS EXCLUDE POISSON AND WEIBULL ## # Don't recommend ABS
fits = c('Random.Triangular','Random.Normal','Random.Lognormal','Random.Exponential', 'Random.Gamma','Random.LogLogistic')
aic = c(abs(Random.Triangular$aic),abs(Random.Normal$aic),abs(Random.Lognormal$aic),abs(Random.Exponential$aic),abs(Random.Gamma$aic),abs(Random.LogLogistic$aic))
######################

###  EXCLUDE POISSON AND WEIBULL ##  ## RECOMMEND TO RUN
fits = c('Random.Triangular','Random.Normal','Random.Lognormal','Random.Exponential', 'Random.Gamma','Random.LogLogistic')
aic = c(Random.Triangular$aic,Random.Normal$aic,Random.Lognormal$aic,Random.Exponential$aic,Random.Gamma$aic,Random.LogLogistic$aic)
######################


dist_aic = min(aic,na.rm = TRUE)

aic[match(dist_aic, aic)]
dist = fits[match(dist_aic, aic)]
dist

param = parse(text = fits[match(dist_aic, aic)])

op = eval(param)[1]

op = unlist(op)


yeet=paste(dist,'(', op[1], sep='')
for (z in length(op)-1){
  yeet=paste(yeet,',',op[z+1], sep='')
}
yeet = paste(yeet, ')', sep='')
yeet



## *Optional* Save If necessary ##
#####################



df[nrow(df) + 1,] = c(x,y,yeet) # Append to DF if necessary
write.csv(df, "OUTPUT_DISTRIBUTION.csv", row.names = FALSE) # Output to CSV if necessary

rm(list = ls()) # CLEAR ALL VARIABLES
#####################


