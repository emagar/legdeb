# Analyzes non-convergence warnings in glmer. 
# See https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html (and related Stack overflow http://stackoverflow.com/questions/23478792/warning-messages-when-trying-to-run-glmer-in-r)

packageVersion("lme4")

library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")

# check data size
nrow(data[data$dpresoff==0,])
length(getME(fit,"theta"))
length(fixef(fit))

# rescale and center all continous parameters -> this took care of the issue, the rest I did not attempt
