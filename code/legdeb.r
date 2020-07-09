################################################################################
## USE data-prep.r FIRST TO PREPARE DATA FRAMES WITH DIPUTADO-BY-DIPUTADO DATA ##
#################################################################################

library(lubridate) # date handling
library(MASS) # negbin estimation

rm(list = ls())

# paths
dd <- "/home/eric/Downloads/Desktop/data/leg-debate/data/"
wd <- "/home/eric/Downloads/Desktop/data/leg-debate/"
gd <- "/home/eric/Downloads/Desktop/data/leg-debate/plots/"
setwd(dd)

# custom greps
grep.e <- function(pattern, x, perl = TRUE, ignore.case = TRUE){
    tmp <- grep(pattern = pattern, x = x, perl = perl, ignore.case = ignore.case);
    return(tmp)
}
sub.e <- function(pattern, replacement, x, perl = TRUE, ignore.case = TRUE){
    tmp <- sub(pattern = pattern, replacement = replacement, x = x, perl = perl, ignore.case = ignore.case);
    return(tmp)
}
gsub.e <- function(pattern, replacement, x, perl = TRUE, ignore.case = TRUE){
    tmp <- gsub(pattern = pattern, replacement = replacement, x = x, perl = perl, ignore.case = ignore.case);
    return(tmp)
}
# function to drops accents and CAPITALIZE text
cap.emm <- function(x=dips$nom){
    x <- gsub.e("á", "a", x)
    x <- gsub.e("é", "e", x)
    x <- gsub.e("í", "i", x)
    x <- gsub.e("ó", "o", x)
    x <- gsub.e("ú", "u", x)
    x <- gsub.e("ü", "u", x)
    x <- toupper(x)
    return(x)
}
#cap.emm(x = "áÁéíóú") # test

#############################
## READ DIPUTADO CSV FILES ##
#############################
library(plyr) # used for recodes
all.legs <- 57:64
#all.legs <- c(60,62,64)
all.dips <- vector("list", length(all.legs)) # will receive all data
#
for (l in 1:length(all.legs)){
    #l <- 1 # debug
    tmp.path <- paste("../../rollcall/DipMex/data/diputados/dip", all.legs[l], ".csv", sep = "")
    dips <- read.csv(file = tmp.path, stringsAsFactors = FALSE)
    #head(dips) # debug
    # add leg
    dips$leg <- all.legs[l];
    ## # drop columns
    dips$nota <- dips$notas <- dips$fuente <- NULL
    # if part empty but postulo not, use latter
    sel <- which( (is.na(dips$part) | dips$part=="") & !is.na(dips$postulo))
    dips$part[sel] <- dips$postulo[sel]
    # party size
    dips$ptysh[dips$dsup==0] <- as.numeric(ave(dips$part[dips$dsup==0], as.factor(dips$part[dips$dsup==0]), FUN=length, na.rm=TRUE)) / 500 # shares of propietarios only
    tmp <- dips[dips$dsup==0,c("part","ptysh")] # recode vectors for suplentes (dift n possible)
    tmp <- tmp[duplicated(tmp$part)==FALSE,] 
    dips$ptysh[dips$dsup==1] <- mapvalues(dips$part[dips$dsup==1], from = tmp[,1], to = tmp[,2], warn_missing = FALSE)
    dips$ptysh <- as.numeric(dips$ptysh)
    # consolidate name last name
    dips$nom <- paste(dips$pila, dips$patmat)
    # CAPITALIZE
    dips$nom <- cap.emm(x = dips$nom)
    # insert to data list
    all.dips[[l]] <- dips
}
names(all.dips) <- paste("leg", all.legs, sep = "")
summary(all.dips)
head(all.dips$leg60)

# LOAD DATA
# periodo aggregates
load(file = "speech-periodo-60.RData")
load(file = "speech-periodo-62.RData")
load(file = "speech-periodo-64.RData")
# member-day aggregates
load(file = "speech-day-60.RData")
load(file = "speech-day-62.RData")
load(file = "speech-day-64.RData")
ls()

data.member.day.60[1:5,]
data.periodo.60[1,]

# merge into single data frame
data <- data.frame()
data <- rbind(data, data.periodo.60)
data <- rbind(data, data.periodo.62)
data <- rbind(data, data.periodo.64)
rownames(data) <- NULL
rm(data.periodo.60,data.periodo.62,data.periodo.64) # clean
data[1,]
#
# merge into single data frame
data.dy <- data.frame()
data.dy <- rbind(data.dy, data.member.day.60)
data.dy <- rbind(data.dy, data.member.day.62)
data.dy <- rbind(data.dy, data.member.day.64)
rownames(data.dy) <- NULL
head(data.dy)
rm(data.member.day.60,data.member.day.62,data.member.day.64) # clean

# drop redundant column
data$doath <- data$agg <- NULL
data$dv.nword.sh <- data$dv.nword / data$ev.pot.sh # fix dv using share, not pct in denom
## sel <- grep.e("CUEVAS MELO",data$nom)
## data[sel,]
## x

#########################
## member descriptives ##
#########################
sel.col <- grep.e("(?:dv|ev|dpresoff)",colnames(data))  # keep only dv and ev
colnames(data)[sel.col] # debug
tmp.mem <- data[,sel.col] # keep only selected numeric columns
tmp.mem <- aggregate(tmp.mem, by = list(data$nom), FUN = "sum")
tmp.mem$ev.pot.sh <- tmp.mem$ev.pot.dys / tmp.mem$ev.all.dys # fix share for member
tmp.mem$dv.nword.by.dy <- tmp.mem$dv.nword / tmp.mem$ev.pot.dys # words by day
tmp.mem$dpresoff <- as.numeric(tmp.mem$dpresoff>0) # fix dummy
ctrl <- tmp.mem$dpresoff # will contrast presiding officers against rest
tmp.mem$dv.nword.sh <- tmp.mem$ev.all.dys <- tmp.mem$ev.pot.sh <- tmp.mem$dpresoff <- NULL # drop useless
summ <- as.list(rep(NA,7)); names(summ) <- c("min","p10","p25","p50","p75","p90","max") # will receive descriptive stats
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "min"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$min <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "quantile", probs = .1); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$p10 <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "quantile", probs = .25); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$p25 <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "quantile", probs = .5); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$p50 <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "quantile", probs = .75); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$p75 <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "quantile", probs = .9); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$p90 <- tmp
tmp <- aggregate(tmp.mem[,-1], by = list(ctrl), FUN = "max"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$max <- tmp
#
print("* * Descriptives for diputados vs presiding officers * *")
print(summ)

# ALL MEMBERS IN SPEECH DATASET (NOT IN DIPS)
nrow(tmp.mem)
# HOW MANY NEVER SPOKE
nrow(tmp.mem[tmp.mem$dv.nword==0,])
# How may presiding officers
sel.col <- grep.e("(?:dv|ev|dpresoff)",colnames(data))  # keep only dv and ev
sel.row <- grep.e("extra", data$sel.agg)
ctrl <- data$nom[-sel.row] # agg criterion
tmp.mem <- data[-sel.row,sel.col] # keep only selected numeric columns and ordinary sessions
tmp.mem <- aggregate(tmp.mem, by = list(ctrl), FUN = "sum")
table(tmp.mem$dpresoff)
#
tmp.mem$dpresoff <- as.numeric(tmp.mem$dpresoff>0) # fix dummy
#
tmp.mem[1,]
rm(ctrl, sel,sel.col,tmp.mem,tmp) # clean


#############################################################################
##                        RETURNING MEMBERS                                ##
## OJO: THIS OBJECT DROPS DUPLICATES FOR RETURNING MEMBERS, HENCE N = 1710 ##
#############################################################################
tmp.dips <- data.frame()
for (i in c(60,62,64)){ # merge into single dataframe
    sel <- grep(i, names(all.dips))
    tmp <- all.dips[[sel]]
    tmp.dips <- rbind(tmp.dips, tmp)
} 
sel <- which(tmp.dips$doath==0); tmp.dips <- tmp.dips[-sel,] # drop members no oath
# recode missings
sel <- grep.e("[0-9]", tmp.dips$repite)
tmp.dips$repite[-sel] <- "no" # assume all missing are non-returning
tmp$repite[is.na(tmp$repite)] <- "no" # assume all missing are non-returning
tmp.dips <- tmp.dips[duplicated(tmp.dips$nom)==FALSE,] # keep one obs per member
table(tmp.dips$repite, useNA = "always")
# remove past no oaths from label
tmp.dips$repite <- gsub.e("([0-9]{2}0-)", "", tmp.dips$repite) # no oath
tmp.dips$repite <- gsub.e("(-[0-9]{2}0)", "", tmp.dips$repite) # no oath
table(tmp.dips$repite, useNA = "always")
###########################
## simplified categories ##
###########################
tmp.dips$repite2 <- NA
sel <- which(tmp.dips$repite %in% c("no", "60", "62", "64", "60-sen")) # PRESENT IN ONE LEGISLATURA ONLY
tmp.dips$repite2[sel] <- "in one leg only"
sel <- which(tmp.dips$repite %in% c("57-60", "57-62", "57-60-62", "58-60-62", "57-60-sen-64", "58-sen-62-64", "59-62-64", "59-sen-62-64", "60-62", "60-62-64", "60-64", "60-sen-64", "62-64", "59-62-64")) # PRESENT IN TWO LEGISLATURAS
tmp.dips$repite2[sel] <- "in two legs"
sel <- which(tmp.dips$repite %in% c("47-60", "49-51-54-60-sen", "52-54-64", "52-59-sen-62", "53-57-59", "53-57-59-62", "55-62", "55-sen-60", "56-60", "56-58-60", "56-58-61", "57-59-61-64", "57-59-sen-62", "57-59-sen-64", "57-59-64", "57-59-sen-64", "56-59-61-64", "57-60-63", "57-60-sen", "57-61-64", "57-61-sen-64", "57-sen-62", "57-sen-62-sen", "57-64", "58-60", "58-60-63", "58-62", "58-64", "58-sen-62", "59-62", "59-sen-62", "59-64", "59-sen-64", "60-63", "61-64", "sen-52-sen-56-60", "sen-60", "sen-60-sen", "sen-62", "sen-64", "61-sen-64", "61-sen-64", "53-sen-64", "55-59-64", "58-61-sen-64")) # PRESENT IN ANOTHER LEGISLATURA NOT OBSERVED
tmp.dips$repite2[sel] <- "in another (unobserved) leg"
# rename object
all.dips.norep <- tmp.dips
#
# diputados repeating
table(all.dips.norep$repite2, useNA = "always")
#all.dips.norep$leg[is.na(all.dips.norep$repite2)] # debug
#all.dips.norep$repite[is.na(all.dips.norep$repite2)] # debug
all.dips.norep$drepite <- as.numeric(all.dips.norep$repite2!="in one leg only")
tmp <- table(all.dips.norep$drepite)
sum(tmp)
tmp; tmp/sum(tmp)
###########
## WOMEN ##
###########
# object to export plot data
women <- data.frame(party = c("pan","pri","left","other","total"),
                    pct.women = NA,
                    pct.speech = NA,
                    pct.words = NA)
# start to fill it up
sel1 <- which(all.dips.norep$gen=="F" & all.dips.norep$part=="pan")
sel2 <- which(                          all.dips.norep$part=="pan")
tmp <- nrow(all.dips.norep[sel1,]) * 100 / nrow(all.dips.norep[sel2,])
women$pct.women[women$party=="pan"] <- round(tmp,2)
#
sel1 <- which(all.dips.norep$gen=="F" & all.dips.norep$part=="pri")
sel2 <- which(                          all.dips.norep$part=="pri")
tmp <- nrow(all.dips.norep[sel1,]) * 100 / nrow(all.dips.norep[sel2,])
women$pct.women[women$party=="pri"] <- round(tmp,2)
#
sel1 <- which(all.dips.norep$gen=="F" & (all.dips.norep$part=="prd" | all.dips.norep$part=="morena"))
sel2 <- which(                          (all.dips.norep$part=="prd" | all.dips.norep$part=="morena"))
tmp <- nrow(all.dips.norep[sel1,]) * 100 / nrow(all.dips.norep[sel2,])
women$pct.women[women$party=="left"] <- round(tmp,2)
#
sel1 <- which(all.dips.norep$gen=="F" & all.dips.norep$part!="pan" & all.dips.norep$part!="pri" & all.dips.norep$part!="prd" & all.dips.norep$part!="morena")
sel2 <- which(                          all.dips.norep$part!="pan" & all.dips.norep$part!="pri" & all.dips.norep$part!="prd" & all.dips.norep$part!="morena")
tmp <- nrow(all.dips.norep[sel1,]) * 100 / nrow(all.dips.norep[sel2,])
women$pct.women[women$party=="other"] <- round(tmp,2)
#
sel1 <- which(all.dips.norep$gen=="F")
tmp <- nrow(all.dips.norep[sel1,]) * 100 / nrow(all.dips.norep)
women$pct.women[women$party=="total"] <- round(tmp,2)
#
women
#############################
## fill-up continues below ##
#############################


########################################################################
## plenary session (day) descriptives --- dropping presiding officers ##
########################################################################
#######################################
## 1. days aggregated by legislatura ##
#######################################
#############################
## version with n speakers ##
#############################
data.dy$dpresoff <- 1 - as.numeric(data.dy$role=="diputado")
sel.col <- grep.e("(?:nword|leg)",colnames(data.dy))  # keep only dv and ev
#colnames(data.dy)[sel.col] # debug
tmp.dy <- data.dy[data.dy$dpresoff==0,sel.col] # keep diputados only and selected numeric columns
#cbind(tmp.dy[1:20,],ctrl[1:20])
#
# follow legislaturas
ctrl <- data.dy$leg[data.dy$dpresoff==0]
summ <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = c(.5,.9))
summ <- summ[,-grep.e("leg",colnames(summ))]
# add more stats
tmp2 <- aggregate(tmp.dy, by = list(ctrl), FUN = "max")
summ$nword.day.max <- tmp2$nword.day
#
ctrl <- data.dy$date[data.dy$dpresoff==0] # follow days first to produce nspeaker stats
tmp2 <- aggregate(tmp.dy, by = list(ctrl), FUN = "mean"); tmp3 <- tmp2$leg # preserves leg
tmp2 <- aggregate(tmp.dy, by = list(ctrl), FUN = "length") # get nspeakers
tmp2$nspeakers.day <- tmp2$nword.day
tmp2$leg <- tmp3
tmp2$Group.1 <- tmp2$nword.day <- NULL # keep nspeakers only
ctrl <- tmp2$leg # now follow legs
tmp2 <- aggregate(tmp2, by = list(ctrl), FUN = "quantile", probs = c(.5,.9))
summ$nspeakers.day <- tmp2$nspeakers.day
#
print("* * Descriptives by legislatura * *")
print(round(summ,0))

#######################################
# version with nword percentiles only #
#######################################
# follow legislaturas
ctrl <- data.dy$leg[data.dy$dpresoff==0]
summ <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = c(0,.1,.25,.5,.75,.9,1))
summ <- summ[,-grep.e("leg",colnames(summ))]
#
print("* * Descriptives by legislatura * *")
print(round(summ,0))


####################################
## 2. days aggregated by periodos ##
####################################
ctrl <- data.dy$periodo[data.dy$dpresoff==0]
summ <- aggregate(tmp.dy, by = list(ctrl), FUN = "min")
summ <- summ[,-grep.e("leg",colnames(summ))]
summ$nword.day.min <- summ$nword.day; summ$nword.day <- NULL
# add more stats
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = .1)
summ$nword.day.10 <- tmp$nword.day
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = .25)
summ$nword.day.25 <- tmp$nword.day
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = .5)
summ$nword.day.50 <- tmp$nword.day
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = .75)
summ$nword.day.75 <- tmp$nword.day
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "quantile", probs = .9)
summ$nword.day.90 <- tmp$nword.day
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "max")
summ$nword.day.max <- tmp$nword.day
colnames(summ)[1] <- "periodo"
#
print("* * Descriptives by periodo * *")
print(summ)
head(summ)
#
# date-ify periodo for plotting
summ$date <- c(ymd("20060901"), ymd("20070201"), ymd("20070901"), ymd("20080201"), ymd("20080901"), ymd("20090201"), ymd("20120901"), ymd("20130201"), ymd("20130515"), ymd("20130901"), ymd("20140201"), ymd("20140515"), ymd("20140901"), ymd("20150201"), ymd("20180901"), ymd("20190201"), ymd("20190515"), ymd("20190901"), ymd("20200201"), ymd("20200630"))
#
summ$dextra <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1) # extraordinarios
summ$color <- ifelse(summ$dextra==0, "black", "gray") # color for extraordinarios
#
summ[,grep.e("nword",colnames(summ))] <- log(summ[,grep.e("nword",colnames(summ))],10) # log scale
#

# plot periodo quartiles
pdf(file = paste(gd, "quantiles-periodo.pdf", sep = ""), height = 7, width = 7)
#png(filename = paste(gd, "quantiles-periodo.png", sep = ""), height = 480, width = 480)
par(mar=c(4,4,0,1)+0.1) # drop title space and left space
plot(c(rep(min(summ$nword.day.min),20), rep(max(summ$nword.day.max),20)), c(rep(summ$date,2)), type = "n", axes = FALSE,
#     main = "Speeches in the legislative periods observed",
     main = "",
     xlab = "Daily speechmaking by deputy (number of words, log scale)", ylab = "Calendar year",
#     xlab = "Palabras diarias por orador (escala log)", ylab = "Año",
     xlim = c(1.69,4.5), 
     ylim = c(ymd("20060901"), ymd("20210606"))) # set ranges
#log(15000,10)
axis(1, at = c(1.69,2,2.69,3,3.69,4,4.69), labels = c("50","100","500","1000","5k","10k","50k"))
axis(2, at = seq(ymd("20070101"),ymd("20210101"),"year"), labels = FALSE)
axis(2, at = c(ymd("20080101"), ymd("20100101"), ymd("20120101"), ymd("20140101"), ymd("20160101"), ymd("20180101"), ymd("20200101")), labels = seq(2008,2020,2))
#
abline(v = log(median(data.dy$nword.day[data.dy$role=="diputado"]), 10), lty = 2)
abline(h = c(ymd("20060702"), ymd("20090705"), ymd("20120701"), ymd("20150707"), ymd("20180701")), lty = 3, col = "gray") # ymd("20210606")
text(rep(2,3), c(ymd("20060702"),ymd("20120701"),ymd("20180701")), labels = "Presidential election", pos = 1, offset = .2, col = "gray", cex = .67)
#text(rep(2,3), c(ymd("20060702"),ymd("20120701"),ymd("20180701")), labels = "Elección presidencial", pos = 1, offset = .2, col = "gray", cex = .67)
text(rep(2,2), c(ymd("20090705"),ymd("20150707")), labels = "Midterm election", pos = 3, offset = .2, col = "gray", cex = .67)
#text(rep(2,2), c(ymd("20090705"),ymd("20150707")), labels = "Elección intermedia", pos = 3, offset = .2, col = "gray", cex = .67)
points(summ$nword.day.min, summ$date, col = summ$color, cex = .5)
points(summ$nword.day.max, summ$date, col = summ$color, cex = .5)
for (i in 1:20){
    lines(c(summ$nword.day.10[i], summ$nword.day.90[i]), rep(summ$date[i],2), col = summ$color[i])
    lines(c(summ$nword.day.25[i], summ$nword.day.75[i]), rep(summ$date[i],2), lwd = 3, col = summ$color[i])
    points(summ$nword.day.50[i], summ$date[i], pch = 20, col = summ$color[i])
}
text(4.35, ymd("20080101"), labels = "60th")
text(4.35, ymd("20140101"), labels = "62nd")
text(4.35, ymd("20190915"), labels = "64th")
## text(4.35, ymd("20080101"), labels = "LX")
## text(4.35, ymd("20140101"), labels = "LXII")
## text(4.35, ymd("20190915"), labels = "LXIV")
## text(4.35, ymd("20190215"), labels = "(parcial)")
text(4.35, ymd("20110401"), labels = "61st",      col = "gray")
text(4.35, ymd("20100901"), labels = "(unobserved)", col = "gray")
text(4.35, ymd("20170401"), labels = "63rd",      col = "gray")
text(4.35, ymd("20160901"), labels = "(unobserved)", col = "gray")
## text(4.35, ymd("20110401"), labels = "LXI",      col = "gray")
## text(4.35, ymd("20100901"), labels = "(no obs.)", col = "gray")
## text(4.35, ymd("20170401"), labels = "LXIII",      col = "gray")
## text(4.35, ymd("20160901"), labels = "(no obs.)", col = "gray")
text(log(median(data.dy$nword.day[data.dy$role=="diputado"]), 10), ymd("20210606"), labels = paste("Median =", median(data.dy$nword.day[data.dy$role=="diputado"]), "words") )
## text(log(median(data.dy$nword.day[data.dy$role=="diputado"]), 10), ymd("20210606"), labels = paste("Mediana =", median(data.dy$nword.day[data.dy$role=="diputado"]), "palabras") )
#dev.off()

####################
## longest speech ##
####################
sel <- which(data.dy$nword.day == max(data.dy$nword.day[data.dy$dpresoff==0]))
data.dy[sel,]

############################
## sum of words in period ##
############################
data[1,]
tmp <- data[,c("sel.agg","leg","dpresoff","dv.nword","dv.nspeech")]
tmp <- tmp[tmp$dpresoff==0,]
tmp$wordsum <- ave(tmp$dv.nword, as.factor(tmp$sel.agg), FUN=sum, na.rm=TRUE)
tmp$speechsum <- ave(tmp$dv.nspeech, as.factor(tmp$sel.agg), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$sel.agg)==FALSE,]
tmp[1:10,]
# periodos ordiarios only, no presiding officers
tmp[-grep.e("extra", tmp$sel.agg),c("sel.agg","wordsum","speechsum")]
summary(tmp$wordsum[-grep.e("extra", tmp$sel.agg)])
summary(tmp$speechsum[-grep.e("extra", tmp$sel.agg)])
(1465*500*.508)/789300


##############################################
## histogram of number speakers in sessions ##
##############################################
ctrl <- data.dy$date[data.dy$dpresoff==0] # follow daily stats
tmp <- aggregate(tmp.dy, by = list(ctrl), FUN = "length")
tmp$nspeakers <- tmp$leg
tmp$leg <- tmp$nword.day <- NULL

tmp[which(tmp$nspeakers>100),]

#pdf(file = paste(gd, "nspeakers.pdf", sep = ""), height = 5, width = 7)
#png(filename = paste(gd, "nspeakers.png", sep = ""), height = 345, width = 480)
par(mar=c(4,4,1,1)+0.1) # drop title space and left space
hist(tmp$nspeakers, breaks = 24,
#     main = "How many spoke in a plenary session",
     main = "",
#     xlab = "Number of speechmakers in daily session",
     xlab = "Diputados en tribuna al día", ylab = "Frecuencia",
     xlim = c(0,120),
     ylim = c(0,80))
abline(v=median(tmp$nspeakers),lty=2)
#text(median(tmp$nspeakers), 77, labels = paste("Median =", median(tmp$nspeakers)) )
text(median(tmp$nspeakers), 77, labels = paste("Mediana =", median(tmp$nspeakers)) )
#dev.off()

data.mem$ev.pot.sh <- data.mem$ev.pot.dys / data.mem$ev.all.dys # fix share for member
data.mem$dv.nword.by.dy <- data.mem$dv.nword / data.mem$ev.pot.dys # words by day
data.mem$dpresoff <- as.numeric(data.mem$dpresoff>0) # fix dummy
ctrl <- data.mem$dpresoff # will contrast presiding officers against rest
data.mem$dv.nword.sh <- data.mem$ev.all.dys <- data.mem$ev.pot.sh <- data.mem$dpresoff <- NULL # drop useless
summ <- as.list(rep(NA,5)); names(summ) <- c("mean","median","sd","min","max")
tmp <- aggregate(data.mem[,-1], by = list(ctrl), FUN = "mean"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$mean <- tmp
tmp <- aggregate(data.mem[,-1], by = list(ctrl), FUN = "median"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$median <- tmp
tmp <- aggregate(data.mem[,-1], by = list(ctrl), FUN = "sd"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$sd <- tmp
tmp <- aggregate(data.mem[,-1], by = list(ctrl), FUN = "min"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$min <- tmp
tmp <- aggregate(data.mem[,-1], by = list(ctrl), FUN = "max"); tmp[,1] <- factor(tmp[,1], labels = c("dips","pres.off"))
summ$max <- tmp
summ
rm(ctrl, sel,sel.col,tmp) # clean


#################
## WOMEN TABLE ##
#################
# diputadas by legislatura (doath==1) --- keeps repeaters in
sel <- which(all.dips$leg60$doath==1)
round( table(all.dips$leg60$gen[sel])*100 / length(all.dips$leg60$gen[sel]), 0); length(all.dips$leg60$gen[sel])
sel <- which(all.dips$leg62$doath==1)
round( table(all.dips$leg62$gen[sel])*100 / length(all.dips$leg62$gen[sel]), 0); length(all.dips$leg62$gen[sel])
sel <- which(all.dips$leg64$doath==1)
round( table(all.dips$leg64$gen[sel])*100 / length(all.dips$leg64$gen[sel]), 0); length(all.dips$leg64$gen[sel])
# num women
tmp <- 0
sel <- which(all.dips$leg60$doath==1 & all.dips$leg60$gen=="F"); tmp <- tmp + length(sel)
sel <- which(all.dips$leg62$doath==1 & all.dips$leg62$gen=="F"); tmp <- tmp + length(sel)
sel <- which(all.dips$leg64$doath==1 & all.dips$leg64$gen=="F"); tmp <- tmp + length(sel)
tmp # women in 3 legs
tmp <- 0
sel <- which(all.dips$leg60$doath==1 & all.dips$leg60$gen=="M"); tmp <- tmp + length(sel)
sel <- which(all.dips$leg62$doath==1 & all.dips$leg62$gen=="M"); tmp <- tmp + length(sel)
sel <- which(all.dips$leg64$doath==1 & all.dips$leg64$gen=="M"); tmp <- tmp + length(sel)
tmp # men in 3 legs
# diputadas presiding officer
sel <- which(data.dy$role=="pres")
table(data.dy$who[sel])
# diputadas committee chair
tmp.dips$dchair <- 1 - as.numeric(tmp.dips$prescom=="")
table(tmp.dips$gen, tmp.dips$dchair)
35/(35+108) # pct chairs women
tmp <- round(table(tmp.dips$gen, tmp.dips$dchair) * 100 / rowSums(table(tmp.dips$gen, tmp.dips$dchair)), 0)
cbind(tmp, tot=100, N= rowSums(table(tmp.dips$gen, tmp.dips$dchair)))
# diputadas party leader
sel <- grep.e("coor", tmp.dips$lider)
tmp.dips$dleader <- 0; tmp.dips$dleader[sel] <- 1
table(tmp.dips$gen, tmp.dips$dleader)
5/(5+19) # pct leaders women
sel <- which(tmp.dips$part=="pan" | tmp.dips$part=="pri" | tmp.dips$part=="morena" | tmp.dips$part=="prd")
table(tmp.dips$gen[sel], tmp.dips$dleader[sel])
table(tmp.dips$gen[-sel], tmp.dips$dleader[-sel])
5/(5+7)
0 # pct leaders maj party women
# words women speakers
tmp <- round(sum(data$dv.nword[data$dfem==1])*100 / (sum(data$dv.nword[data$dfem==1]) + sum(data$dv.nword[data$dfem==0])), 2)# %
tmp
                                                     sum(data$dv.nword[data$dfem==1]) + sum(data$dv.nword[data$dfem==0])     # N
#################################
## continue filling women data ##
#################################
women$pct.words[women$party=="total"] <- tmp
# words women speakers
sel1 <- which(data$dfem==1 & data$part=="pan")
sel2 <- which(               data$part=="pan")
tmp <- round( sum(data$dv.nword[sel1])*100 / sum(data$dv.nword[sel2]), digits = 2) # %
#tmp
women$pct.words[women$party=="pan"] <- tmp
#
sel1 <- which(data$dfem==1 & data$part=="pri")
sel2 <- which(               data$part=="pri")
tmp <- round( sum(data$dv.nword[sel1])*100 / sum(data$dv.nword[sel2]), digits = 2) # %
#tmp
women$pct.words[women$party=="pri"] <- tmp
#
sel1 <- which(data$dfem==1 & (data$part=="prd" | data$part=="morena"))
sel2 <- which(                data$part=="prd" | data$part=="morena" )
tmp <- round( sum(data$dv.nword[sel1])*100 / sum(data$dv.nword[sel2]), digits = 2) # %
#tmp
women$pct.words[women$party=="left"] <- tmp
#
sel1 <- which(data$dfem==1 & data$part!="pan" & data$part!="pri" & data$part!="prd" & data$part!="morena")
sel2 <- which(               data$part!="pan" & data$part!="pri" & data$part!="prd" & data$part!="morena")
tmp <- round( sum(data$dv.nword[sel1])*100 / sum(data$dv.nword[sel2]), digits = 2) # %
#tmp
women$pct.words[women$party=="other"] <- tmp
#
women

# nspeech women speakers
tmp <- round(sum(data$dv.nspeech[data$dfem==1])*100 / (sum(data$dv.nspeech[data$dfem==1]) + sum(data$dv.nspeech[data$dfem==0])), 2) # %
tmp
                                                       sum(data$dv.nspeech[data$dfem==1]) + sum(data$dv.nspeech[data$dfem==0])   # N
#################################
## continue filling women data ##
#################################
women$pct.speech[women$party=="total"] <- tmp
# words women speakers
sel1 <- which(data$dfem==1 & data$part=="pan")
sel2 <- which(               data$part=="pan")
tmp <- round( sum(data$dv.nspeech[sel1])*100 / sum(data$dv.nspeech[sel2]), digits = 2) # %
#tmp
women$pct.speech[women$party=="pan"] <- tmp
#
sel1 <- which(data$dfem==1 & data$part=="pri")
sel2 <- which(               data$part=="pri")
tmp <- round( sum(data$dv.nspeech[sel1])*100 / sum(data$dv.nspeech[sel2]), digits = 2) # %
#tmp
women$pct.speech[women$party=="pri"] <- tmp
#
sel1 <- which(data$dfem==1 & (data$part=="prd" | data$part=="morena"))
sel2 <- which(                data$part=="prd" | data$part=="morena" )
tmp <- round( sum(data$dv.nspeech[sel1])*100 / sum(data$dv.nspeech[sel2]), digits = 2) # %
#tmp
women$pct.speech[women$party=="left"] <- tmp
#
sel1 <- which(data$dfem==1 & data$part!="pan" & data$part!="pri" & data$part!="prd" & data$part!="morena")
sel2 <- which(               data$part!="pan" & data$part!="pri" & data$part!="prd" & data$part!="morena")
tmp <- round( sum(data$dv.nspeech[sel1])*100 / sum(data$dv.nspeech[sel2]), digits = 2) # %
#tmp
women$pct.speech[women$party=="other"] <- tmp
#
women
#######################################
## data for women plot ready, export ##
#######################################
write.csv(women, file = "data-for-women-figure-mexico.csv")

# do barplot
#pdf(file = paste(gd, "women-bar.pdf", sep = ""), height = 5, width = 7)
#png(filename = paste(gd, "women-bar.png", sep = ""), height = 340, width = 480)
abcd <- c(0,.25,.5,.75)+.125 # increments for 3 cols .25-wide, centered at .5
par(mar=c(0,4,0,1)+0.1) # bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1)
plot(x = 1:5, y = seq(-6,55,length.out = 5), ylab = "Percentage", xlab = "", type = 'n', axes = FALSE)
axis(2)
text(x = (1:4)+.5, y = -2, labels = c("PAN","PRI","Left","Other"))
text(x = 3, y = -6, labels = c("Party"))
abline(h = seq(0,50,10), col = "gray")
for (p in 1:4){
    polygon( x = p + c(abcd[1],abcd[1],abcd[2],abcd[2]), y = c(0,women$pct.speech[p],women$pct.speech[p],0), col = "gray85", border = "gray85" )
    polygon( x = p + c(abcd[2],abcd[2],abcd[3],abcd[3]), y = c(0,women$pct.words [p],women$pct.words [p],0), col = "gray60", border = "gray60" )
    polygon( x = p + c(abcd[3],abcd[3],abcd[4],abcd[4]), y = c(0,women$pct.women [p],women$pct.women [p],0), col = "gray35", border = "gray35" )
}
legend(4,55, legend = c("% speeches","% words","% women"), fill = c("gray85","gray60","gray35"))
#dev.off()
rm(abcd,i,l,p,sel,sel1,sel2,sel.row,tmp,tmp2,tmp3,tmp.dips,tmp.dy,tmp.path) # clean

#############
## PLOT DV ##
#############
#####################
## NUMBER OF WORDS ##
#####################
tmp <- data$dv.nword[data$dpresoff==0]
quantile(tmp, probs = seq(0,1,.05))
sel <- which(tmp>8000)
length(tmp)
length(sel)/length(tmp)
summary(tmp[sel])
tmp[sel][order(tmp[sel])]
data[which(data$dv.nword==max(tmp)),]
tmp[sel] <- 7999 # bunch outliers at fake level
# zeroes
sel <- which(tmp==0)
length(sel)
length(sel)/length(tmp)
# filibusters
sel <- which(data$dv.nword[data$dpresoff==0]>20000)
data$nom[data$dpresoff==0][sel]
data$part[data$dpresoff==0][sel]
data$leg[data$dpresoff==0][sel]
data$dv.nword[data$dpresoff==0][sel]

#pdf(file = "../plots/dv-histogram.pdf", width = 7, height = 5)
#png(filename = "../plots/dv-histogram.png", height = 400, width = 480)
par(mar=c(4,4,1,1)+0.1) # drop title space and left space
#hist(tmp, breaks = 100, xlim = c(0,8000), ylim = c(0,3600), main = NULL, xlab = "Words member spoke per period")
hist(tmp, breaks = 100, xlim = c(0,8000), ylim = c(0,3600), main = NULL, xlab = "Palabras del diputado en el periodo", ylab = "Frecuencia")
text(7950, 350, "*")
axis(1, at = seq(0,8000,1000), labels = FALSE)
abline(v=median(tmp), lty = 2)
#text(median(tmp), 3500, paste("Median =", median(tmp), "words"), pos = 4)
text(median(tmp), 3500, paste("Mediana =", median(tmp), "palabras"), pos = 4)
#dev.off()

########################
## NUMBER OF SPEECHES ##
########################
tmp <- data$dv.nspeech[data$dpresoff==0]
quantile(tmp, probs = seq(0,1,.05))
# zeroes
sel <- which(tmp==0)
length(sel)
length(sel)/length(tmp)
# hi participants
sel <- which(data$dv.nspeech[data$dpresoff==0]>10)
data$nom[data$dpresoff==0][sel]
table(data$nom[data$dpresoff==0][sel], data$leg[sel])
data$part[data$dpresoff==0][sel]
data$leg[data$dpresoff==0][sel]
data$dv.nspeech[data$dpresoff==0][sel]
# tabular
table(tmp)
summary(tmp)
sd(tmp)

#pdf(file = "../plots/dv-nspeech-histogram.pdf", width = 7, height = 5)
#png(filename = "../plots/dv-nspeech-histogram.png", height = 400, width = 480)
par(mar=c(4,4,1,1)+0.1) # drop title space and left space
#hist(tmp, breaks = 37, xlim = c(0,40), ylim = c(0,6000), main = NULL, xlab = "Speeches member gave per period")
hist(tmp, breaks = seq(-.5,37.5,1), xlim = c(0,40), ylim = c(0,3600), main = NULL, xlab = "Intervenciones del diputado en el periodo", ylab = "Frecuencia")
abline(v=median(tmp), lty = 2)
#text(median(tmp), 5800, paste("Median =", median(tmp), "speech"), pos = 4)
text(median(tmp), 3500, paste("Mediana =", median(tmp), "discurso"), pos = 4)
text(max(tmp), 300, paste("Máximo =", max(tmp)))
#dev.off()



####################
## Dichotomous DV ##
####################
data$dword <- as.numeric(data$dv.nword > 0)
###########################################################
## COMPOSITIONAL-CONSCIOUS PARTY SHARE (RELATIVE TO PRI) ##
###########################################################
data$rptysh <- NA # new variable
sel <- which(data$leg==60 & data$dpri==1); tmp <- data$ptysh[sel][1] # tmp is the pri's share
sel <- which(data$leg==60); data$rptysh[sel] <- data$ptysh[sel] / tmp
sel <- which(data$leg==62 & data$dpri==1); tmp <- data$ptysh[sel][1] # tmp is the pri's share
sel <- which(data$leg==62); data$rptysh[sel] <- data$ptysh[sel] / tmp
sel <- which(data$leg==64 & data$dpri==1); tmp <- data$ptysh[sel][1] # tmp is the pri's share
sel <- which(data$leg==64); data$rptysh[sel] <- data$ptysh[sel] / tmp
#######################
## PRESIDENT'S PARTY ##
#######################
data$dprespty <- 0
data$dprespty[data$leg==60 & data$dpan==1] <- 1
data$dprespty[data$leg==62 & data$dpri==1] <- 1
data$dprespty[data$leg==64 & data$dmorena==1] <- 1
#####################
## MAJORITY STATUS ##
#####################
data$dmaj <- 0
data$dmaj[data$leg==64 & data$dmorena==1] <- 1
#################
## AGE SQUARED ##
#################
data$agesq <- data$age^2
data$lnage <- log(data$age)
#########################
## PRD 60 62 AS MORENA ##
#########################
data$dleft2 <- data$dmorena
data$dleft2[data$dleft==1 & data$leg<64] <- 1
data[1,]
############
## MEMBER ##
############
data$individual <- as.numeric(factor(data$nom))
########################
## CARDINAL SENIORITY ##
########################
data$nterms <- 0 # no previous terms is default
# leg 64
data$tmp <- data$repite # for manipulation
sel <- which(data$leg==64)
tmp1 <- nchar(data$tmp[sel]) # ncharacters before dropping hyphens
data$tmp[sel] <- gsub.e("-", "", data$tmp[sel]) # remove hyphens
tmp2 <- nchar(data$tmp[sel]) # ncharacters after dropping hyphens
data$nterms[sel] <- tmp1 - tmp2 # dif is number terms previously served
# leg 62
data$tmp <- data$repite # for manipulation
sel <- which(data$leg==62)
sel2 <- grep.e("62-", data$tmp[sel])
data$tmp[sel] <- gsub.e("(62)-.+$", "", data$tmp[sel]) # remove 62 post 
tmp1 <- nchar(data$tmp[sel]) # ncharacters before dropping hyphens
data$tmp[sel] <- gsub.e("-", "", data$tmp[sel]) # remove hyphens
tmp2 <- nchar(data$tmp[sel]) # ncharacters after dropping hyphens
data$nterms[sel] <- tmp1 - tmp2 # dif is number terms previously served
# leg 60
data$tmp <- data$repite # for manipulation
sel <- which(data$leg==60)
sel2 <- grep.e("62-", data$tmp[sel])
data$tmp[sel] <- gsub.e("(62)-.+$", "", data$tmp[sel]) # remove 62 post 
tmp1 <- nchar(data$tmp[sel]) # ncharacters before dropping hyphens
data$tmp[sel] <- gsub.e("-", "", data$tmp[sel]) # remove hyphens
tmp2 <- nchar(data$tmp[sel]) # ncharacters after dropping hyphens
data$nterms[sel] <- tmp1 - tmp2 # dif is number terms previously served
#
# turn into the number of years the MP has spent in the legislature
data$tmp1 <- 0 # default: add nothing
sel <- grep.e("y2", data$sel.agg)
data$tmp1[sel] <- 1 # add 1 to y2
sel <- grep.e("y3", data$sel.agg)
data$tmp1[sel] <- 2 # add 2 to y3
#
data$seniority <- (3*data$nterms) + data$tmp1 # past terms into years + current yrs
#
sel <- grep.e("sen", data$repite)
data$seniority[sel] <- data$seniority[sel] + 3 # past senadores need 3 more years for that term
#
data$tmp <- data$tmp1 <- NULL # clean
table(data$seniority, data$nterms) # dpastleg did not consider senado tenures
#######################
## PARTY SIZE TO MAJ ##
#######################
data$ptysh <- data$ptysh*100 # to percent
data$size.maj <- NA
data$size.maj <- data$ptysh - 50 # deficit towards majority in pct
## #version with zeroes above .5
## sel <- which(data$ptysh > .5)
## data$size.maj[sel] <- 0 # above majority to zero
## data$size.maj[-sel] <- data$ptysh[-sel]*100 - 50 # rest are deficit towards majority in pct
######################
## RESCALE CONT VAR ##
######################
data$ev.pot.dysS <- NA
data$size.majS <- NA
data$seniorityS <- NA
data$ev.pot.dysS[data$dpresoff==0] <- scale(data$ev.pot.dys[data$dpresoff==0])
data$size.majS  [data$dpresoff==0] <- scale(data$size.maj  [data$dpresoff==0])
data$seniorityS [data$dpresoff==0] <- scale(data$seniority [data$dpresoff==0])
###############################
## LEGISTATURA FIXED EFFECTS ##
###############################
data$d60 <- as.numeric(data$leg==60)
data$d62 <- as.numeric(data$leg==62)
data$d64 <- as.numeric(data$leg==64)
data$dsmd64 <- data$dsmd*data$d64
#####################
## EXTRAORDINARIAS ##
#####################
sel <- grep.e("extra", data$sel.agg)
data$dextra <- 0
data$dextra[sel] <- 1


############################################
## nwords, nspeech and nterms (from data) ##
############################################
table(data$nterms[data$dpresoff==0])
sel <- which(data$nterms==0 & data$dpresoff==0)
mean(data$dv.nword[sel])
mean(data$dv.nspeech[sel])
length(sel)
sel <- which(data$nterms >0 & data$dpresoff==0)
mean(data$dv.nword[sel])
mean(data$dv.nspeech[sel])
length(sel)
sel <- which(data$nterms==1 & data$dpresoff==0)
mean(data$dv.nword[sel])
mean(data$dv.nspeech[sel])
length(sel)
sel <- which(data$nterms==2 & data$dpresoff==0)
mean(data$dv.nword[sel])
mean(data$dv.nspeech[sel])
length(sel)
sel <- which(data$nterms >2 & data$dpresoff==0)
mean(data$dv.nword[sel])
mean(data$dv.nspeech[sel])
length(sel)
data$nom[sel]


#######################################
## Session days ordinarias vs extras ##
#######################################
tmp <- data[duplicated(data$sel.agg)==FALSE,]
mean(tmp$ev.all.dys[tmp$dextra==1])
mean(tmp$ev.all.dys[tmp$dextra==0])



## GRAFICA MATRIZ DE CORRELACIONES: COOL!
sel <- c("dv.nword", "ev.pot.dys", "dmaj", "rptysh", "ptysh", "size.maj", "dpan", "dleft2", "seniority", "dleader", "dchair", "dsmd", "dsup", "dfem")
tmp <- cor(data[data$dpresoff==0,sel])
library('corrplot')   # plots correlation matrix!
corrplot(tmp, color = TRUE) #plot matrix
#corrplot(tmp, method = "circle") #plot matrix


######################
## VAR DESCRIPTIVES ##
######################
summary(data[data$dpresoff==0, c("dv.nword","dv.nword.sh","dv.nspeech", "ev.pot.dys", "size.maj", "ptysh", "seniority", "nterms", "age")])
round(sapply(data[data$dpresoff==0, c("dv.nword","dv.nword.sh","dv.nspeech","ev.pot.dys", "size.maj", "ptysh", "seniority", "nterms", "age")], sd), digits = 2) # std devs
round(sd(data$age[data$dpresoff==0], na.rm = TRUE), digits = 2) # std devs
#
table(data$dword  [data$dpresoff==0])
table(data$dmaj   [data$dpresoff==0])
table(data$dleader[data$dpresoff==0])
table(data$dchair [data$dpresoff==0])
table(data$dsmd   [data$dpresoff==0])
table(data$dsup   [data$dpresoff==0])
table(data$dextra [data$dpresoff==0])
table(data$dfem   [data$dpresoff==0])
table(data$d60    [data$dpresoff==0])
table(data$d62    [data$dpresoff==0])
table(data$d64    [data$dpresoff==0])
table(data$dpan   [data$dpresoff==0])
table(data$dleft2 [data$dpresoff==0])
table(data$dpri   [data$dpresoff==0])

#########################
## NEGBIN MODELS nword ## 
#########################
f25 <- formula("dv.nword ~ log(ev.pot.dys)  + dmaj + size.maj  + dpan + dleft2 + dleader + dchair + dsmd + dsmd64 + seniority + dfem + dsup + d62 + d64 + dextra")
f24 <- formula("dv.nword ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + age")
#f23 <- formula("dv.nword ~ log(ev.pot.dysS) + dmaj + size.majS + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + (1|individual)")
f23 <- formula("dv.nword ~ log(ev.pot.dys)  + dmaj + size.maj  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + (1|individual)")
f22 <- formula("dv.nword ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64")
f21 <- formula("dv.nword ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd          + nterms + dfem + dsup           ")

fit21 <- glm.nb  (formula = f21, data = data, subset = dpresoff==0)
fit22 <- glm.nb  (formula = f22, data = data, subset = dpresoff==0)
library(lme4)
#fit23 <- glmer.nb(formula = f23, data = data, subset = dpresoff==0)
#fit23 <- glmer(formula = f23, data = data, family = poisson, subset = dpresoff==0)

summary(fit22)$coefficients
summary(fit21)


###########################
## NEGBIN MODELS nspeech ## 
###########################
f5 <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + size.maj  + dpan + dleft2 + dleader + dchair + dsmd + dsmd64 + seniority + dfem + dsup + d62 + d64 + dextra")
f4 <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + age")
#f3 <- formula("dv.nspeech ~ log(ev.pot.dysS) + dmaj + size.majS + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + (1|individual)")
f3 <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + size.maj  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + (1|individual)")
f2 <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + dextra")
f1 <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd          + nterms + dfem + dsup           ")

fit1 <- glm.nb  (formula = f1, data = data, subset = dpresoff==0)
fit2 <- glm.nb  (formula = f2, data = data, subset = dpresoff==0)
fit2p <- glm    (formula = f2, data = data, family = poisson, subset = dpresoff==0)
## library(lme4)
## fit3 <- glmer.nb(formula = f3, data = data, subset = dpresoff==0)
## fit3 <- glmer(formula = f3, data = data, family = poisson, subset = dpresoff==0)

summary(fit2)$coefficients
summary(fit2p)



# log likelihood
logLik(fit1)
logLik(fit2)
logLik(fit2p)

# n
nobs(fit1)
nobs(fit2)
nobs(fit2p)


################
## OLS MODELS ##
################
## fx <- formula("dv.nword.sh ~ ev.pot.dys + dmaj + size.maj + dpan + dleft2 + dleader + dchair + dsmd + dsmd64 + seniority + dsup + dfem + d62 + d64 + age")
f <- formula("dv.nword.sh ~ dmaj + ptysh + dleader + dchair + dsmd + dsmd64 + seniority + dfem + dsup + d62 + d64 + age") #ev.pot.dys
f13 <- formula("dv.nword.sh ~ dmaj + ptysh + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + dextra + (1|individual)")
f12 <- formula("dv.nword.sh ~ dmaj + ptysh + dleader + dchair + dsmd + dsmd64 + nterms + dfem + dsup + d62 + d64 + dextra")
f11 <- formula("dv.nword.sh ~ dmaj + ptysh + dleader + dchair + dsmd          + nterms + dfem + dsup")

fit11 <- lm(formula = f11, data=data, subset = dpresoff==0)
fit12 <- lm(formula = f12, data=data, subset = dpresoff==0)
library(lme4)
library(lmerTest)
fit13 <- lmer(formula = f13, data=data, subset = dpresoff==0)
rand(fit13) # test random effects
class(fit13) <- "lmerMod" # stargazer needs this

summary(fit13)$coefficients
summary(fit13)$list
names(summary(fit13))


################################
## ZERO INFLATED NEGBIN MODEL ##
################################
library(pscl) # zero inflated poisson
f2z <- formula("dv.nspeech ~ log(ev.pot.dys)  + dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + seniority + dfem + dsup + d62 + d64 + dextra | ev.pot.dys + dsup + seniority + dsmd + dsmd64 + d64 + dextra")
fit2z <- zeroinfl(formula = f2, data=data, dist = "poisson", subset = dpresoff==0) 
summary(fit2z)$coefficients

help(AIC)


#################
## LOGIT MODEL ##
#################
tmp <- table(data$dword[data$dpresoff==0], factor(data$dfem[data$dpresoff==0], labels = c("M","F")))
tmp; tmp / rowSums(tmp)
data$dword[data$dpresoff==0])
table(data$dword[data$dpresoff==0], data$dfem[data$dpresoff==0])
f <- formula("dword ~ dmaj + ptysh  + dleader + dchair + dsmd + dsmd64 + seniority + dfem + dsup + d62 + d64")
fit <- glm(formula = f, data=data, family = "binomial", subset = dpresoff==0) 
summary(fit)
x

###########################
## FIT VS INTERCEPT-ONLY ##
###########################
f <- formula("dv.nspeech ~ log(ev.pot.dys)  + 1")
f <- formula("dv.nspeech ~ 1")
fit0 <- glm.nb  (formula = f, data = data, subset = dpresoff==0)
anova(fit1, fit0)
anova(fit2, fit0)
## fit0 <- glm  (formula = f, data = data, family = poisson, subset = dpresoff==0)
## anova(fit2p, fit0)
# not applicable to zero-inflated: anova(fit2z, fit0)
f <- formula("dv.nword.sh ~ 1")
fit0 <- lm(formula = f, data=data, subset = dpresoff==0)
anova(fit11, fit0)
anova(fit12, fit0)
anova(fit13, fit0)
x

##################
## EXPORT TABLE ##
##################
library(stargazer)
stargazer(fit11, fit12, fit13, fit1, fit2, fit2z, align=TRUE, report = ('vc*s'),
          title = "Regression results",
          type = "text", out = "tmp-tab.txt",
          digits = 2,
          dep.var.labels = c("Words/exposure in period", "Speeches in period")
          , covariate.labels=
 c("Exposure (logged)",
   "Majority",
   "Party size",
#   "Left",
   "Party leader",
   "Comm. chair",
   "SMD",
   "SMD x reelect",
   "Seniority",
   "Woman",
   "Suplente",
   "62nd Leg.",
   "64th Leg.",
   "Extraordinary",
   "Constant")
          )

## library(apsrtable)
## apsrtable(fit1, fit2)

############################
# Average marginal effects #
############################
library(margins)
mar2 <- margins(fit2)
mar2 <- summary(mar2)
#tmp <- c(6,4,2,3,1,9,10,11,7,8,5); mar3 <- mar3[order(tmp),] # sort rows so coefs appear in same order as in table  *with dmocion*
tmp <-  c(11,12,4,13,6,3,2,8,9,10,1,5,7);   # sort rows so coefs appear in same order as in table
data.frame(1:13,mar2[,1],tmp)[order(tmp),] # view sort tmp's order
mar2 <- mar2[order(tmp),]
tmp <- c("Tenure",
         "Majority",
         "Party leader",
         "Comm. chair",
         "Seniority",
         "Woman",
         "Party size",
         "SMD",
         "SMD x reelection",
         "Suplente",
         "62nd Leg.",
         "64th Leg.",
         "Extraordinary")

#drop some regressors to not report them
mar2.dup <- mar2
mar2 <- mar2[1:9,]
tmp <- tmp[1:9]

#pdf(file = "../plots/avgMgEffects.pdf", width = 7, height = 5)
#png(filename = "../plots/avgMgEffects.png", height = 480, width = 480)
par(mar=c(4,2,2,2)+0.1) # drop title space and left space
plot(x=c(-1.5,2.5),#)(-8250,1850),
     y=-c(1,nrow(mar2)),
     type="n", axes = FALSE,
     xlab = "Average marginal effect (speeches in period)",
     ylab = "")
abline(v=seq(-.5, 2.5, .5), col = "gray70")
abline(v=0, lty=2)
abline(h=seq(-1,-nrow(mar2),-1), col = "gray70")
axis(1, at = seq(-.5, 2.5, .5), labels = FALSE)
axis(1, at = seq(-.5, 2.5, .5), labels = c("-0.5","0","","+1","","+2",""))
for (i in c(-1:-nrow(mar2))){
    points(y=i, x=mar2$AME[-i], pch=20, cex=1.5, col = "black")
    lines(y=rep(i, 2), x=c(mar2$lower[-i],mar2$upper[-i]), lwd = 2, col = "black")
}
#mar2$factor
polygon(x= c(-2,-.55,-.55,-2), y=c(-9.1,-9.1,0,0), col = "white", border = "white")
text(x=-1.5, y=-1:-nrow(mar2), labels=tmp, pos=4)
#dev.off()


##########################
## SIMULATE AND PREDICT ##
##########################
# std error version
sims2 <- with(data,
              data.frame(ev.pot.dys=median(ev.pot.dys),
                         dmaj = c( rep(0,51), rep(1,11), rep(0,51), rep(1,11) ),
                         ptysh = rep(c(seq(0,50,by=1), seq(50,60,by=1)), 2),
                         dleader = 0,
                         dchair = 0,
                         dsmd = 1, 
                         dsmd64 = c(rep(0,62),rep(1,62)), 
                         nterms = 0,
                         dsup = 0,
                         dfem = 1,
                         d62 = 1, #c(rep(1,60),rep(0,60)),
                         d64 = 0  #c(rep(0,60),rep(1,60))
                         )
              )
#sims2$dmaj <- as.numeric(sims2$size.maj > 0) # fix dmaj
#tail(sims2,10)
sims2$pr <- predict(fit2, newdata = sims2, type = "response")
sims2 <- cbind(sims2, predict(fit2, newdata = sims2, type="link", se=TRUE))
sims2 <- within(sims2, {
  PredictedWords <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})
#sims2$size.maj <- seq(from=-50, to=10, length.out = 120) # for plot


######################
## PLOT SIMULATIONS ##
######################
#pdf (file = "../plots/predictedWords.pdf", width = 7, height = 5)
#png(filename = "../plots/predictedWords.png", height = 480, width = 480)
par(mar=c(4,4,2,2)+0.1) # drop title space and left space
plot(sims2$ptysh, sims2$UL, ylim = c(0,max(sims2$UL)), axes = FALSE, type = "n",
     main = "", xlab = "Speechmaker's party size (%)", ylab = "Predicted speeches per member in period")
axis(1, at = seq(0,60,5), labels = FALSE)
axis(1, at = seq(0,60,10), labels = seq(0,60,10))
axis(2, at = seq(0,9,1), labels = FALSE)
axis(2, at = seq(0,9,2), labels = TRUE)
abline(h = seq(0,9,1), col = "gray")
abline(v = seq(0,60,5), col = "gray")
abline(v=50, lty = 2)
#
#couleur <- rgb(175/255, 175/255, 175/255, alpha = .5) # gray
couleur <- rgb(255/255, 0/255, 0/255, alpha = .33) # red
sel <- which(sims2$dmaj==0 & sims2$dsmd64==0)
polygon(c(sims2$ptysh[sel], rev(sims2$ptysh[sel])), c(sims2$LL[sel], rev(sims2$UL[sel])), col = couleur, border = couleur)
#
sel <- which(sims2$dmaj==1 & sims2$dsmd64==0)
polygon(c(sims2$ptysh[sel], rev(sims2$ptysh[sel])), c(sims2$LL[sel], rev(sims2$UL[sel])), col = couleur, border = couleur)
#
#couleur <- rgb(150/255, 150/255, 150/255, alpha = .5) # gray
couleur <- rgb(0/255, 0/255, 255/255, alpha = .33) # blue
sel <- which(sims2$dmaj==0 & sims2$dsmd64==1)
polygon(c(sims2$ptysh[sel], rev(sims2$ptysh[sel])), c(sims2$LL[sel], rev(sims2$UL[sel])), col = couleur, border = couleur)
#
sel <- which(sims2$dmaj==1 & sims2$dsmd64==1)
polygon(c(sims2$ptysh[sel], rev(sims2$ptysh[sel])), c(sims2$LL[sel], rev(sims2$UL[sel])), col = couleur, border = couleur)
#
#
sel <- which(sims2$dmaj==0 & sims2$dsmd64==0)
lines(sims2$ptysh[sel], sims2$PredictedWords[sel], col = "red")
#
sel <- which(sims2$dmaj==1 & sims2$dsmd64==0)
lines(sims2$ptysh[sel], sims2$PredictedWords[sel], col = "red")
#
sel <- which(sims2$dmaj==0 & sims2$dsmd64==1)
lines(sims2$ptysh[sel], sims2$PredictedWords[sel], col = "blue")
#
sel <- which(sims2$dmaj==1 & sims2$dsmd64==1)
lines(sims2$ptysh[sel], sims2$PredictedWords[sel], col = "blue")
#
text(50,8, labels = "Majority", srt = 90, cex = .75, pos = 4)
text(23,5.3, labels = "member", col = "blue")
text(23,4.8, labels = "can reelect", col = "blue")
text(10,2.2, labels = "member is", col = "red")
text(10,1.7, labels = "term-limited", col = "red")
#
# add individuals
tmp <- data[, c("ptysh", "individual")]
head(tmp)
tmp <- tmp[duplicated(tmp$individual)==FALSE,]
table(tmp$ptysh)
tmp$xjitter <- runif(nrow(tmp),-1,1)/2
tmp$yjitter <- runif(nrow(tmp),-1,1)/12
couleur <- rgb(175/255, 175/255, 175/255, alpha = .5) # gray
points(tmp$ptysh+tmp$xjitter, -.15+tmp$yjitter, cex = .005, col = couleur)
#dev.off()


####################################
## PREPARE VARIABLES FOR ANALYSIS ##
####################################

DONE 1. drop speeches of less than 50 words

DONE 2. include speeches regardless of their nature in the analysis. In some countries, there are reasons to include only a certain type of speech (e.g., bill debates). We are happy to accommodate chapters where the authors do not use all debates, provided that there is a good justification in the text.

DONE 3. In terms of window of observation/time period under study: we don’t have a particular guideline for this. Please use the window of observation that you believe is more representative of the politics of legislative debate in your country. Ideally we would like each chapter to include several legislative periods, but we are pragmatic here, considering data availability.

EMM: Terminology
- A Legislature (with Roman numerals for reasons I ignore) is an elected chamber for a legislative term, called a Congress in the U.S. Concurrent with presidential elections the chamber of deputies renovates in whole, and again at the presidential mid-term. Diputados remain three years in office and were single term-limited up to 2021. The 2021 mid-term election will be the first since 1932 to allow incumbents on the ballot, a major change in Mexican legislative politics.
- Legislative years break into two "ordinary periods", one covering the months of September through December, inclusive, another February through April, also inclusive. "Extraordinary periods" may be convened during the recess in order to consider a specific bill. Analysis aggregates each member's speeches in the duration of a given period (merging together all extraordinary periods that year, if any). So members in a legislative year like 2012-13 (that had no extraordinary periods) have two word aggregates in the dataset, one for each ordinary period; in a year like 2013-14 (that did), they have three word aggregates in the data. Periods are the units of aggregation in the analysis. 
- A plenary session is a specific date in the calendar when diputados met. During ordinary periods, sessions are usually held on Tuesdays and Thursdays, and may be scheduled in other weekdays if the Jucopo so decides. Diputados met on forty and thirty-one days in the first and second ordinary periods of 2013-14, respectively, and nine days in extraordinary periods, for a yearly total of eighty session days. (A session in North-American legislative parlance is a Mexican period.)

DONE 4. What counts as a debate? Please concatenate all interventions that MPs make in a debate, even if the MP speaks multiple times in the said debate.


###########################
## MULTIVARIATE ANALYSIS ##
###########################

VAR DONE, ESTIMATE MODELS 1. Number of speeches that a legislator delivered in the time unit you defined (presumably, for most of you, the legislative term). In doing so, use a negative binomial regression.

VAR DONE, ESTIMATE MODELS 2. Number of words divided by exposure (see below how to operationalize) that a legislator delivered in the time unit you defined (presumably, for the most of you, the legislative term). In doing so, use an OLS.
3. For both cases, please included fixed-effects for the time period of interest (e.g., for the legislative term or the legislative session, depending on your choice). 
4. Please include standard errors clustered at the legislator level.

How to build the DV for the OLS models: 

DONE Where the outcome is the number of Words, you should use Exposure as the denominator to create a ratio. The said ratio should consist of the total number of words legislator i delivered during legislative term t/percentage of time legislator i sat in legislative term t.

The rationale behind this measure is that we want to capture the time that each legislator sits in parliament during a given session. Obviously, a legislator who sits for the duration of the terms has higher chances of taking the floor than a legislator that takes her sit in the middle of the term.

Don’t forget to include Term FE, plus clustered standard errors at the MP level.


################
## COVARIATES ##
################

DONE 1. Gender – dummy variable that takes a value of 1 for Women and 0 for Men
DONE 2. Party Size – continuous variable that measures the absolute number of members of the legislative party
DONE 3. Seniority – continuous variable that measures the number of years the legislator has been in the parliament
DONE, NEEDS MORE DATA 4. Age
DONE 5. Age Squared
DONE 6. Party Family (Dummy variables, using one of party families as reference category)
DONE 7. Committee Chair – dummy variable that takes a value of 1 if the MP holds a committee chair and 0 for all others
DROPPED 8. Minister – dummy variable that takes a value of 1 if the MP is a minister and 0 otherwise
DONE 9. Government party member – dummy variable that takes a value of 1 if the MP belongs to a legislative party that belongs the government and 0 otherwise. Note that we only consider parties that are formally in a coalition (i.e., have members in the executive). Supporting parties, e.g. contract parliamentarism, do not count towards government parties.
DONE 10. Legislative Party Leadership – dummy variable that takes a value of 1 if the MP belongs to the leadership of the parliamentary party group
DROPPED 11. Party Leader – dummy variable that takes a value of 1 if the MP is the party leader and 0 otherwise
DONE 12. Exposure (logged) – continuous variable that measures the percentage of time in which the MP held to her seat in parliament during the unit of time defined in your chapter. For example, if you are using a MP-legislative term unit of observation, in this variable you need to include the percentage of time during the legislative term in which the MP was in the parliament. If MP was in parliament for whole session that would be 1. If the MP joined the parliament later, it could be .7 or .8. If you are using month as the time unit, the same rationale applies. The logged version should *only* be included in the count models (negative binomial). 


################
## NEGBIN OLS ##
################

Please produce a table including both the negative binomial models and the OLS. For negative binomial models, please report the AIC.

Please include up to 5 models in the tables. Consider using a step-wise approach to regression by including covariates into the equation that make most sense in your context. 
Ultimately, we need 2 final models, where all variables are included – one where the dependent variable is the Number of Speeches and the other where the dependent variable is the Number of Words.

As a default we consider the following variables as explanatory:
1. Gender
2. Seniority
3. Committee Chairs
4. Minister
5. Government party member
6. Legislative Party Leadership
7. Party Leader

The following variables are considered controls:

1. Age
2. Age Squared
3. Party Family
4. Exposure (logged)

Please feel free to use variables interchangeably between the two categories depending on the context. 

Please plot marginal effects using the full specification of the negative binomial model. In the said plot, please include explanatory variables only. Controls variables can be omitted.


