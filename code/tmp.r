
#########################################
## ADD DV = NUMBER SPEECHES BY PERIODO ##
#########################################
###########
## leg60 ##
###########
# count number of speeches in member-periodo
tmp.dy <- data.member.day.60
tmp.dy$nspeach <- ave(tmp.dy$who, as.factor(paste(tmp.dy$who,tmp.dy$periodo)), FUN=length, na.rm=TRUE)
tmp.dy <- tmp.dy[duplicated(as.factor(paste(tmp.dy$who,tmp.dy$periodo)))==FALSE,]
tmp.dy <- tmp.dy[order(tmp.dy$who),]
tmp.dy <- tmp.dy[, c("who","periodo","nspeach")]
#tmp.dy[1:10,]
#
# open slot to add nspeech as dv
tmp.per <- data.periodo.60 # duplicate for manipulation
tmp.per$dv.nspeech <- 0
# loop to fill in nspeech 
for (i in 1:nrow(tmp.dy)){
    #i <- 2 # debug
    sel <- which(
                 as.factor(paste(tmp.per$nom,tmp.per$sel.agg))
                 %in%
                 as.factor(paste(tmp.dy$who,tmp.dy$periodo))[i]
                )
    if (length(sel)==0) break
    tmp.per$dv.nspeech[sel] <- as.numeric(tmp.dy$nspeach[i])
}
# return data
data.periodo.60 <- tmp.per
#
###########
## leg62 ##
###########
# count number of speeches in member-periodo
tmp.dy <- data.member.day.62
tmp.dy$nspeach <- ave(tmp.dy$who, as.factor(paste(tmp.dy$who,tmp.dy$periodo)), FUN=length, na.rm=TRUE)
tmp.dy <- tmp.dy[duplicated(as.factor(paste(tmp.dy$who,tmp.dy$periodo)))==FALSE,]
tmp.dy <- tmp.dy[order(tmp.dy$who),]
tmp.dy <- tmp.dy[, c("who","periodo","nspeach")]
#tmp.dy[1:10,]
#
# open slot to add nspeech as dv
tmp.per <- data.periodo.62 # duplicate for manipulation
tmp.per$dv.nspeech <- 0
# loop to fill in nspeech 
for (i in 1:nrow(tmp.dy)){
    #i <- 2 # debug
    sel <- which(
                 as.factor(paste(tmp.per$nom,tmp.per$sel.agg))
                 %in%
                 as.factor(paste(tmp.dy$who,tmp.dy$periodo))[i]
                )
    if (length(sel)==0) break
    tmp.per$dv.nspeech[sel] <- as.numeric(tmp.dy$nspeach[i])
}
# return data
data.periodo.62 <- tmp.per
#
###########
## leg64 ##
###########
# count number of speeches in member-periodo
tmp.dy <- data.member.day.64
tmp.dy$nspeach <- ave(tmp.dy$who, as.factor(paste(tmp.dy$who,tmp.dy$periodo)), FUN=length, na.rm=TRUE)
tmp.dy <- tmp.dy[duplicated(as.factor(paste(tmp.dy$who,tmp.dy$periodo)))==FALSE,]
tmp.dy <- tmp.dy[order(tmp.dy$who),]
tmp.dy <- tmp.dy[, c("who","periodo","nspeach")]
#tmp.dy[1:10,]
#
# open slot to add nspeech as dv
tmp.per <- data.periodo.64 # duplicate for manipulation
tmp.per$dv.nspeech <- 0
# loop to fill in nspeech 
for (i in 1:nrow(tmp.dy)){
    #i <- 2 # debug
    sel <- which(
                 as.factor(paste(tmp.per$nom,tmp.per$sel.agg))
                 %in%
                 as.factor(paste(tmp.dy$who,tmp.dy$periodo))[i]
                )
    if (length(sel)==0) break
    tmp.per$dv.nspeech[sel] <- as.numeric(tmp.dy$nspeach[i])
}
# return data
data.periodo.64 <- tmp.per
