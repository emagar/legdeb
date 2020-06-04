
#####################################################################
##              AGGREDATE BY DAYS FIRST IN ORDER TO                ##
##                DROP NON-SPEECHES OF nword < 50                  ##
## Note: I only consider days' aggregates, so anyone uttering less ##
## than 50 word throughout a single day gets zero words that day   ##
#####################################################################
# create a list with subsets of each speaker's lines 
speeches$ord <- 1:nrow(speeches)
speech.list <- split(speeches, speeches$who)
# produce word counts for each speaker, agg all leg, by month, by session...
#
# aggregate unit's words
speech.list <- lapply(speech.list, FUN = function(x){
    #x <- speech.list[[1]] # debug
    nword2 = ave(x$nword,                                   
                 as.factor(year(x$date)*10000+month(x$date)*100+day(x$date)),# agg by day
#                 as.factor(year(x$date)*100+month(x$date)),                 # agg by month
                 FUN=sum, na.rm=TRUE);
###    x <- x[duplicated(x$date)==FALSE,]           # drop redundant lines
    x <- cbind(x, nword2 = nword2)                                   
    x <- x[,c("ord","who","date","nword2")]             # keep cols of interest
#    x <- x[,c("who","leg","nword")]                 # keep cols of interest
    colnames(x) <- c("ord","who","sel.agg","nword2")       # rename cols for merging
    return(x)
})
# add empty column to receive id and dip index
speech.list <- lapply(speech.list, FUN = function(x) cbind(x, id = NA))
speech.list <- lapply(speech.list, FUN = function(x) cbind(x, i = NA))
#
# Indicator of days to make zero for each diputado
speech.list <- lapply(speech.list, function(x){
    dnonspeech <- ifelse(x$nword < 50, 1, 0)
    x <- cbind(x, dnonspeech=dnonspeech)
    return(x)
    })
#
#speech.list[[1]][1:3,] # debug
#
# unlist agg data into dataframe, merge with other legs
tmp <- do.call(rbind, speech.list)
tmp <- tmp[order(tmp$ord),]
rownames(tmp) <- NULL
#
# return to data
speeches$nword.day  <- tmp$nword2
speeches$dnonspeech <- tmp$dnonspeech

x <- tmp.agg[[sel.dips[1]]]
x[1,]
as.data.frame(x)

is.na(tmp.agg[1][,"nom"])

tmp <- unlist(
    lapply(tmp.agg[sel.dips], function(x) ifelse(length(which(is.na(x[,"nom"]))) > 0, 1, 0))
)

x <- lapply(tmp.agg, subset, sel.dips)

?subset

# check in/out mismathches with speeches one-by-one 
tmp <- data.frame(dip = dips$id)
tmp$dhasnas <- 0 # set dummy to zero default
for (i in sel.dips){
    #i <- 1 #debug
    tmp1 <- tmp.agg[[i]] # extract dataframe
    tmp2 <- ifelse(length(which(is.na(tmp1$nom))) > 0, 1, 0)
    tmp$dhasnas[i] <- tmp2 # return dummy
}
which(tmp$dhasnas==1) # mismatches have NAs
