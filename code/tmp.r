
# function to aggregate and count potential dates given aggregation criterion for negbin reg
agg.emm <- function(x, agg = "periodo"){
    # x = pot[[1001]] # debug
    if (agg=="leg"){          x$sel.agg <- sub.e("^([0-9]+)y.^", "\\1", x$periodo)      # select full legislatura
    } else if (agg=="years"){ x$sel.agg <- sub.e("^[0-9]+(y[1-3]).^", "\\1", x$periodo) # select by years
    } else if (agg=="month"){ x$sel.agg <- year(x$date)*100 + month(x$date)             # select by month
    } else {                  x$sel.agg <- x$periodo                                    # default: select by periodo    
    }
    x$pot.dys <- ave(x$sel.agg, as.factor(x$sel.agg), FUN=length, na.rm=TRUE)
    x <- x[duplicated(x$sel.agg)==FALSE, ] # drop redundant rows
    x$agg <- rep(agg, nrow(x))             # indicate aggregation unit
    x <- x[, c("sel.agg","pot.dys","agg")] # keep columns on interest
    return(x)
}

# days potential speaker given tenure
tmp <- vector("list", nrow(dips)) # will receive all data
tmp[] <- 0 # fill with zero days
# apply aggregation function to potential speakers
tmp2 <- lapply(pot[sel], FUN = function(x) agg.emm(x, agg = "periodo")) # n potential dates

tmp[sel] <- tmp2
tmp[[1001]]

# now decide where should info be stored for analysis
# below should help
dips$pot.days <- unlist(tmp) # plug to data
dips$pot.sh <- dips$pot.days / length(all.ses$date) # n session days dip could have attended relative to all legislatura
dips$all.days <- length(all.ses$date)
#dips[19,]   # debug
#ins[[1001]] # debug
rm(pot2,sel,sel2) # clean




# select period to report (full legislatura, or one year, or one periodo ordinario etc
sel3 <- grep("y2", all.ses$periodo) # select full year
all.ses$periodo[sel3][all.ses$leg[sel3]==leg] # and intersect with leg being analyzed
all.ses$date   [sel3][all.ses$leg[sel3]==leg]

sel3 <- grep("-1", all.ses$periodo)     # select full periodo ordinario (or extraordinarios bunched)
sel3 <- grep("-2", all.ses$periodo)     # select full periodo ordinario (or extraordinarios bunched)
sel3 <- grep("-extra", all.ses$periodo) # select full periodo ordinario (or extraordinarios bunched)
all.ses$periodo[sel3][all.ses$leg[sel3]==leg]
all.ses$date   [sel3][all.ses$leg[sel3]==leg]


which(all.ses$date %in% tmp)
which(tmp %in% all.ses$date)

# intersect with sessions to get list of sessions where dips was potential speaker
pot[sel] <- lapply(X = pot[sel], FUN = function(x) all.ses$date[all.ses$date %in% x])


sel3 <- which(all.ses$periodo=="60y1-1")
pot[sel] <- lapply(X = pot[sel], FUN = function(x) all.ses$date[all.ses$date %in% x])



  

# create a list with subsets of each speaker's lines 
speech.list <- split(speeches, speeches$who)
# produce word counts for each speaker, agg and by session
speeches$tmp <- ave(speeches$nchar, as.factor(speeches$date), FUN=sum, na.rm=TRUE) # agg by session
 <- lapply(X = speech.list, FUN = function(X) ave(X$nword, X$date, FUN=sum, na.rm=TRUE) # agg by session
 <- lapply(X = speech.list, FUN = function(X) colSums(X)$nchar # agg by legislatura
 <- lapply(X = speech.list, FUN = function(X) colSums(X)$nword # agg by legislatura
summary(speech.list[[1]])
# search each speaker's lines in dips, paste aggs

