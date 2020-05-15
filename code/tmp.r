
# create a list with subsets of each speaker's lines 
speech.list <- split(speeches, speeches$who)
# produce word counts for each speaker, agg and by session
 <- lapply(X = speech.list, FUN = function(X) ave(X$nchar, X$date, FUN=sum, na.rm=TRUE) # agg by session
 <- lapply(X = speech.list, FUN = function(X) ave(X$nword, X$date, FUN=sum, na.rm=TRUE) # agg by session
 <- lapply(X = speech.list, FUN = function(X) colSums(X)$nchar # agg by legislatura
 <- lapply(X = speech.list, FUN = function(X) colSums(X)$nword # agg by legislatura
summary(speech.list[[1]])
# search each speaker's lines in dips, paste aggs

