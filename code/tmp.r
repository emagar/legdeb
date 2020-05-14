
for (l in 1:length(all.legs)){
    tmp <- paste("../../rollcall/DipMex/data/diputados/dip", l, ".csv", sep = "")
    dips <- read.csv(file = tmp, stringsAsFactors = FALSE)
    # add leg
    dips$leg <- all.legs[l];
    # consolidate name last name
    dips$nom <- paste(dips$pila, dips$patmat)
    # CAPITALIZE
    dips$nom <- cap.emm(x = dips$nom)
    # insert to data list
    all.dips[[l]] <- dips
