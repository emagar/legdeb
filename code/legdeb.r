library(zoo) # has na.locf function used below
library(lubridate)

rm(list = ls())

dd <- "/home/eric/Downloads/Desktop/data/rollcall/DipMex/diariosActas/diarioDebates/"
ve <- "/home/eric/Downloads/Desktop/data/leg-debate/estenograficas/"
wd <- "~/Dropbox/data/leg-debate/"
setwd(ve)

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


# full list of vesrion estenografica files in disk (from sesiones.txt)
ves60 <- c(   "20060829",   "20060901",   "20060905",   "20060907",   "20060912",   "20060914",   "20060919",   "20060921",   "20060926",
"20060929",   "20061003",   "20061004",   "20061005",   "20061010",   "20061011",   "20061012",   "20061017",   "20061019",   "20061024",
"20061026",   "20061030",   "20061031",   "20061107",   "20061109",   "20061114",   "20061116",   "20061121",   "20061124",   "20061128",
"20061201",   "20061205",   "20061207",   "20061212",   "20061214",   "20061218",   "20061219",   "20061221",   "20061222",   "20070201",
"20070201-I", "20070206",   "20070208",   "20070213",   "20070215",   "20070220",   "20070222",   "20070227",   "20070301",   "20070306",
"20070308",   "20070313",   "20070315",   "20070320",   "20070320-I", "20070322",   "20070327",   "20070328",   "20070329",   "20070410",
"20070411",   "20070412",   "20070417",   "20070419",   "20070424",   "20070425",   "20070426",   "20070830",   "20070901",   "20070903",
"20070904",   "20070906",   "20070911",   "20070912",   "20070913",   "20070914",   "20070918",   "20070920",   "20070925",   "20070927",
"20071002",   "20071004",   "20071009",   "20071011",   "20071016",   "20071018",   "20071025",   "20071030",   "20071031",   "20071106",
"20071108",   "20071112",   "20071114",   "20071120",   "20071122",   "20071127",   "20071129",   "20071204",   "20071206",   "20071211",
"20071212",   "20071213",   "20080201",   "20080201-I", "20080205",   "20080207",   "20080212",   "20080214",   "20080219",   "20080221",
"20080226",   "20080228",   "20080304",   "20080308",   "20080311",   "20080313",   "20080325",   "20080326",   "20080327",   "20080401",
"20080403",   "20080408",   "20080410",   "20080415",   "20080417",   "20080422",   "20080424",   "20080429",   "20080430",   "20080828",
"20080901",   "20080902",   "20080909",   "20080911",   "20080917",   "20080918",   "20080923",   "20080925",   "20080930",   "20081002",
"20081007",   "20081009",   "20081014",   "20081015",   "20081016",   "20081021",   "20081023",   "20081028",   "20081030",   "20081104",
"20081106",   "20081111",   "20081118",   "20081119",   "20081125",   "20081127",   "20081202",   "20081204",   "20081209",   "20081211",
"20090201",   "20090201-I", "20090204",   "20090205",   "20090210",   "20090212",   "20090217",   "20090219",   "20090224",   "20090226",
"20090303",   "20090305",   "20090310",   "20090312",   "20090318",   "20090319",   "20090324",   "20090324-I", "20090326",   "20090331",
"20090401",   "20090402",   "20090414",   "20090415",   "20090416",   "20090421",   "20090423",   "20090423-I", "20090428",   "20090430")
#
ves62 <- c(   "20120829",   "20120901",   "20120904",   "20120906",   "20120911",   "20120913",   "20120918",   "20120920",   "20120925",
"20120927",   "20120928",   "20120928v",   "20121002",   "20121004",   "20121009",   "20121011",   "20121016",   "20121018",   "20121023",
"20121025",   "20121030",   "20121031",   "20121106",   "20121108",   "20121113",   "20121115",   "20121120",   "20121122",   "20121127",
"20121128",   "20121201",   "20121204",   "20121206",   "20121211",   "20121212",   "20121213",   "20121218",   "20121219",   "20121220",
"20121221",   "20130201-2", "20130206",   "20130207",   "20130212",   "20130214",   "20130219",   "20130221",   "20130222",   "20130226",
"20130228",   "20130305",   "20130307",   "20130312",   "20130314",   "20130319",   "20130320",   "20130321-I", "20130321-II","20130402",
"20130403",   "20130404",   "20130409",   "20130411",   "20130416-I", "20130416-II","20130417",   "20130418",   "20130423",   "20130424",
"20130425",   "20130429",   "20130430-I", "20130430-II","20130716-1", "20130716-2", "20130717",   "20130821",   "20130822",   "20130831",
"20130901-1", "20130901-2", "20130903",   "20130905",   "20130910",   "20130911",   "20130912",   "20130918",   "20130919",   "20130924",
"20130926",   "20131001",   "20131002",   "20131003",   "20131008",   "20131009-1", "20131009-2", "20131010",   "20131015",   "20131016",   "20131017-1", "20131017-2",
"20131018",   "20131022",   "20131024",   "20131029",   "20131031",   "20131105",   "20131107",   "20131112",   "20131113",   "20131120",
"20131121",   "20131126",   "20131127",   "20131128",   "20131203",   "20131204",   "20131205",   "20131210",   "20131211",   "20140201-2",
"20140204",   "20140205",   "20140206",   "20140211",   "20140212",   "20140213",   "20140218",   "20140220",   "20140225",   "20140227",
"20140304",   "20140305",   "20140306",   "20140311",   "20140313",   "20140318",   "20140319",   "20140320-1", "20140320-2", "20140325",
"20140327",   "20140401",   "20140403",   "20140408",   "20140409",   "20140410",   "20140422-1", "20140422-2", "20140423",   "20140424",
"20140428",   "20140429",   "20140430",   "20140514-1", "20140514-2", "20140515",   "20140619-1", "20140619-2", "20140708",   "20140728",
"20140729",   "20140730",   "20140731",   "20140801",   "20140828",   "20140901",   "20140902",   "20140904",   "20140909",   "20140910",
"20140911",   "20140917",   "20140918",   "20140923",   "20140925",   "20140930",   "20141002",   "20141007",   "20141009-1", "20141009-2",
"20141014",   "20141016",   "20141021",   "20141022-1", "20141022-2", "20141023",   "20141028",   "20141030-1", "20141030-2", "20141104",
"20141106",   "20141111",   "20141113",   "20141119-1", "20141119-2", "20141120",   "20141125",   "20141127",     "20141202", "20141203",
"20141204",   "20141209",   "20141210",   "20141211-1", "20141211-2", "20141215-1", "20150201",   "20150203",   "20150205",   "20150210",
"20150212",   "20150217",   "20150219",   "20150224",   "20150226",   "20150303",   "20150305",   "20150310",   "20150312",   "20150318",
"20150319",   "20150324",   "20150325",   "20150326",   "20150407",   "20150408",   "20150409",   "20150414",   "20150416",   "20150421",
"20150422",   "20150423",   "20150428",   "20150429",   "20150430")
#
ves64 <- c(   "20180829",   "20180901",   "20180904",   "20180906",   "20180911",   "20180913",   "20180918",   "20180919",   "20180920",
"20180925",   "20180927",   "20181002-1", "20181002-2", "20181003",   "20181004",   "20181009",   "20181011",   "20181016",   "20181018-1",
"20181018-2", "20181023",   "20181025",   "20181030",   "20181031",   "20181106",   "20181108",   "20181113-1", "20181113-2", "20181115",
"20181120",   "20181122",   "20181127",   "20181128",   "20181201",   "20181204",   "20181206",   "20181211",   "20181213",   "20181217",
"20181218",   "20181219",   "20181221",   "20181223",   "20190116-1", "20190116-2", "20190201-1", "20190201-2", "20190206",   "20190207",
"20190212-1", "20190212-2", "20190214",   "20190219-1", "20190219-2", "20190221",   "20190226",   "20190228",   "20190305",   "20190307-1",
"20190307-2", "20190312",   "20190314",   "20190402",   "20190403",   "20190404",   "20190408",   "20190409",   "20190410-1", "20190410-2",
"20190411-1", "20190411-2", "20190423",   "20190424-1", "20190424-2", "20190425",   "20190426",   "20190429",   "20190430",   "20190508-1",
"20190508-2", "20190508-3", "20190523-1", "20190523-2", "20190627",   "20190628-1", "20190628-2", "20190718",   "20190725",   "20190831",
"20190901",   "20190903",   "20190905",   "20190910",   "20190918",   "20190919",   "20190924",   "20190926",   "20191001",   "20191002",
"20191003",   "20191008",   "20191009",   "20191010",   "20191015",   "20191016",   "20191017",   "20191022-1", "20191022-2", "20191023",
"20191024",   "20191029",   "20191030",   "20191031",   "20191105",   "20191106",   "20191126",   "20191128",   "20191203",   "20191205",
"20191210-1", "20191210-2", "20191211",   "20191212",   "20200201",   "20200205",   "20200206",   "20200211",   "20200213",   "20200218",
"20200220",   "20200225",   "20200227",   "20200303",   "20200305-1", "20200305-2", "20200310-1", "20200310-2",
"Ve12mar2020",   "Ve18mar2020",   "Ve19mar2020-JP",   "Ve19mar2020")

# check that all files are indeed in dir
setdiff(dir("../estenograficas/"),
        paste(c(ves60, ves62, ves64), ".html", sep = ""))
# how many files? 
all.ves <- data.frame(ves = c(ves60, ves62, ves64), stringsAsFactors = FALSE)
all.ves$leg <- c(rep(60, length(ves60)),
                 rep(62, length(ves62)),
                 rep(64, length(ves64)))

# function to drops accents CAPITALIZE
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
#cap.emm(x = "áÁéíóú")

# read diputado csv files
all.legs <- c(60,62,64)
all.dips <- vector("list", 3) # will receive all data
#
for (l in 1:length(all.legs)){
    tmp.path <- paste("../../rollcall/DipMex/data/diputados/dip", all.legs[l], ".csv", sep = "")
    dips <- read.csv(file = tmp.path, stringsAsFactors = FALSE)
    # add leg
    dips$leg <- all.legs[l];
    # consolidate name last name
    dips$nom <- paste(dips$pila, dips$patmat)
    # CAPITALIZE
    dips$nom <- cap.emm(x = dips$nom)
    # insert to data list
    all.dips[[l]] <- dips
}
summary(all.dips)

# LOOP OVER all.legs WILL START HERE
leg <- 62 # pick one
sel <- which(all.ves$leg==leg)
ves <- all.ves$ves[sel]; 
dips <- all.dips[[2]]
length(ves) # debug

# read one file
## tmp <- readLines(con = "Ve18mar2020.html", encoding = "latin1")

# will receive joined speech for export
speeches <- data.frame()
for (f in 1:length(ves)){
#for (f in 1:50){
    #f <- 100 # debug
    message(sprintf("loop %s of %s", f, length(ves)))
    tmp <- readLines(con = paste(ves[f], ".html", sep = ""), encoding = "latin1", )
    #
    source("../code/fix-typos.r") # minor typos that create exceptions
    #
    # debug
    #grep.e("de la voz, el diputado Marco", tmp)
    #tmp[791]
    #x
    #
    # session's date
    fch <- ses <- NA
    sel <- grep.e(pattern = "^Versión estenográfica de la (?:sesión|comparecencia)", tmp)
    if (length(sel)>0){
        fch <- ses <- tmp[sel]
        fch <- sub.e(".*el (?:lunes|martes|miércoles|jueves|viernes|sábado|domingo) (.+)$", "\\1", fch)
        ses <- sub.e("^Versión estenográfica de la ", "", ses)
        ses <- sub.e("(?:, )?celebrada el (?:lunes|martes|miércoles|jueves|viernes|sábado|domingo).+$", "", ses)
    }
    #
    # bookmarks and session start-end lines
    sel <- grep.e(pattern = "<p><a href=", tmp)
    if (length(sel)>0) {
        bookmarks <- tmp[sel]
        sel <- grep.e(pattern = "Apertura de la sesión", bookmarks)
        start <- 1
        if (length(sel)>0){
            start <- sub.e(pattern = '^.+[#]([a-zA-Z0-9]+)QM.+$', replacement = "\\1", bookmarks[sel]) # start bookmark
            start <- grep.e(pattern = paste("<a name=QM", start, "QM", sep = ""), tmp) # index of speech start
        }
        sel <- grep.e(pattern = "(?:Clausura|Cierre)(?: de la sesión)?", bookmarks, ignore.case = FALSE)
        end <- length(tmp)
        if (length(sel)>0){
            if (length(sel)>1) sel <- sel[length(sel)] # take last occurrence if many
            grep.e(pattern = "(?:Clausura|Cierre)(?: de la sesión)?", bookmarks, ignore.case = FALSE)
            #bookmarks[sel] # debug
            end <- sub.e(pattern = '^.+[#]([a-zA-Z0-9]+)QM.+$', replacement = "\\1", bookmarks[sel]) # start bookmark
            end <- grep.e(pattern = paste("<a name=QM", end, "QM", sep = ""), tmp) # index of speech end
        }
        # will receive data
        text  <-  tmp[start:end]
        rm(start,end)
    } else {
        text  <- tmp # if no bokmarks (as in 2009) then take all readLines
    }
    # subset speech lines
    speech <- data.frame(n = 1:length(text),
                         f = rep(f, length(text)),
                         role = NA,
                         who = NA,
                         fch = rep(fch, length(text)),
                         leg = rep(leg, length(text)),
                         file = rep(ves[f], length(text)),
                         stringsAsFactors = FALSE)
    #speech$who3 <- speech$who2 <- NA # debug
    #
    # presiding officer's lines
    sel <- grep.e(pattern = "<[pb]( style.+)?>(?:<a.+/a>)?<[bp]>(?:El|La) president[ea],? diputad[oa]", text) 
#    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) president[ea],? diputad[oa]", text) 
    speech$role[sel] <- "pres"
    speech$who[sel] <- sub.e(pattern = "^.*president[ea],? diputad[oa]s? ([-a-záéíóúüñ. ]+)[ QM:;]*</b>.*$", replacement = "\\1", text[sel])
    #
    # secretario's lines
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) secretari[oa] diputad[oa]", text) 
    speech$role[sel] <- "secr"
    speech$who[sel] <- sub.e(pattern = "^.*secretari[oa] diputad[oa]s? ([-a-záéíóúüñ,. ]+)[ QM:;]*</b>.*$", replacement = "\\1", text[sel])
    #
    # other diputados
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) diputad[oa]", text)
    speech$role[sel] <- "diputado"
    speech$who[sel] <- sub.e(pattern = "^.*<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) diputad[oa] ([-()a-záéíóúüñ. ]+)[ QM:;]*</b>.*$", replacement = "\\1", text[sel])
    #
    # senadores in joint sessions
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La)(?: secretari[oa]|president[ea])? senador", text)
    speech$role[sel] <- speech$who[sel] <- "senador"
    #
    # secretario de estado (comparacencias)
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) Secretari[oa] de", text)
    speech$role[sel] <- "exec"
    speech$who[sel] <- sub.e(pattern = "^.*<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) Secretari[oa] de ([-;,a-záéíóúüñ. ]+)[ QM:;]*</b>.*$", replacement = "\\1", text[sel])
    #
    # assume that all rows in between belong to last speaker
    # ignore starting rows up to first non-NA, if any
    sel <- which(!is.na(speech$who))[1]
    if (sel > 1){ # if non-NA is not first row
        sel <- sel:nrow(speech)
    } else {
        sel <- 1:nrow(speech)
    }
    speech$role[sel] <- na.locf(speech$role[sel]) # fill NAs forward
    speech$who[sel] <- na.locf(speech$who[sel]) # fill NAs forward
    #
    # merge several sessions
    speeches <- rbind(speeches, cbind(speech, text, stringsAsFactors = FALSE)) # to include text
    #speeches <- rbind(speeches, speech)              # not
}
#    
# export to visualize
#write.csv(speeches, file = "tmp.csv", row.names = FALSE)

# Search new diputados
sel <- grep.e("sí,? protesto", speeches$text)
sel <- sel[2]; speeches$fch[sel] # check one by one
sel <- c((sel-5),(sel-4),(sel-3),(sel-2),(sel-1),sel,(sel+1),(sel+2)); speeches$text[sel]

# Figure this out, looks useful for encoding issues
# https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
library(stringi)
terme  <- c("Millésime", 
            "boulangère", 
            "üéâäàåçêëèïîì")
stri_trans_general(str = terme, id = "Latin-ASCII")

# fix name misspellings
tmp <- speeches # duplicate
speeches <- tmp # restore
source("../code/fix-names.r")
# tabulate names to spot mispellings
#table(speeches$who)

## # debug
## sel <- which(nchar(speeches$who)==max(nchar(speeches$who), na.rm = TRUE)) # report longest string
## speeches$who[sel[1]]
## sel <- which(nchar(speeches$fch)==max(nchar(speeches$fch), na.rm = TRUE)) # report longest string
## speeches$fch[sel]

# CAPITALIZE speeches$who
speeches$who <- gsub.e("á", "a", speeches$who)
speeches$who <- gsub.e("é", "e", speeches$who)
speeches$who <- gsub.e("í", "i", speeches$who)
speeches$who <- gsub.e("ó", "o", speeches$who)
speeches$who <- gsub.e("ú", "u", speeches$who)
speeches$who <- gsub.e("ü", "u", speeches$who)
speeches$who <- toupper(speeches$who)

# list hits
unique(speeches$who)[order(unique(speeches$who))]

# CLEAN
sel <- which(is.na(speeches$who))
#speeches$text[sel]
speeches <- speeches[-sel,]
#
sel <- grep.e("varios.protestan", speeches$who)
#speeches$text[sel]
speeches <- speeches[-sel,]
#
# drop senadores and cabinet members
sel <- which(speeches$who=="SENADOR" | speeches$who=="EXEC")
#speeches$text[sel]
speeches <- speeches[-sel,]
#
# drop missing names in versión estenográfica
sel <- grep.e("no.registrado", speeches$who)
#speeches$text[sel]
speeches <- speeches[-sel,]

# posix date
tmp <- speeches$file
# drop suffixes, if any
tmp <- gsub.e("[-].+$", "", tmp)
## # rename ve files
## sel <- grep.e("ve", tmp)
## tmp[sel] <- sub.e("^ve", "202003", tmp[sel])
## tmp[sel] <- sub.e("mar.+$", "", tmp[sel])
# date
tmp <- ymd(tmp)
speeches$date <- tmp # return to data


# figure out intervals when diputado was active
dips$in1  <- ymd(dips$yrin1*10000  + dips$moin1*100  + dips$dyin1)
dips$out1 <- ymd(dips$yrout1*10000 + dips$moout1*100 + dips$dyout1)
dips$in2 <- ymd(dips$yrin2*10000  + dips$moin2*100  + dips$dyin2)
dips$out2 <- ymd(dips$yrout2*10000 + dips$moout2*100 + dips$dyout2)
#
dips$yrin1<- dips$moin1 <- dips$dyin1 dips$yrout1<- dips$moout1 <- dips$dyout1 dips$yrin2<- dips$moin2 <- dips$dyin2 dips$yrout2<- dips$moout2 <- dips$dyout2 <- NULL
#
dips[1,]
# fix these, if any, in dip.csv file
sel <- which(is.na(dips$in2)==FALSE & is.na(dips$out1)==TRUE)
sel
# fill exit dates
sel <- which(is.na(dips$in2)==FALSE & is.na(dips$out2)==TRUE)
dips$out2[sel] <- ymd("20150827")
sel <- which(is.na(dips$in1)==FALSE & is.na(dips$in2)==TRUE & is.na(dips$out1)==TRUE)
dips$out1[sel] <- ymd("20150827")

# all dates when assembly convened
all.ses <- speeches[,c("date","leg")]
all.ses <- all.ses[duplicated(all.ses$date)==FALSE,]

# open slots for new data
dips$pot.sh <- dips$pot.days <- NA
# fill them up
dips[1,]
for (i in 1:dips){
    i <- 1 # debug
    interval(dips$in1, dips$out1)[i]
    interval(dips$in2, dips$out2)[i]
    # number of sessions diputado could have attended given tenure
    dips$pot.days[i] <- length(
        which(all.ses$date %within% interval(dips$in1, dips$out1)[i])
        )
    # number of sessions diputado could have attended relative to all sessions in legislatura
    tmp.all <- nrow(all.ses[all.ses$leg==leg,])
    dips$pot.sh[i] <- dips$pot.days[i] / tmp.all

    
x



# DE-HTMLIZE TEXT
# rvest is supposed to achieve this as Beautiful Soup does, couldn't get it to work
# library(rvest) # R's Beautiful Soup
text <- speeches$text # duplicate
# drop heading names
text <- gsub.e("<[pb]>(?:<a.+>)?<[pb]>((?:El|La) (?:Secretari[oa]|President[ea]) diputad.+:)", "", text) 
text <- gsub.e("<[pb]>(?:<a.+>)?<[pb]>((?:El|La) diputad.+:)", "", text) 
#
text <- gsub.e("^[ ]+", "", text) # drop heading spaces
text <- gsub.e("[ ]$", "", text) # drop trailing spaces
text <- gsub.e("<a (?:name|ref).+>", "", text) # drop paragraph marks
text <- gsub.e("<p class.+>", "", text) # drop paragraph marks
text <- gsub.e("<p ", "", text) # drop paragraph marks
text <- gsub.e("</?[prbaiu]+>", "", text) # drop marks
text <- gsub.e("</?bold.+>", "", text) # drop marks
text <- gsub.e("</?(?:span|body|html|div|em|sub)>", "", text) # drop marks
text <- gsub.e("<span .+>", "", text) # drop marks
text <- gsub.e("<p$", "", text) # drop marks
text <- gsub.e("<br style.+>", "", text) # drop marks
text <- gsub.e("<(?:span|ba)", "", text) # drop marks
text <- gsub.e("style=.+>", "", text) # drop marks
text <- gsub.e("line-break.>", "", text) # drop marks
text <- gsub.e("[!].+>", "", text) # drop marks
text <- gsub.e("<[!].if$", "", text) # drop marks
text <- gsub.e("<a href.+[.]mx", "", text) # drop marks
text <- gsub.e("(?:^ +| +$)", "", text) # drop heading/trailing spaces
text <- gsub.e("[.,;]", "", text) # drop periods and commas
## # debug
## sel <- grep.e("<", text)
## text[sel]
## write.csv(text, file = "tmp.csv")
## text[221]
speeches$text.only <- text # return manipulation to data
#rm(text)
## Should be able to count words with this text...
#
# count words and characters
speeches$nword <- lengths(gregexpr(pattern = "\\W+", speeches$text.only)) + 1
speeches$nchar <- nchar(speeches$text.only)
speeches[3,]

## matched names
uni <- unique(speeches$who); 
uni <- data.frame(nom = uni, id = NA, stringsAsFactors = FALSE)
for (i in 1:nrow(uni)){
    #i <- 10 # debug
    hits <- grep.e(uni$nom[i], dips$nom)
    dips$nom[hits]
    if (length(hits)>1){
        print("WARNING: multiple hits");
        break
    }
    if (length(hits)==1){
        #uni$hit[i] <- hits[1] # report first hit
        uni$id[i] <- dips$id[hits]
    }
}
# debug: WHICH NAMES WON'T MATCH
## uni <- uni[order(uni$nom),]
## uni
## x

# A block here to get each diputado's words each session
#dip.words  <- list()
dip.words  <- vector(mode = "list", length = nrow(uni)) # init empty list
names(dip.words) <- uni$id
# populate the list
for (i in 1:nrow(uni)){
    #i <- 1
    sel <- which(speeches$who==uni$nom[i])
    tmp <- speeches[sel,]
    #
    dip.words[[i]] <- tmp
    #summary(dip.words[[i]])
}

dip.words[[1]][1,]
dip.words[[1]]$nword

# extract one diputado to produce session aggregates
tmp <- dip.words[[10]]
tmp$interventions <- NA

tmp$interventions <- ave(tmp$nchar, as.factor(tmp$date), FUN=length, na.rm=TRUE)
tmp$tmp <- ave(tmp$nchar, as.factor(tmp$date), FUN=sum, na.rm=TRUE)
tmp$nchar <- tmp$tmp
tmp$tmp <- ave(tmp$nword, as.factor(tmp$date), FUN=sum, na.rm=TRUE)
tmp$nword <- tmp$tmp
tmp$tmp <- NULL

tmp <- tmp[duplicated(tmp$date)==FALSE, ] # keep one obs only

head(tmp)
table(tmp$date)
x



