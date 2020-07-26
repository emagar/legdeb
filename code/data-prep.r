library(zoo) # has na.locf function used below
library(lubridate) # date handling
library(plyr) # used for recodes

rm(list = ls())

# paths
dd <- "/home/eric/Downloads/Desktop/data/rollcall/DipMex/diariosActas/diarioDebates/" # alternative not used
ve <- "/home/eric/Downloads/Desktop/data/leg-debate/estenograficas/"
wd <- "/home/eric/Downloads/Desktop/data/leg-debate/"
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


# full list of version estenografica files in disk (from sesiones.txt)
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
"20080226",   "20080228",   "20080304",   "20080306",   "20080311",   "20080313",   "20080325",   "20080326",   "20080327",   "20080401",
"20080403",   "20080408",   "20080410",   "20080415",   "20080417",   "20080422",   "20080424",   "20080429",   "20080430",   "20080828",
"20080901",   "20080902",   "20080909",   "20080911",   "20080917",   "20080918",   "20080923",   "20080925",   "20080930",   "20081002",
"20081007",   "20081009",   "20081014",   "20081015",   "20081016",   "20081021",   "20081023",   "20081028",   "20081030",   "20081104",
"20081106",   "20081111",   "20081118",   "20081119",   "20081125",   "20081127",   "20081202",   "20081204",   "20081209",   "20081211",
"20090201",   "20090201-I", "20090204",   "20090205",   "20090210",   "20090212",   "20090217",   "20090219",   "20090224",   "20090226",
"20090303",   "20090305",   "20090310",   "20090312",   "20090318",   "20090319",   "20090324",   "20090324-I", "20090326",   "20090331",
"20090401",   "20090402",   "20090414",   "20090415",   "20090416",   "20090421",   "20090423",   "20090423-I", "20090428",   "20090430")
#
ves60 <- ves60[-which(ves60=="20061124")] # versión estenografica in web is from 20051124! drop it --- possible to locate real one?
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
"20130926",   "20131001",   "20131002",   "20131003",   "20131008",   "20131009-1", "20131009-2", "20131010",   "20131015",   "20131016",
"20131017-1", "20131017-2",
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
"Ve12mar2020",   "Ve18mar2020",   "Ve19mar2020-JP",   "Ve19mar2020",  "Ve30jun2020")

# check that all files are indeed in dir
setdiff(dir("../estenograficas/"),
        paste(c(ves60, ves62, ves64), ".html", sep = ""))
# how many files? 
all.ves <- data.frame(ves = c(ves60, ves62, ves64), stringsAsFactors = FALSE)
all.ves$leg <- c(rep(60, length(ves60)),
                 rep(62, length(ves62)),
                 rep(64, length(ves64)))

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
#
## # Figure this out, looks useful for encoding issues
## # https://stackoverflow.com/questions/39148759/remove-accents-from-a-dataframe-column-in-r
## library(stringi)
## terme  <- c("Millésime", 
##             "boulangère", 
##             "üéâäàåçêëèïîì")
## stri_trans_general(str = terme, id = "Latin-ASCII")



########################################
## LOOP OVER all.legs WILL START HERE ##
########################################
#leg <- 60 # pick one
#leg <- 62 # pick one
leg <- 64 # pick one
sel <- which(all.ves$leg==leg)
ves <- all.ves$ves[sel]; 
length(ves) # debug
# read one file
## tmp <- readLines(con = "Ve18mar2020.html", encoding = "latin1")

######################################################
## READ ALL VERSION ESTENOGRAFICAS TO JOIN SPEECHES ##
######################################################
# will receive joined speech for manipulation
speeches <- data.frame()
for (f in 1:length(ves)){
#for (f in 1:50){
    #f <- 113 # debug
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
        ses <- sub.e("(?:, )?(?:celebrada el|del) (?:lunes|martes|miércoles|jueves|viernes|sábado|domingo).+$", "", ses)
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
        text  <- tmp # if no bookmarks (as in 2009) then take all readLines
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
    # diputado presidente inverted
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) diputad[oa] president[ea]", text)
    text[sel] <- sub.e("(diputad[oa]) (president[ea])", "//2 //1", text[sel])
    # presiding officer's lines
    sel <- grep.e(pattern = "<[pb]( style.+)?>(?:<a.+/a>)?<[bp]>(?:El|La) president[ea],? diputad[oa]", text) 
#    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) president[ea],? diputad[oa]", text) 
    speech$role[sel] <- "pres"
    speech$who[sel] <- sub.e(pattern = "^.*president[ea],? diputad[oa]s? ([-a-záéíóúüñ. ]+)[ QM:;]*</b>.*$", replacement = "\\1", text[sel])
    #
    # diputado secretario inverted
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) diputad[oa] secretari[oa]", text)
    text[sel] <- sub.e("(diputad[oa]) (secretari[oa])", "//2 //1", text[sel])
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
    # assume that all rows in between belong to speaker above
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

# drop some empty lines
sel <- grep("^</I>$", speeches$text)
if (length(sel) > 0) speeches <- speeches[-sel,]

######################################################################
## NEXT LINES IDENTIFY OATHS TO CHECK AGAINST IN/OUT DATES IN dips  ##
######################################################################
## # Search new diputados, one by one...
## i <- i+1; print(i)
## sel <- grep.e("sí,? protesto", speeches$text)
## sel <- sel[i]; speeches$fch[sel] # check one by one
## #speeches[sel,]
## #sel <- c((sel-5),(sel-4),(sel-3),(sel-2),(sel-1),sel,(sel+1),(sel+2)); speeches$text[sel]
## sel <- c((sel-3),(sel-2),(sel-1),sel,(sel+1),(sel+2)); speeches$text[sel]
## x

###########################
## FIX NAME MISSPELLINGS ##
###########################
## tmp.speeches <- speeches # duplicate when debugging fix-names
## # speeches <- tmp.speeches # restore when debugging
#
# CAPITALIZE speeches$who
speeches$who <- cap.emm(speeches$who)
source("../code/fix-names.r")
# tabulate names to debug mispellings
#table(speeches$who)

# debug
sel <- which(nchar(speeches$who)==max(nchar(speeches$who), na.rm = TRUE)) # report longest string -- should be a long name
speeches$who[sel[1]]
#
# list hits debug
## unique(speeches$who)[order(unique(speeches$who))]

# CLEAN
# drop un-identified speakers
sel <- which(is.na(speeches$who))
#speeches$text[sel]
if (length(sel) > 0) speeches <- speeches[-sel,]
#
sel <- grep.e("varios.protestan", speeches$who)
#speeches$text[sel]
if (length(sel) > 0) speeches <- speeches[-sel,]
#
# drop senadores and cabinet members
sel <- which(speeches$who=="SENADOR" | speeches$who=="EXEC")
#speeches$text[sel]
if (length(sel) > 0) speeches <- speeches[-sel,]
#
# drop missing names in versión estenográfica
sel <- grep.e("(?:no.registrado|not-a-diputado|ambiguous-name)", speeches$who)
#speeches$text[sel]
if (length(sel) > 0) speeches <- speeches[-sel,]
#
# add periodo indicator function to data frame
add.periodo <- function(x){
    x$periodo <- NA # open slot in data.frame
    sel <- which(x$date >= ymd("20060825") & x$date < ymd("20070101")) ; x$periodo[sel] <- "60y1-1"
    sel <- which(x$date >= ymd("20070201") & x$date < ymd("20070501")) ; x$periodo[sel] <- "60y1-2"
    sel <- which(x$date >= ymd("20070501") & x$date < ymd("20070825")) ; x$periodo[sel] <- "60y1-extra"
    sel <- which(x$date >= ymd("20070825") & x$date < ymd("20080101")) ; x$periodo[sel] <- "60y2-1"
    sel <- which(x$date >= ymd("20080201") & x$date < ymd("20080501")) ; x$periodo[sel] <- "60y2-2"
    sel <- which(x$date >= ymd("20080501") & x$date < ymd("20080825")) ; x$periodo[sel] <- "60y2-extra"
    sel <- which(x$date >= ymd("20080825") & x$date < ymd("20090101")) ; x$periodo[sel] <- "60y3-1"
    sel <- which(x$date >= ymd("20090201") & x$date < ymd("20090501")) ; x$periodo[sel] <- "60y3-2"
    sel <- which(x$date >= ymd("20090501") & x$date < ymd("20090825")) ; x$periodo[sel] <- "60y3-extra"
                                        #
    sel <- which(x$date >= ymd("20120825") & x$date < ymd("20130101")) ; x$periodo[sel] <- "62y1-1"
    sel <- which(x$date >= ymd("20130201") & x$date < ymd("20130501")) ; x$periodo[sel] <- "62y1-2"
    sel <- which(x$date >= ymd("20130501") & x$date < ymd("20130825")) ; x$periodo[sel] <- "62y1-extra"
    sel <- which(x$date >= ymd("20130825") & x$date < ymd("20140101")) ; x$periodo[sel] <- "62y2-1"
    sel <- which(x$date >= ymd("20140201") & x$date < ymd("20140501")) ; x$periodo[sel] <- "62y2-2"
    sel <- which(x$date >= ymd("20140501") & x$date < ymd("20140825")) ; x$periodo[sel] <- "62y2-extra"
    sel <- which(x$date >= ymd("20140825") & x$date < ymd("20150101")) ; x$periodo[sel] <- "62y3-1"
    sel <- which(x$date >= ymd("20150201") & x$date < ymd("20150501")) ; x$periodo[sel] <- "62y3-2"
    sel <- which(x$date >= ymd("20150501") & x$date < ymd("20150825")) ; x$periodo[sel] <- "62y3-extra"
                                        #
    sel <- which(x$date >= ymd("20180825") & x$date < ymd("20190101")) ; x$periodo[sel] <- "64y1-1"
    sel <- which(x$date >= ymd("20190101") & x$date < ymd("20190131")) ; x$periodo[sel] <- "64y1-extra"
    sel <- which(x$date >= ymd("20190201") & x$date < ymd("20190501")) ; x$periodo[sel] <- "64y1-2"
    sel <- which(x$date >= ymd("20190501") & x$date < ymd("20190825")) ; x$periodo[sel] <- "64y1-extra"
    sel <- which(x$date >= ymd("20190825") & x$date < ymd("20200101")) ; x$periodo[sel] <- "64y2-1"
    sel <- which(x$date >= ymd("20200201") & x$date < ymd("20200501")) ; x$periodo[sel] <- "64y2-2"
    sel <- which(x$date >= ymd("20200501") & x$date < ymd("20200825")) ; x$periodo[sel] <- "64y2-extra"
    sel <- which(x$date >= ymd("20200825") & x$date < ymd("20210101")) ; x$periodo[sel] <- "64y3-1"
    sel <- which(x$date >= ymd("20210201") & x$date < ymd("20210501")) ; x$periodo[sel] <- "64y3-2"
    sel <- which(x$date >= ymd("20210501") & x$date < ymd("20210825")) ; x$periodo[sel] <- "64y3-extra"
    return(x)
}
#
# posix date
tmp <- speeches$file
# drop suffixes, if any
tmp <- gsub.e("[-].+$", "", tmp)
# rename ve files with proper date format
sel <- grep.e("ve", tmp)
if (length(sel) > 0){
    tmp[sel] <- sub.e("^ve", "", tmp[sel])
    tmp[sel] <- gsub.e("^([0-9]+)([a-z]+)([0-9]+)", "\\3\\2\\1", tmp[sel])
    tmp[sel] <- sub.e("ene", "01", tmp[sel])
    tmp[sel] <- sub.e("feb", "02", tmp[sel])
    tmp[sel] <- sub.e("mar", "03", tmp[sel])
    tmp[sel] <- sub.e("abr", "04", tmp[sel])
    tmp[sel] <- sub.e("may", "05", tmp[sel])
    tmp[sel] <- sub.e("jun", "06", tmp[sel])
    tmp[sel] <- sub.e("jul", "07", tmp[sel])
    tmp[sel] <- sub.e("ago", "08", tmp[sel])
    tmp[sel] <- sub.e("sep", "09", tmp[sel])
    tmp[sel] <- sub.e("oct", "10", tmp[sel])
    tmp[sel] <- sub.e("nov", "11", tmp[sel])
    tmp[sel] <- sub.e("dic", "12", tmp[sel])
}
#tmp[sel]
#
# date
tmp <- ymd(tmp)
speeches$date <- tmp # return to data
# add periodo
speeches <- add.periodo(speeches)

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
## head(text)
## sel <- grep.e("<", text)
## text[sel]
## write.csv(text, file = "tmp.csv")
## text[221]
speeches$text.only <- text # return manipulation to data
#rm(text)

# drop roll calls
## sel <- grep.e("[:](?:</B>)?[ ]*(?:(?:A|En) (?:favor|pro|contra)|por la (?:afirmativa|negativa)|abstención|me abstengo)[. ]*</p>$", speeches$text)
## speeches <- speeches[-sel,]
## sel <- grep.e("[:](?:</B>)? (?:.+), (?:(?:a|en) (?:favor|pro|contra)|por la (?:afirmativa|negativa)|abstención|me abstengo)[.]</p>$", speeches$text) # con nombre intercalado
## #speeches[sel[10],]0
## speeches <- speeches[-sel,]
## #sel <- grep.e("A favor.", speeches$text) # debug
## #speeches$text[sel]

# count words --- raw count that will be filtered below 
speeches$nword <- lengths(gregexpr(pattern = "\\W+", speeches$text.only)) + 1
#speeches$nchar <- nchar(speeches$text.only) # another metric ignored
speeches[3,]

## # debug: never spoke
## sel <- grep.e("noriega galaz", speeches$who)
## speeches$text.only[sel]
## speeches[sel[1],]
## x

#################################################################
##  * * *  START HERE IF ADDING MORE DIPUTADO DATA  * * *      ##
##           SAVE/LOAD MANIPULATED SPEECH OBJECTS              ##
## RE-RUN CODE IF SPEECH DATA MODIFIED OR ERRORS SPOTTED/FIXED ##
#################################################################
if (leg==60) save.image(file = "../data/manipulated-speeches-60.RData")
if (leg==62) save.image(file = "../data/manipulated-speeches-62.RData")
if (leg==64) save.image(file = "../data/manipulated-speeches-64.RData")

rm(list = ls())
leg <- 64
if (leg==60) load(file = "../data/manipulated-speeches-60.RData")
if (leg==62) load(file = "../data/manipulated-speeches-62.RData")
if (leg==64) load(file = "../data/manipulated-speeches-64.RData")

#############################
## READ DIPUTADO CSV FILES ##
#############################
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
#
## ################################################
## ## merge all dip names into single data frame ##
## ## export it, edit in excel, then re-import   ##
## ## to re-do/export dip.csv clean files        ##
## ################################################
## tmp <- data.frame()
## for (l in 1:length(all.legs)){
##     #l <- 2
##     tmp <- rbind(tmp, all.dips[[l]])
## }
## tmp$cabecera <- cap.emm(tmp$cabecera)
## tmp$pila <- gsub.e("  +", " ", tmp$pila) # remove double spaces
## tmp$pila <- gsub.e("^ +", "", tmp$pila) # remove heading spaces
## tmp$pila <- gsub.e(" +$", "", tmp$pila) # remove trailing spaces
## tmp$nom <- gsub.e("  +", " ", tmp$nom) # remove double spaces
## tmp$nom <- gsub.e("^ +", "", tmp$nom) # remove heading spaces
## tmp$nom <- gsub.e(" +$", "", tmp$nom) # remove trailing spaces
## # fill NAs in birth/gen with non-NAs
## tmp2 <- as.factor(tmp$nom)
## tmp <- split(tmp, tmp2)
## tmp[[sel]]
## #sel  <- grep.e("lilia luna munguia", names(tmp))
## for (i in 1:length(tmp)){
##     x <- tmp[[i]]
##     if (length(which(is.na(x$birth)))>0) x$birth <- rep(x$birth[which(is.na(x$birth)==FALSE)[1]], length(x$birth))
##     if (length(which(is.na(x$gen)))>0)   x$gen <-   rep(x$gen[which(is.na(x$gen)==FALSE)[1]], length(x$gen))
##     tmp[[i]] <- x
##     }
## tmp <- unsplit(tmp, tmp2)
## write.csv(tmp, file = "../data/tmp.csv", row.names = FALSE)
## 
## # re-import after manipulation
## tmp <- read.csv(file = "../data/tmp.csv", stringsAsFactors = FALSE)
## head(tmp)
## for (l in 1:length(all.legs)){
##     sel <- which(tmp$leg==all.legs[l])
##     write.csv(tmp[sel,], file = paste("../data/tmp", all.legs[l], ".csv", sep = ""), row.names = FALSE)
## }
#
dips <- all.dips[[grep(leg, names(all.dips))]] # pick one legislatura's dips
nrow(dips) # debug

###################################################
## figure out intervals when diputado was active ##
###################################################
## dips$in1  <- ymd(dips$ yrin1*10000 + dips$ moin1*100 + dips$ dyin1)
## dips$out1 <- ymd(dips$yrout1*10000 + dips$moout1*100 + dips$dyout1)
## dips$in2  <- ymd(dips$ yrin2*10000 + dips$ moin2*100 + dips$ dyin2)
## dips$out2 <- ymd(dips$yrout2*10000 + dips$moout2*100 + dips$dyout2)
## dips$in3  <- ymd(dips$ yrin3*10000 + dips$ moin3*100 + dips$ dyin3)
## # clean
## dips$yrin1 <- dips$moin1 <- dips$dyin1 <- dips$yrout1 <- dips$moout1 <- dips$dyout1 <- dips$yrin2<- dips$moin2 <- dips$dyin2 <- dips$yrout2 <- dips$moout2 <- dips$dyout2 <- dips$yrin3<- dips$moin3 <- dips$dyin3 <- NULL
# fix these by hand, if any, in dip.csv file
sel <- which(is.na(dips$in2)==FALSE & is.na(dips$out1)==TRUE)
sel
sel <- which(is.na(dips$in3)==FALSE & is.na(dips$out2)==TRUE)
sel
# fill exit dates
sel <- which(is.na(dips$in2)==FALSE & is.na(dips$out2)==TRUE)
if (leg==60     & length(sel)>0) dips$out2[sel] <- ymd("20090827")
if (leg==  62   & length(sel)>0) dips$out2[sel] <- ymd(  "20150827")
if (leg==    64 & length(sel)>0) dips$out2[sel] <- ymd(    "20210827")
sel <- which(is.na(dips$in1)==FALSE & is.na(dips$in2)==TRUE & is.na(dips$out1)==TRUE)
if (leg==60     & length(sel)>0) dips$out1[sel] <- ymd("20090827")
if (leg==  62   & length(sel)>0) dips$out1[sel] <- ymd(  "20150827")
if (leg==    64 & length(sel)>0) dips$out1[sel] <- ymd(    "20210827")
# intervals
dips$int1 <- interval(dips$in1,dips$out1)
dips$int2 <- interval(dips$in2,dips$out2)
#dips$int3 <- interval(dips$in3,dips$out3)
#dips[60,] # debug
all.dips[[grep(leg, names(all.dips))]]  <- dips # return manip dips to data
#
# all dates when assembly convened
all.ses <- speeches[,c("date","leg")]
#tail(all.ses)
all.ses <- all.ses[duplicated(all.ses$date)==FALSE,]
all.ses <- all.ses[order(all.ses$date),] # sort
# add periodo
all.ses <- add.periodo(all.ses)
#
# list will receive vector with session dates when diputado was potential speaker --- EXPOSURE
pot  <- lapply(X = dips$int1, FUN = function(X) X) # list of intervals
pot2 <- lapply(X = dips$int2, FUN = function(X) X) # list of 2nd intervals
sel.dips  <- which(is.na(dips$int1)==FALSE)             # selection to manipulate
sel2      <- which(is.na(dips$int2)==FALSE)             # subselection to manipulate
# turn in/out into all its days, then intersect with all session dates
#
#pot[[599]] # debug 60th
#
pot[sel.dips] <- lapply(pot [sel.dips], FUN = function(x) as.Date( seq(int_start(x), int_end(x), by = "1 day")))
pot2[sel2]    <- lapply(pot2[sel2],     FUN = function(x) as.Date( seq(int_start(x), int_end(x), by = "1 day")))
# merge cases with two intervals
pot[sel2] <- Map(na.omit(c), pot[sel2], pot2[sel2])
# intersect with sessions to get list of sessions where dips was potential speaker
pot[sel.dips] <- lapply(pot[sel.dips], FUN = function(x) all.ses$date[all.ses$date %in% x])
#
# change potential dates vector with dataframe also reporting period (for subsetting)  
pot[sel.dips] <- lapply(pot[sel.dips], FUN = function(x){
    sel <- which(all.ses$date %in% x)
    x <- data.frame(date = x) # change into data frame format
    x$periodo <- all.ses$periodo[sel]
    return(x)})

################################################################################################
## function to aggregate and count potential dates given aggregation criterion for negbin reg ##
################################################################################################
agg.emm <- function(x, agg = "periodo"){
    #x = pot[[999]] # debug
    if          (agg=="leg"){ x$sel.agg <- sub.e("^([0-9]+)y.+$", "\\1", x$periodo)        # select full legislatura
    } else if (agg=="year") { x$sel.agg <- sub.e("^[0-9]+(y[1-3]).+$", "\\1", x$periodo)   # select by years
    } else if (agg=="month"){ x$sel.agg <- year(x$date)*100 + month(x$date)                # select by month
    } else if (agg=="day")  { x$sel.agg <- year(x$date)*10000+month(x$date)*100+day(x$date)# select by days
    } else {                  x$sel.agg <- x$periodo                                       # default: by periodo    
    }
    x$pot.dys <- as.numeric( ave(x$sel.agg, as.factor(x$sel.agg), FUN=length, na.rm=TRUE) )
    x <- x[duplicated(x$sel.agg)==FALSE, ] # drop redundant rows
    x$agg <- rep(agg, nrow(x))             # indicate aggregation unit
    x <- x[, c("sel.agg","pot.dys","agg")] # keep columns on interest
    return(x)
}


###################################################################
#              AGGREDATE BY DAYS FIRST IN ORDER TO                #
#                DROP NON-SPEECHES OF nword < 50                  #
# Note: I only consider days' aggregates, so someone with eg. ten #
# 5-word interventions counted as having made a speech that day   #
###################################################################
# create a list with subsets of each speaker's lines 
speeches$ord <- 1:nrow(speeches)
tmp.list <- split(speeches, speeches$who)
# produce word counts for each speaker, agg all leg, by month, by session...
#
# aggregate unit's words
tmp.list <- lapply(tmp.list, FUN = function(x){
    #x <- tmp.list[[1]] # debug
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
tmp.list <- lapply(tmp.list, FUN = function(x) cbind(x, id = NA))
tmp.list <- lapply(tmp.list, FUN = function(x) cbind(x, i = NA))
#
# Indicator of days to make zero for each diputado
tmp.list <- lapply(tmp.list, function(x){
    dnonspeech <- ifelse(x$nword < 50, 1, 0)
    x <- cbind(x, dnonspeech=dnonspeech)
    return(x)
    })
#
#tmp.list[[1]][1:3,] # debug
#
# unlist agg data into dataframe, merge with other legs
tmp <- do.call(rbind, tmp.list)
tmp <- tmp[order(tmp$ord),]
rownames(tmp) <- NULL
#
# return to data
speeches$nword.day  <- tmp$nword2
speeches$dnonspeech <- tmp$dnonspeech
rm(tmp,tmp.list) # clean
#
############################
## DROP NON-SPEECHES HERE ##
############################
sel <- which(speeches$dnonspeech==1 & speeches$role=="diputado")
## speeches[sel[132],]
## x
#
message(paste("  **********************************************************\n  ** Non-speeches of less than 50 words to be dropped:", length(sel), "\n  **********************************************************"))
#
if (length(sel)>0) speeches <- speeches[-sel,]
rm(sel)
speeches$dnonspeech <- NULL # clean
#
#speeches[1:5,c("date","role","who","nword.day")]
#x

#########################################################
##          AGGREGATE WORDS BY PERIODO                 ##
## * *  SELECT AGGREGATION UNIT JUST BELOW  * *        ##
#########################################################
#
################################################################
## DAYS DIP WAS POTENTIAL SPEAKER GIVEN TENURE (FOR EXPOSURE) ##
################################################################
tmp.agg <- vector("list", nrow(dips)) # will receive all data
tmp.agg[] <- 0 # fill with zero days
#
######################################################
## apply aggregation function to potential speakers ##
##     * *  SELECT AGGREGATION UNIT HERE  * *       ##
######################################################
tmp2 <- lapply(pot[sel.dips], FUN = function(x) agg.emm(x, agg = "periodo")) # n potential dates
# add agg unit length to x (denom for rel exposure)
tmp3 <- agg.emm(all.ses, agg = "periodo") # unit lengths
tmp3$all.dys <- as.numeric(tmp3$pot.dys) # rename
# merge to x, compute rel exposure
tmp2 <- lapply(tmp2, FUN = function(x) merge(x, tmp3[,c("sel.agg","all.dys")], by = "sel.agg", all.x = TRUE, all.y = FALSE))
tmp2 <- lapply(tmp2, FUN = function(x) cbind(x, pot.sh = (x$pot.dys / x$all.dys), stringsAsFactors = FALSE))
# return to data
tmp.agg[sel.dips] <- tmp2
names(tmp.agg) <- dips$id
## # debug
## dips[32,]
## pot[32]
## tmp.agg[[32]]
## sel <- grep.e("aguayo perez", speeches$nom)
## x
#
#############################
## add controls to tmp.agg ##
#############################
for (i in sel.dips){
    #which(dips$id %in% names(tmp.agg)[1])
    #i <- 60 # debug
    sel.col <- which(colnames(dips) %in% c("pila","patmat","in1","out1","in2","out2","in3","int1","int2")) # drop these
    tmp2 <- do.call("rbind", replicate(nrow(tmp.agg[[i]]), dips[i,-sel.col], simplify = FALSE)) # multiply dip info n units
    tmp.agg[[i]] <- cbind(tmp.agg[[i]], tmp2)
}
# tmp.agg[[601]]


################################################
##          Aggregate unit's words            ##
##  * *  SELECT AGGREGATION UNIT BELOW  * *   ##
################################################
# add diputado's words in unit (DV) here to tmp.agg
# create a list with subsets of each speaker's lines 
speech.list <- split(speeches, speeches$who)
# produce word counts for each speaker, agg all leg, by month, by session...
#
speech.list <- lapply(speech.list, FUN = function(x){
    #x <- speech.list[[1]] # debug
    nword2 <- ave(x$nword,                                   
#                  as.factor(x$date),                          # agg by day
#                  as.factor(year(x$date)*100+month(x$date)),  # agg by month
                  as.factor(x$periodo),                       # agg by periodo 1,2,extras
#                  as.factor(x$leg),                           # agg by legislatura
                  FUN=sum, na.rm=TRUE);
    x <- cbind(x, nword2)
    x <- x[duplicated(x$periodo)==FALSE,]           # drop redundant lines
    x <- x[,c("who","periodo","nword2")]            # keep cols of interest
#    x <- x[,c("who","leg","nword")]                 # keep cols of interest
    colnames(x) <- c("who","sel.agg","nword")       # rename cols for merging
    return(x)
})
# add empty column to receive id and dip index
## speech.list <- lapply(speech.list, FUN = function(x) cbind(x, id = NA))
## speech.list <- lapply(speech.list, FUN = function(x) cbind(x, i = NA))
#
#speech.list[[60]]

# will receive names in ve not in dips for debug
d.no.hits <- vector()

## # debug to locate speech dates
## sel <- grep.e("CERRILLO GARNICA", speeches$who)
## table(speeches$date[sel])
## x
## #debug
## speech.list[[232]]
## sel <- grep("cps08p", names(speech.list))
## x

# search each speaker's lines in dips, paste in tmp.agg
for (i in 1:length(speech.list)){
    #i <- 232 # debug
    tmp <- speech.list[[i]] # select one data frame for manipulation
    hit <- which(dips$nom==tmp$who[1]) # matches a name in dips
    if (length(hit)==0){ # no match
        #d.no.hits[i] <- tmp$id[(is.na(tmp$id)==FALSE)[1]];
        d.no.hits <- c(d.no.hits, tmp$who) # report no hit's id
        next
    }
    tmp2 <- tmp.agg[[hit]] # select target data frame for manipulation
    #colnames(tmp2) # debug
    tmp2 <- merge(x = tmp2, y = tmp[,c("sel.agg","nword")], by = "sel.agg", all = TRUE)
    tmp2$nword[is.na(tmp2$nword)] <- 0 # fill silent units with zeroes
    tmp.agg[[hit]] <- tmp2 # return manipulation to data
    ## # fill info for debugging (empty ids and i means name not found in dips)
    ## tmp$id <- rep(dips$id[hit], nrow(tmp))
    ## tmp$i  <- rep(hit,          nrow(tmp))
}

## ##debug
## sel <- grep.e("torres ramirez", speeches$who)
## table(speeches$date[sel])
## table(speeches$periodo[sel])
#
## tmp.agg[[601]]
## tmp.agg[["zac03p"]]
## #
## tmp <- vector()
## for (i in sel.dips){
##     tmp <- c(tmp, tmp.agg[[i]]$leg)
## }
## tmp <- data.frame()
## for (i in sel.dips){
##     i <- 28
##     tmp <- rbind(tmp, tmp.agg[[i]][,c("leg","sel.agg")])
## }
## tmp



####################################################
## debug names in ve not in dips --- fix spelling ##
####################################################
if (length(d.no.hits)==0){
message(" ********************************************\n ** Checked: all speaker names in versión  **\n **  estenográfica have a match in dips    **\n ******************************************** ")
    rm(d.no.hits) # when empty, drop it
} else {print(d.no.hits)}


#######################################################
## check in/out mismatches with speeches one-by-one  ##
#######################################################
tmp <- data.frame(dip = dips$id)
tmp$dhasnas <- 0 # set dummy to zero default
for (i in sel.dips){
    #j <- j+1; i <- sel.dips[j]; print(j) #debug
    tmp1 <- tmp.agg[[i]] # extract dataframe
    tmp2 <- ifelse(length(which(is.na(tmp1$nom))) > 0, 1, 0)
    tmp$dhasnas[i] <- tmp2 # return dummy
}
which(tmp$dhasnas==1) # mismatches have NAs in other variables

##############################################################################
## Add nword column that is missing from diputados who did not utter a word ##
##############################################################################
tmp <- tmp.agg
for (i in 1:length(tmp)){
    #i <- 1 # debug
    tmp2 <- grep("nword", colnames(tmp[[i]]))
    if (is.data.frame(tmp[[i]])==TRUE & length(tmp2)==0) tmp[[i]]$nword <- rep(0, nrow(tmp[[i]]))
}
tmp.agg <- tmp

# unlist agg data into dataframe, merge with other legs
tmp.df <- do.call(rbind, tmp.agg[sel.dips])
## tmp.df[which(tmp.df$id=="zac03p"),]
## tmp.agg[["zac03p"]]
## dips[sel.dips,"id"]
## x
#
#########################
## ADD DV AND EXPOSURE ##
#########################
tmp.df$dv.nword <- tmp.df$nword;                    # DV raw
tmp.df$ev.pot.dys <- tmp.df$pot.dys # exposure: number days dip was not on leave in unit 
tmp.df$ev.all.dys <- tmp.df$all.dys # exposure: number days dips met in unit (period)
tmp.df$ev.pot.sh <- tmp.df$pot.sh   # exposure: share of all days dip was not on leave in unit 
#tmp.df$dv.nword.sh <- tmp.df$nword / (tmp.df$pot.sh*100) # DV for OLS using pct
tmp.df$dv.nword.sh <- tmp.df$nword / tmp.df$pot.sh # DV for OLS
tmp.df$nword <- tmp.df$pot.dys <- tmp.df$all.dys <- tmp.df$pot.sh <- NULL # clean

## COVARIATES
################
## dip gender ##
################
tmp.df$dfem = as.numeric(tmp.df$gen=="F")
tmp.df$gen <- NULL # clean
###################################
## party size (move it to right) ##
###################################
tmp.df$tmp  <- tmp.df$ptysh; tmp.df$ptysh <- NULL; tmp.df$ptysh <- tmp.df$tmp; tmp.df$tmp <- NULL
##########################
## seniority = dpastleg ##
##########################
#table(tmp.df$repite)
tmp.df$dpastleg <- 0
tmp <- rep(0, length(tmp.df$repite)) # will identify manipulation subset
tmp[grep.e("5[0-9][-]", tmp.df$repite)] <- 1 # was dip in fifty-something
tmp.df$dpastleg[tmp==1] <- 1
tmp[grep.e("6[01][-]", tmp.df$repite)] <- 1 # or was dip in 60 or 61
tmp.df$dpastleg[tmp.df$leg==62 & tmp==1] <- 1 
tmp.df$dpastleg[tmp.df$leg==64 & tmp==1] <- 1 
tmp[grep.e("6[23][-]", tmp.df$repite)] <- 1 # or was dip in 62 or 63
tmp.df$dpastleg[tmp.df$leg==64 & tmp==1] <- 1
#xtabs(~ repite+dpastleg+leg, data=tmp.df) #debug
######################################
## Age at start of legislative term ##
######################################
tmp.df$age <- NA
tmp.df$age[tmp.df$leg==60] <- 2006 - tmp.df$birth[tmp.df$leg==60]
tmp.df$age[tmp.df$leg==62] <- 2012 - tmp.df$birth[tmp.df$leg==62]
tmp.df$age[tmp.df$leg==64] <- 2018 - tmp.df$birth[tmp.df$leg==64]
#############################################
## party dummies instead of party families ##
#############################################
table(tmp.df$part)
tmp.df$dpan <- as.numeric(tmp.df$part=="pan")
tmp.df$dpri <- as.numeric(tmp.df$part=="pri")
tmp.df$dmorena <- as.numeric(tmp.df$part=="morena")
tmp.df$dleft <- as.numeric(tmp.df$part=="prd")
tmp.df$doport <- as.numeric(tmp.df$part=="pvem" | tmp.df$part=="pt" | tmp.df$part=="panal" | tmp.df$part=="pes" | tmp.df$part=="mc" | tmp.df$part=="conve" | tmp.df$part=="asd" | tmp.df$part=="sp")
#######################################################################
## president's party or coalition instead of government party member ##
#######################################################################
tmp.df$dwithpres <- 0
tmp <- rep(0, length(tmp.df$dwithpres)) # will identify manipulation subset
tmp <- as.numeric(tmp.df$part=="pan"    & tmp.df$leg==60) # panistas with Calderon
tmp.df$dwithpres[tmp] <- 1
tmp <- as.numeric(tmp.df$part=="pri"    & tmp.df$leg==62) # or priistas with Peña
tmp.df$dwithpres[tmp] <- 1
tmp <- as.numeric(tmp.df$part=="pvem"   & tmp.df$leg==62) # or pvem with Peña --- had one governor
tmp.df$dwithpres[tmp] <- 1
## tmp <- as.numeric(tmp.df$part=="panal"  & tmp.df$leg==62) # or panal with Peña
## tmp.df$dwithpres[tmp] <- 1
tmp <- as.numeric(tmp.df$part=="morena" & tmp.df$leg==64) # or morena with amlo
tmp.df$dwithpres[tmp] <- 1
## tmp <- as.numeric(tmp.df$part=="pt"     & tmp.df$leg==64) # or pt with amlo
## tmp.df$dwithpres[tmp] <- 1
## tmp <- as.numeric(tmp.df$part=="pes"    & tmp.df$leg==64) # or pes with amlo
## tmp.df$dwithpres[tmp] <- 1
## tmp <- as.numeric(tmp.df$part=="pvem"   & tmp.df$leg==64) # or pvem with amlo
## tmp.df$dwithpres[tmp] <- 1
#####################
## committee chair ##
#####################
#table(tmp.df$dchair)
tmp.df$dchair <- 1 - as.numeric(tmp.df$prescom=="")
##################
## party leader ##
##################
table(tmp.df$lider)
tmp.df$dleader <- 0
tmp.df$dleader[grep("coor", tmp.df$lider)] <- 1
##############
## controls ##
##############
#########
## smd ##
#########
tmp.df$tmp <- tmp.df$dsmd; tmp.df$dsmd <- NULL; tmp.df$dsmd <- tmp.df$tmp; tmp.df$tmp <- NULL
#######################
## presiding officer ##
#######################
## tmp <- speeches[-grep.e("diputad", speeches$role),]
## table(tmp$who, tmp$role)
## table(tmp$who, tmp$periodo)
tmp.df$dpresoff <- 0
# presidentes y secretarios de la cámara leg 64 up to end 2nd year ordinaria
if (leg==60){
                                        #                 60y1-1 60y1-2 60y2-1 60y2-2 60y3-1  60y3-2
                                        #60y1
    sel <- which(tmp.df$nom %in%
                 c("CARLOS ARMANDO BIEBRICH TORRES",    #     49      0      0      0      0       0
                   "HECTOR HUGO OLIVARES VENTURA",      #     45      0      0      0      0       0
                   "JESUS CUAUHTEMOC VELASCO OLIVA",    #    476    155      1      0      0       0
                   "JORGE ZERMEÑO INFANTE",             #   5225   2474      0      0      0       0
                   "JOSE GILDARDO GUERRERO TORRES",     #    385    457      0      0      0       0
                   "EDUARDO SERGIO DE LA TORRE JARAMILLO"))#    514    384      3      0      0       0
    sel2 <- grep("60y1", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
                                        #60y1 60y2
    sel <- which(tmp.df$nom %in%
                 c("LILIA GUADALUPE MERODIO REZA",      #    231    233     49      0      0       0
                   "MARIA ELENA ALVAREZ BERNAL",        #    860   1044     38      0      0       0
                   "ANTONIO XAVIER LOPEZ ADAME",        #    744    294    503    106      0       0
                   "ARNOLDO OCHOA GONZALEZ",            #    203    242    247     71      0       0
                   "RUTH ZAVALETA SALGADO",             #    558    598   6498   2639     68       0
                   "MARIA MERCEDES MACIEL ORTIZ"))      #    290    366    412    296     19       0
    sel2 <- grep("60y[12]", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
                                        #60y2
    sel <- which(tmp.df$nom %in%
                 c("CRISTIAN CASTAÑO CONTRERAS",        #      0      0    882    711      0       0
                   "OLGA PATRICIA CHOZAS Y CHOZAS",     #      0      0      0    203      0       0
                   "VENANCIO LUIS SANCHEZ JIMENEZ",     #      0      0   1096    641      0       0
                   "PATRICIA VILLANUEVA ABRAJAN",       #      0      0    656    405     42       0
                   "ESMERALDA CARDENAS SANCHEZ",        #      0      0    812    592     28       0
                   "MARIA DEL CARMEN SALVATORI BRONCA"))#      0      0    829    489      9       0
    sel2 <- grep("60y2", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
                                        #60y2 60y3
    sel <- which(tmp.df$nom %in%
                 c("SANTIAGO GUSTAVO PEDRO CORTES"))    #      0      0    273    178    191     342
    sel2 <- grep("60y[23]", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
                                        #60y3
    sel <- which(tmp.df$nom %in%
                 c("MANUEL PORTILLA DIEGUEZ",           #      0      0      0      0    318     193
                   "MARGARITA ARENAS GUZMAN",           #      0      0      0      0    784     813
                   "MARIA DEL CARMEN PINETE VARGAS",    #      0      0      0      0    409     522
                   "CESAR HORACIO DUARTE JAQUEZ",       #      0      0      0      0   3690    2777
                   "JOSE LUIS ESPINOSA PIÑA",           #      0      0      0      0    381     631
                   "JOSE MANUEL DEL RIO VIRGEN",        #      0      0      0      0    322     441
                   "MARTHA HILDA GONZALEZ CALDERON",    #      0      0      0      0   1404     770
                   "ROSA ELIA ROMERO GUZMAN"))          #      0      0      0      0    475     893
    sel2 <- grep("60y3", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
                                        #all
    sel <- which(tmp.df$nom %in%
                 c("MARIA EUGENIA JIMENEZ VALENZUELA",  #    525    305    624    281    401     560
                   "JACINTO GOMEZ PASILLAS"))           #    346    365    415    310    494     505
    tmp.df$dpresoff[sel] <- 1
}
#
if (leg==62){
    sel <- which(tmp.df$nom %in%
                 c("ANGEL CEDILLO HERNANDEZ",            
                   "ALEIDA ALAVEZ RUIZ",                   
                   "FERNANDO BRIBIESCA SAHAGUN",           
                   "FRANCISCO AGUSTIN ARROYO VIEYRA",      
                   "JAVIER OROZCO GOMEZ",                  
                   "JOSE GONZALEZ MORFIN",                 
                   "MAGDALENA DEL SOCORRO NUÑEZ MONREAL",  
                   "MERILYN GOMEZ POZOS",                  
                   "XAVIER AZUARA ZUÑIGA"))
    tmp.df$dpresoff[sel] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("AMALIA DOLORES GARCIA MEDINA",       
                   "ARNOLDO OCHOA GONZALEZ",               
                   "ELOY CANTU SEGOVIA",                   
                   "JESUS MURILLO KARAM"))
    sel2 <- grep("62y1-1", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("PATRICIA ELENA RETAMOZA VEGA",       
                   "TANYA RELLSTAB CARRETO"))
    sel2 <- grep("62y1", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("ANGELINA CARREÑO MIJARES",           
                   "MARCELO DE JESUS TORRES COFIÑO",       
                   "MARICELA VELAZQUEZ SANCHEZ",           
                   "RICARDO ANAYA CORTES"))
    sel2 <- grep("62y2", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("FRANCISCA ELENA CORRALES CORRALES",  
                   "GRACIELA SALDAÑA FRAIRE",              
                   "JULIO CESAR MORENO RIVERA",            
                   "LAURA BARRERA FORTOUL",                
                   "LIZBETH EUGENIA ROSAS MONTERO",        
                   "LUIS ANTONIO GONZALEZ ROLDAN",         
                   "MARIA BEATRIZ ZAVALA PENICHE",         
                   "MARTHA BEATRIZ CORDOVA BERNAL",        
                   "MARTIN ALONSO HEREDIA LIZARRAGA",      
                   "SERGIO AUGUSTO CHAN LUGO",             
                   "SILVANO AUREOLES CONEJO",              
                   "TOMAS TORRES MERCADO"))
    sel2 <- grep("62y3", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
}                                     
#
if (leg==64){
    sel <- which(tmp.df$nom %in%
                 c("CARMEN JULIETA MACIAS RABAGO",
                   "DULCE MARIA SAURI RIANCHO",
                   "HECTOR RENE CRUZ APARICIO",
                   "JESUS CARLOS VIDAL PENICHE",
                   "KARLA YURITZI ALMAZAN BURGOS",
                   "LIZETH SANCHEZ GARCIA",
                   "MARCO ANTONIO ADAME CASTILLO",
                   "MARIA DE LOS DOLORES PADIERNA LUNA",
                   "MARIA LUCERO SALDAÑA PEREZ",
                   "MARIANA DUNYASKA GARCIA ROJAS",
                   "MARIA SARA ROCHA MEDINA",
                   "MARIO MARTIN DELGADO CARRILLO",
                   "MONICA BAUTISTA RODRIGUEZ",
                   "PORFIRIO ALEJANDRO MUÑOZ LEDO Y LAZO DE LA VEGA"))
    tmp.df$dpresoff[sel] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("LAURA ANGELICA ROJAS HERNANDEZ",
                   "MARIBEL MARTINEZ RUIZ",
                   "LIZBETH MATA LOZANO",
                   "MARIBEL MARTINEZ RUIZ"))
    sel2 <- grep("64y2", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("ANA GABRIELA GUEVARA ESPINOZA",
                   "PABLO GOMEZ ALVAREZ"))
    sel2 <- which(tmp.df$sel.agg=="64y1-1")
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
                                        #
    sel <- which(tmp.df$nom %in%
                 c("LILIA VILLAFUERTE ZAVALA",
                   "LYNDIANA ELIZABETH BUGARIN CORTES"))
    sel2 <- grep("64y1", tmp.df$sel.agg)
    tmp.df$dpresoff[intersect(sel, sel2)] <- 1
}
#table(tmp.df$dpresoff)
## ##########################
## ## progressive ambition ##
## ##########################
## tmp.df$dlicencia <- --- intersect with election year in state

tmp.df[1,]

############################################
## AGREGATE AND SAVE BY MEMBER-DAY OBJECT ##
############################################
tmp <- speeches
tmp$mem.day <- paste(tmp$who, tmp$date)
tmp <- tmp[duplicated(tmp$mem.day)==FALSE,] # keep one obs per member-day
tmp$text <- tmp$text.only <- tmp$nword <- tmp$n <- tmp$f <- tmp$fch <- tmp$file <- tmp$ord <- tmp$mem.day <- NULL
#
if (leg==60){
    data.member.day.60 <- tmp
    save(data.member.day.60, file = "../data/speech-day-60.RData")
}
if (leg==62){
    data.member.day.62 <- tmp
    save(data.member.day.62, file = "../data/speech-day-62.RData")
}
if (leg==64){
    data.member.day.64 <- tmp
    save(data.member.day.64, file = "../data/speech-day-64.RData")
}

###################################
## rename aggregated data object ##
###################################
if (leg==60){
    data.periodo.60 <- tmp.df
}
if (leg==62){
    data.periodo.62 <- tmp.df
}
if (leg==64){
    data.periodo.64 <- tmp.df
}

#########################################
## ADD DV = NUMBER SPEECHES BY PERIODO ##
#########################################
###########
## leg60 ##
###########
# count number of speeches in member-periodo
if (leg==60){
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
}
#
###########
## leg62 ##
###########
# count number of speeches in member-periodo
if (leg==62){
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
}
#
###########
## leg64 ##
###########
# count number of speeches in member-periodo
if (leg==64){
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
}

#################################
## save aggregated data object ##
#################################
if (leg==60) save(data.periodo.60, file = "../data/speech-periodo-60.RData")
if (leg==62) save(data.periodo.62, file = "../data/speech-periodo-62.RData")
if (leg==64) save(data.periodo.64, file = "../data/speech-periodo-64.RData")



###########################
## save diputados object ##
###########################
save(all.dips, file = "../data/all-dips-list.RData")
x

#####################################
## COMMENTS/INSTRUCTIONS BY EDITOR ##
#####################################

DONE 1. drop speeches of less than 50 words

DONE 2. include speeches regardless of their nature in the analysis. In some countries, there are reasons to include only a certain type of speech (e.g., bill debates). We are happy to accommodate chapters where the authors do not use all debates, provided that there is a good justification in the text.

DONE 3. In terms of window of observation/time period under study: we don’t have a particular guideline for this. Please use the window of observation that you believe is more representative of the politics of legislative debate in your country. Ideally we would like each chapter to include several legislative periods, but we are pragmatic here, considering data availability.

EMM: Terminology
- A Legislature (with Roman numerals for reasons I ignore) is an elected chamber for a legislative term, called a Congress in the U.S. Concurrent with presidential elections the chamber of deputies renovates in whole, and again at the presidential mid-term. Diputados remain three years in office and were single term-limited up to 2021. The 2021 mid-term election will be the first since 1932 to allow incumbents on the ballot, a major change in Mexican legislative politics.
- Legislative years break into two "ordinary periods", one covering the months of September through December, inclusive, another February through April, also inclusive. "Extraordinary periods" may be convened during the recess in order to consider a specific bill. Analysis aggregates each member's speeches in the duration of a given period (merging together all extraordinary periods that year, if any). So members in a legislative year like 2012-13 (that had no extraordinary periods) have two word aggregates in the dataset, one for each ordinary period; in a year like 2013-14 (that did), they have three word aggregates in the data. Periods are the units of aggregation in the analysis. 
- A session is a specific date in the calendar when diputados met. During ordinary periods, sessions are usually held on Tuesdays and Thursdays, and may be scheduled in other weekdays if the Jucopo so decides. Diputados met on forty and thirty-one days in the first and second ordinary periods of 2013-14, respectively, and nine days in extraordinary periods, for a yearly total of eighty session days. (A session in North-American legislative parlance is a Mexican period.)

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


