
ls()
summary(speeches)


sel <- grep.e("ieps", speeches$text.only)
speeches[sel, c("date","role","who","n")]



