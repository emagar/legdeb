# -*- coding: utf-8 -*-
##################################
## Script invoked from legdeb.r ##
##################################

# drop escaped quotation marks
tmp <- gsub(pattern = "\"", replacement = "QM", tmp, perl = TRUE)
# drop empty lines
sel <- grep("^ *$", tmp, perl = TRUE)
tmp <- tmp[-sel]
# for some reason 2009 files do not respect encoding, change garbage manually
tmp <- gsub.e(pattern = "&aacute;", replacement = "á", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&eacute;", replacement = "é", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&iacute;", replacement = "í", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&oacute;", replacement = "ó", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&uacute;", replacement = "ú", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&ntilde;", replacement = "ñ", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&uuml;"  , replacement = "ü", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(" " , " ", tmp)
tmp <- gsub.e(pattern = "&Aacute;", replacement = "Á", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Eacute;", replacement = "É", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Iacute;", replacement = "Í", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Oacute;", replacement = "Ó", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Uacute;", replacement = "Ú", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Ntilde;", replacement = "Ñ", x = tmp, ignore.case = FALSE)
tmp <- gsub.e(pattern = "&Uuml;"  , replacement = "Ü", x = tmp)
tmp <- gsub.e(pattern = "&iquest;", replacement = "¿", x = tmp)
tmp <- gsub.e(pattern = "&ndash;" , replacement = "-", x = tmp)
tmp <- gsub.e(pattern = "&mdash;" , replacement = "-", x = tmp)
tmp <- gsub.e(pattern = "\u0085"  , replacement = "" , x = tmp) # drop points de suspention
tmp <- gsub.e(pattern = "\u0097"  , replacement = "-" , x = tmp)
tmp <- gsub.e(pattern = "\u0093"  , replacement = " " , x = tmp)
tmp <- gsub.e(pattern = "\u0094"  , replacement = " " , x = tmp)
tmp <- gsub.e(pattern = "\u0096"  , replacement = " " , x = tmp)
tmp <- gsub.e(pattern = "&quot;"  , replacement = "QM", x = tmp)

tmp <- gsub.e("A</B>p", "</B>Ap", tmp)
tmp <- gsub.e("B</B>u", "</B>Bu", tmp)
tmp <- gsub.e("C</B>o", "</B>Co", tmp)
tmp <- gsub.e("G</B>r", "</B>Gr", tmp)
tmp <- gsub.e("P</B>i", "</B>Pi", tmp)
tmp <- gsub.e("S</B>e", "</B>Se", tmp)
tmp <- gsub.e("S</B>ol", "</B>Sol", tmp)
tmp <- gsub.e("[-]</B>", "</B>", tmp)
tmp <- gsub.e("Se</B> de", "</B>Se de", tmp)
tmp <- gsub.e("Gracias.</B>", "</B>Gracias. ", tmp)
tmp <- gsub.e("Elena &Auml;lvarez", "Elena Alvarez", tmp)
tmp <- gsub.e("La diputadas ", "La diputada ", tmp)
tmp <- gsub.e("<U>P</U>atricio", "Patricio", tmp)
tmp <- gsub.e("&#9;", "", tmp)
tmp <- gsub.e("<I>: </B>", "</B>: <I>", tmp)
tmp <- gsub.e("Á</B>b", "</B>Áb", tmp)
tmp <- gsub.e(":</P>$", "</B>:</P>", tmp)
tmp <- gsub.e("¿</b>"  , "</b>¿", tmp)
tmp <- gsub.e("Le</b>"  , "</b>Le", tmp)
tmp <- gsub.e("[(]</B>"  , "</B>(", tmp)
tmp <- gsub.e(" Aprobado.</B>"  , "</B> Aprobado.", tmp)
tmp <- gsub.e("La Presidenta diputadas"  , "La Presidenta diputada", tmp)
tmp <- gsub.e("<span style='mso-bidi-font-weight:bold'>([-a-záéíóúüñ. ]+:)</span>"  , "\\1", tmp)
tmp <- gsub.e("<span style='font-weight:normal;mso-bidi-font-weight: bold'>" , "", tmp)
tmp <- gsub.e("<span class=QMSpellEQM>" , "", tmp)
tmp <- gsub.e("<span style='mso-tab-count:1'>.+checar contra video[)]<b>" , " no-registrado-en-ve", tmp)
tmp <- gsub.e("<a name=QMdictamediodiscuQM></a>" , "", tmp)
tmp <- gsub.e("<span style='font-weight:normal;mso-bidi-font-weight:bold'>" , "", tmp)
tmp <- gsub.e("<span style='font-weight:normal'>" , "", tmp)
tmp <- gsub.e("<span style='mso-tab-count:1'>" , "", tmp)
tmp <- gsub.e("Muchas</span></b>" , "</b> Muchas", tmp)
tmp <- gsub.e("</B>desde la curul<B>", "desde la curul", tmp)
tmp <- gsub.e("</span></b>" , "</b>", tmp)
tmp <- gsub.e("</b><b>" , "", tmp)
tmp <- gsub.e("</B> <B>", " ", tmp)
tmp <- gsub.e("<i> </i>", " ", tmp)
tmp <- gsub.e(", </b>", "</b>, ", tmp)
tmp <- gsub.e(": </b>", "</b>: ", tmp)
#tmp <- sub.e("([a-záéíóúüñ]) +</b>", "\\1</b>: ", tmp)
tmp <- gsub.e(": +(Gracias(. )?|Acepta|Adelante|Ahora|Aprobado|Concluya|Consulte|Continúe|Damos|De|Diputad[ao]s?|En)</b>" , "</b>: \\1", tmp)
tmp <- gsub.e(": +(Háganse|Muchas|No|Permítame|Por|Puede|Queda|Remítase|Ruego|Sonido|Tiene)</b>" , "</b>: \\1", tmp)
tmp <- gsub.e("T</b>úrnese" , "</b>Túrnese", tmp)
tmp <- sub.e(": E</b>n vot" , "</b>: En vot", tmp)
tmp <- sub.e(": P</b>or " , "</b>: Por ", tmp)
tmp <- gsub.e(": +Está</b>" , "</b> Está", tmp)
tmp <- gsub.e(": ...</b>" , "</b>: ... ", tmp)
tmp <- gsub.e(": F</b>inalmente" , "</b>: Finalmente", tmp)
tmp <- gsub.e("[(]11:24 horas[)]:</b> " , "</b> (11:24 horas): ", tmp)
tmp <- sub.e("<b>La diputada. (Aída|María)" , "<b>La diputada \\1", tmp)
tmp <- sub.e(":k9</b> " , "</b>: ", tmp)
tmp <- sub.e("El diputado Martín Alonso Heredia Lizárraga, la diputada Lizbeth Eugenia Rosas Montero, el diputado Luis Antonio González Roldán:", "Los diputados Martín Alonso Heredia Lizárraga, Lizbeth Eugenia Rosas Montero, Luis Antonio González Roldán:", tmp) # toman protesta
tmp <- sub.e("<p><b>La diputada</b>: [(]desde la curul[)]" , "<p><b>La diputada no-registrado-en-ve</b> (desde la curul): ", tmp)
tmp <- sub.e("<p><b>La diputada </b>[(]desde la curul[)]:" , "<p><b>La diputada no-registrado-en-ve</b> (desde la curul):", tmp)
tmp <- sub.e("Gutiérrez, </b>" , "Gutiérrez</b>", tmp)

# unbold in next line
sel <-     grep.e(pattern = "^</B>", tmp)
tmp[sel] <- sub.e(pattern = "^</B>", replacement = "", x = tmp[sel])
tmp[(sel-1)] <- sub.e(pattern = "</P>$", replacement = "</B></P>", x = tmp[(sel-1)]) # bring up

# needs processing after unbold net line
tmp <- gsub.e(".desde la curul.: A favor.</B>", "</B>(desde la curul): A favor.", tmp)

# line break in date
sel <- grep(pattern = "<br>[^&]", tmp, perl = TRUE, ignore.case = TRUE) # find extraneous text
#tmp[sel] <- sub("<br>", "", tmp[sel]) # remove extraneous text
tmp[(sel-1)] <- paste(tmp[(sel-1)], tmp[sel]) # merge two lines
tmp[(sel-1)] <- sub(",","", tmp[(sel-1)]) # drop comma

tmp <- gsub(" ", " ", tmp) # udenrscores to spaces

# in extra 62y2
tmp <- sub.e("<SPAN STYLE=.COLOR:BLACK.>", "", tmp)




