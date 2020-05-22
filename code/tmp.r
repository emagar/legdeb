    # diputado secretario inverted
    sel <- grep.e(pattern = "<[pb]>(?:<a.+/a>)?<[bp]>(?:El|La) diputad[oa] secretari[oa]", text)
    text[sel] <- sub.e("(diputad[oa]) (secretari[oa])", "//2 //1", text[sel])
