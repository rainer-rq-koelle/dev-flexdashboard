apts <- list.files("./data-ad-charts/", pattern = "[A-Z]{4}\\.png") %>% strtrim(4)
nope <- c("EGNT","ENBR","ENVA","ENZV","GCFV","LCLK","LFBO","LFML","LIMF","WSSS")
#done <- list.files("./boards/", pattern = "[A-Z]{4}\\.html") %>% strtrim(4)
#apts <- setdiff(apts, done)
apts <- setdiff(apts, nope)
