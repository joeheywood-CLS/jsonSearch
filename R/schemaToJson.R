library(stringr)
library(jsonlite)

sch <- readLines("T:/Joe/bcs70/BCS70.sch")
sch <- gsub("''", "___", sch)
st <- which(grepl("^RECORD SCHEMA ", sch))
stp <- which(grepl("^END ", sch))

getRecordDetails <- function(s) { # s is first line
    exp <- "RECORD SCHEMA.*?([0-9]+)\\s*(\\w+).*"
    list(recNum = gsub(exp, "\\1", s), recName = gsub(exp, "\\2", s))
}

getDocDescription <- function(ss) { # second line?
    s <- ss[which(grepl("^DOCUMENT", ss))]
    gsub("^DOCUMENT\\s+", "", s)
}

getDataListText <- function(ss) {
    stt <- which(grepl("^DATA LIST", ss))
    ix <- stt + 1
    while(substr(ss[ix], 1, 1) == " ") {
        ix <- ix + 1
    }
    ss <- ss[(stt + 1):(ix-1)]
    ss <- gsub("^\\s+([0-9]?)\\s+", "", ss)
    list(vars = gsub("^(\\w+).*", "\\1", ss), 
         type = gsub(".*\\((\\w+)\\)$", "\\1", ss) )
}

getVarLabels <- function(ss) {
    stt <- which(grepl("^VAR LABEL", ss))
    ix <- stt + 1
    while(substr(ss[ix], 1, 1) == " ") {
        ix <- ix + 1
    }
    ss <- ss[stt:(ix-1)]
    ss[1] <- gsub("VAR LABEL", "", ss[1])
    expr <- "^\\s+(\\w+).*\\s.*['](.*)['].*"
    labs <- as.list(gsub(expr, "\\2", ss))
    names(labs) <- gsub(expr, "\\1", ss)
    labs
}

getValueLabels <- function(ss) {
    stt <- which(grepl("^VALUE LABELS", ss))
    ix <- stt + 1
    while(substr(ss[ix], 1, 1) == " ") {
        ix <- ix + 1
    }
    ss <- ss[stt:(ix-1)]
    ss[1] <- gsub("VALUE LABELS", "", ss[1])
    ss <- str_trim(ss)
    vrb <- which(substr(ss, 1, 1) != "(")
    vrbs <- str_trim(gsub("^(\\w+).*", "\\1", ss[vrb]))
    ss[vrb] <- str_trim(gsub("^\\w+", "", ss[vrb]))
    vrbEnds <- c((vrb[-1]-1), length(ss))
    output <- list()
    for(i in 1:length(vrb)) {
        tt <- ss[vrb[i]:vrbEnds[i]]
        expr <- "[(](.*)[)].*['](.*)['].*"
        labs <- gsub(expr, "\\2", tt)
        names(labs) <- gsub(expr, "\\1", tt)
        output[[vrbs[i]]] <- labs
    }
    output
}

getMissing <- function(ss) {
    stt <- which(grepl("^MISSING VALUES", ss))
    if(length(stt) == 0) {
        return(list())
    }
    ix <- stt + 1
    while(substr(ss[ix], 1, 1) == " ") {
        ix <- ix + 1
    }
    ss <- ss[stt:(ix-1)]
    ss[1] <- gsub("MISSING VALUES", "", ss[1])

    ss <- str_trim(ss)
    vrb <- which(grepl("^[^-0-9]", ss))
    ss <- gsub("[()]", "", ss)

    vrbEnds <- c((vrb[-1]-1), length(ss))
    output <- list()
    for(i in 1:length(vrb)) {
        tt <- ss[vrb[i]:vrbEnds[i]]
        vvb <- str_trim(gsub("^(\\w+).*", "\\1", tt[1]))
        output[[vvb]] <-  str_trim(gsub("^\\w+", "", tt))
    }
    output
}

buildObject <- function(x) {
    ss <- sch[st[x]:stp[x]]
    obj <- getRecordDetails(ss[1])
    # do we stop if we don't get record details?
    obj$doc <- getDocDescription(ss)
    obj$dimension <- list()
    datList <- getDataListText(ss)
    vrb <- getVarLabels(ss)
    vlbs <- getValueLabels(ss)
    mss <- getMissing(ss)
    for(a in 1:length(datList$vars)) {
        obj$dimension[[datList$vars[a]]] <- buildVariable(a, datList, vrb, vlbs, mss )
    }
    obj
}

buildVariable <- function(x, d, v, vl, m) {
    #save(x,d,v,vl,m, file = paste0("N:/jsonRda/", d$vars[x], ".Rda"))
    vrb <- d$vars[x]
    if(vrb %in% names(vl)) {
        index <- names(vl[[vrb]])
        lbl <- as.list(vl[[vrb]])
        names(lbl) <- index
    } else {
        index <- c()
        lbl <- list()
    }
    if(vrb %in% names(m)) {
        mss <- m[[vrb]]
    } else {
        mss <- c()
    }
    # HERE! correct index
    
    category <- list(index = index, label = lbl, missing = mss)
    list(itemname = vrb, displaylabel = as.character(v[vrb]), category = category)
}

saveAllObjects <- function() {
    lapply(3:length(st), function(x) {
        try({
            print(x)
            a <- buildObject(x)
            jsnTxt <- toJSON(a, auto_unbox = TRUE)
            cat(prettify(jsnTxt, 4), 
                file = paste0("N:/jsonRda/rec", a$recNum, ".json"))
        },
        )
    })
}






# "T:/DataStore/working/bcs/2016 data/Feed forward data/MainVersion/final_version"