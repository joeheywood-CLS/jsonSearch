library(jsonlite)
library(dplyr)
source("~/jsonSearch/R/nodeParsing.R")

getRecJSON <- function(r, kyw = "blahblahbla") {
    fl <- paste0("N:/jsonRda//rec", r, ".json")
    rl <- readLines(fl, warn = FALSE)
    rl <- gsub(kyw, paste0("<strong>", kyw, "</strong>"), rl)
    rl <- gsub("___", "&#39;", rl)
    fromJSON(rl)
}

saveRecJSON <- function(jsn) {
    jsnTxt <- toJSON(jsn, auto_unbox = TRUE)
    cat(prettify(jsnTxt, 4), 
        file = paste0("N:/jsonRda/rec", jsn$recNum, ".json"))
}


# Public methods #

renameVariable <- function(rec, old, nw) {
    jsn <- getRecJSON(rec)
    nd <- jsn$dimension[[old]]
    jsn[[old]] <- NULL
    nd$itemname <- new
    jsn$dimension[[nw]] <- nd
    rm(nd)
    saveRecJSON(jsn)
}

getVariableLabel <- function(rec, vb) {
    jsn <- getRecJSON(rec)
    getNode(c(vb, "displaylabel"), jsn)
}

setVariableLabel <- function(rec, vb, newLabel) {
    jsn <- getRecJSON(rec)
    jsn <- setNode(c(vb, "displaylabel"), value = newLabel, jsonObj = jsn)
    saveRecJSON(jsn)
}

getValueLabel <- function(rec, vb, ix) {
    jsn <- getRecJSON(rec)
    getNode(list(vb, "category", "label", ix), jsonObj = jsn) 
}

setValueLabel <- function(rec, vb, ix, lab) {
    jsn <- getRecJSON(rec)
    ## first find out if node exists...
    if(getValueLabel(rec, vb, ix) != FALSE) {
        setNode(list(vb, "category", "label", ix), value = lab, jsonObj = jsn)
    } else {
        stop(paste0("Value label doesn't exist. Youll need to add category first"))
    }
    
}

setValueToMissing <- function(rec, vrb, ix) {
    # get path to variable
    jsn <- getRecJSON(rec)
    mss <- getNode(list(vb, "category", "missing",), jsonObj = jsn)
    ix <- as.character(ix)
    if(ix %in% mss) {
        print(paste0("Value: ", ix, " is already missing"))
        return(FALSE)
    } else {
        jsn <- addToNode(list(vb, "category", "missing"), ix, jsonObj = jsn)
    }
    saveRecJSON(jsn)    
}

removeMissingCategory <- function() {
    # get path to variable
    
    # check that it's not missing already
    
    # remove missing from vector
    
    # save
}

addCategory <- function(rec, vb) {
    
}

removeCategory <- function(rec, vb, ix) {
    
}

# ------------------ #

searchRec <- function(rec, ky) {
    jsn <- getRecJSON(rec)
    labels <- do.call(rbind, lapply(jsn$dimension, function(v) {
        rbind(addVariableLabel(v,rec), addValueLabels(v, rec))
    }))
    lv <- unique(labels$var[which(grepl(ky, labels$label))])
    out <- lapply(lv, function(x) {
        labels[which(labels$var == x),]
    })
    names(out) <- lv
    out
}

addVariableLabel <- function(v, r) {
    data.frame(rec = r, var = v$itemname, label = v$displaylabel, 
               type = "variable label", stringsAsFactors = FALSE)
    
}

addValueLabels <- function(v, r) {
    lbls <- unlist(v$category$label)
    if(length(lbls) == 0) lbls = ""
    data.frame(rec = r, var = v$itemname, label = lbls, 
               type = "value label", stringsAsFactors = FALSE)
}

getRecFiles <- function() {
    fls <- dir("N:/jsonRda/")
    nums <- as.numeric(gsub("^rec(\\d+).json", "\\1", fls))
    data.frame(file = fls, recNum = nums) %>% arrange(recNum)
}


addNewVariable <- function(vbName, vrbLabel, values, labels) {
    # 

}






