getNode <- function(lbl, jsonObj, ret = "node", expectedClass = "any") {
    node <- jsonObj
    pth <- paste0("jsonObj")
    ord <- 1:length(lbl)
    lbl <- as.list(lbl)
    while(length(ord) > 0) {
        pth <- fetchNodePath(lbl[[ ord[1] ]], node, pth)
        node <- eval(parse(text = pth))
        ord <- ord[-1]
    }
    if(ret == "node") {
        if(expectedClass != "any" && class(node) != expectedClass) {
            stop("Classes do not match")
        }
        return(node)
    } else if (ret == "path") {
        return(pth)
    } else if(ret == "both") {
        list(node = node, path = pth)
    }
    FALSE
}

# has to be an existing node at the moment.
setNode <- function(lbl, value, jsonObj) {
    classOfValue <- class(value)
    print(lbl)
    nd <- getNode(lbl, jsonObj, ret = "path", expectedClass = classOfValue)
    expr <- paste0(nd, " <- '", value, "'")
    print(expr)
    eval(parse(text = expr))
    jsonObj
}

addToNode <- function(lbl, value, jsonObj) {
    classOfValue <- class(value)
    print(lbl)
    nd <- getNode(lbl, jsonObj, ret = "path", expectedClass = classOfValue)
    expr <- paste0(nd, " <- c(", nd, ", '", value, "')")
    print(expr)
    eval(parse(text = expr))
    jsonObj
}

removeFromNode <- function(lbl, value, jsonObj) {
    classOfValue <- class(value)
    print(lbl)
    nd <- getNode(lbl, jsonObj, ret = "path", expectedClass = classOfValue)
    expr <- paste0(nd, " <- ", nd, "[which(", nd, " != '", value, "')]")
    print(expr)
    eval(parse(text = expr))
    jsonObj
}

# add new node to an existing one.




fetchNodePath <- function(ndl, hay, pth) {
    if(ndl %in% names(hay)) {
        return(paste0(pth, "$`", ndl, "`"))
    } else {
        for(nm in names(hay)) {
            if(class(hay[[nm]]) == "list" && length(hay[[nm]]) > 0) {
                pth <- paste0(pth, "$`", nm, "`")
                return(fetchNodePath(ndl, hay[[nm]], pth))
            }
        }
    }
    # returns false if node not found at all
    FALSE
}


#####################

createNode <- function() {
    
}

removeNode <- function() {
    
}


getMissingLabels <- function(v, r = NULL) { # r is json object
    ## return all missing values, all labelled values that are less than
    ## zero or 98, 99, 999 etc.
    print(v)
    vrb <- r$dimension[[v]]$category
    if(length(vrb$index) > 0) {
        out <- data.frame(rec = r$recNum, var = r$dimension[[v]]$itemname, val = vrb$index, 
                          lab =  vapply(vrb$index, function(x){vrb$label[[x]]}, ""),
                          missing = vrb$index %in% vrb$missing, stringsAsFactors = FALSE)
        return(out[which(out$missing == TRUE | as.numeric(out$val) < 0 | as.numeric(out$val) %in% c(98,99,999)),])
    } else {
        return(data.frame())
    }
}

getMissingForRec <- function(rn) {
    jsn <- getRecJSON(rn)
    do.call(rbind, lapply(names(jsn$dimension), getMissingLabels, r = jsn))
}
