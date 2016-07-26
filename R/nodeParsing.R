

buildList <- function(val, pth) {
    x <- length(pth)
    out <- list()
    out[[pth[x]]] <- val
    print(names(out))
    if(x > 1) {
        return(buildList(out, pth[1:(x-1)]))
    } else {
        return(out)
    } 
}

getNodeValue <- function(lbs, jsn) {
    pth <- c()
    while(length(lbs) > 0) {
        pth <- fetchNodePath(lbs[1], jsn, pth)
        for(p in pth) {
            jsn <- jsn[[p]]
        }
        pth <- c()
        lbs <- lbs[-1]
    }
    jsn
}

fetchNodePath <- function(ndl, hay, pth) {
    if(ndl %in% names(hay)) {
        return(c(pth, ndl))
    } else {
        for(nm in names(hay)) {
            lastPath <- pth
            if(is.list(hay[[nm]]) && length(hay[[nm]]) > 0) {
                pth <- c(pth, nm)
                x <- fetchNodePath(ndl, hay[[nm]], pth)
                if(!is.null(x))return(x)
            } 
            pth <- lastPath
        }
    }
    NULL
}

setNodeValue <- function(lbs, jsn, newVal) {
    fullPth <- pth <- c()
    out <- jsn
    while(length(lbs) > 0) {
        pth <- fetchNodePath(lbs[1], jsn, pth)
        for(p in pth) {
            jsn <- jsn[[p]]
        }
        fullPth <- c(fullPth, pth)
        pth <- c()
        lbs <- lbs[-1]
    }
    print(jsn)
    bl <- buildList(newVal, fullPth)
    modifyList(out, bl)
}


