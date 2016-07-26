getCategoryInfo <- function(cat) { 
    print("Getting category info")
    if(length(cat) != 3 | length(cat$label) == 0) {
        return(data.frame(index = NA, label = "No labels for this variable", missing = NA))
    } else {
        output <- data.frame(index = cat$index, label = "", missing = FALSE, 
                             stringsAsFactors = FALSE)
        for(i in 1:nrow(output)) {
            output$label[i] <- cat$label[[output$index[i]]]
            output$missing[i] <- output$index[i] %in% cat$missing
        }
        return(output)
    }
}

saveAllLabelsToRDA <- function() {
    fls <- getRecFiles()
    labels <- data.frame()
    for(rec in as.character(fls$recNum)) {
        jsn <- getRecJSON(rec)
        labels <- rbind(labels, do.call(rbind, lapply(jsn$dimension, function(v) {
            rbind(addVariableLabel(v,rec), addValueLabels(v, rec))
        })))
    }
    labels <- labels[which(labels$label != "NULL" & nchar(labels$label) > 0),]
    save(labels, file = "N:/jsonSearch/rda/labels.Rda" )
}

getVarDesc <- function(j) {
    print("Getting var desc")
    out <- names(j$dimension)
    names(out) <- vapply(out, function(x) {
        paste0(x, " - ", j$dimension[[x]]$displaylabel)
    }, "") 
    return(out)
}