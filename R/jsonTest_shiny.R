getCategoryInfo <- function(cat) { 
    if(length(cat) != 3) {
        return(data.frame())
    }
    output <- data.frame(index = cat$index, label = "", missing = FALSE, 
                         stringsAsFactors = FALSE)
    for(i in 1:nrow(output)) {
        output$label[i] <- cat$label[[output$index[i]]]
        output$missing[i] <- output$index[i] %in% cat$missing
    }
    output
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