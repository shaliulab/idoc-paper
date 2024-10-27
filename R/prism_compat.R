add_ghost_data <- function(dt, id, x, columns, group, to_wide=TRUE) {
    dt <- dt[, c(id, x, group, columns), with=F]
    dt$group__ <- dt[[group]]
    dt$x <- dt[[x]]
    dt[[group]] <- NULL
    dt[[x]] <- NULL
    group_size <- dt[, .(n_flies = length(unique(id))), by=group__]
    group_size$max <- max(group_size$n_flies)
    group_size[, diff := max - n_flies]
    index <- sort(unique(dt$x))
    groups <- levels(group_size$group__)
    # group_size$group__<-factor(group_size$group__, levels=groups)
    group_size<-group_size[order(group__),]
   
    time_index <- data.table(x=index)
    for (group in groups) {
        diff <- group_size[group__ == group, diff]

        if (length(diff)>0 && diff > 0) {
            for (i in 1:diff) {
                d <- data.table(group__ = group, id = paste0(group, "_", i), x = index)
                for (column in columns) {
                    d[[column]] <- NA
                }
                                
                dt <- rbind(
                    dt[, c(id, "x", "group__", columns), with=F],
                    d[, c(id, "x", "group__", columns), with=F]
                )
            }
        }
    }
    
    if (to_wide) {
        l <- lapply(groups, function(group) {
            out <- dcast(
                dt[group__ == group, ][, c(id, "x", "group__", columns), with=FALSE],
                as.formula(paste0("x ~ ", id)), value.var=columns
            )
            # out <- out[, setdiff(colnames(out), "x"), with=FALSE]
            out <- merge(time_index, out, by="x",  all.x=TRUE, all.y=FALSE)
            out
        })
        l_without_x <- lapply(l, function(x) {
            x[, setdiff(colnames(x), "x"), with=FALSE]
        })
        lapply(l_without_x, function(x) print(dim(x)))
        out <- do.call(cbind, l_without_x)
        # out_columns <- colnames(out)
        out$x <- time_index$x
        
    } else {
        out <- dt
    }
    
    out[[x]] <- out$x
    out$x <- NULL
    out <- out[, c(x, setdiff(colnames(out), x)), with=F]
    
    if (!to_wide) {
        out[[group]] <- out$group__
        out[, group__ := NULL]
    }
    return(out)
    
}