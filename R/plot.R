library(data.table)
library(ggplot2)
library(ggprism)
theme_set(
    theme_prism() + theme(
        text=element_text(size=30),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40)
    )
)


zt05_11 <- paste0("ZT", seq(5, 10.5, 0.5))
zt12_18 <- paste0("ZT", seq(12, 17.5, 0.5))
zt05_24 <- paste0("ZT", seq(5, 23.5, 0.5))
zt_all <- paste0("ZT", seq(4, 30, 0.5))
zts <- list(
    zt05_11 = zt05_11,
    zt12_18 = zt12_18,
    zt05_24 = zt05_24,
    zt_all = zt_all
)

colors_panel1 <- c("blue", "red")
colors_panel2 <- c("blue", "brown4", "darkmagenta", "black")
colors_panel3 <- c("darkgoldenrod3", "#15acaa", "#770fdf", "darkolivegreen")
colors_panel4 <- c("darkblue", "darkgoldenrod3", "black")
colors_panel5 <- c("chocolate4", "#62BBC1","#B23A48", "darkgoldenrod3")
#8B4513 = chocolate4


y_limits_panel1 <- c(-1, 1)
y_limits_panel2 <- c(-1, 0.7)

make_annotation_df <- function(df, variable, ...) {
    
    values <- levels(df[[variable]])
    if (is.null(values)) {
        values <- unique(df[[variable]])
    }
    
    # if (!is.null(variable)) {
    #     values <- levels(df[[variable]])
    #     if (is.null(values)) {
    #         values <- unique(df[[variable]])
    #     }
    # } else {
    #     variable <- "group__"
    #     values <- "A"
    #     df$group__ <- "A"
    # }

    p_values <- sapply(values, function(val){
        x <- df[df[[variable]]==val & test == "PRE", PI]
        y <- df[df[[variable]]==val & test == "POST", PI]
        if (length(x) == 0) {return(NA)}
        t.test(
            x = x,
            y = y,
            paired=TRUE,
            ...
        )$p.value
    })

    stars <- ifelse(p_values > 0.05, "NS", ifelse(p_values > 0.01, "*", ifelse(p_values > 0.005, "**", "***")))
    p_values <- sapply(p_values, function(p) max(p, 0.001))
    
    annotation_df <- data.table(
        y_position=0.5,
        annotations = round(p_values[values], 4),
        stars=stars
    )
    annotation_df[[variable]] <- factor(values, levels=values)
    annotation_df$annotations <- ifelse(annotation_df$annotations == 0.001, "< 0.001", as.character(annotation_df$annotations))
    # divide by two because each fly is represented twice (once for the pre and once for the post)
    annotation_df$N <- sapply(annotation_df[[variable]], function(val) {nrow(df[df[[variable]]==val,])/2})
                       

                       
    df$var__ <- df[[variable]]
    stats_df <- df[, .(PI = mean(PI), PI_median = median(PI), PI_std = sd(PI)), by=.(var__, test)]
    stats_df[[variable]] <- stats_df$var__
    stats_df$var__ <- NULL
    annotation_df <- merge(annotation_df, stats_df, by=variable)
                       
    annotation_df[, std_error := PI_std / sqrt(N)]
    annotation_df$group__ <- annotation_df[[variable]]
    annotation_df
}

expansion_x_left <- 0.1
expansion_x_right <- 0.1
expansion_y_left <- 0
expansion_y_right <- 0

point_size <- 0.02
errorbar_width <- point_size*4

unpaired_t_test <- function(x, y, ...) {
    t.test(x=x, y=y, alt="two.sided", paired=FALSE, ...)
}
    
                       
paired_t_test <- function(x, y, ...) {
    t.test(x=x, y=y, alt="two.sided", paired=TRUE, ...)
}
paired_wilcoxon_test <- function(x, y, ...) {
    wilcox.test(x=x, y=y, alt="two.sided", paired=TRUE, ...)
}

save_learning_plot <- function(plot, ratio, size_unit=5, ...) {
    if (plot$direction=="horizontal") {
        width <- plot$n_facets*size_unit
        height <- size_unit*ratio
    } else if (plot$direction=="vertical") {
        height <- plot$n_facets*size_unit/ratio
        width <- size_unit*ratio
    
    }
    svg(width=width, height=height, ...)
        print(plot$gg)
    dev.off()
    
    # ggsave(plot=plot$gg, height=height, width=width, ...)
}
                       
save_summ_plot <- function(plot, ratio, size_unit=5, ...) {
    # ggsave(plot=plot$gg, width=plot$n_facets*size_unit, height=size_unit*ratio, ...) 
    width=plot$n_facets*size_unit
    height=size_unit*ratio
    svg(width=width, height=height, ...)
    print(plot$gg)
    dev.off()
}
                       
learning_plot <- function(
    data, group, direction="horizontal",
    test=paired_wilcoxon_test,
    map_signif_level=TRUE,
    y_limits=c(-1, 1),
    colors=NULL, starsize=15,
    hjust_text=0.5,
    y_annotation=NULL,
    text_y_size=20,
    title_y_size=25,
    textsize=10,
    y_step=0.5
) {

    if (is.null(group)) df$group__ <- "A"
    else if (!(group %in% colnames(data))) {
        data$group__ <- group
    } else data$group__ <- data$group__ <- data[[group]]
    group <- "group__"
    
    annotation_df <- make_annotation_df(data, group, alternative="greater")
    

    data$x <- ifelse(data$test == "PRE", 1, 2)
    annotation_df$x <- ifelse(annotation_df$test == "PRE", 1, 2)
    original_y_lim <- y_limits

    y_min <- min(y_limits[1], min(data$PI)*1.02)
    y_max <- max(y_limits[2], max(data$PI)*1.02)
    y_mult <- c(0.02, 0.02)
    
    if (y_max > y_limits[2]) {
        y_limits[2] <- y_max
        y_mult[2] <- 0.01
    }
    if (y_min < y_limits[1]) {
        y_limits[1] <- y_min
        y_mult[1] <- 0.01
    }
    
    print(y_limits)
    n_facets <- length(unique(data$group__))

    # 1.2
    from=floor(original_y_lim[1]*0.5)/0.5
    to=ceiling(original_y_lim[2]/0.5)*0.5


    # 1.3
    from=original_y_lim[1]
    to=original_y_lim[2]
    y_limits <- c(from, to)

    if (is.null(y_annotation)) {
        y_annotation <- y_limits[2]*0.9
    }
    
    panel <- ggplot(data=data, aes(x=x, y = PI, group=group__)) +
        # geom_point(size=2, color="#808080")
        ggforce::geom_circle(fill="#808080", color=NA, aes(r=point_size, x0=x, y0=PI)) +
        geom_line(aes(group=id), color="#808080")
    
    
    if (is.null(colors)) {
        panel <- panel +
            geom_line(data=annotation_df, aes(x=x, y=PI, group=group__), color="#ff6000", linewidth=2) +
            ggforce::geom_circle(data=annotation_df, fill="#ff6000", color=NA, aes(r=point_size, x0=x, y0=PI, group=group__))
    } else {
        stopifnot(length(colors)==length(unique(annotation_df$group__)))
        panel <- panel +
            geom_line(data=annotation_df, aes(x=x, y=PI, col=group__, group=group__), linewidth=2) +
            ggforce::geom_circle(data=annotation_df, color=NA, aes(fill=group__, r=point_size, x0=x, y0=PI, group=group__)) +
            scale_fill_manual(values=colors) +
            scale_color_manual(values=colors) + guides(fill="none", color="none")
    }
    

    panel <- panel +     
        # geom_point(data=annotation_df, aes(x=x, y=PI, group=group__), color="#ff6000", size=2) +
        geom_errorbar(data=annotation_df, aes(x=x, y=PI, ymin=PI-std_error, ymax=PI+std_error, color=group__, group=group__), width=errorbar_width) +
        scale_y_continuous(breaks=seq(from=from,to=to, y_step), limits=y_limits, expand=expansion(mult=c(expansion_y_left, expansion_y_right))) +
    scale_x_discrete(expand = expansion(mult = c(expansion_x_left, expansion_x_right))) + theme(axis.line.x=element_blank())
    
    if (direction == "horizontal") {
        panel <- panel + facet_grid(. ~ group__)
    } else if (direction == "vertical") {
        panel <- panel + facet_grid(group__ ~ .)
    }

    panel <- panel + geom_text(data=annotation_df, y=y_limits[1]+0.15, size=textsize, x=1, hjust=hjust_text, mapping=aes(label = paste0("N = ", N)))
    print(y_annotation)
    if (!is.null(test)) {
        if (map_signif_level) {
            panel <- panel + geom_signif(
                data = annotation_df, 
                aes(annotations = stars), xmin=1, xmax=2,
                y_position = y_annotation, test=test,
                textsize=starsize, size=1,
                manual = TRUE,  tip_length = 0
            )
        } else {
            panel <- panel + geom_signif(
                data = annotation_df, 
                aes(annotations = annotations), xmin=1, xmax=2,
                y_position = y_annotation, test=test,
                manual = TRUE,  tip_length = 0
            )        
        }
    }
    # panel <- panel + coord_cartesian(ylim=y_limits*1.2, expand = expansion(mult = c(expansion, expansion))) 
    panel <- panel + theme(
            axis.text.y = element_text(size=text_y_size),
            axis.title.y = element_text(size=title_y_size),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            strip.text = element_blank()
        )
    
    data$group__ <- NULL
    return(list(gg=panel, n_facets=n_facets, direction=direction, annotation=annotation_df))
}
                       

preprocess_summary_data_deltaPI <- function(data, group, test, value.var="PI") {
    data$group__ <- data[[group]]
    data$test__ <- data[[test]]
    data <- dcast(data, id + group__ ~ test__, value.var=value.var)
    print(head(data))
    data[[value.var]] <- data$POST-data$PRE
    data[[test]] <- data$test__
    data[, test__ := NULL]
    return(data)
}

preprocess_summary_data_postPI <- function(data, group, test, value.var="PI") {
    data$group__ <- data[[group]]
    data$test__ <- data[[test]]
    data <- data[test=="POST",]
    data[[test]] <- data$test__
    data[, test__ := NULL]
    return(data)
}
                   

                       
summary_plot <- function(
    data, group, comparisons, annotation_y, test=unpaired_t_test, map_signif_level=TRUE, colors=NULL, x_labels_angle=0, starsize=15,
    text_y_size=20, title_y_size=25, y_limits=NULL, percentile=c(0.025, 0.975),
    preprocess_function=preprocess_summary_data_postPI, y_axis_label="Post PI", y_breaks=waiver()
) {
    stopifnot(length(comparisons) == length(annotation_y))
    percentage<-1

    data <- preprocess_function(data=data, group=group, test="test", value.var="PI")
    stopifnot("PI" %in% colnames(data))
    data[, PI:=PI*percentage]
    y_limits<-y_limits*percentage


    if (!is.null(colors)) {
        gg <- ggplot(data=data, aes(x=group__, y=PI, fill=group__))
    } else {
        gg <- ggplot(data=data, aes(x=group__, y=PI))
    }
    data_summ <- data[, .(
        ymin=quantile(PI, percentile[1]),
        lower = quantile(PI, 0.25),
        middle = median(PI),
        upper = quantile(PI, 0.75),
        ymax = quantile(PI, percentile[2])
    ), by=group__
    ]
    data[, outlier := FALSE]
    for (grp in data_summ$group__) {
        data[group__ == grp & (PI < data_summ[group__==grp, ymin] | PI > data_summ[group__==grp, ymax]), outlier := TRUE]
    }
    
    thickness<-1.5
    gg <- gg + 
        geom_boxplot(data=data_summ, stat="identity", aes(x=group__, y=NULL, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax), size=thickness, fatten=0.75) +
        geom_segment(data=data_summ, aes(x = as.numeric(group__) - 0.25, xend = as.numeric(group__) + 0.25, y = ymin, yend = ymin), linewidth=thickness) +  # Lower whisker
        geom_segment(data=data_summ, aes(x = as.numeric(group__) - 0.25, xend = as.numeric(group__) + 0.25, y = ymax, yend = ymax), linewidth=thickness) +  # Upper whisker
        geom_jitter(data=data[outlier == TRUE,], aes(x=group__, y=PI), width=0.1)
        # scale_y_continuous(breaks=seq(-1, 1, 0.5), limits=c(-1, 1)) +
    
    if (x_labels_angle == 0) {
        gg <- gg + scale_x_discrete(
            # expand = expansion(mult = c(0.1, 0.1)),
            labels = function(x) stringr::str_wrap(x, width = 5),
            name=""
        ) 
    } else {
        gg <- gg + scale_x_discrete(name="")
    }
    
    if (is.null(y_limits)) {
        gg <- gg + scale_y_continuous(name=y_axis_label, breaks=y_breaks, expand=expansion(mult=c(expansion_y_left, expansion_y_right)))
    } else {
        gg <- gg + scale_y_continuous(name=y_axis_label, breaks=y_breaks, expand=expansion(mult=c(expansion_y_left, expansion_y_right)), limits=y_limits)
    }
    if (!is.null(colors)) {
        if(! length(colors) == length(unique(data$group))) {
            print(paste0(length(colors) != length(unique(data$group))))
        }
        gg <- gg + scale_fill_manual(values=colors) + guides(fill="none")
    }
    gg <- gg + theme(
        axis.text.y = element_text(size=text_y_size),
        axis.title.y = element_text(size=title_y_size),        
        axis.text.x = element_text(hjust=1, vjust=1, angle=x_labels_angle)
    )
    
    for (i in 1:length(comparisons)) {
        gg <- gg + geom_signif(
            textsize=starsize,
            size=2,
            comparisons=list(comparisons[[i]]),
            y_position = annotation_y[i]*percentage,
            map_signif_level=map_signif_level,
            test=test
        ) 
    }
    n_facets <- length(unique(data$group__))

    data$group__ <- NULL
    return(list(gg=gg, n_facets=n_facets))    
}

                       

preprocess_sleep_data <- function(data, group) {
    if (!is.null(group)) {
        if (!(group %in% colnames(data))) {
            data$group__ <- group
        } else {
            data$group__ <- data[[group]]
        }
    } else {
        data$group__ <- "A"
    }
    

    dt_sleep <- melt(
        data =  data[test == "POST",],
        id.vars=c("fly_name_reference", "group__"),
        value.name="value",
        variable.name="ZT",
        measure.vars=zts$zt_all,
    )
    dt_interactions <- melt(
        data =  data[test == "POST",],
        id.vars=c("fly_name_reference", "group__"),
        value.name="value",
        variable.name="ZT",
        measure.vars=paste0(zts$zt_all, "_interactions"),
    )
    
    dt <- rbind(
        cbind(dt_sleep, variable="asleep"),
        cbind(dt_interactions, variable="interactions")
    )
    
    
    dt[, t := as.numeric(gsub(x=gsub(pattern = "ZT", x = ZT, replacement = ""), pattern="_interactions", replacement=""))*3600]
    dt <- dt[order(fly_name_reference),]
    missing_data <- dt[variable=="asleep", .(missing=all(is.na(value))), by=fly_name_reference]
    missing_data_animals <- missing_data[missing==TRUE, fly_name_reference]
    dt <- dt[!(fly_name_reference %in% missing_data_animals),]  
    dt_summ <- dt[, .(mu = mean(value, na.rm=TRUE), sigma = sd(value, na.rm=TRUE), n = .N), by=.(t, group__, variable)]
    dt_summ[, sem := sigma / sqrt(n)]
    # dt_summ <- dt_summ[ t >= behavr::hours(5),]
    return(list(dt=dt, dt_summ=dt_summ))

}
                       
sleep_plot <- function(dt, point_size=3, line_size=2, color_by_group=TRUE, colors=NULL) {
    y_lab <- "Sleep (per 30 min bin)"
    
    

    if (color_by_group) {
        stopifnot(length(colors) == length(unique(dt$group__)))
        sleep_plot <- ggplot(data=dt[variable=="asleep",], aes(x=t, y=mu, color=group__, group=group__)) +
            # ggforce::geom_circle(aes(x0=t, y0=mu, r=.05)) +
            geom_point(size=point_size) +
            geom_line(size=line_size) +
            geom_errorbar(aes(ymax=mu+sem, ymin=mu-sem)) +
            scale_y_continuous(name = y_lab, breaks = seq(0, 1, 0.2), labels=seq(0, 1, 0.2)*30, limits=c(0, 1)) +
            scale_color_manual(values=colors)
    } else {
        # Assume y2 needs to be scaled down to be on similar scale as y1
        # transformation_factor <- max(dt[variable=="asleep", mu], na.rm=TRUE) / max(dt[variable=="interactions", mu], na.rm=TRUE)
        transformation_factor <- 0.1
        dt[,mu_transformed := mu]
        dt[,sem_transformed := sem]
        
        dt[variable=="interactions", mu_transformed := mu * transformation_factor]
        dt[variable=="interactions", sem_transformed := sem * transformation_factor]
        print(transformation_factor)
        # return(dt)

        sleep_plot <- ggplot() +
            # ggforce::geom_circle(aes(x0=t, y0=mu, r=.05)) +
            geom_point(data=dt[variable=="asleep", ], size=3, aes(x=t, color=variable, group=variable, y=mu)) +
            geom_line(data=dt[variable=="asleep", ], aes(x=t, color=variable, group=variable, y=mu)) +
            geom_errorbar(data=dt[variable=="asleep", ], aes(x=t, color=variable, group=variable, y=mu, ymax=mu+sem, ymin=mu-sem)) +

            geom_point(data=dt[variable=="interactions", ], size=3, aes(x=t, color=variable, group=variable, y=mu_transformed)) +
            geom_line(data=dt[variable=="interactions", ], aes(x=t, color=variable, group=variable, y=mu_transformed)) +
            geom_errorbar(data=dt[variable=="interactions", ], aes(x=t, color=variable, group=variable, y=mu_transformed, ymax=mu_transformed+sem_transformed, ymin=mu_transformed-sem_transformed)) +
            scale_y_continuous(
                name = ylab, breaks = seq(0, 1, 0.2), labels=seq(0, 1, 0.2)*30,
                sec.axis = sec_axis(trans = ~./transformation_factor, name = "Interactions")
            ) +
            scale_color_manual(values=c("interactions"="blue", "asleep"="black"))
    }
    sleep_plot <- sleep_plot +
        scale_x_hours(breaks=behavr::hours(seq(4, 30, 2)), labels=seq(4, 30, 2) %% 24, name="ZT") +
        stat_ld_annotations()   

    return(sleep_plot)
}