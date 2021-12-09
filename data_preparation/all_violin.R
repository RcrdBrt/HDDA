require(reshape2)
require(patchwork)

ids <- sample(c(1:nrow(acc)), 1e4)
small_acc <- acc[ids,]

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             # Original function by Jan Gleixner (@jan-glx)
                             # Adjustments by Wouter van der Bijl (@Axeman)
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                               quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           }
)

create_quantile_segment_frame <- function(data, draw_quantiles, split = FALSE, grp = NULL) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles)
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
  violin.xs <- (stats::approxfun(data$y, data$x))(ys)
  if (grp %% 2 == 0) {
    data.frame(
      x = ggplot2:::interleave(violin.xs, violin.xmaxvs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  } else {
    data.frame(
      x = ggplot2:::interleave(violin.xminvs, violin.xs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  }
}

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

make_violin <- function(data, variables, title="Distribuzioni", trim=T){
  ggplot(reshape2::melt(data %>% select(all_of(variables))), aes(x = variable, y = value, fill = variable)) + 
    geom_violin(trim = trim) +
    geom_boxplot(width=0.1) +
    facet_wrap(~variable, scales = "free", strip.position="bottom") +
    ggtitle(title) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size=32, face="bold"),
          axis.title = element_blank(),
          axis.text=element_text(size=20),
          axis.text.x = element_blank(),
          legend.position = "none", 
          strip.text = element_text(size=20,face="bold"))
}

make_barplot <- function(data, variables, title="Distribuzioni"){
  p <- list()
  for (i in 1:length(variables)){
    p[[i]] <- 
      ggplot(data, aes_string(x = 1, fill = variables[i])) + 
      geom_bar(position = "fill", width = 0.3) +
      scale_y_continuous(labels = scales::percent) +
      ggtitle(variables[i]) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size=20, face="bold"),
            axis.text=element_text(size=20),
            axis.title=element_text(size=20,face="bold"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "bottom", 
            legend.title = element_blank(),
            legend.box="vertical",
            legend.text = element_text(size=18)) +
      guides(fill=guide_legend(nrow=2,byrow=TRUE))
  }
  p[[1]] <- p[[1]] + ylab("Percentuale osservazioni")
  if (length(p) == 2){
    (p[[1]] | p[[2]])
  } else{
    (p[[1]] | p[[2]] | p[[3]])
  }
}

make_violin(small_acc,numeric_variables[1:6], title="Distribuzioni")
make_violin(small_acc,numeric_variables[7:12], title="")
make_violin(small_acc,numeric_variables[13:18], title="")
make_violin(small_acc,numeric_variables[19:24], title="")
make_violin(small_acc,numeric_variables[25:27], title="")

make_barplot(small_acc, c("application_type", "emp_length","grade"))
make_barplot(small_acc, c("home_ownership", "initial_list_status", "purpose"))
make_barplot(small_acc, c("term", "verification_status"))


