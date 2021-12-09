piechart <- function(d, group, legend_title = "Outcome"){
  piepercent <- d %>% 
    group_by((!!sym(group))) %>% 
    summarise(n_group = n()) %>% 
    mutate(n_perc= round( n_group/nrow(d)* 100,2))
  
  ggplot(piepercent, aes(x = "", y = n_perc, fill = !!sym(group))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = n_perc), position = position_stack(vjust = 0.5),color = "white")+
    guides(fill=guide_legend(title=legend_title))+
    theme_void()
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

MyCorrelationMatrix <- function(d){
  # matrix of the p-value of the correlation
  chart_pic_file <- tempfile(pattern="file", tmpdir=tempdir(), fileext = ".png")
  p.mat <- cor.mtest(d)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  png(filename = chart_pic_file, width = 900, height = 900, type = "cairo")
  corrplot::corrplot(cor(d), method="color", col=col(200),  
                     type="upper", 
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col="black", tl.srt=45, tl.cex = .7, number.cex=0.75,  #Text label color and rotation
                     # Combine with significance
                     p.mat = p.mat, sig.level = 0.2, insig = "blank", 
                     # hide correlation coefficient on the principal diagonal
                     diag=T 
  )
  chart_pic_file
}

MyConfusionMatrix <- function(actuals, predictions, threshold=0.5, title=""){
  cm <- caret::confusionMatrix(as.factor(ifelse(predictions>threshold,1,0)), actuals, threshold, dnn=c("Prediction", "Actual"), positive="1", mode="prec_recall")
  ggplot(as.data.frame(cm$table), aes(Actual, Prediction, fill= Freq)) +
    geom_tile() + geom_text(aes(label=Freq), fontface="bold") +
    scale_fill_distiller(palette="Greens",guide="none", direction = 1) +
    labs(x = "Actual",y = "Prediction", title=title, subtitle=
           paste(
             "Precision at 0.5 threshold:",
             round(precision(actuals,predictions,threshold),3)*100, "%")) +
    scale_x_discrete(labels=c("Default", "Fully Paid")) +
    scale_y_discrete(labels=c("Fully Paid","Default"), limits=rev) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) 
}

plot_precision_vs_npred <- function(actuals, predictions, title=""){ ### non serve più, cancellare T.T
  p <- vector("list", length(predictions))
  names(p) <- names(predictions)
  n <- vector("list", length(predictions))
  names(n) <- names(predictions)
  for (i in 1:length(predictions)){
    for (threshold in seq(0,1,0.01)){
      p[[i]] <- c(p[[i]],precision(actuals, predictions[[i]], threshold = threshold))
      n[[i]] <- c(n[[i]],length(predictions[[i]][predictions[[i]] >= threshold]))
    }
  }
  
  res <- as.data.frame(list(p=p,n=n))
  res$i <- 1:nrow(res)
  res <- gather(res,var,val,-i) %>% separate(var, c("variable", "Model")) %>% reshape(idvar = c("i","Model"), timevar = "variable", direction = "wide")
  names(res)[c(3,4)] <- c("p","n")
  
  res <- res %>% arrange(Model, p)
  plotly::plot_ly(res) %>%
    plotly::add_trace(x=~p*100, y = ~n/length(actuals)*100, yaxis="y", type="scatter", mode = "lines+markers", color=~Model, opacity = 1) %>% 
    plotly::add_trace(x=~p*100, y = ~round(n/length(actuals)*4e4), yaxis="y2", type="scatter", mode = "lines+markers", color=~Model, opacity = 0, showlegend=F) %>%     
    plotly::layout(title = "Results",
                   xaxis = list(title = 'Precision',  ticksuffix = "%"),
                   yaxis = list(title = 'Percentage of predicted Fully Paid', ticksuffix = "%", gridcolor=rgb(0, 0, 0, max = 255, alpha = 80)),
                   yaxis2= list(tickfont = list(color = "red"), gridcolor=rgb(255, 0, 0, max = 255, alpha = 20), overlaying = "y",side = "right", automargin = T,title = "Number of predicted Fully Paid"),
                   legend = list(x=0.7,y=0.9))
  
}

precision_at_k <- function(actuals, predictions){
  d <- data.frame(actuals = actuals, score = predictions) %>%
    arrange(-score)
  d$k <- 1:nrow(d)
  p_at_k <- rep(0,nrow(d))
  n_relevant <- 0
  for (k in 1:nrow(d)){
    if (d[k,"actuals"] == 1){
      n_relevant <- n_relevant+1
    }
    p_at_k[k] <- n_relevant/k
  }
  p_at_k
}

plot_precision_at_k <- function(actuals, predictions, resolution_k=100){
  test_size <- length(actuals)
  p_at_k <- vector("list", length(predictions))
  names(p_at_k) <- names(predictions)
  for (i in 1:length(predictions)){
    p_at_k[[i]] <- precision_at_k(actuals, predictions[[i]])
  }
  p_at_k <-  p_at_k %>% 
    sapply('[', seq(max(sapply(p_at_k, length)))) %>% 
    as.data.frame()
  p_at_k <- p_at_k %>%
    mutate(k=1:nrow(p_at_k)) %>%
    gather(Model, p,-k) %>%
    filter( k == 1 | k %% resolution_k == 0)
  plotly::plot_ly(p_at_k) %>%
    plotly::add_trace(x=~round(k/test_size*100,3), y = ~p*100, type="scatter", mode = "lines+markers", color=~Model) %>% 
    plotly::add_trace(x=~round(k/test_size*4e4), y = ~p*100, xaxis="x2", type="scatter", mode = "lines+markers", color=~Model, opacity = 0, showlegend=F) %>%     
    plotly::layout(xaxis = list(title = 'K', ticksuffix= "%", gridcolor=rgb(0, 0, 0, max = 255, alpha = 60)),
                   xaxis2= list(tickfont = list(color = "red"), gridcolor=rgb(255, 0, 0, max = 255, alpha = 20), overlaying = "x",side = "top", automargin = T, title = list(text="Expected K")),
                   yaxis = list(title = 'Precision @ K', ticksuffix = "%", gridcolor=rgb(0, 0, 0, max = 255, alpha = 60)),
                   legend = list(x=0.8,y=0.9))
} 

MyCalibrationPlot <- function(actuals, predictions){
  quant.obs <- vector("list", length(predictions))
  names(quant.obs) <- names(predictions)
  for (mod_id in 1:length(predictions)){
    for (i in seq(0,0.9,0.1)){
      tbl <- table(actuals[predictions[[mod_id]] >= i & predictions[[mod_id]] < i+0.1])
      quant.obs[[mod_id]] <- c(quant.obs[[mod_id]],tbl[2]/(tbl[1]+tbl[2]))
    }
  }
  quant.obs <- as.data.frame(quant.obs)
  quant.obs$quantile <- seq(0.05, 0.95, 0.1)
  quant.obs <- gather(quant.obs,Model,val,-quantile)
  ggplot(quant.obs) +
    geom_line(aes(x=quantile,y=val, color=Model)) +
    geom_point(aes(x=quantile,y=val, color=Model)) +
    geom_line(aes(x=quantile,y=quantile), color="gray", linetype = "dashed") +
    scale_x_continuous(labels=scales::percent, limits = c(0,1)) +
    scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
    labs(x = "Estimated Probability of Fully Paid", y = "Actual Probability of Fully Paid", title="Calibration Plot") +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 16)) 
}