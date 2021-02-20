model_objects <- function(m) {
  # m response has to be TRUE/FALSE
  TF <- c(TRUE, FALSE)
  out <- list()
  out$m <- m
  out$actual <- factor(
    unname(model.response(model.frame(m))),
    levels = TF)
  N <- nrow(m$data)
  out$prob <- predict(m, type = "response")
  out$prob_lin <- predict(m)
  out$class <- ifelse(out$prob > 0.5, TRUE, FALSE) %>% 
    factor(labels = TF, levels = TF)
  out$cm <- caret::confusionMatrix(
    out$class, out$actual, positive = 'TRUE')
  
  out$broom <- broom::tidy(m)
  out$broom$exp_estimate <- exp(out$broom$estimate)
  out$roc <- suppressMessages(pROC::roc(out$actual, out$prob))
  out$auc <- out$roc$auc %>% as.numeric
  out$chisq <- 1 - pchisq(
    m$null.deviance - m$deviance, m$df.null - m$df.residual)
  out$confint <- suppressMessages(suppressWarnings(confint(m)))
  out$R2 <- (1-exp((m$deviance-m$null.deviance)/N))/(1-exp(-m$null.deviance/N))
  
  A <- out$cm$table[1, 1]
  B <- out$cm$table[1, 2]
  C <- out$cm$table[2, 1]
  D <- out$cm$table[2, 2]
  
  out$accuracy <- (A + D) / (A + B + C + D)
  out$classification_error_rate <- (B + C) / (A + B + C + D)
  out$precision <- A / (A + B)
  out$sensitivity <- recall <- A / (A + C)
  out$specificity <- D / (D + B)
  out$F1_score <- 
    (2 * out$precision * out$sensitivity) / (out$precision + out$sensitivity)
  out$PPV <- A / (A + B)
  out$NPV <- D / (C + D)
  
  return(out)
}
halfnorm_plot <- function(mo) {
  faraway::halfnorm(hatvalues(mo$m))
}
plot_roc <- function(mo) {
  orig_pty <- par()$pty
  par(pty="s")
  on.exit(par(pty = orig_pty))
  plot(
    mo$roc, 
    print.auc = TRUE, 
    auc.polygon = TRUE, 
    grid = c(0.1, 0.2),
    grid.col = c("green", "red"), 
    max.auc.polygon = TRUE,
    auc.polygon.col = "lightblue", 
    print.thres = FALSE)
}
hosmer_lemeshow <- function(mo) {
  out <- list()
  M <- floor(nrow(mo$m$data) / 30)
  qs <- (0:M)/M
  breaks <- unique(quantile(mo$prob_lin, qs))
  buckets <- cut(mo$prob_lin, breaks = breaks)
  
  d <- data.frame(
    buckets = buckets, 
    class = as.integer(mo$actual) - 1, 
    prob = mo$prob)
  
  gd <- d %>% 
    group_by(buckets) %>% 
    summarise(
      y = sum(class), 
      ppred = mean(prob), 
      count = n()) %>% 
    mutate(
      se.fit = sqrt(ppred*(1-ppred)/count))
  
  out$plot <- ggplot(
    gd,
    aes(x=ppred,y=y/count,ymin=y/count-2*se.fit,ymax=y/count+2*se.fit))+
    geom_point()+
    geom_linerange(color=grey(0.75))+
    geom_abline(intercept=0,slope=1)+
    xlab("Predicted Probability")+
    ylab("Observed Proportion")
  
  out$hlstat <- with(
    gd, 
    sum( (y-count*ppred)^2/(count*ppred*(1-ppred))))
  
  out$pval <- 1 - pchisq(out$hlstat, nrow(gd) - 1)
  return(out)
}
info <- function(x, ...) {
  UseMethod("info")
}
info.data.frame <- function(dataframe) {
  
  out <- NULL
  for (i in 1:ncol(dataframe)) {
    
    v <- dataframe[, i, drop = TRUE]
    num <- is.numeric(v) | is.logical(v)
    lv <- length(v)
    vn <- v[!is.na(v)]
    uniques <- length(unique(vn))
    typ <- class(v)
    
    if (num) {
      qv <- stats::quantile(
        vn, 
        probs = c(0, 0.25, 0.5, 0.75, 1))
      sdv <- sd(vn)
      muv <- mean(vn)
      zero_count = sum(vn == 0)
      lvls <- NA
    } else {
      qv <- rep(NA, 5)
      sdv <- NA
      muv <- NA
      zero_count = NA
      lvls <- table(v) %>% sort(decreasing = TRUE) %>% 
        names %>% `[`(., 1:min(10, length(.))) %>% paste0(collapse = ', ')
    }
    
    qvr <- data.frame(
      row.names = i,
      column = names(dataframe)[i],
      type = typ,
      min = qv[1],
      q25 = qv[2],
      median = qv[3],
      mean = muv,
      q75 = qv[4],
      max = qv[5],
      sd = sdv,
      zero_count,
      # sd2ratio = sum(abs(vn - muv) < 2 * sdv) / length(vn),
      na_count = lv - length(vn),
      unique_values = uniques,
      levels = lvls)
    
    if (is.null(out)) { 
      out <- qvr } else { out <- rbind(out, qvr) }  
    
  }
  return(out)
}


