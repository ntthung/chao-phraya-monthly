#' Get correlation matrix between two groups of variables.
#'
#' @param x A matrix, with variable names encoded in column names.
#' @param colInd1 An integer or character vector containing the indices or column names of the first group.
#' @param colInd2 An integer or character vector containing the indices or column names of the second group.
#' @return A correlation matrix as returned by `cor()`.

cor_mat <- function(x, colInd1, colInd2) {
  cor(x[, colInd1], x[, colInd2], use = 'pairwise.complete.obs')
}

#' Bootstrapped correlation
#'
#' Calculate the observed correlation and bootstrap correlation quantiles
#' @inheritParams cor_mat
#' @param groupNames A character vector of length 2: the names of the two groups of variables, in that order.
#' @param B Number of bootstrap replicates
#' @param alpha Significance level, e.g. 0.05, 0.1, etc (must be less than 0.5).
#' @return A data.table with seven columns:
#' * The first two columns are the two groups, each row is a combination of variables.
#' * The next three columns are correlation results: low (alpha-th percentile), median, and high ((1-alpha)-th percentile).
#' * signif, which is TRUE if 0 is not between (low, high).
#' * rho0: the sampling correlation, which is the `t0` returned by `boot::tsboot()`.

cor_boot <- function(X, colInd1, colInd2, groupNames, B = 1000, alpha = 0.05) {
  stopifnot(alpha < 0.5)
  N <- nrow(X)
  # To handle the case where character vectors are supplied in colInd1 and colInd2
  names(varNames) <- varNames <- colnames(X)
  corBoot <- boot::tsboot(X, statistic = cor_mat,
                          colInd1 = colInd1, colInd2 = colInd2,
                          R = B, l = ceiling(sqrt(N)), sim = 'geom')
  rho <- matrixStats::colQuantiles(corBoot$t, probs = c(alpha, 0.5, 1 - alpha), type = 8, drop = FALSE)
  corDT <- cbind(CJ(varNames[colInd2], varNames[colInd1], sorted = FALSE), rho)
  setnames(corDT, c(rev(groupNames), 'low', 'median', 'high'))
  corDT[, ':='(rho0 = c(corBoot$t0),
               signif = !between(0, low, high))][]
}

#' Lagged correlation
#' 
#' Calculate bootstrapped correlation at a specific lag between two annual variables
#' @param X,Y two variables to correlate; matrix with named columns
#' @param l lag
#' @param Xyears,Yyears vectors of time for X and Y
#' @param groupNames A character vector of length 2: the names of the two groups of variables, in that order.
#' @param B Number of bootstrap replicates
#' @param alpha Significance level, e.g. 0.05, 0.1, etc (must be less than 0.5).
#' @return same as `cor_boot()`

cor_lag <- function(X, Y, l, Xyears, Yyears, groupNames, B = 1000, alpha = 0.05) {
  ind <- which(Xyears %in% Yyears) - l
  cor_boot(cbind(X[ind, ], Y),
           1:ncol(X),
           1:ncol(Y) + ncol(X),
           groupNames = groupNames,
           B = B,
           alpha = alpha)
}

#' Correlation between streamflow and tree ring proxies
#' 
cor_Q_proxy <- function(QDT, alpha = 0.05) {
  seasons <- unique(QDT$season)
  QMat <- as.matrix(dcast(QDT, year ~ season, value.var = 'Qa')[, -'year'])
  colnames(QMat) <- seasons
  Qyears <- unique(QDT$year)
  set.seed(42)
  rhoCrn <- lapplyrbind(-2:2, function(l) {
    DT <- cor_lag(crnMat, QMat, l, 1748:2005, Qyears, c('site', 'season'), alpha = alpha)
    DT[, lag := l][]
  })
  rhoOxi <- lapplyrbind(0:2, function(l) {
    DT <- cor_lag(oxiMat, QMat, l, 1748:2005, Qyears, c('site', 'season'), alpha = alpha)
    DT[, lag := l][]
  })
  dtCrn <- CJ(s = seasons, l = -2:2, sorted = FALSE)
  dtCrn[, sl := paste0(s, '(', l, ')')]
  dtOxi <- CJ(s = seasons, l =  0:2, sorted = FALSE)
  dtOxi[, sl := paste0(s, '(', l, ')')]
  
  # Convert time variables to factors for plotting
  rhoCrn[, season := factor(season, seasons)]
  rhoOxi[, season := factor(season, seasons)]
  rhoCrn[, month_lag := paste0(season, '(', lag, ')') |> factor(levels = dtCrn$sl)]
  rhoOxi[, month_lag := paste0(season, '(', lag, ')') |> factor(levels = dtOxi$sl)]
  list(rhoCrn = rhoCrn, rhoOxi = rhoOxi)
}

#' Pre-screening by correlation threshold
#' 
prescreening <- function(rhoOut, threshold) {
  poolDT <- rbind(
    rhoOut$rhoCrn[{signif} & abs(rho0) >= threshold, .(rho0, site, lag), by = season],
    rhoOut$rhoOxi[{signif} & abs(rho0) >= threshold, .(rho0, site, lag), by = season]
  )[, .SD[order(abs(rho0), decreasing = TRUE)], by = season
  ][, head(.SD, 20), by = season
  ][, site2 := paste0(site, lag)]
  setkey(poolDT, season)
  poolDT[]
}

#' Plot correlation results
plot_cor <- function(rhoDT) {
  minLag <- min(rhoDT$lag)
  ggplot(rhoDT) +
    geom_hline(yintercept = 0, colour = 'gray') +
    geom_vline(
      xintercept = length(minLag:2) * (1:12) + 0.5, 
      colour = 'gray') +
    geom_linerange(aes(month_lag, ymin = low, ymax = high, alpha = signif, colour = season)) +
    geom_point(aes(month_lag, median, alpha = signif, colour = season)) +
    scale_colour_discrete(name = 'Month') +
    facet_wrap(vars(site), ncol = 2) +
    scale_alpha_manual(name = 'Significance', values = c(0.25, 1)) +
    scale_x_discrete() +
    labs(
      x = glue('Lags {minLag} to +2 years in each month'), 
      y = 'Correlation [-]') +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_line(size = 0.1),
      axis.ticks.x = element_blank(),
      legend.key.width = unit(0.1, 'cm'),
      legend.text = element_text(margin = margin(r = 0.3, unit = 'cm')),
      legend.title = element_text(margin = margin(r = 0.3, unit = 'cm')),
      legend.position = 'top',
      panel.grid.major.y = element_line('gray90', 0.1),
      panel.border = element_rect(NA, 'black', 0.1),
      strip.text = element_text(face = 'plain'),
      strip.background = element_blank(),
      plot.subtitle = element_markdown(face = 'bold', hjust = 0))
}

#' Plot correlation results for one station
plot_cor_station <- function(rhoOut, stnName) {
  p1 <- plot_cor(rhoOut$rhoCrn) + labs(subtitle = glue({stnName}, ' and ring width'))
  p2 <- plot_cor(rhoOut$rhoOxi) + 
    labs(subtitle = glue({stnName}, ' and &delta;<sup>18</sup>O')) +
    theme(legend.position = 'none')
  p1 / p2 + plot_layout(heights = c(5, 1))
}