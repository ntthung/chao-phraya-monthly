
# Plot utilities ---------------------------------

theme_set(theme_prism(base_size = 10, base_line_size = 0.2, base_fontface = 'plain'))
update_geom_defaults('line', list(size = 0.2))
update_geom_defaults('hline', list(size = 0.2))

skip_label <- function(n) {
  function(x) {
    idx <- seq_along(x)
    x[idx %% n != 1] <- ' '
    x
  }
}

stnLab <- c('p1' = 'P.1 - Ping', 
            'w4' = 'W.4A - Wang', 
            'y17' = 'Y.17 - Yom', 
            'n1' = 'N.1 - Nan')
monthLabShort <- function(x) substr(x, 1, 1)
monthLabNum <- function(x) substr(month.abb, 1, 1)
metLab <- c('R2' = 'R\u00b2', 'RE' = 'RE', 'CE' = 'CE')
lambdaLab <- function(x) paste('\u03bb =', substr(x, 2, 3))

# Handling time -----------------------------------------------------------

make_target <- function(qm) {
  
  qa <- qm[, .(Qa = sum(Q)), by = year]
  qa[, season := 'Ann']
  
  qTar <- rbind(qm[, .(year, season = month.abb[month], Qa = Q)], qa)
  qTar[, season := factor(season, c(month.abb, 'Ann'))]
  setkey(qTar, season)
  qTar[]
}

# Plot a time series contained in a data.table over multiple lines
multi_line_plot <- function(plotDT, var.name, num.lines,
                            daily = FALSE,
                            colour = 'black',
                            xlab = NULL, ylab = NULL, title = NULL) {
  
  DT <- copy(plotDT)
  if (is.null(DT$rep)) DT[, rep := 0]
  DT[, group := rep(1:num.lines, each = ceiling(.N / num.lines), length.out = .N), by = rep]
  L <- length(var.name)
  p <- if (daily) {
    if (L == 1) {
      ggplot(DT) +
        geom_line(aes(date, get(var.name), group = rep), colour = colour)
    } else if (L == 2) {
      ggplot(DT) +
        geom_line(aes(date, get(var.name[1]), colour = var.name[1], group = rep)) +
        geom_line(aes(date, get(var.name[2]), colour = var.name[2], group = rep))
    } else if (L == 3) {
      ggplot(DT) +
        geom_line(aes(date, get(var.name[1]), colour = var.name[1], group = rep)) +
        geom_line(aes(date, get(var.name[2]), colour = var.name[2], group = rep)) +
        geom_line(aes(date, get(var.name[3]), colour = var.name[3], group = rep))
    }
  } else {
    if (L == 1) {
      ggplot(DT) +
        geom_line(aes(year + (month - 1) / 12, get(var.name), group = rep), colour = colour)
    } else if (L == 2) {
      ggplot(DT) +
        geom_line(aes(year + (month - 1) / 12, get(var.name[1]), colour = var.name[1], group = rep)) +
        geom_line(aes(year + (month - 1) / 12, get(var.name[2]), colour = var.name[2], group = rep))
    } else if (L == 3) {
      ggplot(DT) +
        geom_line(aes(year + (month - 1) / 12, get(var.name[1]), colour = var.name[1], group = rep)) +
        geom_line(aes(year + (month - 1) / 12, get(var.name[2]), colour = var.name[2], group = rep)) +
        geom_line(aes(year + (month - 1) / 12, get(var.name[3]), colour = var.name[3], group = rep))
    }
  }
  p <- p +
    facet_wrap(vars(group), ncol = 1, scales = 'free_x') +
    labs(x = xlab, y = ylab, title = title) +
    scale_color_manual(values = colour) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.background = element_blank(),
      strip.text = element_blank())
  
  p
}

# Other utilities ---------------------------------

distance <- function(a, b) sqrt(sum((a - b)^2))

normalize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

rm_null <- function(x) x[!sapply(x, is.null)]

abs_range <- function(x) {
  absMax <- max(abs(range(x)))
  c(-absMax, absMax)
}

roundDT <- function(DT, digits = 2, type = 'numeric') {
  DT[, lapply(.SD,
              function(x) {
                if (is.numeric(x)) {
                  rd <- round(x, digits)
                  if (type == 'numeric') rd else sprintf(glue("%.{digits}f"), rd)
                } else x
              })]
}

'%ni%' <- Negate('%in%')

lapplyrbind <- function(x, fun, ..., id = NULL) rbindlist(lapply(x, fun, ...), idcol = id)
standardize <- function(x, ...) (x - mean(x, ...)) / sd(x, ...)
