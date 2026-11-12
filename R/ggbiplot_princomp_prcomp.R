#' Biplot generic for PCA objects
#'
#' @param x A `prcomp` or `princomp` object, or a score matrix.
#' @param ... Additional arguments passed to methods.
#' @return A `ggplot` object.
#' @export
ggbiplot <- function(x, ...) {
  UseMethod("ggbiplot")
}

#' @rdname ggbiplot
#' @method ggbiplot princomp
#' @param choices Integer vector of length 2 indicating axes to plot.
#' @param scale Numeric scaling factor in `[0, 1]`.
#' @param pc.biplot Logical; if `TRUE`, scales by `sqrt(n)`.
#' @export
ggbiplot.princomp <- function(x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, ...) {
  if (length(choices) != 2L) stop("length of choices must be 2")
  if (!length(scores <- x$scores)) {
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), domain = NA)
  }
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
  if (scale != 0) lam <- lam^scale else lam <- 1
  if (pc.biplot) lam <- lam / sqrt(n)
  
  ggbiplot.default(
    t(t(scores[, choices, drop = FALSE]) / lam),
    t(t(x$loadings[, choices, drop = FALSE]) * lam),
    choices = choices,
    ...
  )
}

#' @rdname ggbiplot
#' @method ggbiplot prcomp
#' @param colordata Optional single-column `data.frame` for coloring observations.
#' @param type `"p"` for points (default) or `"t"` for text labels.
#' @export
ggbiplot.prcomp <- function(x, choices = 1:2, scale = 1, pc.biplot = FALSE,
                            colordata = NULL, type = "p", ...) {
  if (length(choices) != 2L) stop("length of choices must be 2")
  if (!length(scores <- x$x)) {
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), domain = NA)
  }
  if (is.complex(scores)) stop("biplots are not defined for complex PCA")
  
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
  if (scale != 0) lam <- lam^scale else lam <- 1
  if (pc.biplot) lam <- lam / sqrt(n)
  
  ggbiplot.default(
    t(t(scores[, choices, drop = FALSE]) / lam),
    t(t(x$rotation[, choices, drop = FALSE]) * lam),
    choices = choices,
    colordata = colordata,
    type = type,
    ...
  )
}

#' @rdname ggbiplot
#' @method ggbiplot default
#' @param y Loading matrix with 2 columns.
#' @param var.axes Logical; draw variable arrows.
#' @param col Character vector of length 2 for observation and variable colors.
#' @param cex Numeric vector of length 2 for text/point scaling.
#' @param pch Numeric point shape (default: 16).
#' @param xlabs Optional observation labels.
#' @param ylabs Optional variable labels.
#' @param expand Numeric expansion factor for variable vectors.
#' @param xlim,ylim Optional numeric vectors of length 2 for axis limits.
#' @param arrow.len Numeric arrow head length in cm (default: 0.2).
#' @param main,sub,xlab,ylab Optional plot titles and axis labels.
#' @param jitter Numeric jitter factor (default: 0).
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment xlim ylim ggtitle coord_fixed
#' @importFrom grid arrow unit
#' @importFrom rlang .data
#' @export
ggbiplot.default <- function(x, y, choices = 1:2, var.axes = TRUE,
                             col = c("black", "red"), cex = c(1, 1), pch = 16,
                             xlabs = NULL, ylabs = NULL, expand = 1,
                             xlim = NULL, ylim = NULL, arrow.len = 0.2,
                             main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                             colordata = NULL, type = "p", jitter = 0, ...) {
  n <- nrow(x)
  p <- nrow(y)
  if (is.null(xlabs)) {
    xlabs <- dimnames(x)[[1L]]
    if (is.null(xlabs)) xlabs <- seq_len(n)
  }
  xlabs <- as.character(xlabs)
  dimnames(x) <- list(xlabs, dimnames(x)[[2L]])
  
  if (is.null(ylabs)) {
    ylabs <- dimnames(y)[[1L]]
    if (is.null(ylabs)) ylabs <- paste("Var", seq_len(p))
  }
  ylabs <- as.character(ylabs)
  dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
  
  if (length(col) == 1L) col <- c(col, col)
  
  unsigned.range <- function(v) c(-abs(min(v, na.rm = TRUE)), abs(max(v, na.rm = TRUE)))
  rangx1 <- unsigned.range(x[, 1L])
  rangx2 <- unsigned.range(x[, 2L])
  rangy1 <- unsigned.range(y[, 1L])
  rangy2 <- unsigned.range(y[, 2L])
  
  if (is.null(xlim) && is.null(ylim)) {
    xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
  } else if (is.null(xlim)) {
    xlim <- rangx1
  } else if (is.null(ylim)) {
    ylim <- rangx2
  }
  ratio <- max(rangy1 / rangx1, rangy2 / rangy2) / expand
  
  xlim[1] <- ifelse(xlim[1] > 0, xlim[1] * 0.85, xlim[1] * 1.15)
  xlim[2] <- ifelse(xlim[2] < 0, xlim[2] * 0.85, xlim[2] * 1.15)
  ylim[1] <- ifelse(ylim[1] > 0, ylim[1] * 0.85, ylim[1] * 1.15)
  ylim[2] <- ifelse(ylim[2] < 0, ylim[2] * 0.85, ylim[2] * 1.15)
  
  dfx <- as.data.frame(x)
  ax1 <- names(dfx)[1]
  ax2 <- names(dfx)[2]
  dfx$xlabs <- xlabs
  
  if (jitter > 0) {
    dfx[[1]] <- base::jitter(dfx[[1]], factor = jitter)
    dfx[[2]] <- base::jitter(dfx[[2]], factor = jitter)
  }
  
  if (!is.null(colordata) && nrow(as.data.frame(colordata)) == nrow(x)) {
    colordata <- as.data.frame(colordata)
    colorname <- names(colordata)[1]
    dfx[[colorname]] <- colordata[[1]]
    
    p_plot <- ggplot(dfx, aes(x = .data[[ax1]], y = .data[[ax2]])) +
      xlim(xlim[1], xlim[2]) +
      ylim(ylim[1], ylim[2]) +
      ggtitle(main, subtitle = sub) +
      coord_fixed()
    
    if (type == "p") {
      p_plot <- p_plot + geom_point(aes(color = .data[[colorname]]), shape = pch)
    } else {
      p_plot <- p_plot + geom_text(aes(label = .data$xlabs, color = .data[[colorname]]))
    }
  } else {
    p_plot <- ggplot(dfx, aes(x = .data[[ax1]], y = .data[[ax2]])) +
      xlim(xlim[1], xlim[2]) +
      ylim(ylim[1], ylim[2]) +
      ggtitle(main, subtitle = sub) +
      coord_fixed()
    
    if (type == "p") {
      p_plot <- p_plot + geom_point(shape = pch, color = col[1])
    } else {
      p_plot <- p_plot + geom_text(aes(label = .data$xlabs), color = col[1])
    }
  }
  
  if (isTRUE(var.axes)) {
    dfy <- as.data.frame(y / ratio)
    vay1 <- names(dfy)[1]
    vay2 <- names(dfy)[2]
    dfy$vars <- rownames(y)
    dfys <- dfy
    dfys[, 1:2] <- dfys[, 1:2] * 0.8
    
    p_plot <- p_plot +
      geom_text(
        data = dfy,
        aes(x = .data[[vay1]], y = .data[[vay2]], label = .data$vars),
        inherit.aes = FALSE, color = col[2]
      ) +
      geom_segment(
        data = dfys,
        aes(x = 0, y = 0, xend = .data[[vay1]], yend = .data[[vay2]]),
        arrow = grid::arrow(length = grid::unit(arrow.len, "cm")),
        inherit.aes = FALSE, color = col[2]
      )
  }
  
  p_plot
}
