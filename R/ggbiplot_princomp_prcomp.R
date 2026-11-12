# Internal helpers for ggbiplot.default
compute_biplot_limits <- function(x, y, xlim, ylim, expand) {
  unsigned_range <- function(v) {
    c(-abs(min(v, na.rm = TRUE)), abs(max(v, na.rm = TRUE)))
  }
  rangx1 <- unsigned_range(x[, 1L])
  rangx2 <- unsigned_range(x[, 2L])
  rangy1 <- unsigned_range(y[, 1L])
  rangy2 <- unsigned_range(y[, 2L])

  if (is.null(xlim) && is.null(ylim)) {
    xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
  } else if (is.null(xlim)) {
    xlim <- rangx1
  } else if (is.null(ylim)) {
    ylim <- rangx2
  }
  ratio <- max(rangy1 / rangx1, rangy2 / rangy2) / expand

  pad_limit <- function(lim) {
    low <- if (lim[1L] > 0) lim[1L] * 0.85 else lim[1L] * 1.15
    high <- if (lim[2L] < 0) lim[2L] * 0.85 else lim[2L] * 1.15
    c(low, high)
  }

  list(xlim = pad_limit(xlim), ylim = pad_limit(ylim), ratio = ratio)
}

build_pca_obs_plot <- function(dfx,
                               ax1,
                               ax2,
                               colordata,
                               type,
                               pch,
                               col_obs,
                               xlim,
                               ylim,
                               main,
                               sub) {
  p_plot <- ggplot2::ggplot(
    dfx,
    ggplot2::aes(x = .data[[ax1]], y = .data[[ax2]])
  ) +
    ggplot2::xlim(xlim[1L], xlim[2L]) +
    ggplot2::ylim(ylim[1L], ylim[2L]) +
    ggplot2::ggtitle(main, subtitle = sub) +
    ggplot2::coord_fixed()

  if (!is.null(colordata) && nrow(as.data.frame(colordata)) == nrow(dfx)) {
    colordata <- as.data.frame(colordata)
    colorname <- names(colordata)[1L]
    dfx[[colorname]] <- colordata[[1L]]
    p_plot$data <- dfx

    if (type == "p") {
      p_plot <- p_plot + ggplot2::geom_point(
        ggplot2::aes(color = .data[[colorname]]),
        shape = pch
      )
    } else {
      p_plot <- p_plot + ggplot2::geom_text(
        ggplot2::aes(label = .data$xlabs, color = .data[[colorname]])
      )
    }
  } else if (type == "p") {
    p_plot <- p_plot + ggplot2::geom_point(shape = pch, color = col_obs)
  } else {
    p_plot <- p_plot + ggplot2::geom_text(
      ggplot2::aes(label = .data$xlabs),
      color = col_obs
    )
  }
  p_plot
}

#' Biplot generic for PCA objects
#'
#' @param x A `prcomp` or `princomp` object, or a score matrix.
#' @param ... Additional arguments passed to methods.
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' pca <- prcomp(iris[, 1:4], scale. = TRUE)
#' ggbiplot(pca, colordata = iris[, 5, drop = FALSE])
ggbiplot <- function(x, ...) {
  UseMethod("ggbiplot")
}

#' @rdname ggbiplot
#' @method ggbiplot princomp
#' @param choices Integer vector of length 2 indicating axes to plot.
#' @param scale Numeric scaling factor in `[0, 1]`.
#' @param pc.biplot Logical; if `TRUE`, scales by `sqrt(n)`.
#' @export
# nolint start: object_name_linter.
ggbiplot.princomp <- function(x,
                              choices = c(1L, 2L),
                              scale = 1,
                              pc.biplot = FALSE,
                              ...) {
  # nolint end
  if (length(choices) != 2L) {
    stop("length of choices must be 2")
  }
  scores <- x$scores
  if (!length(scores)) {
    stop(
      gettextf("object '%s' has no scores", deparse(substitute(x))),
      domain = NA
    )
  }
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) {
    warning("'scale' is outside [0, 1]")
  }
  lam <- if (scale != 0) lam^scale else 1
  if (pc.biplot) {
    lam <- lam / sqrt(n)
  }

  ggbiplot.default(
    t(t(scores[, choices, drop = FALSE]) / lam),
    t(t(x$loadings[, choices, drop = FALSE]) * lam),
    choices = choices,
    ...
  )
}

#' @rdname ggbiplot
#' @method ggbiplot prcomp
#' @param colordata Optional single-column `data.frame` for coloring
#'   observations.
#' @param type `"p"` for points (default) or `"t"` for text labels.
#' @export
# nolint start: object_name_linter.
ggbiplot.prcomp <- function(x,
                            choices = c(1L, 2L),
                            scale = 1,
                            pc.biplot = FALSE,
                            colordata = NULL,
                            type = "p",
                            ...) {
  # nolint end
  if (length(choices) != 2L) {
    stop("length of choices must be 2")
  }
  scores <- x$x
  if (!length(scores)) {
    stop(
      gettextf("object '%s' has no scores", deparse(substitute(x))),
      domain = NA
    )
  }
  if (is.complex(scores)) {
    stop("biplots are not defined for complex PCA")
  }

  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) {
    warning("'scale' is outside [0, 1]")
  }
  lam <- if (scale != 0) lam^scale else 1
  if (pc.biplot) {
    lam <- lam / sqrt(n)
  }

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
#' @param pch Numeric point shape (default: `16`).
#' @param xlabs Optional observation labels.
#' @param ylabs Optional variable labels.
#' @param expand Numeric expansion factor for variable vectors.
#' @param xlim,ylim Optional numeric vectors of length 2 for axis limits.
#' @param arrow.len Numeric arrow head length in cm (default: `0.2`).
#' @param main,sub,xlab,ylab Optional plot titles and axis labels.
#' @param jitter Numeric jitter factor (default: `0`).
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment xlim ylim
#' @importFrom ggplot2 ggtitle coord_fixed
#' @importFrom grid arrow unit
#' @importFrom rlang .data
#' @export
# nolint start: object_name_linter.
ggbiplot.default <- function(x,
                             y,
                             choices = c(1L, 2L),
                             var.axes = TRUE,
                             col = c("black", "red"),
                             cex = c(1, 1),
                             pch = 16,
                             xlabs = NULL,
                             ylabs = NULL,
                             expand = 1,
                             xlim = NULL,
                             ylim = NULL,
                             arrow.len = 0.2,
                             main = NULL,
                             sub = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             colordata = NULL,
                             type = "p",
                             jitter = 0,
                             ...) {
  # nolint end
  if (is.null(xlabs)) {
    xlabs <- dimnames(x)[[1L]]
    if (is.null(xlabs)) {
      xlabs <- seq_len(nrow(x))
    }
  }
  xlabs <- as.character(xlabs)
  dimnames(x) <- list(xlabs, dimnames(x)[[2L]])

  if (is.null(ylabs)) {
    ylabs <- dimnames(y)[[1L]]
    if (is.null(ylabs)) {
      ylabs <- paste("Var", seq_len(nrow(y)))
    }
  }
  ylabs <- as.character(ylabs)
  dimnames(y) <- list(ylabs, dimnames(y)[[2L]])

  if (length(col) == 1L) {
    col <- c(col, col)
  }

  lims <- compute_biplot_limits(x, y, xlim, ylim, expand)
  dfx <- as.data.frame(x)
  ax1 <- names(dfx)[1L]
  ax2 <- names(dfx)[2L]
  dfx$xlabs <- xlabs

  if (jitter > 0) {
    dfx[[1L]] <- base::jitter(dfx[[1L]], factor = jitter)
    dfx[[2L]] <- base::jitter(dfx[[2L]], factor = jitter)
  }

  p_plot <- build_pca_obs_plot(
    dfx = dfx,
    ax1 = ax1,
    ax2 = ax2,
    colordata = colordata,
    type = type,
    pch = pch,
    col_obs = col[1L],
    xlim = lims$xlim,
    ylim = lims$ylim,
    main = main,
    sub = sub
  )

  if (isTRUE(var.axes)) {
    dfy <- as.data.frame(y / lims$ratio)
    vay1 <- names(dfy)[1L]
    vay2 <- names(dfy)[2L]
    dfy$vars <- rownames(y)
    dfys <- dfy
    dfys[, c(1L, 2L)] <- dfys[, c(1L, 2L)] * 0.8

    p_plot <- p_plot +
      geom_text(
        data = dfy,
        aes(x = .data[[vay1]], y = .data[[vay2]], label = .data$vars),
        inherit.aes = FALSE,
        color = col[2L]
      ) +
      geom_segment(
        data = dfys,
        aes(x = 0, y = 0, xend = .data[[vay1]], yend = .data[[vay2]]),
        arrow = grid::arrow(length = grid::unit(arrow.len, "cm")),
        inherit.aes = FALSE,
        color = col[2L]
      )
  }

  p_plot
}
