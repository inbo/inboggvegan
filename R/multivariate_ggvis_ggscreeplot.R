#' Create a screeplot for principal components and ordination analyses
#'
#' Creates a `ggplot2`-based screeplot showing the proportion of variance
#' explained by each component/axis in a multivariate analysis. Supports
#' standard R PCA objects (`prcomp`, `princomp`), `MASS::lda`, and `vegan`
#' ordination objects (`rda`, `cca`, `capscale`).
#'
#' @param pcobj An ordination or PCA object (`prcomp`, `princomp`, `rda`,
#'   `cca`, `capscale`, or `lda`).
#' @param type Character string specifying the type of screeplot:
#'   * `"both"` (default): Both proportion and cumulative explained variance.
#'   * `"pev"`: Proportion of Explained Variance.
#'   * `"cev"`: Cumulative Explained Variance.
#' @param max_axes Optional integer limiting the maximum number of axes shown
#'   on the x-axis.
#' @param bstick Logical; if `TRUE`, overlays the broken-stick distribution
#'   expectation using `vegan::bstick()` when supported.
#'
#' @return A `ggplot` object showing the screeplot.
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' pca <- prcomp(USArrests, scale. = TRUE)
#' ggscreeplot(pca, type = "both")
#'
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' ggscreeplot(vare_rda, type = "pev", max_axes = 10, bstick = TRUE)
ggscreeplot <- function(pcobj,
                        type = c("both", "pev", "cev"),
                        max_axes = NULL,
                        bstick = FALSE) {
  type <- match.arg(type)
  types_to_plot <- if (type == "both") c("pev", "cev") else type

  if (inherits(pcobj, c("rda", "cca", "capscale"))) {
    d <- vegan::eigenvals(pcobj)
    d <- d[d > 0]
  } else if (inherits(pcobj, c("princomp", "prcomp", "lda"))) {
    d <- pcobj$sdev^2
  } else {
    stop("Unsupported object class: ", paste(class(pcobj), collapse = ", "))
  }

  total_var <- sum(d)
  pev_vec <- as.numeric(d / total_var)
  cev_vec <- as.numeric(cumsum(d) / total_var)
  k <- length(d)

  if (!is.null(max_axes)) {
    k <- min(k, as.integer(max_axes))
    pev_vec <- pev_vec[seq_len(k)]
    cev_vec <- cev_vec[seq_len(k)]
  }

  df_list <- list()
  if ("pev" %in% types_to_plot) {
    df_list$pev <- data.frame(
      pc_axis = seq_len(k),
      yvar = pev_vec,
      plot_type = "pev"
    )
  }
  if ("cev" %in% types_to_plot) {
    df_list$cev <- data.frame(
      pc_axis = seq_len(k),
      yvar = cev_vec,
      plot_type = "cev"
    )
  }
  if (isTRUE(bstick)) {
    bs <- tryCatch(vegan::bstick(pcobj), error = function(e) NULL)
    if (!is.null(bs)) {
      bs_prop <- as.numeric(bs / total_var)[seq_len(min(k, length(bs)))]
      df_list$bstick <- data.frame(
        pc_axis = seq_along(bs_prop),
        yvar = bs_prop,
        plot_type = "bstick"
      )
    } else {
      warning("Broken-stick model is not available for this ordination object.")
    }
  }

  df <- do.call(rbind, df_list)
  df$plot_type <- factor(
    df$plot_type,
    levels = c("pev", "cev", "bstick"),
    labels = c("proportion", "cumulative\nproportion", "broken stick")
  )
  df <- droplevels(df)

  ggplot(
    data = df,
    aes(x = .data$pc_axis, y = .data$yvar, colour = .data$plot_type)
  ) +
    geom_point() +
    geom_line() +
    xlab("Ordination axis / Principal component") +
    scale_x_continuous(breaks = unique(round(pretty(seq_len(k))))) +
    scale_y_continuous(
      "Proportion of explained variance",
      limits = c(0, 1),
      labels = scales::percent
    )
}
