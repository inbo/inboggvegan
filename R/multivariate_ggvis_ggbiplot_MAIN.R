# Internal layer helpers to keep cyclomatic complexity <= 15
set_xy_mapping <- function(mapping, ax1, ax2,
                           default_mapping = ggplot2::aes()) {
  if (is.null(mapping)) {
    mapping <- default_mapping
  }
  mapping$x <- as.symbol(ax1)
  mapping$y <- as.symbol(ax2)
  class(mapping) <- "uneval"
  mapping
}

init_biplot_base <- function(df_u) {
  axis_labels <- attr(df_u, "axis.labels")
  if (is.null(axis_labels)) {
    warning("site_data should have the attribute axis.labels.")
    axis_labels <- c("", "")
  }
  mybreaks <- pretty(c(df_u[[1L]], df_u[[2L]]), n = 5L, eps.correct = 1L)
  ax1 <- names(df_u)[1L]
  ax2 <- names(df_u)[2L]

  ggplot2::ggplot(
    data = df_u,
    ggplot2::aes(x = .data[[ax1]], y = .data[[ax2]])
  ) +
    ggplot2::xlab(axis_labels[1L]) +
    ggplot2::ylab(axis_labels[2L]) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(breaks = mybreaks) +
    ggplot2::scale_y_continuous(breaks = mybreaks)
}

add_site_layer <- function(g,
                           df_u,
                           site_geom,
                           site_mapping,
                           base_color,
                           base_shape,
                           base_size) {
  if (site_geom == "blank") {
    return(g)
  }
  site_mapping <- set_xy_mapping(site_mapping, names(df_u)[1L], names(df_u)[2L])
  g + geom_selector(
    data = df_u,
    geom = site_geom,
    mapping = site_mapping,
    base_color = base_color,
    base_shape = base_shape,
    base_size = base_size
  )
}

add_centroid_layer <- function(g,
                               df_u,
                               centroid_geom,
                               centroid_mapping,
                               base_color,
                               base_size) {
  if (centroid_geom == "blank") {
    return(g)
  }
  if (is.null(centroid_mapping)) {
    centroid_mapping <- ggplot2::aes(group = 1L)
  }
  c_args <- list(mapping = centroid_mapping, data = df_u, geom = centroid_geom)
  if (is.null(centroid_mapping$colour) && is.null(centroid_mapping$color)) {
    c_args$color <- base_color
  }
  if (is.null(centroid_mapping$size)) {
    c_args$size <- base_size
  }
  g + do.call(stat_centroid, c_args)
}

add_ellipse_layer <- function(g,
                              df_u,
                              ax1,
                              ax2,
                              ellipse_geom,
                              ellipse_mapping,
                              ellipse_level,
                              base_color) {
  if (ellipse_geom == "blank") {
    return(g)
  }
  ellipse_mapping <- set_xy_mapping(ellipse_mapping, ax1, ax2)
  has_col <- !is.null(ellipse_mapping$colour) || !is.null(ellipse_mapping$color)
  if (!has_col) {
    g + ggplot2::stat_ellipse(
      mapping = ellipse_mapping,
      data = df_u,
      level = ellipse_level,
      inherit.aes = FALSE,
      color = base_color
    )
  } else {
    g + ggplot2::stat_ellipse(
      mapping = ellipse_mapping,
      data = df_u,
      level = ellipse_level,
      inherit.aes = FALSE
    )
  }
}

add_species_layer <- function(g,
                              df_v,
                              species_geom,
                              species_mapping,
                              base_color,
                              base_shape,
                              base_size) {
  if (is.null(df_v) || species_geom == "blank") {
    return(g)
  }
  species_mapping <- set_xy_mapping(
    species_mapping,
    names(df_v)[1L],
    names(df_v)[2L]
  )
  has_col <- !is.null(species_mapping$colour) || !is.null(species_mapping$color)

  if (species_geom != "arrow") {
    return(
      g + geom_selector(
        mapping = species_mapping,
        data = df_v,
        geom = species_geom,
        base_color = base_color,
        base_shape = base_shape,
        base_size = base_size,
        inherit.aes = FALSE
      )
    )
  }

  if (!has_col) {
    g <- g + geom_arrow(
      data = df_v,
      mapping = species_mapping,
      inherit.aes = FALSE,
      color = base_color
    )
  } else {
    g <- g + geom_arrow(
      data = df_v,
      mapping = species_mapping,
      inherit.aes = FALSE
    )
  }

  if (!is.null(species_mapping$label)) {
    if (!has_col) {
      g <- g + geom_arrowlabel(
        data = df_v,
        mapping = species_mapping,
        inherit.aes = FALSE,
        color = base_color,
        size = base_size
      )
    } else {
      g <- g + geom_arrowlabel(
        data = df_v,
        mapping = species_mapping,
        inherit.aes = FALSE,
        size = base_size
      )
    }
  }
  g
}

add_corcircle_layer <- function(g, df_v, cor_circle_geom, base_color) {
  if (cor_circle_geom == "blank") {
    return(g)
  }
  r <- attr(df_v, "r")
  if (is.null(r) || !is.numeric(r) || is.na(r)) {
    r <- 1
  }
  g + geom_corcircle(radius = r, color = base_color)
}

add_lc_layer <- function(g,
                         df_lc,
                         lc_geom,
                         lc_mapping,
                         base_color,
                         base_shape,
                         base_size) {
  if (is.null(df_lc) || lc_geom == "blank") {
    return(g)
  }
  lc_mapping <- set_xy_mapping(lc_mapping, names(df_lc)[1L], names(df_lc)[2L])
  g + geom_selector(
    data = df_lc,
    geom = lc_geom,
    mapping = lc_mapping,
    base_color = base_color,
    base_shape = base_shape,
    base_size = base_size,
    inherit.aes = FALSE
  )
}

add_bp_layer <- function(g,
                         df_bp,
                         bp_geom,
                         bp_mapping,
                         base_color,
                         base_shape,
                         base_size) {
  if (is.null(df_bp) || bp_geom == "blank") {
    return(g)
  }
  bp_mapping <- set_xy_mapping(bp_mapping, names(df_bp)[1L], names(df_bp)[2L])
  if (bp_geom == "arrow") {
    g <- g +
      geom_arrow(
        data = df_bp,
        mapping = bp_mapping,
        inherit.aes = FALSE,
        color = base_color
      ) +
      geom_arrowlabel(
        data = df_bp,
        mapping = bp_mapping,
        inherit.aes = FALSE,
        color = base_color,
        size = ggplot2::rel(base_size)
      )
  } else if (bp_geom == "point") {
    g <- g + ggplot2::geom_point(
      data = df_bp,
      mapping = bp_mapping,
      inherit.aes = FALSE,
      color = base_color,
      shape = base_shape,
      size = ggplot2::rel(base_size)
    )
  } else if (bp_geom == "text") {
    if (is.null(bp_mapping$label)) {
      bp_mapping$label <- as.symbol("Row.names")
    }
    g <- g + ggplot2::geom_text(
      data = df_bp,
      mapping = bp_mapping,
      inherit.aes = FALSE,
      color = base_color,
      size = ggplot2::rel(base_size)
    )
  }
  g
}

add_cn_layer <- function(g,
                         df_cn,
                         cn_geom,
                         cn_mapping,
                         base_color,
                         base_shape,
                         base_size) {
  if (is.null(df_cn) || cn_geom == "blank") {
    return(g)
  }
  cn_mapping <- set_xy_mapping(
    cn_mapping,
    names(df_cn)[1L],
    names(df_cn)[2L],
    default_mapping = ggplot2::aes(label = .data$.level)
  )
  g + geom_selector(
    data = df_cn,
    geom = cn_geom,
    mapping = cn_mapping,
    base_color = base_color,
    base_shape = base_shape,
    base_size = base_size,
    inherit.aes = FALSE
  )
}

#' Create a ggplot-based biplot for multivariate analysis objects
#'
#' This function creates biplots (visualization of both observations and
#' variables) for multivariate analysis objects including `vegan` ordination
#' objects (`rda`, `cca`, `capscale`) and standard R PCA objects (`prcomp`,
#' `princomp`).
#'
#' @param x An ordination object (`rda`, `cca`, `capscale`, `prcomp`, or
#'   `princomp`).
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A `ggplot` object representing the biplot.
#' @export
#' @rdname ggbiplot_vegan
#'
#' @examples
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' ggbiplot_vegan(vare_rda)
ggbiplot_vegan <- function(x, ...) {
  UseMethod("ggbiplot_vegan")
}

#' @describeIn ggbiplot_vegan Method for `rda` objects
#' @method ggbiplot_vegan rda
#' @export
ggbiplot_vegan.rda <- function(x, ...) {
  ggbiplot_vegan.default(x, ...)
}

#' @describeIn ggbiplot_vegan Method for `cca` objects
#' @method ggbiplot_vegan cca
#' @export
ggbiplot_vegan.cca <- function(x, ...) {
  ggbiplot_vegan.default(x, ...)
}

#' @describeIn ggbiplot_vegan Method for `capscale` objects
#' @method ggbiplot_vegan capscale
#' @export
ggbiplot_vegan.capscale <- function(x, ...) {
  ggbiplot_vegan.default(x, ...)
}

#' @describeIn ggbiplot_vegan Method for `princomp` objects
#' @method ggbiplot_vegan princomp
#' @export
ggbiplot_vegan.princomp <- function(x, ...) {
  ggbiplot.princomp(x, ...)
}

#' @describeIn ggbiplot_vegan Method for `prcomp` objects
#' @method ggbiplot_vegan prcomp
#' @export
ggbiplot_vegan.prcomp <- function(x, ...) {
  ggbiplot.prcomp(x, ...)
}
#nolint start
#' @describeIn ggbiplot_vegan Default method for creating biplots with vegan
#'   objects
#' @param choices Integer vector of length 2 specifying which axes to plot.
#' @param scaling Integer (`1` or `2`) or character specifying the scaling.
#' @param site_data Optional data frame with descriptor variables for sites.
#' @param site_merge_by How to merge `site_data`: `"row.names"`,
#'   `"row.numbers"`, or column name.
#' @param species_data Optional data frame with descriptor variables for
#'   species.
#' @param species_merge_by How to merge `species_data`.
#' @param site_geom `"blank"`, `"point"` (default), or `"text"`.
#' @param site_mapping `ggplot2` aesthetic mapping for sites.
#' @param centroid_geom `"blank"` (default), `"point"`, or `"text"`.
#' @param centroid_mapping `ggplot2` aesthetic mapping for centroids.
#' @param ellipse_geom `"blank"` (default) or `"line"`.
#' @param ellipse_mapping `ggplot2` aesthetic mapping for ellipses.
#' @param species_geom `"arrow"` (default), `"text"`, `"point"`, or `"blank"`.
#' @param species_mapping `ggplot2` aesthetic mapping for species.
#' @param cor_circle_geom `"blank"` (default) or `"line"`.
#' @param cor_circle_mapping `ggplot2` aesthetic mapping for correlation circle.
#' @param lc_geom `"blank"` (default), `"point"`, or `"text"`.
#' @param lc_mapping `ggplot2` aesthetic mapping for linear constraints.
#' @param bp_geom `"blank"` (default), `"arrow"`, `"text"`, or `"point"`.
#' @param bp_mapping `ggplot2` aesthetic mapping for continuous environmental
#'   variables.
#' @param cn_geom `"blank"` (default), `"point"`, or `"text"`.
#' @param cn_mapping `ggplot2` aesthetic mapping for discrete environmental
#'   variables.
#' @param species_abbrev Logical; abbreviate species names (default: `FALSE`).
#' @param species_adjust Numeric adjustment factor for species labels
#'   (default: `1.1`).
#' @param ellipse_level Numeric probability level for ellipses
#'   (default: `0.68`).
#' @param base_colors Character vector of length 3 for sites, species, and env
#'   variables.
#' @param base_shapes Numeric vector of length 3 for sites, species, and env
#'   variables.
#' @param base_sizes Numeric vector of length 3 for sites, species, and env
#'   variables.
#' @param legend_position `"right"` (default), `"top"`, `"bottom"`, `"left"`,
#'   or `"none"`.
#' @importFrom ggplot2 ggplot aes xlab ylab coord_equal scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous stat_ellipse theme labs rel
#' @importFrom rlang .data
#' @export
#' @method ggbiplot_vegan default 
#nolint end
ggbiplot_vegan.default <- function(
  x,
  choices = c(1L, 2L),
  scaling = 2,
  site_data = NULL,
  site_merge_by = "row.names",
  species_data = NULL,
  species_merge_by = "row.names",
  site_geom = "point",
  site_mapping = aes(label = .data$Row.names),
  centroid_geom = "blank",
  centroid_mapping = aes(group = 1L),
  ellipse_geom = "blank",
  ellipse_mapping = NULL,
  species_geom = "arrow",
  species_mapping = aes(label = .data$Row.names),
  cor_circle_geom = "blank",
  cor_circle_mapping = NULL,
  lc_geom = "blank",
  lc_mapping = aes(label = .data$Row.names),
  bp_geom = "blank",
  bp_mapping = NULL,
  cn_geom = "blank",
  cn_mapping = aes(label = .data$.level),
  species_abbrev = FALSE,
  species_adjust = 1.1,
  ellipse_level = 0.68,
  base_colors = c("black", "red", "blue"),
  base_shapes = c(16, 16, 16),
  base_sizes = c(2, 2, 2),
  legend_position = "right",
  ...
) {
  df_u <- get_display_data(
    x,
    data = site_data,
    choices = choices,
    display = "sites",
    scaling = scaling,
    merge_by = site_merge_by
  )
  df_v <- get_display_data(
    x,
    data = species_data,
    choices = choices,
    display = "species",
    scaling = scaling,
    merge_by = species_merge_by,
    species_abbrev = species_abbrev,
    species_adjust = species_adjust
  )
  df_lc <- get_display_data(
    x,
    data = site_data,
    choices = choices,
    display = "lc",
    scaling = scaling,
    merge_by = site_merge_by
  )
  df_bp <- get_display_data(
    x,
    choices = choices,
    scaling = scaling,
    display = "bp",
    species_adjust = species_adjust
  )
  df_cn <- get_display_data(
    x,
    choices = choices,
    scaling = scaling,
    display = "cn"
  )

  ax1 <- names(df_u)[1L]
  ax2 <- names(df_u)[2L]

  g <- init_biplot_base(df_u)
  g <- add_site_layer(
    g,
    df_u,
    site_geom,
    site_mapping,
    base_colors[1L],
    base_shapes[1L],
    base_sizes[1L]
  )
  g <- add_centroid_layer(
    g,
    df_u,
    centroid_geom,
    centroid_mapping,
    base_colors[1L],
    base_sizes[1L]
  )
  g <- add_ellipse_layer(
    g,
    df_u,
    ax1,
    ax2,
    ellipse_geom,
    ellipse_mapping,
    ellipse_level,
    base_colors[1L]
  )
  g <- add_species_layer(
    g,
    df_v,
    species_geom,
    species_mapping,
    base_colors[2L],
    base_shapes[2L],
    base_sizes[2L]
  )
  g <- add_corcircle_layer(g, df_v, cor_circle_geom, base_colors[2L])
  g <- add_lc_layer(
    g,
    df_lc,
    lc_geom,
    lc_mapping,
    base_colors[1L],
    base_shapes[1L],
    base_sizes[1L]
  )
  g <- add_bp_layer(
    g,
    df_bp,
    bp_geom,
    bp_mapping,
    base_colors[3L],
    base_shapes[3L],
    base_sizes[3L]
  )
  g <- add_cn_layer(
    g,
    df_cn,
    cn_geom,
    cn_mapping,
    base_colors[3L],
    base_shapes[3L],
    base_sizes[3L]
  )

  g +
    labs(colour = NULL, shape = NULL) +
    theme(legend.position = legend_position)
}
