#' Create a ggplot-based biplot for multivariate analysis objects
#'
#' @param x An ordination object (`rda`, `cca`, `capscale`, `prcomp`, or `princomp`).
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A `ggplot` object representing the biplot.
#' @export
#' @rdname ggbiplot_vegan
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

#' @describeIn ggbiplot_vegan Default method for creating biplots with vegan objects
#' @param choices Integer vector of length 2 specifying which ordination axes to plot.
#' @param scaling Integer (`1` or `2`) or character specifying the scaling to use.
#' @param site_data Optional data frame with descriptor variables for sites.
#' @param site_merge_by How to merge `site_data`: `"row.names"`, `"row.numbers"`, or column name.
#' @param species_data Optional data frame with descriptor variables for species.
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
#' @param bp_mapping `ggplot2` aesthetic mapping for continuous environmental variables.
#' @param cn_geom `"blank"` (default), `"point"`, or `"text"`.
#' @param cn_mapping `ggplot2` aesthetic mapping for discrete environmental variables.
#' @param species_abbrev Logical; abbreviate species names (default: `FALSE`).
#' @param species_adjust Numeric adjustment factor for species labels (default: `1.1`).
#' @param ellipse_level Numeric probability level for ellipses (default: `0.68`).
#' @param base_colors Character vector of length 3 for sites, species, and env variables.
#' @param base_shapes Numeric vector of length 3 for sites, species, and env variables.
#' @param base_sizes Numeric vector of length 3 for sites, species, and env variables.
#' @param legend_position `"right"` (default), `"top"`, `"bottom"`, `"left"`, or `"none"`.
#' @importFrom ggplot2 ggplot aes xlab ylab coord_equal scale_x_continuous scale_y_continuous stat_ellipse theme labs rel
#' @importFrom rlang .data
#' @export
#' @method ggbiplot_vegan default
ggbiplot_vegan.default <- function(
    x,
    choices = 1:2,
    scaling = 2,
    site_data = NULL,
    site_merge_by = "row.names",
    species_data = NULL,
    species_merge_by = "row.names",
    site_geom = "point",
    site_mapping = aes(label = .data$Row.names),
    centroid_geom = "blank",
    centroid_mapping = aes(group = 1),
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
  df.u <- get_display_data(
    x, data = site_data, choices = choices, display = "sites",
    scaling = scaling, merge_by = site_merge_by
  )
  df.v <- get_display_data(
    x, data = species_data, choices = choices, display = "species",
    scaling = scaling, merge_by = species_merge_by,
    species_abbrev = species_abbrev, species_adjust = species_adjust
  )
  df.lc <- get_display_data(
    x, data = site_data, choices = choices, display = "lc",
    scaling = scaling, merge_by = site_merge_by
  )
  df.bp <- get_display_data(
    x, choices = choices, scaling = scaling, display = "bp",
    species_adjust = species_adjust
  )
  df.cn <- get_display_data(
    x, choices = choices, scaling = scaling, display = "cn"
  )
  
  axis.labels <- attr(df.u, "axis.labels")
  if (is.null(axis.labels)) {
    warning("site_data should have the attribute axis.labels that defines the axis labels in the plot.")
    axis.labels <- c("", "")
  }
  
  r <- attr(df.v, "r")
  if (is.null(r) && !is.null(df.v)) {
    r <- sqrt(max(rowSums(df.v[, 1:2, drop = FALSE]^2, na.rm = TRUE)))
  }
  if (is.null(r) || !is.numeric(r) || is.na(r)) {
    r <- 1
  }
  
  mybreaks <- pretty(c(df.u[[1]], df.u[[2]]), n = 5, eps.correct = 1)
  ax1 <- names(df.u)[1]
  ax2 <- names(df.u)[2]
  
  g <- ggplot(data = df.u, aes(x = .data[[ax1]], y = .data[[ax2]])) +
    xlab(axis.labels[1]) +
    ylab(axis.labels[2]) +
    coord_equal() +
    scale_x_continuous(breaks = mybreaks) +
    scale_y_continuous(breaks = mybreaks)
  
  # Sites
  if (site_geom != "blank") {
    if (is.null(site_mapping)) site_mapping <- aes()
    site_mapping$x <- as.symbol(ax1)
    site_mapping$y <- as.symbol(ax2)
    class(site_mapping) <- "uneval"
    g <- g + geom_selector(
      data = df.u, geom = site_geom, mapping = site_mapping,
      base_color = base_colors[1], base_shape = base_shapes[1], base_size = base_sizes[1]
    )
  }
  
  # Site centroids
  if (centroid_geom != "blank") {
    if (is.null(centroid_mapping)) centroid_mapping <- aes(group = 1)
    c_args <- list(mapping = centroid_mapping, data = df.u, geom = centroid_geom)
    if (is.null(centroid_mapping$colour) && is.null(centroid_mapping$color)) {
      c_args$color <- base_colors[1]
    }
    if (is.null(centroid_mapping$size)) {
      c_args$size <- base_sizes[1]
    }
    g <- g + do.call(stat_centroid, c_args)
  }
  
  # Site ellipses
  if (ellipse_geom != "blank") {
    if (is.null(ellipse_mapping)) ellipse_mapping <- aes()
    ellipse_mapping$x <- as.symbol(ax1)
    ellipse_mapping$y <- as.symbol(ax2)
    class(ellipse_mapping) <- "uneval"
    if (is.null(ellipse_mapping$colour) && is.null(ellipse_mapping$color)) {
      g <- g + stat_ellipse(
        mapping = ellipse_mapping, data = df.u,
        level = ellipse_level, inherit.aes = FALSE, color = base_colors[1]
      )
    } else {
      g <- g + stat_ellipse(
        mapping = ellipse_mapping, data = df.u,
        level = ellipse_level, inherit.aes = FALSE
      )
    }
  }
  
  # Species
  if (!is.null(df.v) && species_geom != "blank") {
    if (is.null(species_mapping)) species_mapping <- aes()
    species_mapping$x <- as.symbol(names(df.v)[1])
    species_mapping$y <- as.symbol(names(df.v)[2])
    class(species_mapping) <- "uneval"
    
    has_sp_col <- !is.null(species_mapping$colour) || !is.null(species_mapping$color)
    
    if (species_geom == "arrow") {
      if (!has_sp_col) {
        g <- g + geom_arrow(data = df.v, mapping = species_mapping, inherit.aes = FALSE, color = base_colors[2])
      } else {
        g <- g + geom_arrow(data = df.v, mapping = species_mapping, inherit.aes = FALSE)
      }
      
      if (!is.null(species_mapping$label)) {
        if (!has_sp_col) {
          g <- g + geom_arrowlabel(
            data = df.v, mapping = species_mapping, inherit.aes = FALSE,
            color = base_colors[2], size = base_sizes[2]
          )
        } else {
          g <- g + geom_arrowlabel(
            data = df.v, mapping = species_mapping, inherit.aes = FALSE,
            size = base_sizes[2]
          )
        }
      }
    } else {
      g <- g + geom_selector(
        mapping = species_mapping, data = df.v, geom = species_geom,
        base_color = base_colors[2], base_shape = base_shapes[2],
        base_size = base_sizes[2], inherit.aes = FALSE
      )
    }
  }
  
  # Correlation circle
  if (cor_circle_geom != "blank") {
    g <- g + geom_corcircle(radius = r, color = base_colors[2])
  }
  
  # Linear constraints (LC)
  if (!is.null(df.lc) && lc_geom != "blank") {
    if (is.null(lc_mapping)) lc_mapping <- aes()
    lc_mapping$x <- as.symbol(names(df.lc)[1])
    lc_mapping$y <- as.symbol(names(df.lc)[2])
    class(lc_mapping) <- "uneval"
    g <- g + geom_selector(
      data = df.lc, geom = lc_geom, mapping = lc_mapping,
      base_color = base_colors[1], base_shape = base_shapes[1],
      base_size = base_sizes[1], inherit.aes = FALSE
    )
  }
  
  # Continuous environmental variables (BP)
  if (!is.null(df.bp) && bp_geom != "blank") {
    if (is.null(bp_mapping)) bp_mapping <- aes()
    bp_mapping$x <- as.symbol(names(df.bp)[1])
    bp_mapping$y <- as.symbol(names(df.bp)[2])
    class(bp_mapping) <- "uneval"
    
    if (bp_geom == "arrow") {
      g <- g + geom_arrow(data = df.bp, mapping = bp_mapping, inherit.aes = FALSE, color = base_colors[3])
      g <- g + geom_arrowlabel(
        data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
        color = base_colors[3], size = rel(base_sizes[3])
      )
    } else if (bp_geom == "point") {
      g <- g + geom_point(
        data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
        color = base_colors[3], shape = base_shapes[3], size = rel(base_sizes[3])
      )
    } else if (bp_geom == "text") {
      if (is.null(bp_mapping$label)) bp_mapping$label <- as.symbol("Row.names")
      g <- g + geom_text(
        data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
        color = base_colors[3], size = rel(base_sizes[3])
      )
    }
  }
  
  # Discrete environmental centroids (CN)
  if (!is.null(df.cn) && cn_geom != "blank") {
    if (is.null(cn_mapping)) cn_mapping <- aes(label = .data$.level)
    cn_mapping$x <- as.symbol(names(df.cn)[1])
    cn_mapping$y <- as.symbol(names(df.cn)[2])
    class(cn_mapping) <- "uneval"
    g <- g + geom_selector(
      data = df.cn, geom = cn_geom, mapping = cn_mapping,
      base_color = base_colors[3], base_shape = base_shapes[3],
      base_size = base_sizes[3], inherit.aes = FALSE
    )
  }
  
  g +
    labs(colour = NULL, shape = NULL) +
    theme(legend.position = legend_position)
}
