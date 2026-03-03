


#####################################################################

#' Create a ggplot-based biplot for multivariate analysis objects
#'
#' This function creates biplots (visualization of both observations and variables)
#' for various multivariate analysis objects including vegan ordination objects
#' (rda, cca, capscale) and standard R PCA objects (prcomp, princomp).
#'
#' @param x An ordination object. Can be of class rda, cca, capscale (from vegan package),
#'   prcomp, or princomp (from stats package).
#' @param ... Additional arguments passed to specific methods
#'
#' @return A ggplot object representing the biplot
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with iris data using prcomp
#' pca <- prcomp(iris[,1:4], scale. = TRUE)
#' ggbiplot_vegan(pca)
#'
#' # Example with vegan rda
#' library(vegan)
#' data(varespec)
#' vare.rda <- rda(varespec)
#' ggbiplot_vegan(vare.rda)
#' }
#' @rdname ggbiplot_vegan
ggbiplot_vegan <- function(x,  ...) {
    UseMethod("ggbiplot_vegan")
}

######################################################################

#' @describeIn ggbiplot_vegan Method for rda (Redundancy Analysis) objects from vegan package
#'
#' @method ggbiplot_vegan rda
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.rda <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


######################################################################


#' @describeIn ggbiplot_vegan Method for cca (Canonical Correspondence Analysis) objects from vegan package
#'
#' @method ggbiplot_vegan cca
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.cca <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


######################################################################


#' @describeIn ggbiplot_vegan Method for capscale (Canonical Analysis of Principal Coordinates) objects from vegan package
#'
#' @method ggbiplot_vegan capscale
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.capscale <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


######################################################################


#' @describeIn ggbiplot_vegan Method for princomp (Principal Components Analysis using correlation/covariance matrix) objects
#'
#' @method ggbiplot_vegan princomp
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.princomp <- function(x, ...)
{
  ggbiplot.princomp(x, ...)
}



######################################################################


#' @describeIn ggbiplot_vegan Method for prcomp (Principal Components Analysis using SVD) objects
#'
#' @method ggbiplot_vegan prcomp
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.prcomp <- function(x, ...)
{
  ggbiplot.prcomp(x, ...)
}


######################################################################


###############################################################################
#' @describeIn ggbiplot_vegan Default method for creating biplots with vegan objects (rda, cca, capscale)
#'
#' This is the main workhorse function that constructs detailed biplots for vegan ordination objects.
#' It provides extensive customization options for visualizing sites (observations), species (variables),
#' environmental variables, centroids, and probability ellipses.
#'
#' @param x A vegan ordination object (rda, cca, or capscale)
#' @param choices Integer vector of length 2 specifying which ordination axes to plot (default: 1:2)
#' @param scaling Integer (1 or 2) specifying the scaling to use:
#'   \itemize{
#'     \item 1 = site (distance) scaling - focuses on relationships among sites
#'     \item 2 = species scaling (default) - focuses on relationships among species; 
#'               with scaling=2, the angle between species vectors approximates their correlation
#'   }
#' @param site_data Optional data frame with descriptor variables for the sites (observations).
#'   Should have the same number of rows as the ordination object has sites.
#' @param site_merge_by Character string specifying how to merge site_data:
#'   \itemize{
#'     \item "row.names" (default) - merge by matching row names
#'     \item "row.numbers" - merge by row position (cbind)
#'     \item Any column name - merge by matching values in that column
#'   }
#' @param species_data Optional data frame with descriptor variables for the species (variables)
#' @param species_merge_by Character string specifying how to merge species_data (see site_merge_by)
#' @param site_geom Character string specifying how to display sites:
#'   "blank" (hidden), "point" (default), or "text" (labels)
#' @param site_mapping ggplot2 aesthetic mapping for sites (created with \code{aes()}).
#'   Should NOT include x and y (automatically set). Can include color, size, shape, label, etc.
#'   Must include label aesthetic if site_geom is "text".
#' @param centroid_geom Character string for displaying site centroids:
#'   "blank" (default, hidden), "point", or "text"
#' @param centroid_mapping ggplot2 aesthetic mapping for centroids
#' @param ellipse_geom Character string for probability density ellipses:
#'   "blank" (default, hidden) or "line"
#' @param ellipse_mapping ggplot2 aesthetic mapping for ellipses
#' @param species_geom Character string for displaying species/variables:
#'   "arrow" (default), "text", "point", or "blank"
#' @param species_mapping ggplot2 aesthetic mapping for species
#' @param cor_circle_geom Character string for correlation circle:
#'   "blank" (default, hidden) or "line". The correlation circle is useful with scaling=2.
#' @param cor_circle_mapping ggplot2 aesthetic mapping for correlation circle
#' @param lc_geom Character string for linear constraints: "blank" (default), "point", or "text"
#' @param lc_mapping ggplot2 aesthetic mapping for linear constraints
#' @param bp_geom Character string for continuous environmental variables (biplot scores):
#'   "blank" (default), "arrow", "text", or "point"
#' @param bp_mapping ggplot2 aesthetic mapping for continuous environmental variables
#' @param cn_geom Character string for discrete environmental variables (centroids):
#'   "blank" (default), "point", or "text"
#' @param cn_mapping ggplot2 aesthetic mapping for discrete environmental variables
#' @param species_abbrev Logical; if TRUE, abbreviate species names (default: FALSE)
#' @param species_adjust Numeric adjustment factor for species label positions to avoid
#'   overlap with arrows (default: 1.1)
#' @param ellipse_level Numeric probability level for ellipses (default: 0.68).
#'   Common values are 0.68 (~1 SD) or 0.95 (~2 SD)
#' @param base_colors Character vector of length 3 with base colors for
#'   sites, species, and environmental variables (default: c("black", "red", "blue"))
#' @param base_shapes Numeric vector of length 3 with base point shapes for
#'   sites, species, and environmental variables (default: c(16, 16, 16))
#' @param base_sizes Numeric vector of length 3 with base sizes for
#'   sites, species, and environmental variables (default: c(2, 2, 2))
#' @param legend_position Character string for legend position:
#'   "right" (default), "top", "bottom", "left", or "none"
#' @param ... Additional arguments (currently unused)
#'
#' @return A ggplot object that can be further customized with ggplot2 functions
#'
#' @export
#' @method ggbiplot_vegan default
#' @rdname  ggbiplot_vegan
#'
#' @examples
#' \dontrun{
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' 
#' # Simple RDA
#' vare.rda <- rda(varespec)
#' ggbiplot_vegan(vare.rda)
#' 
#' # RDA with environmental variables and custom colors
#' vare.rda2 <- rda(varespec ~ N + P + K, data = varechem)
#' ggbiplot_vegan(vare.rda2, 
#'                site_data = varechem,
#'                site_mapping = aes(color = pH, label = Row.names),
#'                bp_geom = "arrow")
#' }
#'
ggbiplot_vegan.default <- function(
  x,
  choices = 1:2,
  scaling = 2,
  site_data = NULL,
  site_merge_by = "row.names",
  species_data = NULL,
  species_merge_by = "row.names",

  site_geom = "point",
  site_mapping = aes(label = Row.names),

  centroid_geom = "blank",
  centroid_mapping = aes(group=1),

  ellipse_geom = "blank",
  ellipse_mapping = NULL,


  species_geom = "arrow",
  species_mapping = aes(label = Row.names),
  cor_circle_geom = "blank",
  cor_circle_mapping = NULL,
  lc_geom = "blank",

  lc_mapping = aes(label = Row.names),

  bp_geom = "blank",
  bp_mapping = NULL,
  cn_geom = "blank",
  cn_mapping = aes(label = .level),

  species_abbrev = FALSE,
  species_adjust = 1.1,
  ellipse_level = 0.68,
  base_colors = c("black", "red", "blue"),
  base_shapes = c(16, 16, 16),
  base_sizes = c(2, 2, 2),
  legend_position= "right",
  ...
){

  ### >>> LOAD DATA
  df.u <- get_display_data(x, data = site_data, choices = choices, display = "sites",
                           scaling = scaling, merge_by = site_merge_by)
  df.v <- get_display_data(x, data = species_data, choices = choices, display = "species",
                           scaling = scaling, merge_by = species_merge_by,
                           species_abbrev = species_abbrev,
                           species_adjust = species_adjust)
  df.lc <- get_display_data(x, data = site_data, choices = choices, display = "lc",
                            scaling = scaling, merge_by = site_merge_by)
  df.bp <- get_display_data(x, choices =  choices, scaling = scaling, display = "bp",
                                  species_adjust = species_adjust)
  df.cn <- get_display_data(x, choices = choices, scaling = scaling, display = "cn")

  ### >>> INIT VARIABLES
  n <- nobs(df.u)
  p <- ncol(df.v)
  axis.labels <- attr(df.u, "axis.labels")
  if (is.null(axis.labels)) {
    warning("site_data should have the attribute axis.labels that defines the axis labels in the plot. It should be a string with 2 elements")
    axis.labels <- c("", "")
  }
  r <- attr(df.v, "r")
  if (is.null(r)){
    v.scale <- rowSums(df.v[,1:2])^2
    r <- sqrt(max(v.scale)) #radius of the correlation circle
  }
  if(!is.numeric(r)) {
    warning("radius of the correlation circle is not numeric, defaulting to 1")
    r <- 1
  }
  mybreaks <- pretty(c(df.u[[1]], df.u[[2]], n = 5, eps.correct =  1))

  ### >>> INIT PLOT
  g <- ggplot(data = df.u, aes_string(x = names(df.u)[1],
                                      y = names(df.u)[2])) +
    xlab(axis.labels[1]) + ylab(axis.labels[2]) + coord_equal() +
    scale_x_continuous(breaks = mybreaks) +
    scale_y_continuous(breaks = mybreaks)


  ### Site
  if (site_geom != "blank"){
    site_mapping$x <- as.symbol(names(df.u)[1])
    site_mapping$y <- as.symbol(names(df.u)[2])
    class(site_mapping) <- "uneval"
    g <- g + geom_selector(data=df.u, geom = site_geom,  mapping = site_mapping,
                           base_color = base_colors[1],
                           base_shape = base_shapes[1],
                           base_size = base_sizes[1]
    )
  }

  ### Site centroid
  if (centroid_geom != "blank"){
    if        ( is.null(centroid_mapping$size) & is.null(centroid_mapping$colour)){
      g <- g + stat_centroid(mapping = centroid_mapping, data = df.u, color = base_colors[1], size = base_sizes[1])
    } else if (!is.null(centroid_mapping$size) & is.null(centroid_mapping$colour)){
      g <- g + stat_centroid(mapping = centroid_mapping, data = df.u, size = base_sizes[1])
    } else if ( is.null(centroid_mapping$size) & !is.null(centroid_mapping$colour)){
      g <- g + stat_centroid(mapping = centroid_mapping, data = df.u, color = base_colors[1])
    } else {
      g <- g + stat_centroid(mapping = centroid_mapping, data = df.u)
    }
  }

  ### Site ellipse
  if (ellipse_geom != "blank") {
    ellipse_mapping$x <- as.symbol(names(df.u)[1])
    ellipse_mapping$y <- as.symbol(names(df.u)[2])
    class(ellipse_mapping) <- "uneval"
    if (is.null(ellipse_mapping$colour)){
      g <- g + stat_ellipse(mapping = ellipse_mapping, data = df.u, level = ellipse_level, inherit.aes = FALSE, color = base_colors[1])

    } else {
      g <- g + stat_ellipse(mapping = ellipse_mapping, data = df.u, level = ellipse_level, inherit.aes = FALSE)

    }
  }


  ### Species
  species_mapping$x <- as.symbol(names(df.v)[1])
  species_mapping$y <- as.symbol(names(df.v)[2])
  class(species_mapping) <- "uneval"

  if (species_geom == "blank"){
    #do nothing
  } else if (species_geom == "arrow") {
    if (is.null(species_mapping$colour)){
      g <- g + geom_arrow(data = df.v, mapping = species_mapping, inherit.aes = FALSE, color = base_colors[2])
    } else {
      g <- g + geom_arrow(data = df.v, mapping = species_mapping, inherit.aes = FALSE)
    }

    if (!is.null(species_mapping$label)){
      species_mapping$hjust <- as.symbol(".hjust")
      species_mapping$angle <- as.symbol(".angle")
      if (is.null(species_mapping$colour)){
        g <- g + geom_arrowlabel(data = df.v, mapping = species_mapping, inherit.aes = FALSE,
                                 color = base_colors[2],
                                 size = base_sizes[2])
      } else {
        g <- g + geom_arrowlabel(data = df.v, mapping = species_mapping, inherit.aes = FALSE,
                                size = base_sizes[2])
      }
    }
  } else  {
    g <- g + geom_selector(mapping = species_mapping, data = df.v, geom = species_geom,
                           base_color = base_colors[2], base_shape = base_shapes[2], base_size = base_sizes[2], inherit.aes = FALSE)
  }


  ### Species cor_circle
  if (cor_circle_geom != "blank"){
    g <- g + geom_corcircle(radius = r, color = base_colors[2])
  }


  ### LC
  if (!is.null(df.lc)){
    if (lc_geom != "blank"){
      lc_mapping$x <- as.symbol(names(df.lc)[1])
      lc_mapping$y <- as.symbol(names(df.lc)[2])
      class(lc_mapping) <- "uneval"
      g <- g + geom_selector(data=df.lc, geom = lc_geom,  mapping = lc_mapping,
                             base_color = base_colors[1],
                             base_shape = base_shapes[1],
                             base_size = base_sizes[1],
                             inherit.aes = FALSE
      )
    }
  }



  ### Environmental vars
  if (!is.null(df.bp)){
    bp_mapping$x <- as.symbol(names(df.bp)[1])
    bp_mapping$y <- as.symbol(names(df.bp)[2])
    class(bp_mapping) <- "uneval"

    if        (bp_geom == "blank"){
    } else if (bp_geom == "arrow"){
      g <- g + geom_arrow(data = df.bp, mapping = bp_mapping, inherit.aes = FALSE, color = base_colors[3])
      g <- g + geom_arrowlabel(data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
                               color = base_colors[3], size = rel(base_sizes[3]))

    } else if (bp_geom == "point"){
      g <- g + geom_point(data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
                          color = base_colors[3], shape = base_shapes[3], size = rel(base_sizes[3]))

    } else if (bp_geom == "text"){
      g <- g + geom_text(data = df.bp, mapping = bp_mapping, inherit.aes = FALSE,
                         color = base_colors[3], size = rel(base_sizes[3]))
    }
  }


  ### Environmental centroids
  if (!is.null(df.cn)){
    cn_mapping$x <- as.symbol(names(df.cn)[1])
    cn_mapping$y <- as.symbol(names(df.cn)[2])
    class(cn_mapping) <- "uneval"

    if (cn_geom == "blank"){
    } else {
      g <- g + geom_selector(data=df.cn, geom = cn_geom,  mapping = cn_mapping,
                             base_color = base_colors[3],
                             base_shape = base_shapes[3],
                             base_size = base_sizes[3],
                             inherit.aes = FALSE)
    }
  }

  ### >>> RETURN
  g <- g + scale_colour_discrete(name = "", position = legend_position) + scale_shape_discrete(name = "", position = legend_position)
  g
}

#' \deqn{}
