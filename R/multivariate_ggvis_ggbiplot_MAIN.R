


#####################################################################

#' GGbiplot prototype function
#'
#' @param x vegan object
#' @param choices which 2 components schould be visualized
#' @param scaling used scaling (1  = site, 2 = species (default)). If scaling = 2, than the angle of the species (variables) matches the correlation
#' @param site_data descriptor data for the sites
#' @param site_merge_by merge the site descriptor with the vegan object on .Rownumbers (cbind), .Rownames or any other column (merge this column with the site scores matrix rownames)
#' @param species_data descriptor data for the species
#' @param species_merge_by merge species data (see site_merge_by)
#' @param site_geom which geom to represent the sites "blank", "point", "text"
#' @param site_mapping ggplot mapping in the format aes() but x and y must not be given, so for example (aes(color = Kolom1, size = Kolom2, label = Kolom3)). This should always contain the label argument if the geom is "text"
#' @param centroid_geom representation of the site centroids ("blank", "point", "text")
#' @param centroid_mapping ggplot mapping for the centroids (see site_mapping)
#' @param ellipse_geom representation of the site probability density ellipses ("blank", "line")
#' @param ellipse_mapping ggplot mapping for the ellipses
#' @param species_geom representation of the species (or biometric variables) ("blank", "text", "arrow", "point")
#' @param species_mapping ggplot mapping for the species
#' @param cor_circle_geom representation of the correlation circle ("blank", "line")
#' @param cor_circle_mapping ggplot mapping for the correlation circle
#' @param lc_geom representation of linear constraints (see site_geom)
#' @param lc_mapping see site_mapping but for the linear constraints
#' @param bp_geom representation of the continuous environmental variables ("blank", "arrow", "text", "point")
#' @param bp_mapping mapping of the continuous environmental variables
#' @param cn_geom representation of the discrete environmental variables
#' @param cn_mapping mapping of the discrete environmental variables
#' @param species_abbrev logical that controls if species names (or variables when not working with vegetation data) should be abbreviated
#' @param species_adjust adjustment of the species labels so they do not collide with the arrows
#' @param ellipse_level probability density on which the ellipses should be based. 0.68 or 0.95 are the most logical choices (1 or 2 sd from the centroids)
#' @param base_colors vector of 3 elements with the base colors of sites, species and environmental variables
#' @param base_shapes vector of 3 elements with the base shapes for sites, species and environmental variables
#' @param base_sizes  vector of 3 elements  with the base sizes for sites, species and environmental variables
#' @param legend_position position of the legend "top", "bottom", "left", "right", "none"
#' @param ... other arguments for ggbiplot_vegan.default
#'
#' @return
#' @export
#'
#' @examples
#' @rdname ggbiplot_vegan
ggbiplot_vegan <- function(x,  ...) {
    UseMethod("ggbiplot_vegan")
}

######################################################################

#' GGbiplot for rda objects
#'
#' @method ggbiplot_vegan rda
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.rda <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


######################################################################


#' GGbiplot for cca objects
#'
#' @method ggbiplot_vegan cca
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.cca <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


######################################################################

#' GGbiplot for capscale objects
#'
#' @method ggbiplot_vegan capscale
#' @export
#' @rdname  ggbiplot_vegan
ggbiplot_vegan.capscale <- function(x, ...)
{
  ggbiplot_vegan.default(x, ...)
}


###############################################################################
#' GGBiplot vegan default
#'
#' Wrapper function around all geoms to construct a ggplot for vegan objects (rda, cca, capscale)
#'
#' @param x vegan object
#' @param choices which 2 components schould be visualized
#' @param scaling used scaling (1  = site, 2 = species (default)). If scaling = 2, than the angle of the species (variables) matches the correlation
#' @param site_data descriptor data for the sites
#' @param site_merge_by merge the site descriptor with the vegan object on .Rownumbers (cbind), .Rownames or any other column (merge this column with the site scores matrix rownames)
#' @param species_data descriptor data for the species
#' @param species_merge_by merge species data (see site_merge_by)
#' @param site_geom which geom to represent the sites "blank", "point", "text"
#' @param site_mapping ggplot mapping in the format aes() but x and y must not be given, so for example (aes(color = Kolom1, size = Kolom2, label = Kolom3)). This should always contain the label argument if the geom is "text"
#' @param centroid_geom representation of the site centroids ("blank", "point", "text")
#' @param centroid_mapping ggplot mapping for the centroids (see site_mapping)
#' @param ellipse_geom representation of the site probability density ellipses ("blank", "line")
#' @param ellipse_mapping ggplot mapping for the ellipses
#' @param species_geom representation of the species (or biometric variables) ("blank", "text", "arrow", "point")
#' @param species_mapping ggplot mapping for the species
#' @param cor_circle_geom representation of the correlation circle ("blank", "line")
#' @param cor_circle_mapping ggplot mapping for the correlation circle
#' @param lc_geom representation of linear constraints (see site_geom)
#' @param lc_mapping see site_mapping but for the linear constraints
#' @param bp_geom representation of the continuous environmental variables ("blank", "arrow", "text", "point")
#' @param bp_mapping mapping of the continuous environmental variables
#' @param cn_geom representation of the discrete environmental variables
#' @param cn_mapping mapping of the discrete environmental variables
#' @param species_abbrev logical that controls if species names (or variables when not working with vegetation data) should be abbreviated
#' @param species_adjust adjustment of the species labels so they do not collide with the arrows
#' @param ellipse_level probability density on which the ellipses should be based. 0.68 or 0.95 are the most logical choices (1 or 2 sd from the centroids)
#' @param base_colors vector of 3 elements with the base colors of sites, species and environmental variables
#' @param base_shapes vector of 3 elements with the base shapes for sites, species and environmental variables
#' @param base_sizes  vector of 3 elements  with the base sizes for sites, species and environmental variables
#' @param legend_position position of the legend "top", "bottom", "left", "right", "none"
#' @param ... other arguments for ggbiplot_vegan.default
#'
#' @export
#' @method ggbiplot_vegan default
#' @rdname  ggbiplot_vegan
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
      g <- g + INBOmisc::stat_centroid(mapping = centroid_mapping, data = df.u, color = base_colors[1], size = base_sizes[1])
    } else if (!is.null(centroid_mapping$size) & is.null(centroid_mapping$colour)){
      g <- g + INBOmisc::stat_centroid(mapping = centroid_mapping, data = df.u, size = base_sizes[1])
    } else if ( is.null(centroid_mapping$size) & !is.null(centroid_mapping$colour)){
      g <- g + INBOmisc::stat_centroid(mapping = centroid_mapping, data = df.u, color = base_colors[1])
    } else {
      g <- g + INBOmisc::stat_centroid(mapping = centroid_mapping, data = df.u)
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
        g <- g + INBOmisc::geom_arrowlabel(data = df.v, mapping = species_mapping, inherit.aes = FALSE, color = base_colors[2])
      } else {
        g <- g + INBOmisc::geom_arrowlabel(data = df.v, mapping = species_mapping, inherit.aes = FALSE)
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
