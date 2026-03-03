
### CENTROIDS ###

#This is not a function definition, but an execution of a function that should happen on load (zzz.R)
# ggcentroids <- ggproto("ggcentroids", Stat,
#                        compute_group = function(data, scales) {
#                          if (is.null(data$centroidlabel)) data$centroidlabel = data$group
#                          data <- data.frame(x=mean(data$x), y=mean(data$y), label = data$centroidlabel[1])
#                        },
#                        required_aes = c("x", "y", "label")
# )

#' Compute and display centroids for grouped data
#'
#' This stat computes the centroid (mean x and y coordinates) for each group
#' in the data and displays it as text or points. Useful for showing group
#' centers in ordination plots.
#'
#' @param mapping ggplot2 aesthetic mapping created with \code{aes()}.
#'   Required aesthetics: x, y, and label (or group)
#' @param data Data frame containing the data to plot
#' @param geom Character string specifying how to display centroids:
#'   "text" (default) for labels, or "point" for points
#' @param position Position adjustment, either as a string or the result of
#'   a call to a position adjustment function (default: "identity")
#' @param na.rm Logical; if FALSE (default), missing values are removed with
#'   a warning. If TRUE, missing values are silently removed.
#' @param show.legend Logical; should this layer be included in the legends?
#'   NA (default) includes if any aesthetics are mapped. FALSE never includes,
#'   TRUE always includes.
#' @param inherit.aes Logical; if FALSE, overrides the default aesthetics rather
#'   than combining with them. Should typically be TRUE (default).
#' @param ... Additional arguments passed to \code{ggplot2::layer()}
#'
#' @return A ggplot2 layer that can be added to a ggplot object
#'
#' @importFrom ggplot2 layer
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' 
#' vare.rda <- rda(varespec)
#' site_data <- get_display_data(vare.rda, display = "sites")
#' 
#' # Add a grouping variable
#' site_data$group <- rep(c("A", "B"), length.out = nrow(site_data))
#' 
#' ggplot(site_data, aes(x = RDA1, y = RDA2, color = group)) +
#'   geom_point() +
#'   stat_centroid(aes(label = group))
#' }
#'
stat_centroid <- function(mapping = NULL, data = NULL, geom = "text",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggcentroids <- ggproto("ggcentroids", Stat,
                         compute_group = function(data, scales) {
                           print(data)
                           print(scales)
                           if (is.null(data$centroidlabel)) data$centroidlabel = data$group
                           data <- data.frame(x=mean(data$x), y=mean(data$y), label = data$centroidlabel[1])
                         },
                         required_aes = c("x", "y", "label"))
  ggplot2::layer(
    stat = ggcentroids, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

### ELLIPSE ###

#stat_ellipse bestaat reeds


### COR CIRCLE ###

#' Add a correlation circle to a biplot
#'
#' Draws a circle representing the correlation structure in a biplot.
#' When scaling = 2, variables (species) with endpoints on or near this circle
#' show high correlation with the ordination axes. The circle radius is typically
#' the maximum length of the species vectors.
#'
#' @param radius Numeric value specifying the radius of the circle.
#'   Typically computed as the maximum length of species vectors in the biplot.
#' @param resolution Integer specifying the number of points to use for drawing
#'   the circle (default: 200). Higher values create smoother circles.
#' @param ... Additional arguments passed to \code{geom_path()}, such as color,
#'   linetype, or size
#'
#' @return A ggplot2 path layer representing the correlation circle
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' 
#' vare.rda <- rda(varespec)
#' 
#' # Basic biplot with correlation circle
#' ggbiplot_vegan(vare.rda, 
#'                scaling = 2,
#'                cor_circle_geom = "line")
#' 
#' # Or add manually
#' species_data <- get_display_data(vare.rda, display = "species", scaling = 2)
#' r <- sqrt(max(rowSums(species_data[,1:2]^2)))
#' 
#' ggplot(species_data, aes(x = RDA1, y = RDA2)) +
#'   geom_arrow(aes(x = RDA1, y = RDA2), species_data) +
#'   geom_corcircle(radius = r, color = "red", linetype = "dashed")
#' }
#'
geom_corcircle <- function(radius, resolution = 200, ...){
  #radius = sqrt(max(rowSums(df.v[,1:2])^2))
  theta <- c(seq(-pi, pi, length = resolution), seq(pi, -pi, length = resolution))
  data <- data.frame(x = radius * cos(theta), y = radius * sin(theta))
  geom_path(data, mapping = aes(x=x, y=y), inherit.aes = FALSE, ...)
}


### SPECIES ARROWS ###

#' Draw arrows for biplot vectors
#'
#' Creates arrow segments from the origin (0,0) to specified points,
#' typically used to represent species/variable loadings or environmental
#' variable effects in ordination biplots.
#'
#' @param mapping ggplot2 aesthetic mapping created with \code{aes()}.
#'   Must include x and y coordinates for arrow endpoints. Can also include
#'   color, size, linetype, etc.
#' @param data Data frame containing the arrow endpoint coordinates and
#'   any variables referenced in the mapping
#' @param inherit.aes Logical; if FALSE (default for biplots), overrides
#'   the default aesthetics rather than combining with them
#' @param arrow_size Numeric value specifying the size of arrow heads
#'   in picas (default: 0.5)
#' @param ... Additional arguments passed to \code{geom_segment()}, such as
#'   color, linetype, or alpha
#'
#' @return A ggplot2 segment layer with arrows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' 
#' vare.rda <- rda(varespec)
#' species_data <- get_display_data(vare.rda, display = "species")
#' 
#' ggplot() +
#'   geom_arrow(aes(x = RDA1, y = RDA2), data = species_data, 
#'              color = "red", arrow_size = 0.3) +
#'   coord_equal()
#' }
#'
geom_arrow <- function(mapping, data, inherit.aes = FALSE, arrow_size = 0.5, ...){
  mapping$xend <- mapping$x
  mapping$yend <- mapping$y
  mapping$x <- 0
  mapping$y <- 0
  geom_segment(data=data, mapping=mapping,
               arrow = arrow(length = unit(arrow_size, 'picas')), inherit.aes = inherit.aes, ...)
}


#' Add labels to biplot arrows
#'
#' Adds text labels at the endpoints of biplot arrows with appropriate
#' horizontal justification and rotation angle to avoid overlap with the arrows.
#' Typically used in conjunction with \code{geom_arrow()} to label species or
#' environmental variables.
#'
#' @param mapping ggplot2 aesthetic mapping created with \code{aes()}.
#'   Must include x, y, and label. The .hjust and .angle variables will be
#'   automatically added from the data if present (created by \code{get_display_data()}).
#' @param data Data frame containing coordinates, labels, and positioning
#'   information (.hjust and .angle columns)
#' @param inherit.aes Logical; if FALSE (default for biplots), overrides
#'   the default aesthetics rather than combining with them
#' @param ... Additional arguments passed to \code{geom_text()}, such as
#'   color, size, or fontface
#'
#' @return A ggplot2 text layer positioned for arrow labels
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' 
#' vare.rda <- rda(varespec)
#' # get_display_data automatically adds .hjust and .angle columns
#' species_data <- get_display_data(vare.rda, display = "species", 
#'                                  species_adjust = 1.1)
#' 
#' ggplot() +
#'   geom_arrow(aes(x = RDA1, y = RDA2), data = species_data, color = "red") +
#'   geom_arrowlabel(aes(x = RDA1, y = RDA2, label = Row.names), 
#'                   data = species_data, color = "red") +
#'   coord_equal()
#' }
#'
geom_arrowlabel <- function(mapping, data, inherit.aes = FALSE, ...){
  mapping$hjust <- as.symbol(".hjust")
  mapping$angle <- as.symbol(".angle")
  if (!("label" %in% names(mapping))){
    mapping$label <- rownames(data)
  }

  geom_text(data = data, mapping = mapping, inherit.aes = inherit.aes, ...)
}

### SPECIES LABELS ###

#standaard geom_text!! let op inherit.aes op FALSE



################################################################################

