
### CENTROIDS ###

#This is not a function definition, but an execution of a function that should happen on load (zzz.R)
# ggcentroids <- ggproto("ggcentroids", Stat,
#                        compute_group = function(data, scales) {
#                          if (is.null(data$centroidlabel)) data$centroidlabel = data$group
#                          data <- data.frame(x=mean(data$x), y=mean(data$y), label = data$centroidlabel[1])
#                        },
#                        required_aes = c("x", "y", "label")
# )

#' Title
#'
#' @param mapping typical mapping construct for ggplots. Typical in the form of "aes(x=, y=, color=, ...)"
#' @param data data to plot
#' @param geom how to visualize the data. "point" for points, "text" for labels
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function. In these plots this will mostly be "identity"
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. For creating a biplot this should be FALSE.
#' @param ... other arguments passed on to layer.
#'
#' @return
#' @importFrom ggplot2 layer
#' @export
#'
#' @examples
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

#' Title
#'
#' @param radius numeric radius of the correlation circle.
#' @param resolution how many points to use to draw the circle
#' @param ... other arguments passed on to layer.
#'
#' @return
#' @export
#'
#' @examples
geom_corcircle <- function(radius, resolution = 200, ...){
  #radius = sqrt(max(rowSums(df.v[,1:2])^2))
  theta <- c(seq(-pi, pi, length = resolution), seq(pi, -pi, length = resolution))
  data <- data.frame(x = radius * cos(theta), y = radius * sin(theta))
  geom_path(data, mapping = aes(x=x, y=y), inherit.aes = FALSE, ...)
}


### SPECIES ARROWS ###

#' Title
#'
#' @param Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. For creating a biplot this should be FALSE.
#' @param arrow_size size of the arrow heads
#' @param ... other arguments passed on to layer.
#'
#' @return
#' @export
#'
#' @examples
geom_arrow <- function(mapping, data, inherit.aes = FALSE, arrow_size = 0.5, ...){
  mapping$xend <- mapping$x
  mapping$yend <- mapping$y
  mapping$x <- 0
  mapping$y <- 0
  geom_segment(data=data, mapping=mapping,
               arrow = arrow(length = unit(arrow_size, 'picas')), inherit.aes = inherit.aes, ...)
}


#' Title
#'
#' @param mapping typical mapping construct for ggplots. Typical in the form of "aes(x=, y=, color=, ...)"
#' @param data The data to be displayed in this layer
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. For creating a biplot this should be FALSE.
#' @param ... other arguments passed on to layer.
#'
#' @return
#' @export
#'
#' @examples
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

