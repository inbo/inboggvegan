#' Compute and display centroids for grouped data
#'
#' Computes the centroid (mean x and y coordinates) for each group in the data
#' and displays it as text or points.
#'
#' @param mapping `ggplot2` aesthetic mapping created with `aes()`.
#'   Required aesthetics: `x`, `y`, and `label` (or `group`).
#' @param data Data frame containing the data to plot.
#' @param geom Character string specifying how to display centroids:
#'   `"text"` (default) for labels, or `"point"` for points.
#' @param position Position adjustment (default: `"identity"`).
#' @param na.rm Logical; if `FALSE` (default), missing values are removed with
#'   a warning.
#' @param show.legend Logical; should this layer be included in the legends?
#' @param inherit.aes Logical; if `FALSE`, overrides the default aesthetics.
#' @param ... Additional arguments passed to `ggplot2::layer()`.
#'
#' @return A `ggplot2` layer.
#' @importFrom ggplot2 layer ggproto Stat
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' site_data <- get_display_data(vare_rda, display = "sites")
#' site_data$grp <- rep(c("A", "B"), length.out = nrow(site_data))
#'
#' ggplot(site_data, aes(x = PC1, y = PC2, color = grp)) +
#'   geom_point() +
#'   stat_centroid(aes(label = grp))
# nolint start: object_name_linter.
stat_centroid <- function(mapping = NULL,
                          data = NULL,
                          geom = "text",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  # nolint end
  stat_ggcentroid <- ggproto(
    "StatGgcentroid",
    Stat,
    compute_group = function(data, scales) {
      lbl <- if (!is.null(data$label)) {
        data$label[1L]
      } else if (!is.null(data$centroidlabel)) {
        data$centroidlabel[1L]
      } else {
        data$group[1L]
      }
      data.frame(
        x = mean(data$x, na.rm = TRUE),
        y = mean(data$y, na.rm = TRUE),
        label = lbl
      )
    },
    required_aes = c("x", "y")
  )
  ggplot2::layer(
    stat = stat_ggcentroid,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Add a correlation circle to a biplot
#'
#' Draws a circle representing the correlation structure in a biplot.
#' When `scaling = 2`, variables (species) with endpoints on or near this
#' circle are well represented by the displayed axes.
#'
#' @param radius Numeric value specifying the radius of the circle.
#' @param resolution Integer specifying the number of points to use for drawing
#'   the circle (default: `200`).
#' @param ... Additional arguments passed to `geom_path()`.
#'
#' @return A `ggplot2` path layer representing the correlation circle.
#' @importFrom ggplot2 geom_path aes
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_corcircle(radius = 1, color = "red", linetype = "dashed") +
#'   coord_equal()
geom_corcircle <- function(radius = 1, resolution = 200L, ...) {
  theta <- seq(-pi, pi, length.out = resolution)
  circle_df <- data.frame(
    x = radius * cos(theta),
    y = radius * sin(theta)
  )
  geom_path(
    data = circle_df,
    mapping = aes(x = .data$x, y = .data$y),
    inherit.aes = FALSE,
    ...
  )
}

#' Draw arrows for biplot vectors
#'
#' Creates arrow segments from the origin `(0, 0)` to specified points,
#' typically used to represent species loadings or environmental effects.
#'
#' @param mapping `ggplot2` aesthetic mapping created with `aes()`.
#' @param data Data frame containing the arrow endpoint coordinates.
#' @param inherit.aes Logical; default `FALSE`.
#' @param arrow_size Numeric value specifying arrow head size in picas
#'   (default: `0.5`).
#' @param ... Additional arguments passed to `geom_segment()`.
#'
#' @return A `ggplot2` segment layer with arrows.
#' @importFrom ggplot2 geom_segment
#' @importFrom grid arrow unit
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' sp_df <- get_display_data(vare_rda, display = "species")
#' ggplot() +
#'   geom_arrow(aes(x = PC1, y = PC2), data = sp_df, color = "red") +
#'   coord_equal()
# nolint start: object_name_linter.
geom_arrow <- function(mapping,
                       data,
                       inherit.aes = FALSE,
                       arrow_size = 0.5,
                       ...) {
  # nolint end
  mapping$xend <- mapping$x
  mapping$yend <- mapping$y
  mapping$x <- 0
  mapping$y <- 0
  geom_segment(
    data = data,
    mapping = mapping,
    arrow = grid::arrow(length = grid::unit(arrow_size, "picas")),
    inherit.aes = inherit.aes,
    ...
  )
}

#' Add labels to biplot arrows
#'
#' Adds text labels at the endpoints of biplot arrows with horizontal
#' justification (`.hjust`) and rotation angle (`.angle`) to avoid overlap
#' with the arrows.
#'
#' @param mapping `ggplot2` aesthetic mapping created with `aes()`.
#' @param data Data frame containing coordinates, labels, and positioning
#'   columns (`.hjust` and `.angle`).
#' @param inherit.aes Logical; default `FALSE`.
#' @param ... Additional arguments passed to `geom_text()`.
#'
#' @return A `ggplot2` text layer positioned for arrow labels.
#' @importFrom ggplot2 geom_text
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' sp_df <- get_display_data(vare_rda, display = "species")
#' ggplot() +
#'   geom_arrow(aes(x = PC1, y = PC2), data = sp_df, color = "red") +
#'   geom_arrowlabel(
#'     aes(x = PC1, y = PC2, label = Row.names),
#'     data = sp_df,
#'     color = "red"
#'   ) +
#'   coord_equal()
# nolint start: object_name_linter.
geom_arrowlabel <- function(mapping, data, inherit.aes = FALSE, ...) {
  # nolint end
  if (".hjust" %in% names(data)) {
    mapping$hjust <- as.symbol(".hjust")
  }
  if (".angle" %in% names(data)) {
    mapping$angle <- as.symbol(".angle")
  }
  if (!("label" %in% names(mapping))) {
    if ("Row.names" %in% names(data)) {
      mapping$label <- as.symbol("Row.names")
    } else {
      data$.row_label <- rownames(data)
      mapping$label <- as.symbol(".row_label")
    }
  }
  geom_text(data = data, mapping = mapping, inherit.aes = inherit.aes, ...)
}
