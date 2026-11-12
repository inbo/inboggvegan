#' Plot convex hulls and spider diagrams for grouped ordination sites
#'
#' A `ggplot2` equivalent of `vegan::ordihull()` and `vegan::ordispider()` that
#' works with any `vegan` ordination object (`rda`, `cca`, `capscale`, `metaMDS`).
#'
#' @param x A `vegan` ordination object.
#' @param groups A vector or factor of group memberships (length equal to the
#'   number of sites), or the name of a column in `site_data`.
#' @param site_data Optional `data.frame` containing site metadata.
#' @param choices Integer vector of length 2 specifying axes to plot (default: `1:2`).
#' @param scaling Scaling method passed to `vegan::scores()` (default: `"symmetric"`).
#' @param show_hull Logical; draw convex hull polygons around each group (default: `TRUE`).
#' @param show_spider Logical; draw segments connecting each site to its group
#'   centroid (default: `TRUE`).
#' @param show_centroids Logical; label group centroids (default: `TRUE`).
#' @param hull_alpha Numeric transparency of convex hull fills (default: `0.2`).
#'
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon geom_segment geom_label coord_equal labs
#' @importFrom grDevices chull
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' ord <- metaMDS(dune, trace = FALSE)
#' ggordihull_spider(ord, groups = dune.env$Management)
ggordihull_spider <- function(x,
                              groups,
                              site_data = NULL,
                              choices = 1:2,
                              scaling = "symmetric",
                              show_hull = TRUE,
                              show_spider = TRUE,
                              show_centroids = TRUE,
                              hull_alpha = 0.2) {
  scores_df <- as.data.frame(vegan::scores(x, display = "sites", choices = choices, scaling = scaling))
  ax1 <- names(scores_df)[1]
  ax2 <- names(scores_df)[2]
  
  if (is.character(groups) && length(groups) == 1L && !is.null(site_data)) {
    if (!groups %in% names(site_data)) {
      stop("Column '", groups, "' not found in site_data.")
    }
    scores_df$.group <- as.factor(site_data[[groups]])
    group_title <- groups
  } else {
    if (length(groups) != nrow(scores_df)) {
      stop("Length of 'groups' must match the number of sites (", nrow(scores_df), ").")
    }
    scores_df$.group <- as.factor(groups)
    group_title <- "Group"
  }
  
  # Compute centroids per group
  centroids <- do.call(
    rbind,
    lapply(split(scores_df, scores_df$.group), function(df_g) {
      data.frame(
        .group = df_g$.group[1],
        cx = mean(df_g[[ax1]], na.rm = TRUE),
        cy = mean(df_g[[ax2]], na.rm = TRUE)
      )
    })
  )
  
  scores_df <- merge(scores_df, centroids, by = ".group", sort = FALSE)
  
  # Compute convex hulls per group
  hulls <- do.call(
    rbind,
    lapply(split(scores_df, scores_df$.group), function(df_g) {
      if (nrow(df_g) < 3L) return(df_g)
      idx <- grDevices::chull(df_g[[ax1]], df_g[[ax2]])
      df_g[idx, , drop = FALSE]
    })
  )
  
  p <- ggplot(scores_df, aes(x = .data[[ax1]], y = .data[[ax2]], colour = .data$.group))
  
  if (isTRUE(show_hull)) {
    p <- p + geom_polygon(
      data = hulls,
      aes(x = .data[[ax1]], y = .data[[ax2]], fill = .data$.group, group = .data$.group),
      alpha = hull_alpha,
      inherit.aes = FALSE,
      colour = NA
    )
  }
  
  if (isTRUE(show_spider)) {
    p <- p + geom_segment(
      aes(x = .data$cx, y = .data$cy, xend = .data[[ax1]], yend = .data[[ax2]], colour = .data$.group),
      alpha = 0.6
    )
  }
  
  p <- p + geom_point(size = 2.5)
  
  if (isTRUE(show_centroids)) {
    p <- p + geom_label(
      data = centroids,
      aes(x = .data$cx, y = .data$cy, label = .data$.group, colour = .data$.group),
      fontface = "bold",
      show.legend = FALSE,
      inherit.aes = FALSE
    )
  }
  
  p +
    coord_equal() +
    labs(colour = group_title, fill = group_title)
}

#' Fit and plot smooth environmental surfaces on ordination diagrams
#'
#' A `ggplot2` wrapper around `vegan::ordisurf()` that fits a GAM smooth surface
#' for a continuous environmental variable and displays contour lines over the
#' ordination site scores.
#'
#' @param x A `vegan` ordination object.
#' @param env_var Numeric vector of the environmental variable (or column name
#'   in `site_data`).
#' @param site_data Optional `data.frame` containing `env_var`.
#' @param choices Integer vector of length 2 specifying ordination axes (default: `1:2`).
#' @param scaling Scaling passed to `vegan::ordisurf()` (default: `"symmetric"`).
#' @param knots Number of knots for the GAM smooth (default: `10`).
#' @param ... Additional arguments passed to `vegan::ordisurf()`.
#'
#' @return A `ggplot` object with site points and smooth contour lines.
#' @importFrom ggplot2 ggplot aes geom_point geom_contour coord_equal labs scale_colour_viridis_c after_stat
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' ord <- metaMDS(varespec, trace = FALSE)
#' ggordisurf(ord, env_var = varechem$Baresoil)
ggordisurf <- function(x,
                       env_var,
                       site_data = NULL,
                       choices = 1:2,
                       scaling = "symmetric",
                       knots = 10,
                       ...) {
  if (is.character(env_var) && length(env_var) == 1L && !is.null(site_data)) {
    var_name <- env_var
    y_vec <- site_data[[env_var]]
  } else {
    var_name <- deparse(substitute(env_var))
    y_vec <- env_var
  }
  
  surf <- vegan::ordisurf(
    x, y = y_vec, choices = choices, scaling = scaling,
    knots = knots, plot = FALSE, ...
  )
  
  grid_z <- surf$grid
  contour_df <- expand.grid(x = grid_z$x, y = grid_z$y)
  contour_df$z <- as.vector(grid_z$z)
  contour_df <- contour_df[!is.na(contour_df$z), , drop = FALSE]
  
  sites_df <- as.data.frame(vegan::scores(x, display = "sites", choices = choices, scaling = scaling))
  ax1 <- names(sites_df)[1]
  ax2 <- names(sites_df)[2]
  sites_df$.env <- y_vec
  
  dev_expl <- summary(surf)$dev.expl
  sub_txt <- sprintf("%s (GAM deviance explained: %0.1f%%)", var_name, 100 * dev_expl)
  
  ggplot(sites_df, aes(x = .data[[ax1]], y = .data[[ax2]])) +
    geom_contour(
      data = contour_df,
      aes(x = .data$x, y = .data$y, z = .data$z, colour = after_stat(level)),
      inherit.aes = FALSE
    ) +
    geom_point(aes(colour = .data$.env), size = 2.5) +
    scale_colour_viridis_c(name = var_name) +
    coord_equal() +
    labs(subtitle = sub_txt)
}

#' Fit and plot significant environmental vectors and centroids via envfit
#'
#' Runs `vegan::envfit()` on an ordination object and overlays statistically
#' significant vectors and factor centroids on a `ggplot2` ordination diagram.
#'
#' @param x A `vegan` ordination object.
#' @param env A `data.frame` of environmental variables to fit.
#' @param choices Integer vector of length 2 specifying axes (default: `1:2`).
#' @param scaling Scaling method passed to `vegan::scores()` and `vegan::envfit()`.
#' @param p.max Numeric p-value threshold for filtering vectors/factors (default: `0.05`).
#' @param permutations Number of permutations for significance testing (default: `999`).
#' @param arrow_color Color for environmental arrows and centroids (default: `"blue"`).
#'
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_point coord_equal
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' ord <- rda(varespec)
#' ggenvfit(ord, env = varechem, p.max = 0.05)
ggenvfit <- function(x,
                     env,
                     choices = 1:2,
                     scaling = "symmetric",
                     p.max = 0.05,
                     permutations = 999,
                     arrow_color = "blue") {
  sites_df <- as.data.frame(vegan::scores(x, display = "sites", choices = choices, scaling = scaling))
  ax1 <- names(sites_df)[1]
  ax2 <- names(sites_df)[2]
  
  ef <- vegan::envfit(
    x, env = env, choices = choices, scaling = scaling,
    permutations = permutations
  )
  
  mul <- tryCatch(vegan::ordiArrowMul(ef, fill = 0.85), error = function(e) 1)
  
  p <- ggplot(sites_df, aes(x = .data[[ax1]], y = .data[[ax2]])) +
    geom_point(colour = "grey30", size = 2) +
    coord_equal()
  
  vec_scores <- tryCatch(vegan::scores(ef, display = "vectors"), error = function(e) NULL)
  if (!is.null(vec_scores) && nrow(vec_scores) > 0L) {
    if (!is.null(p.max) && !is.null(ef$vectors$pvals)) {
      keep <- ef$vectors$pvals <= p.max
      vec_scores <- vec_scores[keep, , drop = FALSE]
    }
    if (nrow(vec_scores) > 0L) {
      df_v <- as.data.frame(vec_scores * mul)
      names(df_v)[1:2] <- c("vx", "vy")
      df_v$Row.names <- rownames(df_v)
      df_v$.angle <- (180 / pi) * atan(df_v$vy / df_v$vx)
      df_v$.hjust <- (1 - 1.1 * sign(df_v$vx)) / 2
      
      p <- p +
        geom_arrow(
          mapping = aes(x = .data$vx, y = .data$vy),
          data = df_v, color = arrow_color
        ) +
        geom_arrowlabel(
          mapping = aes(x = .data$vx, y = .data$vy, label = .data$Row.names),
          data = df_v, color = arrow_color
        )
    }
  }
  
  fac_scores <- tryCatch(vegan::scores(ef, display = "factors"), error = function(e) NULL)
  if (!is.null(fac_scores) && nrow(fac_scores) > 0L) {
    if (!is.null(p.max) && !is.null(ef$factors$pvals)) {
      sig_facs <- names(ef$factors$pvals)[ef$factors$pvals <= p.max]
      fac_var_names <- ef$factors$var.id
      keep_rows <- fac_var_names %in% sig_facs
      fac_scores <- fac_scores[keep_rows, , drop = FALSE]
    }
    if (nrow(fac_scores) > 0L) {
      df_f <- as.data.frame(fac_scores)
      names(df_f)[1:2] <- c("fx", "fy")
      df_f$label <- rownames(df_f)
      p <- p + geom_text(
        data = df_f,
        aes(x = .data$fx, y = .data$fy, label = .data$label),
        colour = arrow_color, fontface = "bold", inherit.aes = FALSE
      )
    }
  }
  
  p
}

#' Shepard stress diagram for NMDS ordinations
#'
#' A `ggplot2` equivalent of `vegan::stressplot()` for `metaMDS` and `monoMDS`
#' objects, displaying ordination distances against observed dissimilarities
#' along with the monotonic step fit and non-metric/linear $R^2$ values.
#'
#' @param object A `metaMDS` or `monoMDS` object from `vegan`.
#' @param point_alpha Transparency of dissimilarity points (default: `0.4`).
#' @param line_color Color of the monotonic step fit line (default: `"red"`).
#'
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_point geom_step labs
#' @importFrom stats cor
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(vegan)
#' data(dune)
#' ord <- metaMDS(dune, trace = FALSE)
#' ggshepard(ord)
ggshepard <- function(object, point_alpha = 0.4, line_color = "red") {
  if (!inherits(object, c("metaMDS", "monoMDS"))) {
    stop("ggshepard requires a 'metaMDS' or 'monoMDS' object from vegan.")
  }
  
  diss <- object$diss
  dist <- object$dist
  dhat <- object$dhat
  
  ord_idx <- order(diss, dist)
  df <- data.frame(
    diss = diss[ord_idx],
    dist = dist[ord_idx],
    dhat = dhat[ord_idx]
  )
  
  nonmetric_r2 <- 1 - object$stress^2
  linear_r2 <- stats::cor(dist, dhat)^2
  sub_label <- sprintf(
    "Stress = %0.3f | Non-metric R\u00b2 = %0.3f | Linear R\u00b2 = %0.3f",
    object$stress, nonmetric_r2, linear_r2
  )
  
  ggplot(df, aes(x = .data$diss, y = .data$dist)) +
    geom_point(alpha = point_alpha, size = 1.5) +
    geom_step(aes(y = .data$dhat), colour = line_color, linewidth = 0.8, direction = "vh") +
    labs(
      x = "Observed Dissimilarity",
      y = "Ordination Distance",
      subtitle = sub_label
    )
}
