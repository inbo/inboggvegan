# Internal helper to validate score matrices cleanly
is_valid_matrix <- function(mat) {
  !is.null(mat) && is.matrix(mat) && nrow(mat) > 0L && !all(is.na(mat))
}

# Internal helper for continuous environmental biplot scores ("bp")
extract_bp_data <- function(x, choices, scaling, species_adjust, ...) {
  bp <- tryCatch(
    vegan::scores(
      x,
      display = "bp",
      scaling = scaling,
      choices = choices,
      ...
    ),
    error = function(e) NULL
  )
  if (!is_valid_matrix(bp)) {
    return(NULL)
  }
  mul <- tryCatch(vegan::ordiArrowMul(bp, fill = 0.95), error = function(e) 1)
  bp <- mul * bp
  cn <- tryCatch(
    vegan::scores(
      x,
      display = "cn",
      scaling = scaling,
      choices = choices,
      ...
    ),
    error = function(e) NULL
  )
  if (is_valid_matrix(cn)) {
    bp <- bp[!(rownames(bp) %in% rownames(cn)), , drop = FALSE]
    if (nrow(bp) == 0L) {
      return(NULL)
    }
  }
  df_bp <- as.data.frame(bp)
  df_bp$.bpname <- rownames(bp)
  df_bp$.angle <- (180 / pi) * atan(df_bp[, 2L] / df_bp[, 1L])
  df_bp$.hjust <- (1 - species_adjust * sign(df_bp[, 1L])) / 2
  df_bp$Row.names <- rownames(df_bp)
  df_bp
}

# Internal helper for discrete environmental factor centroids ("cn")
extract_cn_data <- function(x, choices, scaling, ...) {
  cn <- tryCatch(
    vegan::scores(
      x,
      display = "cn",
      scaling = scaling,
      choices = choices,
      ...
    ),
    error = function(e) NULL
  )
  if (!is_valid_matrix(cn)) {
    return(NULL)
  }
  df_cn <- as.data.frame(cn)
  levlist <- x$terminfo$xlev
  if (!is.null(levlist) && length(levlist) > 0L) {
    lev_interpret <- do.call(
      "rbind",
      lapply(names(levlist), function(levs) {
        cbind(
          .level = levlist[[levs]],
          .fullname = paste0(levs, levlist[[levs]])
        )
      })
    )
    df_cn$.cnname <- rownames(cn)
    df_cn <- merge(
      df_cn,
      lev_interpret,
      by.x = ".cnname",
      by.y = ".fullname",
      sort = FALSE
    )
    df_cn <- df_cn[, c(2L, 3L, 1L, 4L), drop = FALSE]
  } else {
    df_cn$.cnname <- rownames(cn)
    df_cn$.level <- rownames(cn)
  }
  df_cn$Row.names <- df_cn$.cnname
  df_cn
}

clean_score_rownames <- function(display_data, remove_row_prefix) {
  prefixes <- substring(rownames(display_data), 1L, 3L)
  if (all(prefixes %in% c("row", "sit")) && isTRUE(remove_row_prefix)) {
    rownames(display_data) <- substring(
      rownames(display_data),
      4L,
      nchar(rownames(display_data))
    )
  }
  display_data$Row.names <- rownames(display_data)
  display_data$Row.numbers <- seq_len(nrow(display_data))
  display_data
}

normalize_merge_by <- function(merge_by) {
  if (merge_by %in% c("rownames", "row.names", "Row.names", ".Row.names")) {
    return("Row.names")
  }
  if (
    merge_by %in% c("rownumbers", "row.numbers", "Row.numbers", ".Row.numbers")
  ) {
    return("Row.numbers")
  }
  merge_by
}

merge_by_numbers <- function(display_data, data) {
  if (nrow(display_data) != nrow(data)) {
    stop(
      "Merge on row.numbers for 2 datasets that do not have ",
      "the same number of rows"
    )
  }
  extra_cols <- setdiff(names(data), c("Row.names", "Row.numbers"))
  if (length(extra_cols) > 0L) {
    display_data <- cbind(display_data, data[, extra_cols, drop = FALSE])
  }
  display_data
}

merge_by_key <- function(display_data, data, merge_by) {
  if (!all(rownames(display_data) %in% data[[merge_by]])) {
    stop(
      "Not all rownames of the display data are found in the ",
      "merge_by column in the descriptor data"
    )
  }
  extra_data <- data[, setdiff(names(data), "Row.numbers"), drop = FALSE]
  if (merge_by != "Row.names") {
    extra_data$Row.names <- NULL
  }
  merged <- merge(
    display_data,
    extra_data,
    by.x = "Row.names",
    by.y = merge_by,
    all.x = TRUE,
    sort = FALSE
  )
  col_order <- c(2L, 3L, 1L, seq_len(ncol(merged))[-c(1L, 2L, 3L)])
  merged[, col_order, drop = FALSE]
}

merge_descriptor_data <- function(display_data, data, merge_by) {
  if (is.null(data)) {
    data <- data.frame(
      Row.numbers = seq_len(nrow(display_data)),
      Row.names = rownames(display_data)
    )
  } else {
    data <- as.data.frame(data)
    data$Row.numbers <- seq_len(nrow(data))
    data$Row.names <- rownames(data)
  }

  merge_by <- normalize_merge_by(merge_by)
  if (merge_by == "Row.numbers") {
    display_data <- merge_by_numbers(display_data, data)
  } else {
    display_data <- merge_by_key(display_data, data, merge_by)
  }

  if (nrow(display_data) < 2L) {
    warning("After merging with descriptor there are < 2 observations left")
  }
  display_data
}

decorate_sites_data <- function(display_data, x, scaling, choices) {
  is_sp_scale <- identical(scaling, 2) ||
    identical(scaling, 2L) ||
    identical(scaling, "species")
  if (is_sp_scale) {
    u_axis_labs <- paste("standardized", colnames(display_data)[c(1L, 2L)])
  } else {
    u_axis_labs <- colnames(display_data)[c(1L, 2L)]
  }
  eigs <- c(x$CCA$eig, x$CA$eig)
  if (!is.null(eigs) && length(eigs) >= max(choices)) {
    pct <- 100 * eigs[choices] / sum(eigs[eigs > 0])
    u_axis_labs <- paste(
      u_axis_labs,
      sprintf("(%0.1f%% explained var.)", pct)
    )
  }
  attr(display_data, "axis.labels") <- u_axis_labs
  display_data
}

decorate_display_data <- function(display_data,
                                  x,
                                  display,
                                  scaling,
                                  choices,
                                  species_abbrev,
                                  species_adjust) {
  if (display == "sites") {
    return(decorate_sites_data(display_data, x, scaling, choices))
  }
  if (display == "species") {
    if (isTRUE(species_abbrev)) {
      display_data$Row.names <- abbreviate(display_data$Row.names)
    }
    display_data$.angle <-
      (180 / pi) * atan(display_data[, 2L] / display_data[, 1L])
    display_data$.hjust <- (1 - species_adjust * sign(display_data[, 1L])) / 2
    sq_sums <- rowSums(display_data[, c(1L, 2L), drop = FALSE]^2, na.rm = TRUE)
    attr(display_data, "r") <- sqrt(max(sq_sums))
  }
  display_data
}

extract_standard_scores <- function(x, display, scaling, choices, ...) {
  raw_scores <- tryCatch(
    vegan::scores(
      x,
      display = display,
      scaling = scaling,
      choices = choices,
      ...
    ),
    error = function(e) NULL
  )
  if (is.null(raw_scores) || length(raw_scores) == 0L ||
        all(is.na(raw_scores))) {
    return(NULL)
  }
  display_data <- as.data.frame(raw_scores)
  if (nrow(display_data) == 0L) {
    return(NULL)
  }
  display_data
}

#' Extract and prepare display data from ordination objects
#'
#' @param x An ordination object from `vegan` (`rda`, `cca`, `capscale`,
#'   `metaMDS`).
#' @param data Optional data frame with descriptor/metadata variables to
#'   merge.
#' @param choices Integer vector of length 2 specifying ordination axes.
#' @param display Character string: `"sites"`, `"species"`, `"lc"`, `"bp"`,
#'   or `"cn"`.
#' @param scaling Numeric or character scaling method passed to
#'   `vegan::scores()`.
#' @param merge_by Character string: `"row.numbers"`, `"row.names"`, or a
#'   column name in `data`.
#' @param species_abbrev Logical; if `TRUE`, abbreviate species names.
#' @param species_adjust Numeric multiplier for adjusting label positions
#'   (default: `1.1`).
#' @param remove_row_prefix Logical; if `TRUE`, strips `"row"` or `"sit"`
#'   prefixes.
#' @param ... Additional arguments passed to `vegan::scores()`.
#'
#' @return A `data.frame` with ordination coordinates and attributes, or
#'   `NULL` if unavailable.
#' @importFrom vegan scores ordiArrowMul eigenvals
#' @export
#'
#' @examples
#' library(vegan)
#' data(varespec)
#' vare_rda <- rda(varespec)
#' site_scores <- get_display_data(vare_rda, display = "sites")
get_display_data <- function(x,
                             data = NULL,
                             choices = c(1L, 2L),
                             display = "sites",
                             scaling = "species",
                             merge_by = "row.numbers",
                             species_abbrev = FALSE,
                             species_adjust = 1.1,
                             remove_row_prefix = TRUE,
                             ...) {
  if (length(choices) != 2L) {
    stop("Choices must contain exactly 2 elements")
  }
  if (display %in% c("lc", "bp", "cn") && is.null(x$CCA)) {
    return(NULL)
  }
  if (display == "bp") {
    return(extract_bp_data(x, choices, scaling, species_adjust, ...))
  }
  if (display == "cn") {
    return(extract_cn_data(x, choices, scaling, ...))
  }

  display_data <- extract_standard_scores(x, display, scaling, choices, ...)
  if (is.null(display_data)) {
    return(NULL)
  }

  display_data <- clean_score_rownames(display_data, remove_row_prefix)
  display_data <- merge_descriptor_data(display_data, data, merge_by)
  decorate_display_data(
    display_data,
    x,
    display,
    scaling,
    choices,
    species_abbrev,
    species_adjust
  )
}

build_text_geom <- function(mapping, args, base_size) {
  if (is.null(mapping$size) && !is.null(base_size)) {
    args$size <- ggplot2::rel(base_size)
  }
  do.call(ggplot2::geom_text, args)
}

build_point_geom <- function(mapping, args, base_shape, base_size) {
  if (is.null(mapping$size) && !is.null(base_size)) {
    args$size <- ggplot2::rel(base_size)
  }
  if (is.null(mapping$shape) && !is.null(base_shape)) {
    args$shape <- base_shape
  }
  do.call(ggplot2::geom_point, args)
}

#' Select and configure geom layers for biplot elements
#'
#' @param mapping `ggplot2` aesthetic mapping created with `aes()`.
#' @param data Data frame to be plotted.
#' @param geom Character string: `"text"`, `"point"`, `"blank"`, `"line"`,
#'   or `"path"`.
#' @param base_color Base color when `colour` is not mapped.
#' @param base_shape Base point shape when `shape` is not mapped.
#' @param base_size Base size when `size` is not mapped.
#' @param ... Additional arguments passed to the underlying `geom_*`
#'   function.
#'
#' @return A `ggplot2` layer or `NULL` if `geom = "blank"`.
#' @importFrom ggplot2 geom_text geom_point geom_line geom_path rel
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:6, y = 1:6, grp = rep(c("A", "B"), 3))
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_selector(
#'     aes(x = x, y = y),
#'     df,
#'     "point",
#'     base_color = "red",
#'     base_shape = 16,
#'     base_size = 3
#'   )
geom_selector <- function(mapping,
                          data,
                          geom,
                          base_color = NULL,
                          base_shape = NULL,
                          base_size = NULL,
                          ...) {
  if (geom == "blank") {
    return(NULL)
  }

  has_col <- !is.null(mapping$colour) || !is.null(mapping$color)
  args <- list(mapping = mapping, data = data, ...)
  if (!has_col && !is.null(base_color)) {
    args$color <- base_color
  }

  switch(
    geom,
    text = build_text_geom(mapping, args, base_size),
    point = build_point_geom(mapping, args, base_shape, base_size),
    line = do.call(ggplot2::geom_line, args),
    path = do.call(ggplot2::geom_path, args),
    {
      warning(paste("no geom_selector defined for geom", geom))
      NULL
    }
  )
}
