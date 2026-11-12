#' Extract and prepare display data from ordination objects
#'
#' @param x An ordination object from `vegan` (`rda`, `cca`, `capscale`, `metaMDS`).
#' @param data Optional data frame with descriptor/metadata variables to be merged.
#' @param choices Integer vector of length 2 specifying which ordination axes to extract.
#' @param display Character string: `"sites"`, `"species"`, `"lc"`, `"bp"`, or `"cn"`.
#' @param scaling Numeric or character scaling method passed to `vegan::scores()`.
#' @param merge_by Character string: `"row.numbers"`, `"row.names"`, or a column name in `data`.
#' @param species_abbrev Logical; if `TRUE`, abbreviate species names.
#' @param species_adjust Numeric multiplier for adjusting label positions (default: 1.1).
#' @param remove_row_prefix Logical; if `TRUE`, strips `"row"` or `"sit"` prefixes.
#' @param ... Additional arguments passed to `vegan::scores()`.
#'
#' @return A `data.frame` with ordination coordinates and attributes, or `NULL` if unavailable.
#' @importFrom vegan scores ordiArrowMul eigenvals
#' @export
get_display_data <- function(x,
                             data = NULL,
                             choices = 1:2,
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
  
  # Constrained-only displays ("lc", "bp", "cn") return NULL on unconstrained models
  if (display %in% c("lc", "bp", "cn") && is.null(x$CCA)) {
    return(NULL)
  }
  
  # Handle "bp" (biplot arrows for continuous constraints) directly
  if (display == "bp") {
    bp <- tryCatch(
      vegan::scores(x, display = "bp", scaling = scaling, choices = choices, ...),
      error = function(e) NULL
    )
    if (is.null(bp) || !is.matrix(bp) || nrow(bp) == 0L || all(is.na(bp))) {
      return(NULL)
    }
    mul <- tryCatch(vegan::ordiArrowMul(bp, fill = 0.95), error = function(e) 1)
    bp <- mul * bp
    cn <- tryCatch(
      vegan::scores(x, display = "cn", scaling = scaling, choices = choices, ...),
      error = function(e) NULL
    )
    if (!is.null(cn) && is.matrix(cn) && nrow(cn) > 0L && !all(is.na(cn))) {
      bp <- bp[!(rownames(bp) %in% rownames(cn)), , drop = FALSE]
      if (nrow(bp) == 0L) return(NULL)
    }
    df.bp <- as.data.frame(bp)
    df.bp$.bpname <- rownames(bp)
    df.bp$.angle <- (180 / pi) * atan(df.bp[, 2] / df.bp[, 1])
    df.bp$.hjust <- (1 - species_adjust * sign(df.bp[, 1])) / 2
    df.bp$Row.names <- rownames(df.bp)
    return(df.bp)
  }
  
  # Handle "cn" (centroids for factor constraints) directly
  if (display == "cn") {
    cn <- tryCatch(
      vegan::scores(x, display = "cn", scaling = scaling, choices = choices, ...),
      error = function(e) NULL
    )
    if (is.null(cn) || !is.matrix(cn) || nrow(cn) == 0L || all(is.na(cn))) {
      return(NULL)
    }
    df.cn <- as.data.frame(cn)
    levlist <- x$terminfo$xlev
    if (!is.null(levlist) && length(levlist) > 0L) {
      levinterpret <- do.call(
        "rbind",
        lapply(names(levlist), function(levs) {
          cbind(
            .level = levlist[[levs]],
            .fullname = paste0(levs, levlist[[levs]])
          )
        })
      )
      df.cn$.cnname <- rownames(cn)
      df.cn <- merge(df.cn, levinterpret, by.x = ".cnname", by.y = ".fullname", sort = FALSE)
      df.cn <- df.cn[, c(2, 3, 1, 4), drop = FALSE]
    } else {
      df.cn$.cnname <- rownames(cn)
      df.cn$.level <- rownames(cn)
    }
    df.cn$Row.names <- df.cn$.cnname
    return(df.cn)
  }
  
  # Standard extraction for "sites", "species", "lc"
  raw_scores <- tryCatch(
    vegan::scores(x, display = display, scaling = scaling, choices = choices, ...),
    error = function(e) NULL
  )
  if (is.null(raw_scores) || length(raw_scores) == 0L || all(is.na(raw_scores))) {
    return(NULL)
  }
  
  display_data <- as.data.frame(raw_scores)
  if (nrow(display_data) == 0L) {
    return(NULL)
  }
  
  if (all(substring(rownames(display_data), 1, 3) %in% c("row", "sit")) && isTRUE(remove_row_prefix)) {
    rownames(display_data) <- substring(rownames(display_data), 4, nchar(rownames(display_data)))
  }
  display_data$Row.names <- rownames(display_data)
  display_data$Row.numbers <- seq_len(nrow(display_data))
  
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
  
  if (merge_by %in% c("rownames", "row.names", "Row.names", ".Row.names")) {
    merge_by <- "Row.names"
  } else if (merge_by %in% c("rownumbers", "row.numbers", "Row.numbers", ".Row.numbers")) {
    merge_by <- "Row.numbers"
  }
  
  if (merge_by == "Row.numbers") {
    if (nrow(display_data) != nrow(data)) {
      stop("Merge on row.numbers for 2 datasets that do not have the same number of rows")
    }
    extra_cols <- setdiff(names(data), c("Row.names", "Row.numbers"))
    if (length(extra_cols) > 0L) {
      display_data <- cbind(display_data, data[, extra_cols, drop = FALSE])
    }
  } else {
    if (!all(rownames(display_data) %in% data[[merge_by]])) {
      stop("Not all rownames of the display data are found in the merge_by column in the descriptor data")
    }
    extra_data <- data[, setdiff(names(data), "Row.numbers"), drop = FALSE]
    if (merge_by != "Row.names") {
      extra_data$Row.names <- NULL
    }
    display_data <- merge(display_data, extra_data, by.x = "Row.names", by.y = merge_by, all.x = TRUE, sort = FALSE)
    display_data <- display_data[, c(2:3, 1, 4:ncol(display_data)), drop = FALSE]
  }
  
  if (nrow(display_data) < 2L) {
    warning("After merging with descriptor there are less than 2 observations left")
  }
  
  if (display == "sites") {
    if (identical(scaling, 2) || identical(scaling, "species")) {
      u.axis.labs <- paste("standardized", colnames(display_data)[1:2])
    } else {
      u.axis.labs <- colnames(display_data)[1:2]
    }
    eigs <- c(x$CCA$eig, x$CA$eig)
    if (!is.null(eigs) && length(eigs) >= max(choices)) {
      u.axis.labs <- paste(
        u.axis.labs,
        sprintf("(%0.1f%% explained var.)", 100 * eigs[choices] / sum(eigs[eigs > 0]))
      )
    }
    attr(display_data, "axis.labels") <- u.axis.labs
  } else if (display == "species") {
    if (isTRUE(species_abbrev)) {
      display_data$Row.names <- abbreviate(display_data$Row.names)
    }
    display_data$.angle <- (180 / pi) * atan(display_data[, 2] / display_data[, 1])
    display_data$.hjust <- (1 - species_adjust * sign(display_data[, 1])) / 2
    attr(display_data, "r") <- sqrt(max(rowSums(display_data[, 1:2, drop = FALSE]^2, na.rm = TRUE)))
  }
  
  display_data
}

#' Select and configure geom layers for biplot elements
#'
#' @param mapping `ggplot2` aesthetic mapping created with `aes()`.
#' @param data Data frame to be plotted.
#' @param geom Character string: `"text"`, `"point"`, `"blank"`, `"line"`, or `"path"`.
#' @param base_color Base color when `colour` is not mapped.
#' @param base_shape Base point shape when `shape` is not mapped.
#' @param base_size Base size when `size` is not mapped.
#' @param ... Additional arguments passed to the underlying `geom_*` function.
#'
#' @return A `ggplot2` layer or `NULL` if `geom = "blank"`.
#' @importFrom ggplot2 geom_text geom_point geom_line geom_path rel
#' @export
geom_selector <- function(mapping, data, geom, base_color, base_shape, base_size, ...) {
  if (geom == "blank") return(NULL)
  
  has_col <- !is.null(mapping$colour) || !is.null(mapping$color)
  has_sz  <- !is.null(mapping$size)
  has_shp <- !is.null(mapping$shape)
  
  args <- list(mapping = mapping, data = data, ...)
  if (!has_col && !missing(base_color)) args$color <- base_color
  
  if (geom == "text") {
    if (!has_sz && !missing(base_size)) args$size <- ggplot2::rel(base_size)
    return(do.call(ggplot2::geom_text, args))
  } else if (geom == "point") {
    if (!has_sz && !missing(base_size)) args$size <- ggplot2::rel(base_size)
    if (!has_shp && !missing(base_shape)) args$shape <- base_shape
    return(do.call(ggplot2::geom_point, args))
  } else if (geom == "line") {
    return(do.call(ggplot2::geom_line, args))
  } else if (geom == "path") {
    return(do.call(ggplot2::geom_path, args))
  } else {
    warning(paste("no geom_selector defined for geom", geom))
    return(NULL)
  }
}
