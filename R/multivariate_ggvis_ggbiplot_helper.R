
################################################################################

#' Extract and prepare display data from ordination objects
#'
#' This helper function extracts coordinate data (scores) from ordination objects
#' and merges it with optional descriptor data for plotting. It handles different
#' types of scores (sites, species, environmental variables, etc.) and prepares
#' them for use in ggplot-based visualizations.
#'
#' @param x An ordination object (from vegan package: rda, cca, capscale, or similar)
#' @param data Optional data frame with descriptor/metadata variables to be merged
#'   with the ordination scores. Should have matching row identifiers.
#' @param choices Integer vector of length 2 specifying which ordination axes to extract
#'   (default: 1:2)
#' @param display Character string specifying which type of scores to extract:
#'   \itemize{
#'     \item "sites" - sample/site scores (default)
#'     \item "species" - species/variable scores
#'     \item "lc" - linear constraints
#'     \item "bp" - biplot scores for continuous environmental variables
#'     \item "cn" - centroids for discrete environmental variables
#'   }
#' @param scaling Character string or numeric specifying the scaling method.
#'   Common values are 1 (site scaling) or 2/"species" (species scaling).
#'   See \code{vegan::scores()} for details.
#' @param merge_by Character string specifying how to merge the descriptor data:
#'   \itemize{
#'     \item "row.numbers" - merge by row position (cbind)
#'     \item "row.names" - merge by matching row names (default)
#'     \item Any column name - merge by matching values in that column
#'   }
#' @param species_abbrev Logical; if TRUE, abbreviate species names in the output
#'   (default: FALSE). Only applies when display = "species".
#' @param species_adjust Numeric multiplier for adjusting species label positions
#'   to avoid overlap with arrows (default: 1.1). Only applies when display = "species".
#' @param remove_row_prefix Logical; if TRUE (default), remove "row" or "sit" prefix
#'   from row names that are automatically added by some functions
#' @param ... Additional arguments passed to \code{vegan::scores()}
#'
#' @return A data frame with ordination coordinates and merged descriptor data.
#'   For species display, includes additional columns for label positioning (.angle, .hjust).
#'   Returns NULL if no data is available for the requested display type.
#'   The data frame has attributes:
#'   \itemize{
#'     \item axis.labels - formatted axis labels with explained variance (for sites)
#'     \item r - radius of correlation circle (for species)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' 
#' vare.rda <- rda(varespec)
#' 
#' # Extract site scores
#' site_scores <- get_display_data(vare.rda, display = "sites")
#' 
#' # Extract species scores with abbreviation
#' species_scores <- get_display_data(vare.rda, display = "species", 
#'                                     species_abbrev = TRUE)
#' 
#' # Extract site scores merged with environmental data
#' site_data <- get_display_data(vare.rda, data = varechem, 
#'                               display = "sites", merge_by = "row.names")
#' }
#'
get_display_data <- function(x,
                             data = NULL,
                             choices = 1:2,
                             display = "sites",
                             scaling = "species",
                             merge_by = "row.numbers",
                             species_abbrev = FALSE,
                             species_adjust = 1.1,
                             remove_row_prefix = TRUE,
                             ...){
  #Control input arguments
  if (length(choices) != 2) stop("Choices must contain exactly 2 elements")

  
  
  #get the effective data
  display_data <- as.data.frame(vegan::scores(x, display = display, scaling = scaling, choices=choices))
  if(nrow(display_data) > 0){
    if(all(substring(rownames(display_data),1,3) %in% c("row", "sit")) & remove_row_prefix == TRUE) {
      rownames(display_data) <- substring(rownames(display_data), 4, nchar(rownames(display_data)))
    }
    display_data$Row.names <- rownames(display_data)
    display_data$Row.numbers <- 1:nrow(display_data)

    #get and convert the descriptive data
    if (is.null(data)){
      data <- data.frame(Row.numbers = 1:nrow(display_data), Row.names = rownames(display_data))
    } else {
      data$Row.numbers <- 1:nrow(data)
      data$Row.names <- rownames(data)
    }

    #allow merging variants
    if (merge_by %in% c("rownames", "row.names", "Row.names", ".Row.names")) {
      merge_by = "Row.names"
    }
    if (merge_by %in% c("rownumbers", "row.numbers", "Row.numbers", ".Row.numbers")) {
      merge_by = "Row.numbers"
    }

    #Merge the plotdata with the descriptordata
    if (merge_by == "Row.numbers"){
      print(paste(display, "merge by numbers"))
      if (nrow(display_data) != nrow(data)){
        stop("Merge on row.numbers for 2 datasets that do not have the same number of rows")
      } else {
        display_data <- cbind(display_data, data)
      }
    } else { #merge on rownames or variable
      print(paste(display, "merge by", merge_by))
      if (!all(rownames(display_data) %in% as.data.frame(data)[,merge_by])){
        stop("Not all rownames of the display data are found in the merge_by column in the descriptor data")
      } else {
        display_data <- merge(display_data, data, by.x = "Row.names", by.y = merge_by, all.x = TRUE, sort = FALSE)
        display_data <- display_data[ , c(2:3,1, 4:ncol(display_data)), drop = FALSE]
      }
    }

    #Check data after merge
    if (nrow(display_data) < 2) {
      warning("After merging with descriptor there are less than 2 observations left")
    }


    ### >>> SPECIFIC ACTION according to display type

    ###sites
    if (display == "sites"){
      if (scaling == 2) {
        u.axis.labs = paste("standardized", colnames(display_data)[1:2])
      } else {
        u.axis.labs <- colnames(display_data)[1:2]
      }
      u.axis.labs <-
        paste(u.axis.labs,
              sprintf('(%0.1f%% explained var.)',
                      100 * c(x$CCA$eig, x$CA$eig)[choices] / sum(c(x$CCA$eig, x$CA$eig))))
      attr(display_data, "axis.labels") <- u.axis.labs

      ###species
    } else if (display == "species"){
      if(species_abbrev) {
        display_data$Row.names <- abbreviate(display_data$Row.names)
      }

      #Create variables to aid with plotting
      display_data$.angle <- (180/pi) * atan(display_data[,2] / display_data[,1])
      display_data$.hjust <- (1 - species_adjust * sign(display_data[,1])) / 2

      #Add r attribute fot the radius for the correlation circle
      attr(display_data, "r") <- sqrt(max(rowSums(display_data[,1:2])^2))

      ###lc
    } else if (display == "lc"){
      #do nothing extra

      ###bp
    } else if (display == "bp"){
      q <- ncol(x$CCA$v)
      #multiplier for the coordinates of the head of the vector to occumay fill proportion of the plot
      bp <- scores(x, display = "bp", scaling = scaling, choices=choices)
      bp <- ordiArrowMul(bp, fill = 0.95) * bp 
      #bp <- attr(bp, "const") * bp
      cn <- scores(x, display = "cn", scaling = scaling, choices=choices)
      if (!is.null(q)){
        if(!is.na(cn)[1]) {
          bipnam <- rownames(bp)
          cntnam <- rownames(cn)
          bp <- bp[!(bipnam %in% cntnam), , drop = FALSE]
          df.bp <- as.data.frame(bp)
          if (nrow(bp) == 0) df.bp <- NULL

        } else {
          df.bp <- as.data.frame(bp)
        }
      } else {
        df.bp <- NULL
      }
      if(!is.null(df.bp)){
        df.bp$.bpname <- rownames(bp)
        df.bp$.angle <- (180/pi) * atan(df.bp[,2] / df.bp[,1])
        df.bp$.hjust = (1 - species_adjust * sign(df.bp[,1])) / 2
        df.bp$Row.names <- rownames(df.bp)
      }
      display_data <- df.bp



      ###cn
    } else if (display == "cn"){
      q <- ncol(x$CCA$v)
      #bp <- scores(x, display = "bp", scaling = scaling, choices=choices)
      cn <- scores(x, display = "cn", scaling = scaling, choices=choices)
      levlist <- x$terminfo$xlev
      if (!is.null(q)) {
        if (!is.na(cn)[1]) {
          df.cn <- as.data.frame(cn)
        } else {
          df.cn <- NULL
        }
      } else {
        df.cn <- NULL
      }

      if(!is.null(q) & !is.na(cn)[1]){
        # centroid names
        levinterpret <-
          do.call("rbind",
                  lapply(names(levlist), levlist = levlist,
                         FUN= function(levs, levlist){
                           cbind(.level = levlist[[levs]],
                                 .fullname = paste(levs, levlist[[levs]], sep = ""))
                         }))
        df.cn$.cnname <- rownames(cn)
        df.cn <- merge(df.cn, levinterpret, by.x = ".cnname", by.y = ".fullname", sort = FALSE)
        df.cn <- df.cn[c(2,3,1,4)] #door de merge wordt de merge kolom eerst gezet, en we willen dat dit de scores zijn
        df.cn$Row.names <- row.names(df.cn)
        display_data <- df.cn
      } else {
        warning("display not in sites, species, lc, bp, cn")
      }
    }
  } else {
    display_data <- NULL
  }

  ### >>> RETURN
  display_data
}

#-------------------------------------------------------------------------------

#' Select and configure geom layers for biplot elements
#'
#' This helper function creates ggplot2 geom layers with appropriate aesthetics
#' based on the specified geometry type and mappings. It handles conditional
#' application of base colors, shapes, and sizes depending on whether these
#' aesthetics are explicitly mapped.
#'
#' @param mapping ggplot2 aesthetic mapping created with \code{aes()}
#' @param data Data frame to be plotted
#' @param geom Character string specifying the geometry type:
#'   "text", "point", "blank" (no display), "line", or "path"
#' @param base_color Character string for the base color to use when color
#'   is not mapped in the aesthetics
#' @param base_shape Numeric value for the base point shape (pch code) to use
#'   when shape is not mapped
#' @param base_size Numeric value for the base size to use when size is not mapped
#' @param ... Additional arguments passed to the ggplot2 geom function
#'
#' @return A ggplot2 layer (geom_*) or NULL if geom = "blank"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = 1:10, group = rep(c("A", "B"), 5))
#' 
#' # Point geom with base settings
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_selector(aes(x = x, y = y), df, "point", 
#'                 base_color = "red", base_shape = 16, base_size = 3)
#' 
#' # Text geom with mapped color
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_selector(aes(x = x, y = y, color = group, label = group), df, "text",
#'                 base_color = "blue", base_shape = 16, base_size = 4)
#' }
#'
geom_selector <- function(mapping, data, geom, base_color, base_shape, base_size, ...){
  if (       geom == "text"){
    if        (is.null(mapping$colour) & is.null(mapping$size)){
      add <- geom_text(mapping = mapping, data = data, color = base_color, size = rel(base_size))
    } else if (is.null(mapping$colour) & !is.null(mapping$size)){
      add <- geom_text(mapping = mapping, data = data, color = base_color)
    } else if (!is.null(mapping$colour) & is.null(mapping$size)){
      add <- geom_text(mapping = mapping, data = data, size = rel(base_size))
    } else {
      add <- geom_text(mapping = mapping, data = data)
    }

  } else if (geom == "point") {
    if        (is.null(mapping$colour) & is.null(mapping$shape) & is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, color = base_color, shape = base_shape, size = rel(base_size))
    } else if (is.null(mapping$colour) & is.null(mapping$shape) & !is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, color = base_color, shape = base_shape)
    } else if (is.null(mapping$colour) & !is.null(mapping$shape) & is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, color = base_color, size = rel(base_size))
    } else if (is.null(mapping$colour) & !is.null(mapping$shape) & !is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, color = base_color)
    } else if (!is.null(mapping$colour) & is.null(mapping$shape) & is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, shape = base_shape, size = rel(base_size))
    } else if (!is.null(mapping$colour) & is.null(mapping$shape) & !is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, shape = base_shape)
    } else if (!is.null(mapping$colour) & !is.null(mapping$shape) & is.null(mapping$size)){
      add <- geom_point(mapping = mapping, data = data, size = rel(base_size))
    } else {
      add <- geom_point(mapping = mapping, data = data)
    }

  } else if (geom == "blank") {
    add <- NULL

  } else if (geom == "line") {
    if (is.null(mapping$colour)){
      add <- geom_line(mapping = mapping, data = data, color = base_color)
    }

  } else if (geom == "path") {
    add <- geom_path(mapping = mapping, data = data)
  } else {
    warning(paste("no geom_selector defined for geom", geom))
  }
  add
}

#-------------------------------------------------------------------------------





