context("Main visualization functions")

library(vegan)
library(ggplot2)

# Test data setup
data(varespec)
data(varechem)
data(dune)
data(dune.env)

# ============================================================================
# Test ggscreeplot
# ============================================================================

test_that("ggscreeplot works with prcomp objects", {
  pca <- prcomp(USArrests, scale. = TRUE)
  
  # Test different types
  p1 <- ggscreeplot(pca, type = "pev")
  expect_s3_class(p1, "ggplot")
  
  p2 <- ggscreeplot(pca, type = "cev")
  expect_s3_class(p2, "ggplot")
  
  p3 <- ggscreeplot(pca, type = "both")
  expect_s3_class(p3, "ggplot")
})

test_that("ggscreeplot works with princomp objects", {
  pca <- princomp(USArrests, cor = TRUE)
  
  p <- ggscreeplot(pca, type = "pev")
  expect_s3_class(p, "ggplot")
})

test_that("ggscreeplot works with vegan rda objects", {
  vare.rda <- rda(varespec)
  
  p <- ggscreeplot(vare.rda, type = "both")
  expect_s3_class(p, "ggplot")
})

test_that("ggscreeplot works with vegan cca objects", {
  vare.cca <- cca(varespec)
  
  p <- ggscreeplot(vare.cca, type = "pev")
  expect_s3_class(p, "ggplot")
})

test_that("ggscreeplot works with vegan capscale objects", {
  skip_if_not_installed("vegan")
  dune.cap <- capscale(dune ~ 1)
  
  p <- ggscreeplot(dune.cap, type = "cev")
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Test ggbiplot_vegan with different object types
# ============================================================================

test_that("ggbiplot_vegan works with prcomp objects", {
  pca <- prcomp(iris[,1:4], scale. = TRUE)
  
  p <- ggbiplot_vegan(pca)
  expect_s3_class(p, "ggplot")
  
  # Test with choices parameter
  p2 <- ggbiplot_vegan(pca, choices = c(1, 2))
  expect_s3_class(p2, "ggplot")
})

test_that("ggbiplot_vegan works with princomp objects", {
  pca <- princomp(iris[,1:4], cor = TRUE)
  
  p <- ggbiplot_vegan(pca)
  expect_s3_class(p, "ggplot")
})

test_that("ggbiplot_vegan works with rda objects", {
  vare.rda <- rda(varespec)
  
  p <- ggbiplot_vegan(vare.rda)
  expect_s3_class(p, "ggplot")
  
  # Test with environmental variables
  vare.rda2 <- rda(varespec ~ N + P, data = varechem)
  p2 <- ggbiplot_vegan(vare.rda2, bp_geom = "arrow")
  expect_s3_class(p2, "ggplot")
})

test_that("ggbiplot_vegan works with cca objects", {
  dune.cca <- cca(dune ~ A1 + Management, data = dune.env)
  
  p <- ggbiplot_vegan(dune.cca)
  expect_s3_class(p, "ggplot")
})

test_that("ggbiplot_vegan works with capscale objects", {
  skip_if_not_installed("vegan")
  dune.cap <- capscale(dune ~ 1)
  
  p <- ggbiplot_vegan(dune.cap)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Test princomp vs prcomp scaling consistency
# ============================================================================

test_that("princomp and prcomp produce consistent scaling", {
  # Use the same data for both
  data_matrix <- USArrests
  
  # Create both types of PCA
  pca_prcomp <- prcomp(data_matrix, scale. = TRUE)
  pca_princomp <- princomp(data_matrix, cor = TRUE)
  
  # Both should produce ggplot objects
  p1 <- ggbiplot_vegan(pca_prcomp)
  p2 <- ggbiplot_vegan(pca_princomp)
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  
  # The scaling should now be consistent (not testing exact values, 
  # just that both work without errors)
})

# ============================================================================
# Test ggbiplot_vegan customization options
# ============================================================================

test_that("ggbiplot_vegan works with different scaling options", {
  vare.rda <- rda(varespec)
  
  p1 <- ggbiplot_vegan(vare.rda, scaling = 1)
  expect_s3_class(p1, "ggplot")
  
  p2 <- ggbiplot_vegan(vare.rda, scaling = 2)
  expect_s3_class(p2, "ggplot")
})

test_that("ggbiplot_vegan works with different geom options", {
  vare.rda <- rda(varespec)
  
  # Test site geoms
  p1 <- ggbiplot_vegan(vare.rda, site_geom = "point")
  expect_s3_class(p1, "ggplot")
  
  p2 <- ggbiplot_vegan(vare.rda, site_geom = "text")
  expect_s3_class(p2, "ggplot")
  
  p3 <- ggbiplot_vegan(vare.rda, site_geom = "blank")
  expect_s3_class(p3, "ggplot")
  
  # Test species geoms
  p4 <- ggbiplot_vegan(vare.rda, species_geom = "arrow")
  expect_s3_class(p4, "ggplot")
  
  p5 <- ggbiplot_vegan(vare.rda, species_geom = "text")
  expect_s3_class(p5, "ggplot")
  
  p6 <- ggbiplot_vegan(vare.rda, species_geom = "point")
  expect_s3_class(p6, "ggplot")
})

test_that("ggbiplot_vegan works with correlation circle", {
  vare.rda <- rda(varespec)
  
  p <- ggbiplot_vegan(vare.rda, scaling = 2, cor_circle_geom = "line")
  expect_s3_class(p, "ggplot")
})

test_that("ggbiplot_vegan works with ellipses and centroids", {
  vare.rda <- rda(varespec)
  
  # Create a grouping variable
  site_data <- data.frame(group = rep(c("A", "B"), length.out = nrow(varespec)))
  
  p <- ggbiplot_vegan(vare.rda,
                      site_data = site_data,
                      site_mapping = aes(color = group, label = Row.names),
                      ellipse_geom = "line",
                      ellipse_mapping = aes(color = group),
                      centroid_geom = "point",
                      centroid_mapping = aes(color = group))
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Test get_display_data
# ============================================================================

test_that("get_display_data extracts site scores", {
  vare.rda <- rda(varespec)
  
  df <- get_display_data(vare.rda, display = "sites")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true("Row.names" %in% names(df))
})

test_that("get_display_data extracts species scores", {
  vare.rda <- rda(varespec)
  
  df <- get_display_data(vare.rda, display = "species")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true(".angle" %in% names(df))
  expect_true(".hjust" %in% names(df))
})

test_that("get_display_data works with species abbreviation", {
  vare.rda <- rda(varespec)
  
  df <- get_display_data(vare.rda, display = "species", species_abbrev = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
})

test_that("get_display_data merges with descriptor data", {
  vare.rda <- rda(varespec)
  desc_data <- varechem[, c("N", "P", "K")]
  
  df <- get_display_data(vare.rda, data = desc_data, display = "sites", 
                         merge_by = "row.names")
  expect_s3_class(df, "data.frame")
  expect_true("N" %in% names(df))
  expect_true("P" %in% names(df))
  expect_true("K" %in% names(df))
})

# ============================================================================
# Test geom functions
# ============================================================================

test_that("geom_arrow creates a segment layer", {
  vare.rda <- rda(varespec)
  species_data <- get_display_data(vare.rda, display = "species")
  
  # Create a minimal plot
  p <- ggplot() + 
    geom_arrow(aes(x = .data[[names(species_data)[1]]], 
                   y = .data[[names(species_data)[2]]]), 
               data = species_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("geom_corcircle creates a path layer", {
  # Test with a simple radius
  p <- ggplot() + 
    geom_corcircle(radius = 1, color = "red") +
    coord_equal()
  
  expect_s3_class(p, "ggplot")
})

test_that("geom_selector works with different geom types", {
  test_data <- data.frame(x = 1:5, y = 1:5, label = letters[1:5])
  
  # Test point geom
  p1 <- ggplot() + 
    geom_selector(aes(x = x, y = y), test_data, "point", 
                  base_color = "blue", base_shape = 16, base_size = 2)
  expect_s3_class(p1, "ggplot")
  
  # Test text geom
  p2 <- ggplot() + 
    geom_selector(aes(x = x, y = y, label = label), test_data, "text", 
                  base_color = "red", base_shape = 16, base_size = 3)
  expect_s3_class(p2, "ggplot")
})

# ============================================================================
# Test error handling
# ============================================================================

test_that("ggbiplot_vegan handles invalid choices", {
  vare.rda <- rda(varespec)
  
  # Test with invalid choices length
  expect_error(ggbiplot_vegan(vare.rda, choices = c(1, 2, 3)))
})

test_that("get_display_data handles invalid choices", {
  vare.rda <- rda(varespec)
  
  # Test with invalid choices length
  expect_error(get_display_data(vare.rda, choices = c(1, 2, 3)))
})
