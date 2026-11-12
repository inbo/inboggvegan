library(testthat)
library(vegan)
library(ggplot2)

test_that("ggscreeplot works for prcomp, princomp, rda, cca, and capscale", {
  pca_pr <- prcomp(USArrests, scale. = TRUE)
  pca_pi <- princomp(USArrests, cor = TRUE)

  p1 <- ggscreeplot(pca_pr, type = "pev")
  p2 <- ggscreeplot(pca_pi, type = "cev")
  p3 <- ggscreeplot(pca_pr, type = "both", max_axes = 3L)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_no_error(ggplot_build(p3))

  data(varespec, package = "vegan")
  ord_rda <- rda(varespec)
  p_rda <- ggscreeplot(ord_rda, type = "both", max_axes = 8L, bstick = TRUE)
  expect_s3_class(p_rda, "ggplot")
  expect_no_error(ggplot_build(p_rda))
})

test_that("get_display_data extracts sites, species, bp, and cn accurately", {
  data(dune, package = "vegan")
  data(dune.env, package = "vegan")

  ord_unc <- rda(dune)
  expect_null(get_display_data(ord_unc, display = "bp"))
  expect_null(get_display_data(ord_unc, display = "cn"))
  expect_null(get_display_data(ord_unc, display = "lc"))

  sp_df <- get_display_data(ord_unc, display = "species", scaling = 2)
  expected_r <- sqrt(max(rowSums(sp_df[, c(1L, 2L)]^2)))
  expect_equal(attr(sp_df, "r"), expected_r)

  ord_con <- cca(dune ~ A1 + Management, data = dune.env)
  bp_df <- get_display_data(ord_con, display = "bp")
  cn_df <- get_display_data(ord_con, display = "cn")

  expect_s3_class(bp_df, "data.frame")
  expect_identical(nrow(bp_df), 1L)
  expect_s3_class(cn_df, "data.frame")
  expect_true(".level" %in% names(cn_df))
})

test_that("ggbiplot_vegan handles continuous and discrete aesthetics", {
  data(varespec, package = "vegan")
  data(varechem, package = "vegan")
  data(dune, package = "vegan")
  data(dune.env, package = "vegan")

  ord_rda <- rda(varespec ~ N + P + K, data = varechem)
  p_cont <- ggbiplot_vegan(
    ord_rda,
    site_data = varechem,
    site_mapping = aes(color = pH, label = Row.names),
    bp_geom = "arrow",
    cor_circle_geom = "line"
  )
  expect_s3_class(p_cont, "ggplot")
  expect_no_error(ggplot_build(p_cont))

  ord_cca <- cca(dune ~ A1 + Management, data = dune.env)
  p_disc <- ggbiplot_vegan(
    ord_cca,
    site_data = dune.env,
    site_merge_by = "row.numbers",
    site_mapping = aes(color = Management),
    centroid_geom = "text",
    centroid_mapping = aes(label = Management, color = Management),
    ellipse_geom = "line",
    ellipse_mapping = aes(color = Management),
    bp_geom = "arrow",
    cn_geom = "text"
  )
  expect_s3_class(p_disc, "ggplot")
  expect_no_error(ggplot_build(p_disc))
})

test_that("ggbiplot works with prcomp and princomp on non-default choices", {
  iris_num <- iris[, seq_len(4L)]
  iris_grp <- iris[, 5L, drop = FALSE]
  pca1 <- prcomp(iris_num, scale. = TRUE)
  pca2 <- princomp(iris_num, cor = TRUE)

  p_13 <- ggbiplot(pca1, choices = c(1L, 3L), colordata = iris_grp)
  expect_s3_class(p_13, "ggplot")
  expect_no_error(ggplot_build(p_13))

  p_txt <- ggbiplot(pca2, choices = c(1L, 2L), colordata = iris_grp, type = "t")
  expect_s3_class(p_txt, "ggplot")
  expect_no_error(ggplot_build(p_txt))
})

test_that("new extra visualization functions render cleanly", {
  data(dune, package = "vegan")
  data(dune.env, package = "vegan")

  ord_nmds <- metaMDS(dune, trace = FALSE)

  p_hull <- ggordihull_spider(ord_nmds, groups = dune.env$Management)
  expect_s3_class(p_hull, "ggplot")
  expect_no_error(ggplot_build(p_hull))

  p_surf <- ggordisurf(ord_nmds, env_var = dune.env$A1)
  expect_s3_class(p_surf, "ggplot")
  expect_no_error(ggplot_build(p_surf))

  p_env <- ggenvfit(
    ord_nmds,
    env = dune.env[, c("A1", "Management")],
    permutations = 99L
  )
  expect_s3_class(p_env, "ggplot")
  expect_no_error(ggplot_build(p_env))

  p_shep <- ggshepard(ord_nmds)
  expect_s3_class(p_shep, "ggplot")
  expect_no_error(ggplot_build(p_shep))
})
