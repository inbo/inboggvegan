## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5.5,
  warning = FALSE,
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(inboggvegan)
library(vegan)
library(ggplot2)

theme_set(theme_bw())

## ----scree-prcomp-------------------------------------------------------------
pca_arrests <- prcomp(USArrests, scale. = TRUE)
ggscreeplot(pca_arrests, type = "both") +
  ggtitle("Screeplot for USArrests PCA")

## ----scree-vegan--------------------------------------------------------------
data(varespec)
 data(varechem)

vare_pca <- rda(varespec, scale = TRUE)
ggscreeplot(vare_pca, type = "pev", max_axes = 10, bstick = TRUE) +
  ggtitle("Lichen Pasture PCA vs. Broken-Stick Expectation")

## ----biplot-prcomp------------------------------------------------------------
pca_iris <- prcomp(iris[, 1:4], scale. = TRUE)

ggbiplot(
  pca_iris,
  choices = c(1, 2),
  colordata = iris[, "Species", drop = FALSE],
  pch = 16
) +
  ggtitle("Iris PCA Biplot (Axes 1 & 2)")

## ----biplot-prcomp-13---------------------------------------------------------
ggbiplot(
  pca_iris,
  choices = c(1, 3),
  colordata = iris[, "Species", drop = FALSE],
  type = "t"
) +
  ggtitle("Iris PCA Biplot (Axes 1 & 3)")

## ----biplot-vegan-unconstrained-----------------------------------------------
ggbiplot_vegan(
  vare_pca,
  scaling = 2,
  species_abbrev = TRUE,
  cor_circle_geom = "line"
) +
  ggtitle("Unconstrained RDA with Correlation Circle (Scaling = 2)")

## ----biplot-vegan-constrained-cont--------------------------------------------
vare_rda <- rda(varespec ~ N + P + K, data = varechem)

ggbiplot_vegan(
  vare_rda,
  site_data = varechem,
  site_mapping = aes(color = pH),
  species_geom = "text",
  species_abbrev = TRUE,
  bp_geom = "arrow"
) +
  scale_color_viridis_c() +
  ggtitle("Constrained RDA with Environmental Vectors (N, P, K) and Site pH")

## ----biplot-vegan-cca---------------------------------------------------------
data(dune)
data(dune.env)

dune_cca <- cca(dune ~ A1 + Management, data = dune.env)

ggbiplot_vegan(
  dune_cca,
  site_data = dune.env,
  site_merge_by = "row.numbers",
  site_mapping = aes(color = Management, shape = Management),
  ellipse_geom = "line",
  ellipse_mapping = aes(color = Management),
  ellipse_level = 0.68,
  species_geom = "point",
  species_mapping = NULL,
  bp_geom = "arrow",
  cn_geom = "text"
) +
  ggtitle("Constrained CCA of Dune Meadow Vegetation")

## ----manual-layers------------------------------------------------------------
site_df <- get_display_data(
  vare_pca,
  data = varechem,
  display = "sites",
  scaling = 2,
  merge_by = "row.names"
)
species_df <- get_display_data(
  vare_pca,
  display = "species",
  scaling = 2,
  species_abbrev = TRUE
)
circle_r <- attr(species_df, "r")
axis_labs <- attr(site_df, "axis.labels")

ggplot(site_df, aes(x = PC1, y = PC2)) +
  geom_corcircle(radius = circle_r, linetype = "dashed", color = "grey50") +
  geom_arrow(aes(x = PC1, y = PC2), data = species_df, color = "firebrick", alpha = 0.6) +
  geom_arrowlabel(aes(x = PC1, y = PC2, label = Row.names), data = species_df, color = "firebrick", size = 3) +
  geom_point(aes(size = Humdepth, color = Baresoil)) +
  scale_color_viridis_c() +
  coord_equal() +
  labs(x = axis_labs[1], y = axis_labs[2], title = "Custom Multi-Layer Biplot")

## ----hull-spider--------------------------------------------------------------
set.seed(42)
dune_nmds <- metaMDS(dune, trace = FALSE)

ggordihull_spider(
  dune_nmds,
  groups = "Management",
  site_data = dune.env,
  show_hull = TRUE,
  show_spider = TRUE,
  show_centroids = TRUE
) +
  ggtitle("NMDS Ordination with Management Hulls and Spider Webs")

## ----ordisurf-----------------------------------------------------------------
ggordisurf(dune_nmds, env_var = "A1", site_data = dune.env) +
  ggtitle("NMDS with GAM Smooth Surface for A1 Horizon Thickness")

## ----envfit-------------------------------------------------------------------
ggenvfit(
  dune_nmds,
  env = dune.env[, c("A1", "Moisture", "Management")],
  p.max = 0.05,
  permutations = 499
) +
  ggtitle("Significant Environmental Vectors and Factors (p <= 0.05)")

## ----shepard------------------------------------------------------------------
ggshepard(dune_nmds) +
  ggtitle("Shepard Diagram for Dune NMDS")

