## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(inboggvegan)
library(vegan)
library(ggplot2)

## ----scree_prcomp-------------------------------------------------------------
# Perform PCA
pca <- prcomp(USArrests, scale. = TRUE)

# Three types of screeplots
p1 <- ggscreeplot(pca, type = "pev")  # Proportion of variance
p2 <- ggscreeplot(pca, type = "cev")  # Cumulative variance
p3 <- ggscreeplot(pca, type = "both") # Both on same plot

# Display the plots
p1
p2
p3

## ----scree_custom-------------------------------------------------------------
ggscreeplot(pca, type = "both") +
  ggtitle("Screeplot for USArrests PCA") +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----scree_vegan--------------------------------------------------------------
data(varespec)
data(varechem)

# RDA analysis
vare.rda <- rda(varespec ~ N + P + K, data = varechem)

# Screeplot showing constrained and unconstrained axes
ggscreeplot(vare.rda, type = "both") +
  ggtitle("Variance Explained in RDA")

## ----biplot_simple------------------------------------------------------------
# Basic PCA biplot
ggbiplot_vegan(pca)

## ----biplot_pca_color---------------------------------------------------------
# Create a grouping variable based on murder rates
state_data <- data.frame(
  group = ifelse(USArrests$Murder > median(USArrests$Murder), 
                 "High Murder", "Low Murder")
)

# Biplot with colored groups
ggbiplot_vegan(pca,
               site_data = state_data,
               site_mapping = aes(color = group, label = Row.names))

## ----biplot_rda---------------------------------------------------------------
data(dune)
data(dune.env)

# RDA with environmental variables
dune.rda <- rda(dune ~ A1 + Management, data = dune.env)

# Basic biplot with default settings
ggbiplot_vegan(dune.rda)

## ----biplot_rda_custom--------------------------------------------------------
# More detailed biplot showing environmental variables
ggbiplot_vegan(dune.rda,
               site_geom = "point",           # Sites as points
               site_data = dune.env,
               site_mapping = aes(color = Management, label = Row.names),
               species_geom = "text",         # Species as text labels
               species_abbrev = TRUE,         # Abbreviate species names
               bp_geom = "arrow",            # Show environmental variables as arrows
               cn_geom = "text") +           # Show factor centroids as text
  theme_minimal() +
  ggtitle("RDA Biplot of Dune Meadow Vegetation")

## ----custom_basics------------------------------------------------------------
# Map Management to colors and Use to shapes
ggbiplot_vegan(dune.rda,
               site_data = dune.env,
               site_mapping = aes(color = Management, 
                                 shape = Use,
                                 label = Row.names),
               species_abbrev = TRUE) +
  ggtitle("Sites Colored by Management, Shaped by Use")

## ----custom_size--------------------------------------------------------------
# Map A1 (soil thickness) to point size
ggbiplot_vegan(dune.rda,
               site_data = dune.env,
               site_mapping = aes(color = Management, 
                                 size = A1,
                                 label = Row.names),
               species_geom = "text",
               species_abbrev = TRUE) +
  scale_size_continuous(name = "A1 Horizon Thickness") +
  ggtitle("Point Size Shows Soil Thickness")

## ----custom_colors------------------------------------------------------------
ggbiplot_vegan(dune.rda,
               site_data = dune.env,
               site_mapping = aes(color = Management, label = Row.names),
               species_abbrev = TRUE) +
  scale_color_manual(values = c("BF" = "darkblue", 
                                 "HF" = "darkgreen", 
                                 "NM" = "orange", 
                                 "SF" = "darkred")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  ggtitle("Custom Colors and Theme")

## ----custom_labels------------------------------------------------------------
# Sites as points (no labels), species as labels
ggbiplot_vegan(dune.rda,
               site_geom = "point",           # No labels on sites
               site_data = dune.env,
               site_mapping = aes(color = Management),
               species_geom = "text",         # Show species labels
               species_abbrev = TRUE) +
  ggtitle("Species Labeled, Sites as Points Only")

## ----scaling------------------------------------------------------------------
# Scaling = 1: Distance biplot (focus on sites)
p1 <- ggbiplot_vegan(dune.rda, scaling = 1) +
  ggtitle("Scaling = 1 (Distance)")

# Scaling = 2: Correlation biplot (focus on species)
p2 <- ggbiplot_vegan(dune.rda, scaling = 2) +
  ggtitle("Scaling = 2 (Correlation)")

p1
p2

## ----ellipses-----------------------------------------------------------------
ggbiplot_vegan(dune.rda,
               site_data = dune.env,
               site_mapping = aes(color = Management, label = Row.names),
               ellipse_geom = "line",                    # Add ellipses
               ellipse_mapping = aes(color = Management),
               ellipse_level = 0.95,                     # 95% confidence
               centroid_geom = "text",                   # Add group centroids as labels
               centroid_mapping = aes(color = Management, label = Management),
               species_abbrev = TRUE) +
  ggtitle("RDA with 95% Confidence Ellipses and Centroids")

## ----cca----------------------------------------------------------------------
# CCA with environmental variables
dune.cca <- cca(dune ~ A1 + Moisture + Management, data = dune.env)

ggbiplot_vegan(dune.cca,
               site_data = dune.env,
               site_mapping = aes(color = Management, label = Row.names),
               species_geom = "text",
               species_abbrev = TRUE,
               bp_geom = "arrow") +
  theme_minimal() +
  ggtitle("CCA of Dune Meadow Vegetation")

## ----cor_circle---------------------------------------------------------------
ggbiplot_vegan(dune.rda,
               scaling = 2,
               species_abbrev = TRUE,
               cor_circle_geom = "line") +  # Add correlation circle
  ggtitle("RDA with Correlation Circle")

## ----different_axes-----------------------------------------------------------
# If there are enough axes, you can plot different combinations
# For example, axes 1 and 3
ggbiplot_vegan(dune.rda, 
               choices = c(1, 3),
               species_abbrev = TRUE) +
  ggtitle("RDA Axes 1 and 3")

## ----princomp_vs_prcomp-------------------------------------------------------
# Using princomp
pca_princomp <- princomp(USArrests, cor = TRUE)
p1 <- ggbiplot_vegan(pca_princomp) + ggtitle("princomp")

# Using prcomp
pca_prcomp <- prcomp(USArrests, scale. = TRUE)
p2 <- ggbiplot_vegan(pca_prcomp) + ggtitle("prcomp")

p1
p2

## ----session------------------------------------------------------------------
sessionInfo()

