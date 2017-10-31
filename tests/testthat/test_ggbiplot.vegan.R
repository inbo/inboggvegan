# expect_that(x, is_true()) expect_true(x)
# expect_that(x, is_false()) expect_false(x)
# expect_that(x, is_a(y)) expect_is(x, y)
# expect_that(x, equals(y)) expect_equal(x, y)
# expect_that(x, is_equivalent_to(y)) expect_equivalent(x, y)
# expect_that(x, is_identical_to(y)) expect_identical(x, y)
# expect_that(x, matches(y)) expect_matches(x, y)
# expect_that(x, prints_text(y)) expect_output(x, y)
# expect_that(x, shows_message(y)) expect_message(x, y)
# expect_that(x, gives_warning(y)) expect_warning(x, y)
# expect_that(x, throws_error(y)) expect_error(x, y)





#test ggbiplot.vegan
# test_that("ggbiplot.vegan input",{
#   library(dplyr)
#   library(vegan)
#   data(varechem)
#   data(varespec)
#   data(dune)
#   data(dune.env)
#   dataset <- iris
#   dataset$Species2 <- dataset$Species
#   datatibble <- tibble::as_tibble(dataset)
#   levels(dataset$Species2) <- list(Sgroup = "setosa", Vgroup = c("versicolor", "virginica"))
#
#   #test basisfunctionaliteit
#   rdaobj <- rda(dataset[,1:4] ~ 1)
#   rdaobj2 <- rda(dataset[,1:4]  ~ Species, data = iris)
#   capobj2 <- capscale(dataset[,1:4] ~ Species, data = iris)
#
#   #enkel rda object
#   prda <- ggbiplot.vegan(rdaobj, show.labels = FALSE)
#   expect_equal(length(prda),9)
#   expect_equal(prda$labels$y, "standardized PC2 (5.3% explained var.)")
#
#   #enkel cca object
#   expect_silent(prda2 <- ggbiplot.vegan(rdaobj2, show.labels = TRUE))
#
#   #enkel capscale object
#   expect_silent(pcap2 <- ggbiplot.vegan(capobj2, show.labels = TRUE))
#
#
#   #rda object + data.frame + groupsvarname + ellgroupsvarname
#   expect_silent(p1 <- ggbiplot.vegan(rdaobj, data = dataset, groupsvarname = "Species", ellgroupsvarname = "Species2"))
#   expect_silent(p2 <- ggbiplot.vegan(rdaobj, data = datatibble, groupsvarname = "Species", ellgroupsvarname = "Species2"))
#
#   pg <- ggplot2::ggplot_build(ggbiplot.vegan(rdaobj, data = dataset, groupsvarname = "Species", ellgroupsvarname = "Species2"))
#   expect_equal(length(unique(pg$data[[1]]$group)),3)
#   expect_equal(nrow(pg$data[[3]]), 4)
#   expect_equal(all(is.na(pg$data[[4]])), F)
#   expect_equal(length(unique(pg$data[[4]]$colour)), 3)
#
#   #idem maar met een tibble als dataset
#   pt <- ggplot2::ggplot_build(ggbiplot.vegan(rdaobj, data = datatibble, groupsvarname = "Species", ellgroupsvarname = "Species2"))
#   expect_equal(length(unique(pt$data[[1]]$group)),3)
#   expect_equal(nrow(pt$data[[3]]), 4)
#   expect_equal(all(is.na(pt$data[[4]]$x)), F)
#   expect_equal(length(unique(pt$data[[4]]$colour)), 3)
#
#   #check CCA varechem
#   myvarechemdesc <- varechem["pH"]
#   myvarechemdesc$pHclass <- factor(floor(myvarechemdesc$pH))
#   myvarechemdesc$Loco <- factor(sample(c("a","b","d"), nrow(myvarechemdesc)  , replace = TRUE))
#   vare.cca <- cca(varespec  ~ N + P + K + Ca, data = varechem)
#   pccavar <- ggbiplot.vegan(vare.cca, var.as.arrows = FALSE) #is niet zoals gewenst
#
#   #Check CCA dune
#   mod <- cca(dune ~ A1 + Moisture + Management, dune.env)
#   pccadune <- ggbiplot.vegan(mod, data = dune.env, show.labels = FALSE,
#                              var.as.arrows = TRUE, var.axes = TRUE, groupsvarname = "Management")
#
#   #Check capscale varechem
#   varechem$Humdepth <- factor(round(varechem$Humdepth))
#   varechem$labels.cn <- paste("HD:",as.numeric(varechem$Humdepth),sep="")
#   varespec.log <- log(varespec + 1)
#   mod.cap <- capscale(varespec.log ~   N , data=varechem,distance="bray", add=TRUE)
#   pcapvar <- ggbiplot.vegan(mod.cap, choices = 1:2, data = varechem,
#                             labelsvarname = "labels.cn", scaling = 2,
#                             var.axes = TRUE, var.as.arrows = FALSE,
#                             env.axes = c("cn", "bp"), circle = TRUE)
#
# })

