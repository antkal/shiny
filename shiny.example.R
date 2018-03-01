# Example data for setting up geomorph.shiny.R
# AK - 20FEB2018

library(geomorph)

# 2D
data("plethodon")
gpa.2d <- gpagen(plethodon$land)
sh.2d <- gpa.2d$coords

sh.pca.2d <- plotTangentSpace(sh.2d)

x <- list(
  scores = sh.pca.2d$pc.scores,
  imp = sh.pca.2d$pc.summary$importance,
  groups = list(gp.var1 = plethodon$species,
                 gp.var2 = plethodon$site),
  gpa.coords = sh.2d,
  consensus = gpa.2d$consensus,
  shapes = sh.pca.2d$pc.shapes,
  links = plethodon$links
)
  
# 3D
data("scallops")
gpa <- gpagen(scallops$coorddata, curves = scallops$curvslide, surfaces = scallops$surfslide)
sh <- gpa$coords

sh.pca <- plotTangentSpace(sh)

x <- list(
  scores = sh.pca$pc.scores,
  imp = sh.pca$pc.summary$importance,
  groups = NULL,
  gpa.coords = sh,
  consensus = gpa$consensus,
  shapes = sh.pca$pc.shapes,
  links = NULL
)
