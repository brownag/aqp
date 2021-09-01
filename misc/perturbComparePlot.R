#' Single-profile Perturbation Visualization
#'
#' Demonstrate distributions of aggregate properties "perturbed"
#' Demonstrations of interactions between property and layer thickness and deviation from the source value as the central value of distribution
#'
#' @param p a SoilProfileColleciton
#' @param v a column name in `horizonNames(p)`
#' @param n_sim number of perturbed profiles to generate
#' @param bw default bandwidth for density plots
#' @param ... 
#'
#' @return
#' @export
#'#' @examples
#' 
library(aqp)
library(png)
library(grid)
library(magrittr)

set.seed(123)

# wtd avg properties [0-100] cm for profiles with different value and layer thickness

# profile: 2nd horizon [25, 50] has twice the property `value` than the overlying [0,25] or underlying [50,100] horizon
# all horizons have an equal 1cm std deviation
p <- data.frame(
  id = c(1, 1, 1),
  # top = c(0, 25, 50),
  # bottom = c(25, 50, 100),
  top = c(0, 25, 70),
  bottom = c(25, 70, 100),
  hzd = c(1, 1, 1),
  value = c(1, 2, 1)
)

depths(p) <- id ~ top + bottom

n = 0.5 
v <- list(
    "source" = c(0, 0, 0),
    "5x in 1st" = c(5, 1, 1) / n,
    "5x in 2nd" = c(1, 5, 1) / n,
    "5x in 3rd" = c(1, 1, 5) / n
    # "2x in 1st" = c(2, 1, 1),
    # "2x in 2nd" = c(1, 2, 1),
    # "2x in 2nd" = c(1, 1, 2)
  )

perturbComparePlot <- function(p, v, 
                               hzidx, 
                               inprop = "value",
                               outprop = "thickness", 
                               n_sim = 500, 
                               bw = 0.5, ...) {
  hzd <- horizonDepths(p)
  p[[".top"]] <- p[[hzd[1]]]
  p[[".bottom"]] <- p[[hzd[2]]]
  p[[".value"]] <- p[[inprop]]
  
  .perturbutate <- function(x, v, nsim = n_sim, thickness.attr = ".hzd") {

    stopifnot(length(v) == nrow(x))
    x[[thickness.attr]] <- v
    
    x2 <- perturb(x[1], n = n_sim, thickness.attr = '.hzd')
    x2[[".top"]] <- x2[[hzd[1]]]
    x2[[".bottom"]] <- x2[[hzd[2]]]
    x3 <- mutate_profile(x2, 
                         thickness = .bottom - .top,
                         avgvalue = sum(.value * thickness / sum(thickness)))
  }
  
  p2 <- mutate_profile(p, avgvalue = sum(.value * (.bottom - .top) / sum(.bottom - .top)), thickness = .bottom - .top)
  
  profiles <- lapply(v, function(vv) .perturbutate(p, vv))
  
  # setup grid page
  grid::grid.newpage()
  
  # add a series of density plots
  plot(density( profiles[[1]][,2][[outprop]], bw = bw), ...)
  for (i in 2:length(v)) {
    lines(density(profiles[[i]][,2][[outprop]], bw = bw), lty  = i)
  }
  
  # add source line and legend
  abline(v = p2[, 2][[outprop]])
  mtext(side = 3, p2[, 2][[outprop]])
  if (length(names(v)) > 0) {
    n <- names(v)
  } else {
    n <- 1:length(v)
  }
  legend("top", legend = n, lty = 1:length(v))
  
  # write SPC plot to tmp png
  tf <- tempfile(fileext = ".png")
  png(tf)
  par(bg = NA)
  plotSPC(p, color = inprop, cex.depth.axis = 2, cex.names = 2, axis.line.offset = -10)
  dev.off()
  
  # plot SPC grid raster in right viewport
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  grid::pushViewport(grid::viewport(layout.pos.col = 2, layout.pos.row = 1))
  grid::grid.raster(png::readPNG(tf))

}

perturbComparePlot(
  p,
  v,
  hzidx = 2,
  n_sim = 10,
  ylim=c(0,10),
  xlim=c(1,2),
  outprop = "avgvalue", 
  bw = 0.05,
  main = "weighted average `value` [0,100]"
)


