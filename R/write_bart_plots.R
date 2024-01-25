#' Generate maps of BART SDM predictions
#' @description This function writes plots using a combination of PDF and PNG outputs.
#' @param data a `RasterStack` object returned by `embarcadero::predict2.bart()`
#' @param mean_theme a `rasterTheme` used for plotting the mean probability SDM map
#' @param ci_theme a `rasterTheme` used for plotting the posterior width (95% credible interval) SDM map
#' @author Dominic Henry
#' @details The function writes 8 plots related to the species SDM predictions, both probability and binary surfaces. The species' occurrence points have been overlaid on certain plots. Plots are written to the species directory.
#' @note There are several undeclared variables to take note of: `BART_dir`, `tss_threshold`, `occ`, `num_occ_points`, and `occurence_points`
#' @return The following plot files:
#' \enumerate{
#'  \item `SDM_mean_probability.pdf`
#'  \item `SDM_mean_probability.png`
#'  \item `SDM_posterior_width.pdf`
#'  \item `SDM_posterior_width.png`
#'  \item `SDM_four_panel_probability.pdf`
#'  \item `SDM_four_panel_binary.pdf`
#'  \item `SDM_mean_binary.pdf`
#'  \item `SDM_mean_binary.png`
#'
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' write_bart_plots(data = map, mean_theme = maptheme_mean, ci_theme = maptheme_ci)
#' }
write_bart_plots <- function(data, mean_theme, ci_theme) {
  p <- rasterVis::levelplot(data[[1]],
                            maxpixels = 1e10,
                            margin = FALSE,
                            par.settings = mean_theme,
                            scales = list(
                              x = list(draw = FALSE),
                              y = list(draw = FALSE)
                            ),
                            zlim = c(0, 1)
  )

  p1 <- p +
    latticeExtra::layer(sp::sp.points(occurence_points, pch = 20, col = "black", cex = 0.8),
                        data = list(occurence_points = terra::points(points(occ[1:num_occ_points, ])))
    )

  p2 <- p +
    latticeExtra::layer(sp::sp.points(occurence_points, pch = 20, col = "black", cex = 4),
                        data = list(occurence_points = terra::points(points(occ[1:num_occ_points, ])))
    )

  pdf(glue::glue("{BART_dir}/SDM_mean_probability.pdf"), width = 16, height = 9)
  print(p1)
  grid::grid.text("Probability of presence",
                  rot = 90,
                  y = grid::unit(0.5, "npc"),
                  x = grid::unit(0.925, "npc"),
                  gp = grid::gpar(fontsize = 15)
  )
  dev.off()

  grDevices::png(glue::glue("{BART_dir}/SDM_mean_probability.png"), width = 2800, height = 2000)
  print(p2)
  grid::grid.text("Probability of presence",
                  rot = 90,
                  y = grid::unit(0.5, "npc"),
                  x = grid::unit(0.925, "npc"),
                  gp = grid::gpar(fontsize = 45)
  )
  dev.off()

  p3 <- rasterVis::levelplot(data[[3]] - data[[2]],
                             maxpixels = 1e10,
                             margin = FALSE,
                             par.settings = ci_theme,
                             scales = list(
                               x = list(draw = FALSE),
                               y = list(draw = FALSE)
                             ),
                             zlim = c(0, 1)
  )

  pdf(glue::glue("{BART_dir}/SDM_posterior_width.pdf"), width = 16, height = 9)
  print(p3)
  grid::grid.text("Posterior width",
                  rot = 90,
                  y = grid::unit(0.5, "npc"),
                  x = grid::unit(0.925, "npc"),
                  gp = grid::gpar(fontsize = 15)
  )
  dev.off()


  grDevices::png(glue::glue("{BART_dir}/SDM_posterior_width.png"), width = 2800, height = 2000)
  print(p3)
  grid::grid.text("Posterior width",
                  rot = 90,
                  y = grid::unit(0.5, "npc"),
                  x = grid::unit(0.925, "npc"),
                  gp = grid::gpar(fontsize = 45)
  )
  dev.off()

  ## Four measure SDM plots
  pdf(glue::glue("{BART_dir}/SDM_four_panel_probability.pdf"), width = 16, height = 9)
  par(mfrow = c(2, 2))
  par(mar = c(2, 1, 2, 5))
  terra::plot(data[[1]],
               main = "Posterior mean",
               box = F, axes = F
  )
  terra::plot(data[[2]],
               main = "Lower 95% CI bound",
               box = F, axes = F
  )
  terra::plot(data[[3]],
               main = "Upper 95% CI bound",
               box = F, axes = F
  )
  terra::plot(data[[3]] - data[[2]],
               main = "Credible interval width",
               box = F, axes = F
  )
  dev.off()

  ## Binary predictions
  pdf(glue::glue("{BART_dir}/SDM_four_panel_binary.pdf"), width = 16, height = 9)
  par(mfrow = c(2, 2))
  terra::plot(data[[1]] > tss_threshold,
               main = "Posterior mean",
               box = F, axes = F
  )
  terra::plot(data[[2]] > tss_threshold,
               main = "Lower 95% CI bound",
               box = F, axes = F
  )
  terra::plot(data[[3]] > tss_threshold,
               main = "Upper 95% CI bound",
               box = F, axes = F
  )
  quant <- terra::quantile(terra::values(data[[3]] - data[[2]]),
                           0.75,
                           na.rm = TRUE
  )
  terra::plot((data[[3]] - data[[2]]) > quant,
               box = FALSE,
               axes = FALSE,
               main = "Highest uncertainty zones",
               axis.args = list(at = pretty(0:1), labels = pretty(0:1)),
               legend.args = list(text = "", side = 2, line = 1.3)
  )
  dev.off()


  ## Mean binary
  pdf(glue::glue("{BART_dir}/SDM_mean_binary.pdf"), width = 16, height = 9)
  par(mfrow = c(1, 1))
  terra::plot(data[[1]] > tss_threshold,
               main = "Predicted presence",
               box = F, axes = F
  )
  terra::points(points(occ[1:num_occ_points, ], col = "blue", pch = 20))
  dev.off()

  grDevices::png(glue::glue("{BART_dir}/SDM_mean_binary.png"), width = 2800, height = 2000)
  par(mfrow = c(1, 1))
  terra::plot(data[[1]] > tss_threshold,
               main = "Predicted presence",
               box = F, axes = F
  )
  terra::points(points(occ[1:num_occ_points, ], col = "blue", pch = 20, cex = 3.5))
  dev.off()

  print("PLOTS WRITTEN TO SPECIES DIRECTORY")
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("BART_dir", "tss_threshold",
                                                        "occ", "num_occ_points",
                                                        "occurence_points"))
