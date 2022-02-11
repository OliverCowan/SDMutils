#' Create temporal plots of species occurrence data
#' @description This function summarises occurrence data in a spatial context.
#' @param data `sf` point object with species occurrence data
#' @param range `sf` polygon defining the species range (IUCN, expert polygon, or QDS range)
#' @param za shapefile of South Africa read in as an `sf` object
#' @param ptheme ggplot theme object
#' @author Dominic Henry
#' @details The function draws three plots showing occurrence records overlaid on South Africa. Plots are written to the species directory.
#' @note The size of the text and other elements of the plots can be change by customising the `theme` object.
#' @return Spatial JPEG plots: 1) `occ_map_{sppselect}`; 2) `occ_map_decadal_{sppselect}`; and 3) `occ_map_inset_{sppselect}`
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' plot_occ_spatial(occ_data_sf, range, za, ptheme)
#' }
plot_occ_spatial <- function(data, range, za, ptheme) {

  occ_bbox <- sf::st_make_grid(sf::st_buffer(data, 0.3), n = 1) # Make a grid with one cell

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = za, fill = ggplot2::alpha("grey", 0.3), size = 0.5) +
    ggplot2::geom_sf(data = range, fill = NA, col = "dodgerblue", size = 0.8) +
    ggplot2::geom_sf(data = data, size = 1.5, col = "black") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(glue::glue("{sppselect}: {nrow(data)} occurrence points")) +
    ptheme

  ggplot2::ggsave(glue::glue("{sdm_dir}/{sppselect}/occ_map_{sppselect}.jpg"), plot = p1, height = 12, width = 16)

  p2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = za, fill = NA, size = 0.5) +
    ggplot2::geom_sf(data = data, ggplot2::aes(color = decade), size = 3) +
    ggplot2::scale_color_viridis_d(na.value = ggplot2::alpha("grey", 0.7), direction = 1) +
    ggplot2::guides(color = ggplot2::guide_legend(
      title = "Decade",
      override.aes = list(size = 8)
    )) +
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(occ_bbox)$xmin, sf::st_bbox(occ_bbox)$xmax),
      ylim = c(sf::st_bbox(occ_bbox)$ymin, sf::st_bbox(occ_bbox)$ymax)
    ) +
    ggplot2::theme_bw() +
    ptheme +
    ggplot2::ggtitle(glue::glue("{sppselect}"))

  ggplot2::ggsave(glue::glue("{sdm_dir}/{sppselect}/occ_map_decadal_{sppselect}.jpg"), plot = p2, height = 12, width = 16)

  p3 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = za, fill = ggplot2::alpha("grey", 0.3), size = 0.5) +
    ggplot2::geom_sf(data = occ_bbox, fill = NA, col = "dodgerblue", size = 1.2) +
    ggplot2::geom_sf(data = data, size = 1.5, col = "black") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(glue::glue("{sppselect}")) +
    ptheme

  p4 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = za, fill = ggplot2::alpha("grey", 0.3), size = 0.5) +
    ggplot2::geom_sf(data = data, size = 1.5, col = "black") +
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(occ_bbox)$xmin, sf::st_bbox(occ_bbox)$xmax),
      ylim = c(sf::st_bbox(occ_bbox)$ymin, sf::st_bbox(occ_bbox)$ymax)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "dodgerblue", fill = NA, size = 1.6)) +
    ptheme

  p5 <- gridExtra::grid.arrange(grobs = list(p3, p4), nrow = 1)

  ggplot2::ggsave(glue::glue("{sdm_dir}/{sppselect}/occ_map_inset_{sppselect}.jpg"),
                  plot = p5, height = 12, width = 16
  )

  print("SPATIAL PLOTS WRITTEN TO SPECIES SDM OUTPUT DIRECTORY")
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("sdm_dir", "sppselect", "decade"))
