#' Check which occurrence points fall outside of known range
#' @description The known range can be either a IUCN range, hand-delineated expert polygon or QDS cells.
#' @param range polygon defining the species range (can be an `sf` or `sp` object)
#' @param occ_data an `sf` spatial data frame with occurrence points
#' @note It's important to note a couple of things. The buffers are set at 5, 10, and 20 km
#' @author Dominic Henry
#' @details Explain what the function does
#' @return The `occ_data` input with four columns added (core:buff3)
#' @importFrom methods as
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' \dontrun{
#' data <- check_occ_expert(range, occ_data)
#' }
check_occ_expert <- function(range, occ_data) {

  # range <- st_union(range)
  # range <- sf::st_make_valid(range)

  range <- as(range, "Spatial")
  buff1 <- raster::buffer(range, width = 0.05)
  buff2 <- raster::buffer(range, width = 0.1)
  buff3 <- raster::buffer(range, width = 0.2)

  # rgeos::gIsValid(range, byid = FALSE, reason=TRUE)

  b1 <- buff1 - range
  b2 <- buff2 - buff1
  b3 <- buff3 - buff2

  if (nrow(occ_data) == 0) {
    occ_data <- NULL
  } else {
    occ_data <- occ_data %>%
      sf::st_transform(sf::st_crs(range))

    occ_data <- occ_data %>%
      dplyr:: mutate(core = as.logical(sf::st_intersects(occ_data,sf::st_union(sf::st_as_sf(range)), sparse = FALSE))) %>%
      dplyr::mutate(buff1 = as.logical(sf::st_intersects(occ_data, sf::st_as_sf(b1), sparse = FALSE))) %>%
      dplyr::mutate(buff2 = as.logical(sf::st_intersects(occ_data, sf::st_as_sf(b2), sparse = FALSE))) %>%
      dplyr::mutate(buff3 = as.logical(sf::st_intersects(occ_data, sf::st_as_sf(b3), sparse = FALSE)))
  }

  return(occ_data)
}
