#' Check which occurrence points fall outside of known range
#' @description The known range can be either a IUCN range, hand-delineated expert polygon or a QDS shapefile.
#' @param range polygon defining the species range (can be an `sf` or `sp` object)
#' @param occ_data an `sf` spatial data frame with occurrence points
#' @note If a point is located beyond the 20km buffer then all of `core`, `buff1`, `buff2`, and `buff3` will be given the value `FALSE`.
#' @author Dominic Henry
#' @details The function creates three buffers around the range and identifies where a point is located based on the buffers (i.e., within the core range, or within a 5km, 10km, or 20km buffer). Four columns are added to the input data frame (`core`, `buff1`, `buff2`, and `buff3`) and are given a logical value depending on where the point is located (`TRUE` if the point is in the core or buffers, `FALSE` if it isn't).
#' @return The `occ_data` input with four columns added (`core`, `buff1`, `buff2`, and `buff3`)
#' @importFrom methods as
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' \dontrun{
#' data <- check_occ_range(range, occ_data)
#' }
check_occ_range <- function(range, occ_data) {

  range <- as(range, "Spatial")
  buff1 <- terra::buffer(range, width = 0.05)
  buff2 <- terra::buffer(range, width = 0.1)
  buff3 <- terra::buffer(range, width = 0.2)

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
