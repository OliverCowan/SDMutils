#' Write BART raster predictions to GeoTIFF format
#'
#' @description This function writes GeoTIFFs of BART predictions.
#' @param data a `RasterStack` object returned by `embarcadero::predict2.bart()`
#' @author Dominic Henry
#' @details The function write outputs of both binary and probability surfaces at mean values and 95% credible intervals.
#' @return Seven raster layers:
#' \enumerate{
#'  \item `SDM_prob_mean_{sppselect}`: Mean probability of habitat suitability
#'  \item `SDM_prob_lower_{sppselect}`: Lower 95% credible interval probability of habitat suitability
#'  \item `SDM_prob_upper_{sppselect}`: Upper 95% credible interval probability of habitat suitability
#'  \item `SDM_bin_mean_{sppselect}`: Mean binary presence/absence prediction (based on TSS threshold)
#'  \item `SDM_bin_lower_{sppselect}`: Lower 95% credible interval binary presence/absence
#'  \item `SDM_bin_upper_{sppselect}`: Upper 95% credible interval binary presence/absence
#'  \item `SDM_cred_int_width_{sppselect}`: Credible interval probability width (i.e., Upper CI - Lower CI)
#'
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' write_bart_rasters(map)
#' }
write_bart_rasters <- function(data){

  raster::writeRaster(data[[1]],
                      glue::glue("{BART_dir}/SDM_prob_mean_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[2]],
                      glue::glue("{BART_dir}/SDM_prob_lower_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[3]],
                      glue::glue("{BART_dir}/SDM_prob_upper_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[1]] > tss_threshold,
                      glue::glue("{BART_dir}/SDM_bin_mean_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[2]] > tss_threshold,
                      glue::glue("{BART_dir}/SDM_bin_lower_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[3]] > tss_threshold,
                      glue::glue("{BART_dir}/SDM_bin_upper_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  raster::writeRaster(data[[3]]- data[[2]],
                      glue::glue("{BART_dir}/SDM_cred_int_width_{sppselect}.tif"),
                      format = "GTiff",overwrite = TRUE)

  print("RASTERS WRITTEN TO SPECIES DIRECTORY")
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("BART_dir", "sppselect", "tss_threshold"))
