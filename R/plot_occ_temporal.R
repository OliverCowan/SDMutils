#' Create temporal plots of species occurrence data
#' @description This function summarises occurrence data in a temporal context.
#' @param data `sf` point object with species occurrence data
#' @param ptheme ggplot theme
#' @author Dominic Henry
#' @details The function draws two plots showing occurrence record frequencies and writes them to the species directory.
#' @note The size of the text and other elements of the plots can be change by customising the `theme` object.
#' @return Temporal JPEG plots: 1) `occ_frequency_date_1980_{sppselect}` and 2) `occ_frequency_date_all_{sppselect}`.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_occ_temporal(occ_data_sf, ptheme)
#' }
plot_occ_temporal <- function(data, ptheme){

  p1 <- data %>%
  ggplot2::ggplot(ggplot2::aes(x = year_date)) +
  ggplot2::geom_bar() +
  ggplot2::ylab("Count of occurrence records") +
  ggplot2::xlab("") +
  ggplot2::labs(
    title = sppselect,
    subtitle = glue::glue("{undated} undated records from a total of {nrow(data)}")
  ) +
  ptheme

ggplot2::ggsave(
  plot = p1,
  filename = glue::glue("{sdm_dir}/{sppselect}/occ_frequency_date_all_{sppselect}.jpg"),
  height = 9, width = 12
)

p2 <- data %>%
  dplyr::filter(year_date > "1980-01-01") %>%
  ggplot2::ggplot(ggplot2::aes(x = year_date)) +
  ggplot2::geom_bar() +
  ggplot2::ylab("Count of occurrence records") +
  ggplot2::xlab("") +
  ggplot2::labs(
    title = sppselect,
    subtitle = glue::glue("{undated} undated records")
  ) +
  ggplot2::scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  ptheme

ggplot2::ggsave(
  plot = p2,
  filename = glue::glue("{sdm_dir}/{sppselect}/occ_frequency_date_1980_{sppselect}.jpg"),
  height = 9, width = 12
)

print("TEMPORAL PLOTS WRITTEN TO SPECIES SDM OUTPUT DIRECTORY")

}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("sdm_dir", "sppselect",
                                                        "year_date", "undated"))

