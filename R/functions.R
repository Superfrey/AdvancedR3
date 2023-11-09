#' descriptive_continuous_statisitcs
#'
#' @param data
#'
#' @return
#'  summary statistics for continous variables
#' @export
#'  a table with variables mean and sd
#' @examples
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>% # can you do something about the group and define this in the function?
        dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}


#' Plot distrubution of continous scale
#'
#' @param dataset
#'
#' @return histograms
#' @export facetplot of histograms
#'
#' @examples
plot_distribution <- function(data) {
    ggplot2::ggplot(data, aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(vars(metabolite), scales = "free")
}
