#' Title: descriptive stats for dataframes
#'
#' @param a dataframe
#'
#' @return "A data.frame/tibble" (output)
#'
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(
            dplyr::across(
                value,
                base::list(
                    mean = mean,
                    sd = sd
                )
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                tidyselect::where(is.numeric), ~ base::round(.x, digits = 1)
            )
        )
}

#' Title: Making a histogram with ggplo2
#'
#' @param a dataframe
#'
#' @return “A plot object”
#'
plot_distributions <- function(data) {
    data %>% ggplot2::ggplot(ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
