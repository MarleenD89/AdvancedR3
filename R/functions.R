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
#' Title: use snakecase; convert a column's character values to snakecase font
#'
#' @param data the lipidomics dataset
#' @param columns the columns you want to convert
#'
#' @return A dataframe
#'
column_values_to_snake_case <- function(data, columns) {
  data %>%
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}

#' Title: Convert the metabolite long format into a wider one.
#'
#' @param data The lipidomics dataset.
#'
#' @return A wide data frame.
#'
metabolites_to_wider <- function(data, columns) {
    data |>
        tidyr::pivot_wider(
            names_from = {{ columns }},
            values_from = value,
            values_fn = mean,
            names_prefix = "metabolite_"
        )
}

#' Title: A transformation recipe to pre-process the data
#'
#' @param data The lipidomics dataset
#' @param metabolite_variable the column of the metabolites variable
#'
#' @return A dataframe
#'
create_recipe_spec <- function(data, metabolite_variable) {
    data %>%
        recipes::recipe() %>%
        recipes::update_role(
            {{ metabolite_variable }},
            age,
            gender,
            new_role = "predictor"
        ) %>%
        recipes::update_role(class,
                             new_role = "outcome"
        ) %>%
        recipes::step_normalize(tidyselect::start_with("metabolite_"))
}
