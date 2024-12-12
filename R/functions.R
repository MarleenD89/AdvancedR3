#' Title: descriptive stats for dataframes
#' @param a dataframe
#' @return "A data.frame/tibble" (output)
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
#' @param a dataframe
#' @return “A plot object”
plot_distributions <- function(data) {
  data %>% ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}

#' Title: use snakecase; convert a column's character values to snakecase font
#' @param data the lipidomics dataset
#' @param columns the columns you want to convert
#' @return A dataframe
column_values_to_snake_case <- function(data, columns) {
  data %>%
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}

#' Title: Convert the metabolite long format into a wider one.
#' @param data The lipidomics dataset.
#' @return A wide data frame.
metabolites_to_wider <- function(data) {
  data |>
    tidyr::pivot_wider(
      names_from = metabolite,
      values_from = value,
      values_fn = mean,
      names_prefix = "metabolite_"
    )
}

#' Title: A transformation recipe to pre-process the data
#' @param data The lipidomics dataset
#' @param metabolite_variable the column of the metabolites variable
#' @return recipe object
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
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}

#' Title: create a workflow object of the model and transformations
#' @param model_specs the model specs
#' @param recipe_specs the recipe specs
#' @return A workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}

#' Title: Create a tidy output of the model results
#' @param workflow_fitted_model the model workflow that has been fitted
#' @return A dataframe
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}

#' Title: Convert the long form dataset into a list of wide form data frames based on metabolite
#' @param data lipidomics dataset
#' @return list of dataframes
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}

#' Title: Generate results of a model
#' @param data lipidomics dataset
#' @return dataframe
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() %>%
      parsnip::set_engine("glm"),
    data %>%
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Title Function to create the estimates table
#'
#' @param data lipidomics dataset
#'
#' @return dataframe
calculate_estimates <- function(data) {
  model_estimates <- data %>%
    split_by_metabolite() %>%
    purrr::map(generate_model_results) %>%
    purrr::list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "metabolite_"))
  data %>%
    dplyr::select(metabolite) %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snake_case(term) %>%
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
    dplyr::distinct(metabolite, term) %>%
    dplyr::right_join(model_estimates, by = "term")
}
