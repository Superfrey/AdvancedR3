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


#' Change column names to snakecase
#'
#' @param data
#' @param cols
#'
#' @return lowercase letters
#' @export
#'
#' @examples to_two
#'
column_values_to_snake_case <- function(data, cols) {
    data %>%
        dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}

#' Pivot long format to wide format
#' and summary mean by metabolite
#'
#' @param data
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
metabolites_to_wide <- function(data) {
    data %>%
        tidyr::pivot_wider(
            names_from = metabolite,
            values_from = value,
            values_fn = mean,
            names_prefix = "metabolite_"
        )
}


#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable.
#'
#' @return
#'
create_recipe_spec <- function(data, metabolite_variable) {
    recipes::recipe(data) %>%
        recipes::update_role({{ metabolite_variable }}, age, gender, new_role = "predictor") %>%
        recipes::update_role(class, new_role = "outcome") %>%
        recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}



#' Create a workflow object of the model and transformations.
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
#'
create_model_workflow <- function(model_specs, recipe_specs) {
    workflows::workflow() %>%
        workflows::add_model(model_specs) %>%
        workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted.
#'
#' @return A data frame.
#'
tidy_model_output <- function(workflow_fitted_model) {
    workflow_fitted_model %>%
        workflows::extract_fit_parsnip() %>%
        broom::tidy(exponentiate = TRUE)
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset.
#'
#' @return A data frame.
#'
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


#' Convert the long form dataset into a list of wide form data frames.
#'
#' @param data The lipidomics dataset.
#'
#' @return A list of data frames.
#'
split_by_metabolite <- function(data) {
    data %>%
        column_values_to_snake_case(metabolite) %>%
        dplyr::group_split(metabolite) %>%
        purrr::map(metabolites_to_wide)
}

#' Loop analysis with multiple analysis in one go
#'
#' @param data
#'
#' @return estimates and p-values
#' @export a table
#'
#' @examples
loop_analysis_metabolites <- function(data) {
    data %>%
        split_by_metabolite() %>%
        purrr::map(generate_model_results) %>%
        list_rbind() %>%
        dplyr::filter(stringr::str_detect(term, "metabolite_"))
}

#' Create names for table
#'
#' @param model_results
#' @param data for the analusis
#'
#' @return metabolites names
#' @export
#'
#' @examples
add_original_metabolite_names <- function(model_results, data) {
    data %>%
        mutate(term = metabolite) %>%
        column_values_to_snake_case(term) %>%
        mutate(term = str_c("metabolite_", term)) %>%
        distinct(term, metabolite) %>%
        right_join(model_results, by = "term")
}

#' calculate estimates and get names combined
#'
#' @param data and models
#'
#' @return table with estimates
#' @export
#'
#' @examples
calculate_estimates <- function(data) {
    data %>%
        loop_analysis_metabolites() %>%
        add_original_metabolite_names(data)
}


#' Forrest function for results
#'
#' @param results
#'
#' @return
#' @export forrest plot
#'
#' @examples
plot_estimates <- function(results) {
    results %>%
        ggplot(aes(
            x = estimate,
            y = metabolite,
            xmin = estimate - 1.96 * (std.error),
            xmax = estimate + 1.96 * (std.error)
        )) +
        geom_pointrange() +
        coord_fixed(xlim = c(-3, 10))
}
