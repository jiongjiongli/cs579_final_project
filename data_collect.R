################################################################################
# Author: Jiongjiong Li
# Email: jli268@hawk.illinoistech.edu
################################################################################

# Comment out below if they are not installed:
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("curl", type = "source")
# install.packages("tigris")
# install.packages("patchwork")
# install.packages("igraph")

library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(tidycensus)
library(tidyverse)
library(tigris)
library(patchwork)
library(yaml)
library(tibble)
library(igraph)
library(RColorBrewer)


################################################################################
# Start of functions
# ------------------------------------------------------------------------------

format_elapsed <- function(start, end) {
    elapsed <- as.numeric(end - start, units = "secs")

    hours   <- floor(elapsed / 3600)
    minutes <- floor((elapsed %% 3600) / 60)
    seconds <- round(elapsed %% 60, 2)

    parts <- c()
    if (hours > 0)   parts <- c(parts, paste0(hours, "h"))
    if (minutes > 0) parts <- c(parts, paste0(minutes, "m"))
    parts <- c(parts, paste0(seconds, "s"))

    paste(parts, collapse = " ")
}


get_all_community_sf <- function(boundary_file_path) {
    chi_ca_sf <- sf::st_read(boundary_file_path)
    # Converts all coordinates into North America NAD83 CRS
    chi_ca_proj_sf <- sf::st_transform(chi_ca_sf, crs=4269)

    return(chi_ca_proj_sf)
}

plot_all_community_boundaries <- function(community_sf,
                                    width = 10,
                                    height = 3) {

    community_sf_plot <- st_transform(community_sf, crs = 26916)

    community_plot <- ggplot(community_sf_plot) +
    geom_sf(fill = NA, color = "black") +
    geom_sf_text(aes(label = area_numbe), size = 3, color = "red") +
    labs(title = paste("Chicago Community Boundaries")) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(
            hjust = 0.5,    # center horizontally
            face = "plain", # optional: remove bold,
        ),
        panel.background = element_rect(fill = "white", color = NA),    # remove panel border
        plot.background  = element_rect(fill = "white", color = NA),    # remove surrounding border
    ) +
    xlab("Longitude") +
    ylab("Latitude")

    # Save as PNG
    file_stem <- paste(gsub(" ", "_", tolower("Chicago Community Boundaries")))
    file_name <- paste0(file_stem, ".png")

    ggsave(file.path(getwd(), file_name),
           community_plot,
           width = width, height = height, dpi = 300)
}

get_decennial_data <- function(decennial_config, community_sf) {
    geography_unit <- decennial_config$geography_unit
    categories <- decennial_config$categories

    category_results <- list()

    for (category_idx in seq_along(categories)) {
        category_info <- decennial_config$categories[[category_idx]]

        category_name <- category_info$category_name
        items         <- category_info$items

        item_results <- list()

        for (item_idx in seq_along(items)) {
            variable_results <- list()

            item <- items[[item_idx]]

            variables <- item$variables

            for (variable_idx in seq_along(variables)) {
                variable <- variables[[variable_idx]]
                variable_name <- variable$variable_name
                year <- variable$year
                raw_data <- get_decennial(
                    geography   = geography_unit,
                    variables   = variable_name,
                    year        = year,
                    state       = "IL",
                    county      = "Cook",
                    # Include shapefile for mapping
                    geometry    = TRUE,
                    cache_table = TRUE
                )

                is_intersect <- st_intersects(raw_data, community_sf)
                data <- raw_data[lengths(is_intersect) > 0, ]

                # Store result for this variable
                variable_results[[length(variable_results) + 1]] <- list(
                    variable_idx   = variable_idx,
                    variable_name  = variable_name,
                    year           = year,
                    data           = data
                )
            }

            item_results[[length(item_results) + 1]] <- list(
                item_idx         = item_idx,
                description      = item$description,
                variable_results = variable_results
            )
        }

        category_results[[length(category_results) + 1]] <- list(
            category_idx     = category_idx,
            category_name    = category_name,
            item_results     = item_results
        )
    }

    return(category_results)
}


download_decennial_data <- function(decennial_config,
                                    data_type) {
    start_time <- Sys.time()

    geography_unit <- decennial_config$geography_unit
    categories <- decennial_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        items <- category_info$items

        for (item_idx in seq_along(items)) {
            item <- items[[item_idx]]

            description <- item$description
            variables <- item$variables

            for (variable_idx in seq_along(variables)) {
                variable <- variables[[variable_idx]]
                variable_name <- variable$variable_name
                year <- variable$year
                raw_data <- get_decennial(
                    geography   = geography_unit,
                    variables   = variable_name,
                    year        = year,
                    state       = "IL",
                    county      = "Cook",
                    # Include shapefile for mapping
                    geometry    = TRUE,
                    cache_table = TRUE
                )

                file_name <- paste(
                    data_type,
                    "year", year,
                    "cat_idx", category_idx,
                    "item_idx", item_idx,
                    "var_idx", variable_idx,
                    "var_name", variable_name,
                    sep = "_"
                )

                file_name <- gsub(" ", "_", file_name)
                file_name <- paste0(file_name, ".rds")

                raw_data_dir_path <- file.path(getwd(), "raw_data_cache_v2")
                raw_data_file_path <- file.path(raw_data_dir_path, file_name)

                if (!dir.exists(raw_data_dir_path)) {
                    dir.create(raw_data_dir_path)
                }

                saveRDS(raw_data, raw_data_file_path)
            } # end variable loop
        } # end item loop
    } # end category loop

    end_time <- Sys.time()

    cat("Completed! Duration:",
        format_elapsed(start_time, end_time), "\n")
}


download_acs_data <- function(acs_config,
                              data_type,
                              geography_unit = "",
                              survey = "") {
    start_time <- Sys.time()

    if (geography_unit == "") {
        geography_unit <- acs_config$geography_unit
    }

    if (survey == "") {
        survey <- acs_config$survey
    }

    start_year <- acs_config$start_year
    end_year <- acs_config$end_year
    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        category_name <- category_info$category_name
        items <- category_info$items

        for (item_idx in seq_along(items)) {
            item <- items[[item_idx]]

            description <- item$description
            variable_name <- item$variable_name

            for (year in start_year:end_year) {
                if (year == 2020 && survey == "acs1") {
                    message("2020 ACS 1-year is not available. Skipping...")
                    next  # continue to next year
                }

                curr_time <- Sys.time()
                cat("[get_acs]",
                    "[ category_idx", category_idx, "/", length(categories), "]",
                    "[ item_idx", item_idx, "/", length(items), "]",
                    "category_name:", category_name,
                    "description:", description,
                    "variable_name:", variable_name,
                    "year:", year,
                    "geography_unit:", geography_unit,
                    "survey:", survey,
                    "Time elapsed:", format_elapsed(start_time, curr_time),
                    "\n")


                if (geography_unit == "state") {
                    raw_data <- get_acs(
                        geography = geography_unit,
                        variables = variable_name,
                        year      = year,
                        # Include shapefile for mapping
                        geometry  = TRUE,
                        survey = survey
                    )
                } else {
                    raw_data <- get_acs(
                        geography = geography_unit,
                        variables = variable_name,
                        year      = year,
                        state     = "IL",
                        county    = "Cook",
                        # Include shapefile for mapping
                        geometry  = TRUE,
                        survey = survey
                    )
                }

                cat("nrow(raw_data) =", nrow(raw_data), "\n")
                stopifnot(nrow(raw_data) > 0)

                file_name <- paste(
                    data_type,
                    "survey", survey,
                    "geography_unit", geography_unit,
                    "year", year,
                    "cat_idx", category_idx,
                    "item_idx", item_idx,
                    "var_name", variable_name,
                    sep = "_"
                )
                file_name <- gsub(" ", "_", file_name)
                file_name <- paste0(file_name, ".rds")

                raw_data_dir_path <- file.path(getwd(), "raw_data_cache_v2")
                raw_data_file_path <- file.path(raw_data_dir_path, file_name)

                if (!dir.exists(raw_data_dir_path)) {
                    dir.create(raw_data_dir_path)
                }

                saveRDS(raw_data, raw_data_file_path)
            } # end year loop
        } # end item loop

    } # end category loop

    end_time <- Sys.time()

    cat("Completed! Duration:",
        format_elapsed(start_time, end_time), "\n")
}


get_decennial_sum_results <- function(decennial_config,
                                      data_type,
                                      all_community_sf) {
    start_time <- Sys.time()

    value_sum_results <- list()

    geography_unit <- decennial_config$geography_unit
    categories <- decennial_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- decennial_config$categories[[category_idx]]

        category_name <- category_info$category_name
        items         <- category_info$items

        for (item_idx in seq_along(items)) {
            item <- items[[item_idx]]

            description <- item$description
            variables   <- item$variables

            for (variable_idx in seq_along(variables)) {
                variable <- variables[[variable_idx]]
                variable_name <- variable$variable_name
                year <- variable$year

                curr_time <- Sys.time()
                cat("[ category_idx", category_idx, "/", length(categories), "]",
                    "[ item_idx", item_idx, "/", length(items), "]",
                    "category_name:", category_name,
                    "description:", description,
                    "year:", year,
                    "Time elapsed:", format_elapsed(start_time, curr_time),
                    "\n")

                file_name <- paste(
                    data_type,
                    variable_name,
                    year,
                    paste0("cat", category_idx),
                    paste0("item", item_idx),
                    paste0("var", variable_idx),
                    sep = "_"
                )
                file_name <- gsub(" ", "_", file_name)
                file_name <- paste0(file_name, ".rds")

                raw_data_dir_path <- file.path(getwd(), "raw_data_cache_v2")
                raw_data_file_path <- file.path(raw_data_dir_path, file_name)

                if (!file.exists(raw_data_file_path)) {
                    raw_data <- get_decennial(
                        geography   = geography_unit,
                        variables   = variable_name,
                        year        = year,
                        state       = "IL",
                        county      = "Cook",
                        # Include shapefile for mapping
                        geometry    = TRUE,
                        cache_table = TRUE
                    )

                    if (!dir.exists(raw_data_dir_path)) {
                        dir.create(raw_data_dir_path)
                    }

                    saveRDS(raw_data, raw_data_file_path)
                }
                else {
                    raw_data <- readRDS(raw_data_file_path)
                }

                for (community_idx in 1:nrow(all_community_sf)) {
                    community_sf <- all_community_sf[community_idx, ]

                    is_intersect <- st_intersects(raw_data, community_sf)
                    data <- raw_data[lengths(is_intersect) > 0, ]

                    if (data_type == "decennial") {
                        value_sum <- sum(data$value, na.rm = TRUE)
                    } else {
                        value_sum <- sum(data$estimate, na.rm = TRUE)
                    }

                    value_sum_result = list(
                        category_idx   = category_idx,
                        category_name  = category_name,

                        item_idx       = item_idx,
                        description    = description,

                        variable_idx   = variable_idx,
                        variable_name  = variable_name,
                        year           = year,
                        value_sum      = value_sum,

                        community_idx = community_idx,
                        area_number = community_sf$area_numbe,
                        community_name = community_sf$community
                    )

                    value_sum_results[[length(value_sum_results) + 1]] <- value_sum_result

                } # end community loop
            } # end variable loop
        } # end item loop
    } # end category loop

    end_time <- Sys.time()

    cat("Completed! duration:",
        format_elapsed(start_time, end_time), "\n")

    return(value_sum_results)
}


get_acs_sum_results <- function(all_community_sf,
                                acs_config,
                                data_type,
                                geography_unit = "",
                                survey = "") {
    start_time <- Sys.time()

    if (geography_unit == "") {
        geography_unit <- acs_config$geography_unit
    }

    if (survey == "") {
        survey <- acs_config$survey
    }

    value_sum_results <- list()

    start_year <- acs_config$start_year
    end_year <- acs_config$end_year
    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        category_name <- category_info$category_name
        items <- category_info$items

        for (item_idx in seq_along(items)) {
            item <- items[[item_idx]]

            description <- item$description
            variable_name <- item$variable_name

            for (year in start_year:end_year) {
                if (year == 2020 && survey == "acs1") {
                    message("2020 ACS 1-year is not available. Skipping...")
                    next  # continue to next year
                }

                curr_time <- Sys.time()

                cat("[get_acs]",
                    "[ category_idx", category_idx, "/", length(categories), "]",
                    "[ item_idx", item_idx, "/", length(items), "]",
                    "category_name:", category_name,
                    "description:", description,
                    "variable_name:", variable_name,
                    "year:", year,
                    "geography_unit:", geography_unit,
                    "survey:", survey,
                    "Time elapsed:", format_elapsed(start_time, curr_time),
                    "\n")

                file_name <- paste(
                    data_type,
                    "survey", survey,
                    "geography_unit", geography_unit,
                    "year", year,
                    "cat_idx", category_idx,
                    "item_idx", item_idx,
                    "var_name", variable_name,
                    sep = "_"
                )
                file_name <- gsub(" ", "_", file_name)
                file_name <- paste0(file_name, ".rds")

                raw_data_dir_path <- file.path(getwd(), "raw_data_cache_v2")
                raw_data_file_path <- file.path(raw_data_dir_path, file_name)

                if (!file.exists(raw_data_file_path)) {
                    if (geography_unit == "state") {
                        raw_data <- get_acs(
                            geography = geography_unit,
                            variables = variable_name,
                            year      = year,
                            # Include shapefile for mapping
                            geometry  = TRUE,
                            survey = survey
                        )
                    } else {
                        raw_data <- get_acs(
                            geography = geography_unit,
                            variables = variable_name,
                            year      = year,
                            state     = "IL",
                            county    = "Cook",
                            # Include shapefile for mapping
                            geometry  = TRUE,
                            survey = survey
                        )
                    }

                    cat("nrow(raw_data) =", nrow(raw_data), "\n")

                    if (!dir.exists(raw_data_dir_path)) {
                        dir.create(raw_data_dir_path)
                    }

                    saveRDS(raw_data, raw_data_file_path)
                } else {
                    raw_data <- readRDS(raw_data_file_path)
                }

                if (geography_unit == "state") {
                    for (state_idx in 1:nrow(raw_data)) {
                        data <- raw_data[state_idx, ]
                        num_samples <- nrow(data)
                        state_geoid <- data$GEOID
                        state_name <- data$NAME

                        if (data_type == "decennial") {
                            value_sum <- data$value
                        } else {
                            value_sum <- data$estimate
                        }

                        value_sum_result = list(
                            category_idx   = category_idx,
                            category_name  = category_name,

                            item_idx       = item_idx,
                            description    = description,

                            # variable_idx   = variable_idx,
                            variable_name  = variable_name,
                            year           = year,
                            geography_unit = geography_unit,
                            survey         = survey,
                            value_sum      = value_sum,
                            num_samples    = num_samples,

                            state_geoid    = state_geoid,
                            state_name = state_name
                        )

                        value_sum_results[[length(value_sum_results) + 1]] <- value_sum_result

                    } # end state loop
                } else {
                    for (community_idx in 1:nrow(all_community_sf)) {
                        community_sf <- all_community_sf[community_idx, ]

                        is_intersect <- st_intersects(raw_data, community_sf)
                        data <- raw_data[lengths(is_intersect) > 0, ]
                        num_samples <- nrow(data)

                        if (data_type == "decennial") {
                            value_sum <- sum(data$value, na.rm = TRUE)
                        } else {
                            value_sum <- sum(data$estimate, na.rm = TRUE)
                        }

                        value_sum_result = list(
                            category_idx   = category_idx,
                            category_name  = category_name,

                            item_idx       = item_idx,
                            description    = description,

                            # variable_idx   = variable_idx,
                            variable_name  = variable_name,
                            year           = year,
                            geography_unit = geography_unit,
                            survey         = survey,
                            value_sum      = value_sum,
                            num_samples    = num_samples,

                            community_idx = community_idx,
                            area_number = community_sf$area_numbe,
                            community_name = community_sf$community
                        )

                        value_sum_results[[length(value_sum_results) + 1]] <- value_sum_result

                    } # end community loop
                } # end if state or community
            } # end year loop
        } # end item loop
    } # end category loop

    acs_history_sum_df <- bind_rows(value_sum_results)

    sum_file_name <- paste(
                    "acs_history_sum",
                    data_type,
                    "survey", survey,
                    "geography_unit", geography_unit,
                    sep = "_"
                )
    sum_file_name <- gsub(" ", "_", sum_file_name)
    sum_file_name <- paste0(sum_file_name, ".csv")

    cat("Writing acs_history_sum_df to file:", sum_file_name, "\n")

    write.csv(acs_history_sum_df,
              file.path(getwd(), sum_file_name),
              row.names = FALSE)

    end_time <- Sys.time()

    cat("Completed! Duration:",
        format_elapsed(start_time, end_time), "\n")

    return(acs_history_sum_df)
}

find_category_item <- function(category_items,
                              category_name,
                              description) {
    for (ci in category_items) {
        # Compare category_name (case-insensitive)
        if (tolower(ci$category_name) == tolower(category_name)) {
            for (desc in ci$descriptions) {
                if (grepl(desc, description, ignore.case = TRUE)) {
                    return(ci)  # Match found
                }
            }
        }
    }
    return(NULL)
}

plot_category_items <- function(all_community_sf,
                       acs_config,
                       data_type,
                       chicago_history_sum_df,
                       category_items,
                       topk = 3,
                       width = 10,
                       height = 3) {
    all_community_sf_plot <- st_transform(all_community_sf, crs = 26916)
    start_year <- acs_config$start_year
    end_year <- acs_config$end_year
    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        category_name <- category_info$category_name
        items <- category_info$items

        for (item_idx in seq_along(items)) {
            item <- items[[item_idx]]

            description <- item$description
            variable_name <- item$variable_name

            category_item <- find_category_item(category_items,
                                             category_name,
                                             description)

            if (is.null(category_item)) {
                next
            }

            short_category_name <- category_item$short_category_name

            cat("category_name:", category_name,
                "short_category_name:", short_category_name,
                "description:", description,
                "\n")

            subset_df <- chicago_history_sum_df %>%
                filter(variable_name == !!variable_name)

            years <- sort(unique(subset_df$year))

            for (year in years) {
                subset_per_year_df <- subset_df %>%
                filter(year == !!year)

                plot_data <- all_community_sf_plot %>%
                    left_join(subset_per_year_df, by = c("area_numbe" = "area_number"))

                # fill_label <- if (category_name == "Total Median Household Income") {
                #     paste("Income", "in M($)")
                # } else {
                #     category_name
                # }

                all_plot <- ggplot() +
                    geom_sf(data = all_community_sf_plot, fill = "gray90", color = "white") +
                    geom_sf(data = plot_data,
                        aes(fill = value_sum_M),
                        color = "black", size = 0.5) +
                    geom_sf_text(data = all_community_sf_plot,
                                 aes(label = area_numbe), size = 1.5, color = "red") +
                    # scale_fill_viridis_c(option = "magma", na.value = "gray90") +
                    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "gray90") +
                    labs(
                        title = paste(category_name, description, "in", year),
                        fill = short_category_name
                    ) +
                    theme_minimal() +
                    theme(
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        plot.background  = element_rect(fill = "white")
                    )

                # Save as PNG
                all_file_stem <- paste(
                    data_type,
                    "all_community",
                    "category_name", gsub(" ", "_", tolower(category_name)),
                    "description", gsub(" ", "_", tolower(description)),
                    "year", year,
                    sep = "_"
                )

                all_file_name <- paste0(all_file_stem, ".png")

                ggsave(
                    file.path(getwd(), all_file_name),
                    all_plot,
                    width  = width,
                    height = height,
                    dpi = 300
                )
            } # end of year
        } # end of item
    } # end of category
}

plot_top_k <- function(all_community_sf,
                       acs_config,
                       data_type,
                       chicago_history_sum_df,
                       total_categories,
                       topk = 3,
                       width = 10,
                       height = 3) {
    all_community_sf_plot <- st_transform(all_community_sf, crs = 26916)
    start_year <- acs_config$start_year
    end_year <- acs_config$end_year
    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        category_name <- category_info$category_name
        items <- category_info$items

        if (!(tolower(category_name) %in% tolower(total_categories))) {
            next
        }

        item_idx <- 1
        item <- items[[item_idx]]

        description <- item$description
        variable_name <- item$variable_name

        cat("category_name:", category_name,
            "description:", description,
            "\n")

        subset_df <- chicago_history_sum_df %>%
            filter(variable_name == !!variable_name)

        years <- sort(unique(subset_df$year))

        for (year in years) {
            subset_per_year_df <- subset_df %>%
            filter(year == !!year)

            topk_per_year <- subset_per_year_df %>%
                arrange(desc(value_sum)) %>%
                slice_head(n = topk) %>%
                mutate(community_name = str_to_title(community_name))

            # Set factor levels by descending value_sum for legend order
            topk_per_year$community_name <- factor(
                topk_per_year$community_name,
                levels = topk_per_year$community_name
            )

            topk_sf <- all_community_sf_plot %>%
                inner_join(topk_per_year, by = c("area_numbe" = "area_number"))

            plot_image <- ggplot() +
                geom_sf(data = all_community_sf_plot, fill = "gray90", color = "white") +  # base map
                geom_sf(data = topk_sf, aes(fill = community_name), color = "black", size = 0.5) +
                geom_sf_text(data = topk_sf,
                         aes(label = area_numbe), size = 1.5, color = "red") +
                labs(
                    title = paste("Top", topk, category_name, description, "in", year),
                    fill = "Community"
                ) +
                theme_minimal() +
                theme(axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_rect(fill = "white"),
                    plot.background  = element_rect(fill = "white")
                )

            # Save as PNG
            file_stem <- paste(
                data_type,
                "topk", topk,
                "category_name", gsub(" ", "_", tolower(category_name)),
                "description", gsub(" ", "_", tolower(description)),
                "year", year,
                sep = "_"
            )

            file_name <- paste0(file_stem, ".png")

            ggsave(
                file.path(getwd(), file_name),
                plot_image,
                width  = width,
                height = height,
                dpi = 300
            )

            all_plot <- ggplot() +
                geom_sf(data = all_community_sf_plot, fill = "gray90", color = "white") +
                geom_sf(data = all_community_sf_plot %>%
                    left_join(subset_per_year_df, by = c("area_numbe" = "area_number")) %>%
            mutate(value_sum_M = value_sum / 1e6),  # convert to millions
                    aes(fill = value_sum_M),
                    color = "black", size = 0.5) +
                geom_sf_text(data = all_community_sf_plot,
                             aes(label = area_numbe), size = 1.5, color = "red") +
                # scale_fill_viridis_c(option = "magma", na.value = "gray90") +
                scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "gray90") +
                labs(
                    title = paste(category_name, description, "in", year),
                    fill = "Total Income ($M)"
                ) +
                theme_minimal() +
                theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_rect(fill = "white"),
                    plot.background  = element_rect(fill = "white")
                )

            # Save as PNG
            all_file_stem <- paste(
                data_type,
                "all_community",
                "category_name", gsub(" ", "_", tolower(category_name)),
                "description", gsub(" ", "_", tolower(description)),
                "year", year,
                sep = "_"
            )

            all_file_name <- paste0(all_file_stem, ".png")

            ggsave(
                file.path(getwd(), all_file_name),
                all_plot,
                width  = width,
                height = height,
                dpi = 300
            )
        } # end of year
    } # end of category
}

plot_topk_history <- function(acs_config,
    data_type,
    chicago_history_sum_df,
    total_categories,
    topk = 5,
    width = 12,
    height = 4) {

    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {
        category_info <- categories[[category_idx]]

        category_name <- category_info$category_name
        items <- category_info$items

        if (!(tolower(category_name) %in% tolower(total_categories))) {
            next
        }

        item_idx <- 1
        item <- items[[item_idx]]

        description <- item$description
        variable_name <- item$variable_name

        cat("Plotting TOP-K HISTORY:",
            "| Category:", category_name,
            "| Item:", description, "\n")

        # ---- Filter the long dataframe for this variable ----
        df_item <- chicago_history_sum_df %>%
            filter(variable_name == !!variable_name)

        years <- sort(unique(df_item$year))

        # ---- Build top-k per year for one combined figure ----
        topk_list <- lapply(years, function(y) {
            df_item %>%
                filter(year == y) %>%
                arrange(desc(value_sum)) %>%
                slice_head(n = topk) %>%
                mutate(
                    community_name = str_to_title(community_name),
                    year = y
                )
        })

        topk_all_years <- bind_rows(topk_list)

        latest_year <- max(topk_all_years$year)

        latest_topk <- topk_all_years %>%
            filter(year == latest_year) %>%
            arrange(desc(value_sum)) %>%
            pull(community_name)

        # Set factor levels of community_name according to rank in latest year
        topk_all_years$community_name <- factor(topk_all_years$community_name,
                                        levels = latest_topk)

        cat("Unique community names (in top-k order):\n")
        print(unique(topk_all_years$community_name))

        topk_all_years$year <- factor(topk_all_years$year,
                              levels = sort(unique(topk_all_years$year)))


        # ---- Plot ----
        plot_image <- ggplot(topk_all_years %>% mutate(value_sum_M = value_sum / 1e6),
                        aes(x = year,
                            y = value_sum_M,
                            fill = community_name)) +
                    geom_col(position = position_dodge(width = 0.8)) +
                    labs(
                        title = paste0(
                            "Top ", topk, " – ",
                            category_name, ": ", description
                        ),
                        x = "Year",
                        y = "Income ($M)",
                        fill = "Community"
                    ) +
                    theme_minimal(base_size = 11) +
                    theme(
                        plot.title = element_text(size = 13, face = "bold"),
                        panel.background = element_rect(fill = "white"),
                        plot.background = element_rect(fill = "white")
                    )

        # Save as PNG
        file_stem <- paste(
            data_type,
            "topk_history", topk,
            "category_name", gsub(" ", "_", tolower(category_name)),
            "description", gsub(" ", "_", tolower(description)),
            sep = "_"
        )

        file_name <- paste0(file_stem, ".png")

        ggsave(
            file.path(getwd(), file_name),
            plot_image,
            width  = width,
            height = height,
            dpi = 300
        )
    } # end of category
}



plot_history_for_selected_communities <- function(
    acs_config,
    data_type,
    chicago_history_sum_df,
    total_categories,
    width = 12,
    height = 6) {
    # Communities to plot
    focus_communities <- c(
        "Lake View",
        "West Town",
        "Lincoln Park",
        "Near North Side",
        "Logan Square"
    )

    categories <- acs_config$categories

    for (category_idx in seq_along(categories)) {

        category_info <- categories[[category_idx]]
        category_name <- category_info$category_name
        items <- category_info$items

        # Only specified total categories
        if (!(tolower(category_name) %in% tolower(total_categories))) {
            next
        }

        # we use only the first item under this category
        item_idx <- 1
        item <- items[[item_idx]]

        description <- item$description
        variable_name <- item$variable_name

        cat("Plotting HISTORY LINES:",
            "| Category:", category_name,
            "| Item:", description, "\n")

        # # Filter long df for the target variable
        # df_item <- chicago_history_sum_df %>%
        #     filter(variable_name == !!variable_name)

        # # Filter for selected communities only
        # df_focus <- df_item %>%
        #     filter(tolower(community_name) %in% tolower(focus_communities)) %>%
        #     mutate(
        #         community_name = str_to_title(community_name),
        #         year = as.numeric(year)
        #     )

        # # ---- PLOT: One subplot per community ----
        # plot_image <- ggplot(df_focus,
        #         aes(x = year, y = value_sum, group = community_name)) +
        #     geom_line() +
        #     geom_point(size = 2) +
        #     facet_wrap(~ community_name, ncol = 1, scales = "free_y") +
        #     labs(
        #         title = paste0("History of Selected Communities – ",
        #                        category_name, ": ", description),
        #         x = "Year",
        #         y = "Value"
        #     ) +
        #     theme_minimal(base_size = 11) +
        #     theme(
        #         strip.text = element_text(size = 12, face = "bold"),
        #         plot.title = element_text(size = 14, face = "bold"),
        #         panel.background = element_rect(fill = "white"),
        #         plot.background  = element_rect(fill = "white")
        #     )

        # Filter dataframe for variable and selected communities
        df_focus <- chicago_history_sum_df %>%
            filter(variable_name == !!variable_name,
                   tolower(community_name) %in% tolower(focus_communities)) %>%
            mutate(
                community_name = str_to_title(community_name),
                year = as.numeric(year)
            )

        if(nrow(df_focus) == 0){
            cat("No matching data for item:", description, "\n")
            next
        }

        # ---- Plot: All communities in one axis ----
        plot_image <- ggplot(df_focus,
                             aes(x = year, y = value_sum, color = community_name)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            scale_x_continuous(breaks = sort(unique(df_focus$year))) +
            labs(
                title = paste0("History of Selected Communities – ",
                               category_name, ": ", description),
                x = "Year",
                y = "Value",
                color = "Community"
            ) +
            theme_minimal(base_size = 11) +
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                panel.background = element_rect(fill = "white"),
                plot.background  = element_rect(fill = "white"),
                axis.text.x = element_text(angle = 45, hjust = 1)
            )

        # ---- Save image ----
        file_stem <- paste(
            data_type,
            "history_selected_communities_one_axis",
            "category", gsub(" ", "_", tolower(category_name)),
            "description", gsub(" ", "_", tolower(description)),
            sep = "_"
        )

        file_name <- paste0(file_stem, ".png")

        ggsave(
            file.path(getwd(), file_name),
            plot_image,
            width = width,
            height = height,
            dpi = 300
        )

        # ---- Save image ----
        file_stem <- paste(
            data_type,
            "history_selected_communities",
            "category", gsub(" ", "_", tolower(category_name)),
            "description", gsub(" ", "_", tolower(description)),
            sep = "_"
        )

        file_name <- paste0(file_stem, ".png")

        ggsave(
            file.path(getwd(), file_name),
            plot_image,
            width = width,
            height = height,
            dpi = 300
        )
    }
}


create_weighted_adjacency_network <- function(all_community_sf,
                                              data_type,
                                              crs_proj = 26916,
                                              width = 12,
                                              height = 6) {
    # Project to a metric CRS for distance calculation
    all_community_sf_proj <- st_transform(all_community_sf, crs = crs_proj)

    # # 1. Compute distance matrix
    # spatial_dist <- st_distance(all_community_sf_proj)

    # # 2. Create adjacency: 1 if distance == 0 (touching), else 0
    # adj_matrix <- (as.numeric(spatial_dist) == 0)
    # dim(adj_matrix) <- dim(spatial_dist)

    # Compute adjacency: touching polygons
    adj_matrix <- st_touches(all_community_sf_proj, sparse = FALSE)
    # Remove self-loops
    diag(adj_matrix) <- FALSE

    # Compute centroids
    centroids <- st_centroid(all_community_sf_proj$geometry)
    coords <- st_coordinates(centroids)  # x, y coordinates for layout

    # Compute pairwise distances (meters)
    dist_matrix <- st_distance(centroids)
    dist_matrix <- drop_units(dist_matrix)
    # dim(dist_matrix) <- c(nrow(all_community_sf_proj), nrow(all_community_sf_proj))

    # Normalize weights to 0-1
    weight_matrix_norm <- (max(dist_matrix) - dist_matrix) / (max(dist_matrix) - min(dist_matrix))

    # zero for non-adjacent nodes
    weight_matrix_norm[!adj_matrix] <- 0

    # Set row/col names
    rownames(weight_matrix_norm) <- all_community_sf_proj$area_numbe
    colnames(weight_matrix_norm) <- all_community_sf_proj$area_numbe

    # Create igraph object
    g <- graph_from_adjacency_matrix(weight_matrix_norm,
                                     mode = "undirected",
                                     weighted = TRUE,
                                     diag = FALSE)

    file_stem <- paste(data_type,
                       "network_adj_matrix",
                       sep="_")
    file_name <- paste0(file_stem, ".csv")

    write.csv(weight_matrix_norm,
              file.path(getwd(), file_name),
              row.names = FALSE)


    return(list(graph = g, weight_matrix = weight_matrix_norm,
                coords = coords))
}


plot_weighted_network <- function(coords,
                                  g,
                                  data_type,
                                  width = 10,
                                  height = 6) {
    # Assign node IDs
    # V(g)$name <- seq_len(nrow(all_community_sf))
    nodes_df <- data.frame(
        id = V(g)$name,
        x = coords[,1],
        y = coords[,2]
    )

    # Prepare edges for plotting
    edges_df <- as.data.frame(get.edgelist(g))
    colnames(edges_df) <- c("from", "to")
    edges_df$weight <- E(g)$weight

    # Merge coordinates
    edges_df <- merge(edges_df, nodes_df, by.x = "from", by.y = "id")
    edges_df <- merge(edges_df, nodes_df, by.x = "to", by.y = "id", suffixes = c("_from", "_to"))

    # Create network plot
    network_plot <- ggplot() +
        geom_segment(data = edges_df,
                     aes(x = x_from, y = y_from,
                         xend = x_to, yend = y_to,
                         color = weight),
                     size = 0.8) +
        # scale_color_viridis_c(option = "plasma", name = "Weight") +
        scale_color_distiller(palette = "YlOrRd", direction = 1, na.value = "gray90") +
        geom_point(data = nodes_df, aes(x = x, y = y), color = "blue", size = 3) +
        geom_text(data = nodes_df, aes(x = x, y = y, label = id),
                  fontface = "bold", vjust = -1) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.background  = element_rect(fill = "white", color = NA),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        xlab("Longitude") + ylab("Latitude")

    # Save as PNG
    file_stem <- paste(
        data_type,
        "network",
        sep = "_"
    )

    file_name <- paste0(file_stem, ".png")

    ggsave(
        file.path(getwd(), file_name),
        network_plot,
        width  = width,
        height = height,
        dpi = 300
    )
}


plot_history_data <- function(history,
                              data_type,
                              community_ids,
                              community_sf,
                              width = 10,
                              height = 3) {

    for (category_result in history) {
        category_name <- category_result$category_name
        item_results  <- category_result$item_results

        years <- c()
        rows_plots <- list()

        # ---------- Build each row separately (each row gets its own scale) ----------
        for (item_result_idx in seq_along(item_results)) {
            item_result <- item_results[[item_result_idx]]

            description <- item_result$description
            variable_results <- item_result$variable_results
            row_plots <- list()

            # ---- Compute row-level fill_min / fill_max ----
            row_fill_min <- Inf
            row_fill_max <- -Inf

            for (variable_result in variable_results) {
                data <- variable_result$data
                data$fill_value <- if (data_type == "decennial") data$value else data$estimate
                row_fill_min <- min(row_fill_min, min(data$fill_value, na.rm = TRUE))
                row_fill_max <- max(row_fill_max, max(data$fill_value, na.rm = TRUE))
            }

            # ---------- Build each plot in this row ----------
            for (variable_result_idx in seq_along(variable_results)) {

                variable_result <- variable_results[[variable_result_idx]]
                year <- variable_result$year
                data <- variable_result$data

                if (!(year %in% years)) {
                    years <- c(years, year)
                }

                title <- paste(year)

                # Last column per row shows legend
                show_legend <- (variable_result_idx == length(variable_results))

                data$fill_value <- if (data_type == "decennial") data$value else data$estimate

                plot_image <- ggplot() +
                    geom_sf(data = data, aes(fill = fill_value), color = NA) +
                    geom_sf(data = community_sf, fill = NA, color = "black", size = 0.6) +
                    scale_fill_distiller(
                        name      = description,
                        palette   = "YlGn",
                        direction = 1,
                        limits    = c(row_fill_min, row_fill_max),
                        guide     = if (show_legend) "colourbar" else "none"
                    ) +
                    ggtitle(title) +
                    theme_minimal() +
                    theme(
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, face = "plain"),
                        panel.background = element_rect(fill = "white", color = NA),
                        plot.background  = element_rect(fill = "white", color = NA)
                    )

                row_plots[[length(row_plots) + 1]] <- plot_image
            }

            # ---------- Combine this row horizontally ----------
            row_combined <- wrap_plots(
                row_plots,
                ncol = length(row_plots),
                guides = "collect"
            )

            rows_plots[[length(rows_plots) + 1]] <- row_combined
        }

        # ---------- Combine all rows vertically ----------
        combined_plot <- wrap_plots(rows_plots, ncol = 1)

        # ---------- Title ----------
        community_ids_text <- paste(community_ids, collapse = ", ")
        data_type_text <- ifelse(
            tolower(data_type) == "decennial",
            "Decennial",
            toupper(data_type)
        )

        combined_plot <- combined_plot +
            plot_annotation(
                title = paste(
                    "[ CA", community_ids_text, data_type_text, "]",
                    paste0(category_name, ":"),
                    paste(years, collapse = " vs ")
                ),
                theme = theme(
                    plot.title = element_text(
                        hjust = 0.5,
                        face  = "plain",
                        size  = 14
                    ),
                    panel.background = element_rect(fill = "white", color = NA),
                    plot.background  = element_rect(fill = "white", color = NA)
                )
            )

        # ---------- Save ----------
        community_ids_str <- paste(community_ids, collapse = "_")

        file_stem <- paste(
            gsub(" ", "_", tolower(community_ids_str)),
            data_type,
            "history",
            gsub(" ", "_", tolower(category_name)),
            paste(years, collapse = "_"),
            sep = "_"
        )

        file_name <- paste0(file_stem, ".png")

        ggsave(
            file.path(getwd(), file_name),
            combined_plot,
            width  = width,
            height = height * length(rows_plots),  # height grows with rows
            dpi = 300
        )
    }
}



# ------------------------------------------------------------------------------
# End of functions.
################################################################################

################################################################################
# Start of main.
# ------------------------------------------------------------------------------

options(tigris_use_cache = TRUE)

# getwd()

config_file_path <- file.path(getwd(), 'config.yaml')
stopifnot(file.exists(config_file_path))
config <- yaml::read_yaml(config_file_path)

census_api_key_file_path <- file.path(getwd(), 'census_api_key.txt')

if (file.exists(census_api_key_file_path)) {
    census_api_key <- trimws(read_file(census_api_key_file_path))
    census_api_key(census_api_key, install = FALSE)
}


# Data is downloaded from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
# Download steps:
#     1. Click "Export" button
#     2. Export as shapefile.
#     3. Unzip the download zip file.

boundary_file_path <- file.path(getwd(), 'Boundaries - Community Areas_20251102/geo_export_78bb31e8-e65e-47ef-bb49-90dbc4911759.shp')

stopifnot(file.exists(boundary_file_path))

all_community_sf = get_all_community_sf(boundary_file_path)

plot_all_community_boundaries(all_community_sf, width=4, height = 3)


input_communities <- data.frame(
  ID = c(30, 20, 27),
  Name = c("South Lawndale", "Hermosa", "East Garfield Park")
)

latest_year = 2023

input_community_idx = 3
input_communities <- input_communities[input_community_idx,]
community_ids <- input_communities$ID
community_names <- input_communities$Name

community_names_str <- paste(community_names, collapse = ",")
community_sf = get_community_sf(boundary_file_path,
                                input_communities)

plot_community_boundary(community_sf, community_ids, 'decennial', width=4)
# common_decennial_vars <- get_decennial_vars()
# common_acs5_vars <- get_acs_vars(2023, "acs5")
# common_acs1_vars <- get_acs_vars(2023, "acs1")

download_decennial_data(config$history$decennial, "decennial")
download_acs_data(config$history$acs, "acs")

download_acs_data(config$history$acs,
                  "acs",
                  geography_unit = "state")

download_acs_data(config$history$acs,
                  "acs",
                  geography_unit = "state",
                  survey = "acs1")

decennial_history <- get_decennial_data(config$history$decennial, community_sf)

plot_history_data(decennial_history, 'decennial', community_ids,
                  community_sf,
                width=6, height=3)

for community_sf in all_community_sf:
    decennial_history <- get_decennial_data(config$history$decennial, community_sf)
    decennial_tables <- get_decennial_tables(decennial_history, 'decennial')

decennial_history_data <- get_decennial_sum_results(config$history$decennial,
                      "decennial",
                      all_community_sf)

decennial_history_sum_df <- bind_rows(decennial_history_data)
write.csv(decennial_history_sum_df,
          file.path(getwd(), "decennial_sum.csv"),
          row.names = FALSE)

chicago_acs5_history_sum_df <- get_acs_sum_results(all_community_sf,
                                        config$history$acs,
                                        "acs")


us_acs5_history_sum_df <- get_acs_sum_results(NULL,
                                        config$history$acs,
                                        "acs",
                                        geography_unit = "state")


us_acs1_history_sum_df <- get_acs_sum_results(NULL,
                                        config$history$acs,
                                        "acs",
                                        geography_unit = "state",
                                        survey = "acs1")

chicago_acs5_history_sum_df_file_name <- "acs_history_sum_acs_survey_acs5_geography_unit_block_group.csv"
chicago_acs5_history_sum_df_file_path <- file.path(getwd(),
                                                   chicago_acs5_history_sum_df_file_name)

stopifnot(file.exists(chicago_acs5_history_sum_df_file_path))
chicago_acs5_history_sum_df <- read_csv(chicago_acs5_history_sum_df_file_path)


# # Convert income to millions of dollars
# chicago_acs5_history_sum_df <- chicago_acs5_history_sum_df %>%
#     mutate(value_million = value_sum / 1e6)

chicago_acs5_history_sum_df <- chicago_acs5_history_sum_df %>%
    mutate(
        value_sum_M = ifelse(category_name == "Total Median Household Income",
                             value_sum / 1e6,  # convert to millions
                             value_sum)         # keep original for other categories
    )

total_categories <- c(
  "Total Population",
  "Total Housing Units",
  "Total Median Household Income"
)

selected_category_items <- list(
  list(
    category_name = "Total Population by Race",
    short_category_name = "Population",
    descriptions = c("White", "Black or African American")
  ),
  list(
    category_name = "Total Housing Units by Race",
    short_category_name = "Housing Units",
    descriptions = c("White", "Black or African American")
  ),
  list(
    category_name = "Total Median Household Income",
    short_category_name = "Income in M($)",
    descriptions = c("By Age of Householder", "By Race")
  )
)


plot_category_items <- function(all_community_sf,
                                acs_config,
                                data_type,
                                chicago_history_sum_df,
                                category_items,
                                width = 12,
                                height = 12) {  # taller for 3 rows

    all_community_sf_plot <- st_transform(all_community_sf, crs = 26916)

    # Determine all years
    all_years <- chicago_history_sum_df %>% pull(year) %>% unique() %>% sort()

    for (year in all_years) {
        row_plots <- list()  # store each row (category) plots

        # Iterate over category_items (each category = row)
        for (ci in category_items) {
            category_name <- ci$category_name
            descriptions <- ci$descriptions
            short_category_name <- ci$short_category_name
            tile_prefix <- sub(" in.*", "", short_category_name)

            # List to store plots for this category (row)
            cat_plots <- list()

            for (description in descriptions) {
                # Filter data for category, description, year
                subset_per_year_df <- chicago_history_sum_df %>%
                    filter(category_name == !!category_name,
                           grepl(!!description, description, ignore.case = TRUE),
                           year == !!year)

                plot_data <- all_community_sf_plot %>%
                    left_join(subset_per_year_df, by = c("area_numbe" = "area_number"))

                p <- ggplot() +
                    geom_sf(data = plot_data, aes(fill = value_sum_M), color = NA) +
                    geom_sf(data = all_community_sf_plot, fill = NA, color = "black", size = 0.6) +
                    geom_sf_text(data = all_community_sf_plot,
                                 aes(label = area_numbe), size = 1.5, color = "red") +
                    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "gray90") +
                    labs(
                        title = paste(tile_prefix, "by", description),
                        fill = short_category_name,
                        x = NULL,  # remove x-axis label
                        y = NULL   # remove y-axis label
                    ) +
                    theme_minimal() +
                    theme(
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        panel.background = element_rect(fill = "white", color = NA),    # remove panel border
                        plot.background  = element_rect(fill = "white", color = NA),    # remove surrounding border
                    )

                cat_plots <- append(cat_plots, list(p))
            }

            # Combine this category's descriptions horizontally (row)
            row_plot <- wrap_plots(cat_plots, ncol = 2) +
                        plot_annotation(title = short_category_name)

            row_plots <- append(row_plots, list(row_plot))
        }

        # Stack all category rows vertically
        combined_plot <- wrap_plots(row_plots, ncol = 1) +
                        plot_annotation(
                            title = paste("Feature Values in", year),
                            theme = theme(
                                plot.title = element_text(
                                    hjust = 0.5,       # center horizontally
                                    face = "plain",    # remove bold
                                    size = 14
                                ),
                                panel.background = element_rect(fill = "white", color = NA),    # remove gray/black panel background
                                plot.background  = element_rect(fill = "white", color = NA)  # remove surrounding background
                            )
                        )

        # Save as PNG
        all_file_stem <- paste(
            data_type,
            "all_community",
            "year", year,
            sep = "_"
        )

        all_file_name <- paste0(all_file_stem, ".png")
        ggsave(all_file_name, combined_plot, width = width, height = height, dpi = 300)
    }
}


plot_category_items(all_community_sf,
           config$history$acs,
           "acs",
           chicago_acs5_history_sum_df,
           selected_category_items,
           width=12,
           height=12)

plot_top_k(all_community_sf,
           config$history$acs,
           "acs",
           chicago_acs5_history_sum_df,
           total_categories,
           topk=5,
           width=6,
           height=3)

plot_topk_history(
    config$history$acs,
    "acs",
    chicago_acs5_history_sum_df,
    total_categories,
    topk = 5,
    width = 8,
    height = 4
)



plot_history_for_selected_communities(
    acs_config = config$history$acs,
    data_type = "acs",
    chicago_history_sum_df = chicago_acs5_history_sum_df,
    total_categories = total_categories,
    width = 10,
    height = 8
)



result <- create_weighted_adjacency_network(all_community_sf,
                                            "acs",
                                            width = 10,
                                            height = 8)

# Access igraph object
g <- result$graph

plot_weighted_network(result$coords, g, "acs")




# acs_history_sum_df <- bind_rows(acs_history_data)
# write.csv(acs_history_sum_df,
#           file.path(getwd(), "acs_history_sum.csv"),
#           row.names = FALSE)


get_item_sum_results <- function(acs_config,
                             data_type,
                             acs_history_sum_df,
                             all_community_sf,
                             category_name,
                             description,
                             width = 10,
                             height = 3) {
    item_sum_results <- list()

    start_year <- acs_config$start_year
    end_year <- acs_config$end_year

    for (year in start_year:end_year) {
        # 1. Filter the ACS data
        filtered_acs <- acs_history_sum_df %>%
        filter(
               category_name == !!category_name,
               description == !!description,
               year == !!year
        ) %>%
        select(area_number, community_name, value_sum)

        # cat(category_name, description, year, "\n")
        # cat("Rows in filtered_acs:", nrow(filtered_acs), "\n")
        # cat("Rows in all_community_sf:", nrow(all_community_sf), "\n")

        stopifnot(nrow(filtered_acs) == nrow(all_community_sf))

        # 2. Join with spatial dataframe
        acs_geo <- filtered_acs %>%
        inner_join(all_community_sf,
                   by = c("area_number" = "area_numbe"))

        # 3. Make sure the joined data is an sf object
        item_sum_result_sf <- st_as_sf(acs_geo)

        item_sum_results[[length(item_sum_results) + 1]] <- item_sum_result_sf
    }

    return(item_sum_results)
}

plot_item_sum_results <- function(acs_config,
                             data_type,
                             item_sum_results,
                             category_name,
                             description,
                             width = 10,
                             height = 3) {
    start_year <- acs_config$start_year
    end_year <- acs_config$end_year

    stopifnot(length(start_year:end_year) == length(item_sum_results))
    year_idx <- 1

    for (year in start_year:end_year) {
        item_sum_result <- item_sum_results[[year_idx]]

        year_idx <- year_idx + 1

        plot_image <- ggplot(item_sum_result) +
        geom_sf(aes(fill = value_sum), color = NA) + # fill polygons by value_sum
        scale_fill_viridis_c(option = "C") +
        theme_minimal() +
        labs(
             title = paste("[", gsub("_", " ", str_to_title(category_name)),
                           ":",
                           gsub("_", " ", str_to_title(description)),
                           "]",
                           year),
             fill = gsub("_", " ", str_to_title(category_name)),
        )

        # Save as PNG
        file_stem <- paste(gsub(" ", "_", tolower(category_name)),
                        data_type,
                        'history',
                        gsub(" ", "_", tolower(description)),
                        year,
                        sep="_")
        file_name <- paste0(file_stem, ".png")

        ggsave(file.path(getwd(), file_name),
            plot_image,
            width = width, height = height, dpi = 300)
    }
}

category_name <- "Total Population"
description <- "All"
item_sum_results <- get_item_sum_results(config$history$acs,
                 "acs",
                 acs_history_sum_df,
                 all_community_sf,
                 category_name,
                 description)

plot_item_sum_results(config$history$acs,
                      "acs",
                      item_sum_results,
                      category_name,
                      description)


save_item_sum_results <- function(acs_config,
                             data_type,
                             item_sum_results,
                             category_name,
                             description) {

    start_year <- acs_config$start_year
    end_year <- acs_config$end_year

    stopifnot(length(start_year:end_year) == length(item_sum_results))
    year_idx <- 1

    for (year in start_year:end_year) {
        item_sum_result <- item_sum_results[[year_idx]]
        item_sum_result_no_geo <- st_drop_geometry(item_sum_result)

        year_idx <- year_idx + 1

        cols_to_keep <- c("area_number",
                          "community_name",
                          "value_sum",
                          "shape_area")

        # Save as .csv
        file_stem <- paste(gsub(" ", "_", tolower(category_name)),
                        data_type,
                        'item_sum_results',
                        gsub(" ", "_", tolower(description)),
                        year,
                        sep="_")
        file_name <- paste0(file_stem, ".csv")

        item_sum_result_df <- item_sum_result_no_geo[, cols_to_keep, drop = FALSE]
        write.csv(item_sum_result_df,
                  file = file.path(getwd(), file_name),
                  row.names = FALSE)
    }
}


save_item_sum_results(config$history$acs,
                      "acs",
                      item_sum_results,
                      category_name,
                      description)

for (item_idx in seq_along(item_sum_results)) {
  cat("Item", item_idx, "area_number:\n")
  print(item_sum_results[[item_idx]]$area_number)
  cat("\n")
}



all_same_order <- list()
# Get area_number of the first item
first_area_number <- item_sum_results[[1]]$area_number

# Check all other items
for (item_idx in seq_along(item_sum_results)) {
    item_sum_result <- item_sum_results[[item_idx]]
    area_number <- item_sum_result$area_number
    same_order <- identical(first_area_number, area_number)
    all_same_order[[length(all_same_order) + 1]] <- same_order
}

stopifnot(all(unlist(all_same_order)))

first_year_data <- item_sum_results[[1]]

# Ensure geometry is sf
first_year_sf <- st_as_sf(first_year_data)

# 1. Compute distance matrix
spatial_dist <- st_distance(first_year_sf$geometry)

# 2. Create adjacency: 1 if distance == 0 (touching), else 0
adj_matrix <- (as.numeric(spatial_dist) == 0)
dim(adj_matrix) <- dim(spatial_dist)
# Remove self-loops
diag(adj_matrix) <- FALSE

file_stem <- paste(gsub(" ", "_", tolower(category_name)),
                data_type,
                'adj_matrix',
                gsub(" ", "_", tolower(description)),
                sep="_")
file_name <- paste0(file_stem, ".csv")
write.csv(adj_matrix, file.path(getwd(), file_name), row.names = FALSE)

# 3. Convert to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

V(g)$name <- first_year_sf$area_number

coords <- st_coordinates(st_centroid(first_year_sf$geometry))
V(g)$x <- coords[,1]
V(g)$y <- coords[,2]

# V(g)$value_sum <- first_year_sf$value_sum
n_nodes <- length(V(g))
n_years <- length(item_sum_results)

# Initialize a list to store vectors
node_values <- vector("list", n_nodes)

# Loop over each node (community)
for (node_idx in seq_len(n_nodes)) {
    area_num <- V(g)$name[node_idx]

    # Extract value_sum for this community across all years
    values <- sapply(item_sum_results, function(df) {
        df$value_sum[df$area_number == area_num]
    })

    node_values[[node_idx]] <- values
}

# Assign to graph as a vertex attribute
V(g)$value_vector <- node_values

# Node coordinates
nodes_df <- data.frame(
    name = V(g)$name,
    x = V(g)$x,
    y = V(g)$y,
    value_sum = sapply(V(g)$value_vector, `[`, 1)  # first year population
)

# Edge data frame
edges <- as_data_frame(g, what = "edges") %>%
    mutate(
        x_from = V(g)$x[from],
        y_from = V(g)$y[from],
        x_to   = V(g)$x[to],
        y_to   = V(g)$y[to]
    )

ggplot() +
    # Edges
    geom_segment(
        data = edges,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
        color = "gray", alpha = 0.5
    ) +
    # Nodes
    geom_point(
        data = nodes_df,
        aes(x = x, y = y, size = value_sum),
        color = "skyblue"
    ) +
    # Labels
    geom_text(
        data = nodes_df,
        aes(x = x, y = y, label = name),
        size = 3, vjust = -1
    ) +
    theme_minimal() +
    labs(
        title = "Community Network over Geography",
        size = category_name
    )


# Convert node vectors into a long dataframe
nodes_long <- data.frame(
    name = V(g)$name,
    x = V(g)$x,
    y = V(g)$y,
    value_vector = I(V(g)$value_vector)  # list-column
) %>%
    unnest_longer(value_vector) %>%        # expand each list element to a row
    mutate(year = seq_along(value_vector)) # or map to actual years

ggplot() +
    geom_segment(data = edges,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                 color = "gray", alpha = 0.5) +
    geom_point(data = nodes_long,
               aes(x = x, y = y, size = value_vector),
               color = "skyblue") +
    facet_wrap(~ year) +
    theme_minimal() +
    labs(title = "Community Network over Geography by Year", size = category_name)


edges <- as_data_frame(g, what = "edges")

# Get coordinates for source and target nodes
edges <- edges %>%
    mutate(
        x_from = V(g)$x[from],
        y_from = V(g)$y[from],
        x_to   = V(g)$x[to],
        y_to   = V(g)$y[to]
    )

# Node data frame
nodes_df <- data.frame(
    name = V(g)$name,
    x = V(g)$x,
    y = V(g)$y,
    value_sum = V(g)$value_sum
)

# Plot
ggplot() +
    geom_segment(
        data = edges,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
        color = "gray", alpha = 0.5
    ) +
    geom_point(
        data = nodes_df,
        aes(x = x, y = y, size = value_sum),
        color = "skyblue"
    ) +
    geom_text(
        data = nodes_df,
        aes(x = x, y = y, label = name),
        size = 3, vjust = -1
    ) +
    theme_minimal() +
    labs(title = "Community Network over Geography", size = category_name)




# ------------------------------------------------------------------------------
# End of main.
################################################################################
#
