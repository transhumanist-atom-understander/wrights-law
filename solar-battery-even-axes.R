library(tidyverse)
library(ggrepel)

theme_set(theme_grey(base_size = 18))

decimal_labels <- scales::label_number(
    big.mark = ',',
    drop0trailing = TRUE
)

decade_breaks <- function(x) {
    10^seq(
        floor(log10(min(x))),
        ceiling(log10(max(x))),
        by = 1
    )
}

piecewise_formula <- log_price ~ log_cumulative_production + hinge

learning_rate_from_slope <- function(slope) {
    1 - 2^slope
}

add_hinge <- function(data, breakpoint_year) {
    breakpoint_log <-
        data |>
        filter(year == breakpoint_year) |>
        pull(log_cumulative_production)

    data |>
        mutate(hinge = pmax(0, log_cumulative_production - breakpoint_log))
}

# OWID series: Price of lithium-ion battery cells vs. cumulative production, World
# https://ourworldindata.org/grapher/battery-price-vs-cumulative-production
battery_url <-
    'https://ourworldindata.org/grapher/battery-price-vs-cumulative-production.csv?v=1&csvType=full&useColumnShortNames=true'

battery <-
    read_csv(
        battery_url,
        col_types = cols(
            entity = col_character(),
            code = col_character(),
            year = col_double(),
            price = col_double(),
            cumulative_production = col_double()
        )
    ) |>
    filter(!is.na(price), !is.na(cumulative_production)) |>
    arrange(cumulative_production) |>
    mutate(
        log_price = log10(price),
        log_cumulative_production = log10(cumulative_production)
    )

breakpoint_year <-
    battery |>
    slice(2:(n() - 1)) |>
    reframe(
        rss = deviance(lm(piecewise_formula, data = add_hinge(battery, year))),
        .by = year
    ) |>
    slice_min(rss, n = 1, with_ties = FALSE) |>
    pull(year)

battery_fit_data <- add_hinge(battery, breakpoint_year)
battery_fit <- lm(piecewise_formula, data = battery_fit_data)
battery_coef <- coef(battery_fit)

battery_fit_parameters <-
    battery |>
    filter(year == breakpoint_year) |>
    transmute(
        breakpoint_year = year,
        breakpoint_cumulative_production = cumulative_production,
        intercept_before_log10 = battery_coef['(Intercept)'],
        slope_before = battery_coef['log_cumulative_production'],
        learning_rate_before = learning_rate_from_slope(slope_before),
        intercept_after_log10 =
            battery_coef['(Intercept)'] -
            log_cumulative_production * battery_coef['hinge'],
        slope_after =
            battery_coef['log_cumulative_production'] + battery_coef['hinge'],
        learning_rate_after = learning_rate_from_slope(slope_after)
    )

write_csv(battery_fit_parameters, 'battery_piecewise_fit_parameters.csv')

battery_segment_points <-
    battery_fit_data |>
    filter(year %in% c(min(year), breakpoint_year, max(year)))

battery_segment_points <-
    battery_segment_points |>
    mutate(
        log_price = predict(battery_fit, newdata = battery_segment_points),
        price = 10^log_price
    )

battery_x_breaks <- decade_breaks(battery$cumulative_production)
battery_y_breaks <- decade_breaks(battery$price)

battery_plot <-
    battery |>
    ggplot(aes(x=cumulative_production, y=price, label=year)) +
    geom_point() +
    geom_text_repel(max.overlaps=Inf) +
    geom_line(data = battery_segment_points, linewidth = 1) +
    scale_x_log10(
        breaks = battery_x_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    scale_y_log10(
        breaks = battery_y_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    coord_fixed(
        xlim = range(battery_x_breaks),
        ylim = range(battery_y_breaks),
        expand = FALSE
    ) +
    labs(
        x = 'Cumulative production (GWh)',
        y = 'Battery cell price\n(2024 US$/kWh)'
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    'battery_wrights_law_piecewise.png',
    battery_plot,
    width = 11,
    height = 4,
    units = 'in'
)

battery_after <- battery |> filter(year >= breakpoint_year)

battery_segment_points_after <-
    battery_segment_points |>
    filter(year >= breakpoint_year)

battery_after_x_breaks <- decade_breaks(battery_after$cumulative_production)
battery_after_y_breaks <- decade_breaks(battery_after$price)

battery_plot_after_breakpoint <-
    battery_after |>
    ggplot(aes(x=cumulative_production, y=price, label=year)) +
    geom_point() +
    geom_text_repel(max.overlaps=Inf) +
    geom_line(data = battery_segment_points_after, linewidth = 1) +
    scale_x_log10(
        breaks = battery_after_x_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    scale_y_log10(
        breaks = battery_after_y_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    coord_fixed(
        xlim = range(battery_after_x_breaks),
        ylim = range(battery_after_y_breaks),
        expand = FALSE
    ) +
    labs(
        x = 'Cumulative production (GWh)',
        y = 'Battery cell price\n(2024 US$/kWh)'
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    'battery_wrights_law_piecewise_after_breakpoint.png',
    battery_plot_after_breakpoint,
    width = 10,
    height = 5,
    units = 'in'
)

battery_line_fit <- lm(log_price ~ log_cumulative_production, data = battery)
battery_line_coef <- coef(battery_line_fit)

battery_line_fit_parameters <-
    tibble(
        intercept_log10 = battery_line_coef['(Intercept)'],
        slope = battery_line_coef['log_cumulative_production'],
        learning_rate =
            learning_rate_from_slope(
                battery_line_coef['log_cumulative_production']
            )
    )

write_csv(battery_line_fit_parameters, 'battery_line_fit_parameters.csv')

battery_line_points <-
    battery |>
    filter(year %in% c(min(year), max(year))) |>
    select(year, cumulative_production, log_cumulative_production)

battery_line_points <-
    battery_line_points |>
    mutate(
        log_price = predict(
            battery_line_fit,
            newdata = battery_line_points
        ),
        price = 10^log_price
    )

battery_line_plot <-
    battery |>
    ggplot(aes(x=cumulative_production, y=price, label=year)) +
    geom_point() +
    geom_text_repel(max.overlaps=Inf) +
    geom_line(data = battery_line_points, linewidth = 1) +
    scale_x_log10(
        breaks = battery_x_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    scale_y_log10(
        breaks = battery_y_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    coord_fixed(
        xlim = range(battery_x_breaks),
        ylim = range(battery_y_breaks),
        expand = FALSE
    ) +
    labs(
        x = 'Cumulative production (GWh)',
        y = 'Battery cell price\n(2024 US$/kWh)'
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    'battery_wrights_law_line.png',
    battery_line_plot,
    width = 11,
    height = 4,
    units = 'in'
)

# OWID series: Solar PV module prices vs. cumulative capacity, World
# https://ourworldindata.org/grapher/solar-pv-prices-vs-cumulative-capacity
solar_url <-
    'https://ourworldindata.org/grapher/solar-pv-prices-vs-cumulative-capacity.csv?v=1&csvType=full&useColumnShortNames=true'

solar <-
    read_csv(
        solar_url,
        col_types = cols(
            entity = col_character(),
            code = col_character(),
            year = col_double(),
            cost = col_double(),
            cumulative_capacity = col_double()
        )
    ) |>
    filter(!is.na(cost), !is.na(cumulative_capacity)) |>
    arrange(cumulative_capacity) |>
    mutate(
        cumulative_capacity_gw = cumulative_capacity / 1000,
        log_cost = log10(cost),
        log_cumulative_capacity_gw = log10(cumulative_capacity_gw)
    )

solar_fit <- lm(log_cost ~ log_cumulative_capacity_gw, data = solar)
solar_coef <- coef(solar_fit)

solar_fit_parameters <-
    tibble(
        intercept_log10_gw = solar_coef['(Intercept)'],
        slope = solar_coef['log_cumulative_capacity_gw'],
        learning_rate =
            learning_rate_from_slope(
                solar_coef['log_cumulative_capacity_gw']
            )
    )

write_csv(solar_fit_parameters, 'solar_line_fit_parameters.csv')

solar_x_breaks <- decade_breaks(solar$cumulative_capacity_gw)
solar_y_breaks <- decade_breaks(solar$cost)

solar_plot <-
    solar |>
    ggplot(aes(x=cumulative_capacity_gw, y=cost, label=year)) +
    geom_point() +
    geom_text_repel(max.overlaps=Inf) +
    scale_x_log10(
        breaks = solar_x_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    scale_y_log10(
        breaks = solar_y_breaks,
        labels = decimal_labels,
        minor_breaks = NULL
    ) +
    coord_fixed(
        xlim = range(solar_x_breaks),
        ylim = range(solar_y_breaks),
        expand = FALSE
    ) +
    labs(
        x = 'Cumulative capacity (GW)',
        y = 'Module cost\n(2024 US$/W)'
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    'solar_wrights_law.png',
    solar_plot,
    width = 10,
    height = 5,
    units = 'in'
)
