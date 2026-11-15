options(tidyverse.quiet = TRUE)
library(tidyverse)
library(ggrepel)

theme_set(theme_grey(base_size = 18))

decimal_labels <- scales::label_comma(drop0trailing = TRUE)

decade_breaks <- function(x) {
    10^seq(floor(log10(min(x))), ceiling(log10(max(x))))
}

read_owid <- function(chart) {
    str_glue(
        "https://ourworldindata.org/grapher/{chart}.csv",
        "?v=1&csvType=full&useColumnShortNames=true"
    ) |>
        read_csv(
            col_types = cols(
                entity = col_character(),
                code = col_character(),
                .default = col_double()
            )
        )
}

# The robust MM-estimate keeps outlying years from steering the line
fit_methods <- list(least_squares = lm, robust = robustbase::lmrob)

# Wright's law: log10(price) = intercept + slope * log10(cumulative_production)
fit_wrights_law <- function(data, method) {
    fit <- fit_methods[[method]]
    fit(log10(price) ~ log10(cumulative_production), data = data)
}

geom_fit <- function(fit) {
    geom_line(aes(y = 10^fitted(fit)), linewidth = 1)
}

plot_wrights_law <- function(data) {
    ggplot(data, aes(cumulative_production, price)) +
        geom_point() +
        geom_text_repel(aes(label = year), max.overlaps = Inf, max.time = Inf) +
        scale_x_log10(breaks = decade_breaks, labels = decimal_labels) +
        scale_y_log10(breaks = decade_breaks, labels = decimal_labels) +
        coord_fixed(
            xlim = range(decade_breaks(data$cumulative_production)),
            ylim = range(decade_breaks(data$price)),
            expand = FALSE
        ) +
        guides(x = guide_axis(angle = 45)) +
        theme(panel.grid.minor = element_blank())
}

# OWID series: Solar PV module prices vs. cumulative capacity, World
# https://ourworldindata.org/grapher/solar-pv-prices-vs-cumulative-capacity
solar <-
    read_owid("solar-pv-prices-vs-cumulative-capacity") |>
    select(year, price = cost, cumulative_production = cumulative_capacity) |>
    mutate(cumulative_production = cumulative_production / 1000) |> # MW to GW
    drop_na()

# OWID series: Price of lithium-ion battery cells vs. cumulative production, World
# https://ourworldindata.org/grapher/battery-price-vs-cumulative-production
battery <-
    read_owid("battery-price-vs-cumulative-production") |>
    select(year, price, cumulative_production) |>
    drop_na()

solar_plot <-
    plot_wrights_law(solar) +
    labs(x = "Cumulative capacity (GW)", y = "Module cost\n(2025 US$/W)")

battery_plot <-
    plot_wrights_law(battery) +
    labs(
        x = "Cumulative production (GWh)",
        y = "Battery cell price\n(2024 US$/kWh)"
    )

set.seed(1) # lmrob() starts from random subsamples
fits <-
    tribble(
        ~series,   ~data,   ~plot,        ~width, ~height,
        "solar",   solar,   solar_plot,   10,     5,
        "battery", battery, battery_plot, 11,     4
    ) |>
    expand_grid(method = names(fit_methods)) |>
    mutate(fit = map2(data, method, fit_wrights_law))

fits |>
    mutate(
        filename = str_glue("{series}_wrights_law_{method}.png"),
        plot = map2(plot, fit, \(plot, fit) plot + geom_fit(fit))
    ) |>
    select(filename, plot, width, height) |>
    pwalk(ggsave)

fits |>
    mutate(coefficients = map(fit, coef)) |>
    unnest_wider(coefficients) |>
    select(
        series,
        method,
        intercept = `(Intercept)`,
        slope = `log10(cumulative_production)`
    ) |>
    mutate(learning_rate = 1 - 2^slope) |>
    write_csv("line_fit_parameters.csv")
