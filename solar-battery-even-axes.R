options(tidyverse.quiet = TRUE)
library(scales)
library(tidyverse)
library(ggrepel)
library(owidapi)
library(robustbase)
library(broom)

theme_set(theme_grey(base_size = 12))

decimal_labels <- label_comma(drop0trailing = TRUE)

# One break per order of magnitude; a scale ignores those outside its range
decades <- 10^(-9:9)

# Robust linear regression is my go-to, but least squares is more standard. It
# doesn't seem to matter for photovoltaic, but for batteries, it matters a lot,
# so doing both
fit_methods <- list(least_squares = lm, robust = lmrob)

# Wright's law: log10(price) = intercept + slope * log10(cumulative_production)
fit_wrights_law <- function(data, method) {
    fit <- fit_methods[[method]]
    fit(log10(price) ~ log10(cumulative_production), data = data)
}

# geom_abline() takes its line in the units of the scales, so log10 here. Unlike
# a line through the fitted values, it doesn't stretch the axes where the fit
# overshoots the data, as it does for the early years of batteries
geom_fit <- function(fit) {
    geom_abline(
        intercept = coef(fit)[[1]],
        slope = coef(fit)[[2]],
        linewidth = 1
    )
}

# A blank label draws nothing, but its point still pushes the other labels away,
# which it wouldn't if the label layer only got the labelled years
label_every_fifth_year <- function(year) {
    if_else(year %% 5 == 0, as.character(year), "")
}

plot_wrights_law <- function(data) {
    ggplot(data, aes(cumulative_production, price)) +
        geom_point() +
        geom_text_repel(
            aes(label = label_every_fifth_year(year)),
            # Start the labels a third of a decade below and to the left of
            # their points, where a falling trend leaves room. The axes extend
            # that far past the data to fit them
            nudge_x = -0.35,
            nudge_y = -0.35,
            min.segment.length = 0,
            max.overlaps = Inf,
            max.time = Inf
        ) +
        scale_x_log10(breaks = decades, labels = decimal_labels) +
        scale_y_log10(breaks = decades, labels = decimal_labels) +
        # Orders of magnitude are the same size on both axes
        coord_fixed() +
        theme(panel.grid.minor = element_blank())
}

# OWID series: Solar PV module prices vs. cumulative capacity, World
# https://ourworldindata.org/grapher/solar-pv-prices-vs-cumulative-capacity
# As of November 15 2026:
# x axis: MW
# y axis: price in 2025 USD per watt
solar <-
    owid_get("solar-pv-prices-vs-cumulative-capacity") |>
    select(year, price = cost, cumulative_production = cumulative_capacity) |>
    mutate(cumulative_production = cumulative_production / 1000) |> # MW to GW
    drop_na()

# OWID series: Price of lithium-ion battery cells vs. cumulative production, World
# https://ourworldindata.org/grapher/battery-price-vs-cumulative-production
# As of November 15, 2026:
# x axis: GWh
# y axis: 2024 USD per kWh
battery <-
    owid_get("battery-price-vs-cumulative-production") |>
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

set.seed(349) # lmrob() starts from random subsamples
fits <-
    tribble(
        ~series,   ~data,   ~plot,        ~width, ~height,
        "solar",   solar,   solar_plot,   6.5,     3.3,
        "battery", battery, battery_plot, 6.5,     2.2
    ) |>
    expand_grid(method = names(fit_methods)) |>
    mutate(
        fit = map2(data, method, fit_wrights_law),
        plot = map2(plot, fit, \(plot, fit) plot + geom_fit(fit))
    )

fits |>
    mutate(filename = str_glue("{series}_wrights_law_{method}.png")) |>
    select(filename, plot, width, height) |>
    pwalk(ggsave)

# One row per coefficient; the learning rate only means something for the slope
fits |>
    rowwise(series, method) |>
    reframe(tidy(fit)) |>
    mutate(
        learning_rate = if_else(term == "(Intercept)", NA, 1 - 2^estimate)
    ) |>
    write_csv("line_fit_parameters.csv")
