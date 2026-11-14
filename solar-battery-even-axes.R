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

# Wright's law: log10(price) = intercept + slope * log10(cumulative_production)
fit_wrights_law <- function(data) {
    lm(log10(price) ~ log10(cumulative_production), data = data) |>
        coef() |>
        set_names("intercept", "slope") |>
        as_tibble_row()
}

# On log scales geom_smooth() fits the same line as fit_wrights_law()
plot_wrights_law <- function(data) {
    ggplot(data, aes(cumulative_production, price)) +
        geom_point() +
        geom_text_repel(aes(label = year), max.overlaps = Inf) +
        geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            colour = "black",
            linewidth = 1
        ) +
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

ggsave("solar_wrights_law.png", solar_plot, width = 10, height = 5)
ggsave("battery_wrights_law.png", battery_plot, width = 11, height = 4)

list(solar = solar, battery = battery) |>
    map(fit_wrights_law) |>
    list_rbind(names_to = "series") |>
    mutate(learning_rate = 1 - 2^slope) |>
    write_csv("line_fit_parameters.csv")
