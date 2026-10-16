library(tidyverse)
library(sf)
library(msmdata)

# load property tax data
van_prop_tax = st_read("./sdsc-data/outputs/spatialfiles/van_property_tax.geojson")

dt_boundary = st_read("./sdsc-data/outputs/spatialfiles/cma_downtowns.geojson")

van_tax_clip = st_intersection(van_prop_tax, dt_boundary)

van_tax_join = st_join(van_prop_tax, dt_boundary %>% select(DTNAME), join = st_intersects, left = TRUE)

van_tax_sum = van_tax_join %>%
    st_drop_geometry() %>%
    summarise(
        tax_sqm = mean(tax_sqm, na.rm = TRUE), .by = DTNAME
    )

van_tax_sum = van_tax_sum %>%
    mutate(DTNAME = case_when(
        is.na(DTNAME) ~ "Outside Downtown",
        TRUE ~ "Downtown Vancouver"
    ))

library(scales)

van_tax_plot = van_tax_sum %>%
    mutate(
        DTNAME = factor(DTNAME, levels = c("Downtown Vancouver", "Outside Downtown"))
    ) %>%
    ggplot(aes(x = DTNAME, y = tax_sqm, fill = DTNAME)) +
    geom_col(width = 0.5) +
    geom_text(
        aes(label = dollar(tax_sqm, accuracy = 0.01)),
        vjust = -0.5,
        colour = "#0b0b0b",
        size = 4,
        fontface = "bold"
    ) +
    scale_fill_manual(
        values = c("Downtown Vancouver" = "#2a78d6", "Outside Downtown" = "#a3a29b")
    ) +
    scale_y_continuous(
        labels = dollar,
        expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
        title = "Average property tax per square metre, Vancouver",
        x = NULL,
        y = "Average tax per m²"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none"
    )

ggsave(
    "./sdsc-data/outputs/charts/van_tax_sqm.png",
    plot = van_tax_plot,
    width = 6,
    height = 4.5,
    dpi = 300,
    bg = "white"
)


van_big_levy = van_tax_join %>%
    filter(tax_levy >= 50000)

van_big_levy = van_big_levy %>%
    st_drop_geometry() %>%
    count(DTNAME)
