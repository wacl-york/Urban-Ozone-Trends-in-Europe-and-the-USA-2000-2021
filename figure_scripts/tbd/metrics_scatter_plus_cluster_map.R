mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-125,-70), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Drop geometry only once
data <- combined_cluster_data |>
  st_drop_geometry() |>
  filter(cluster != 99,
         year %in% c(2004,2018))

data_sf = data |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Get unique combinations of region, type, and tau
combinations <- unique(data |> select(region, type, tau))

# Open PDF device
pdf("analysis/6MMDA1_scatter_plots_by_region_type_tau_combo.pdf", width = 12, height = 8)

# Loop through each combination
for (i in 1:nrow(combinations)) {
  region_i <- combinations$region[i]
  type_i   <- combinations$type[i]
  tau_i    <- combinations$tau[i]

  # Filter data for the current combination
  test <- data |>
    filter(region == region_i, type == type_i, tau == tau_i)

  test_sf <- data_sf |>
    filter(region == region_i, type == type_i, tau == tau_i)

  # Skip empty subsets
  if (nrow(test) == 0) next

  # Create plot
  p_cluster <- ggplot(test) +
    geom_point(aes(x = `6MMDA1`, y = fit, colour = as.factor(cluster))) +
    #scale_colour_manual(values = p_colours) +
    facet_wrap(~year, scales = "fixed") +
    theme_pubr() +
    theme(legend.position = "bottom") +
    ggtitle(paste0(type_i, ", ", region_i, ", tau = ", tau_i))

  p_significance <- ggplot(test) +
    geom_point(aes(x = `6MMDA1`, y = fit, colour = pvStr)) +
    scale_colour_manual(values = p_colours) +
    facet_wrap(~year, scales = "fixed") +
    theme_pubr() +
    theme(legend.position = "bottom") +
    ggtitle(paste0(type_i, ", ", region_i, ", tau = ", tau_i))

  if (region_i == "Europe") {
    x_limits <- st_coordinates(limEU)[,2]
    y_limits <- st_coordinates(limEU)[,1]
  } else if (region_i == "United States of America") {
    x_limits <- st_coordinates(limUS)[,2]
    y_limits <- st_coordinates(limUS)[,1]
  } else {
    # Optional: default to full world extent
    x_limits <- c(-180, 180)
    y_limits <- c(-90, 90)
  }

  p_map = ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = test_sf, aes(colour = as.factor(cluster)))+
    #scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = x_limits)+
    scale_x_continuous(limits = y_limits) +
    theme(legend.position = "bottom")


  # Patchwork
  library(patchwork)
  # Left column: cluster on top, significance on bottom
  left_side = p_cluster / p_significance  # stacked vertically

  # Final layout: left half (vertical stack) + right half (map)
  final_plot = left_side | p_map + plot_layout(widths = c(1.25, 0.75)) # side-by-side layout


  # Print to PDF (each print adds a new page)
  print(final_plot)
}

# Close PDF device
dev.off()

