# =============================================================================
# Save the "Alpha Shapes by Elevation" plot as SEPARATE files per elevation,
# in TWO versions with the same PANEL size (not necessarily same overall
# image size, since Version B has a legend):
#   Version A ("_hull_nogroup"): points NOT colored by diet
#   Version B ("_diet_nohull")  : points colored by diet (FIXED diet -> color
#                                  mapping, consistent across elevations),
#                                  with its legend stacked below the panel
#                                  in a separate row rather than eating into
#                                  panel space
#
# Both versions include the dashed convex hull.
#
# PLUS: a dedicated 1000m plot that highlights the geometric difference
# between the convex hull and the alpha shape (the "void" region) using
# sf polygon differencing.
#
# Assumes tri_df and plot_data_diet already exist in your environment, i.e.
# this runs AFTER the alpha-shape / tri_df / plot_data_diet block in your
# script. Nothing upstream of that is touched.
# =============================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)   # needed to separate the legend from the panel so panel
# size stays identical between the two versions
library(sf)        # needed for polygon differencing in the highlight plot
# --------------------------------------------------------------------------
# 0. Shared theme tweak: larger title + axis title text
# --------------------------------------------------------------------------
big_text_theme <- theme(
  plot.title   = element_text(size = 20, face = "bold"),
  axis.title   = element_text(size = 16),
  legend.title = element_text(size = 15),
  legend.text  = element_text(size = 13)
)
# --------------------------------------------------------------------------
# 1. Convex hull vertices per elevation (closed polygon, for a dashed outline)
# --------------------------------------------------------------------------
# Normalize Diet labels BEFORE anything else touches them. If a diet label
# has inconsistent whitespace or capitalization across rows (e.g. "blood"
# vs "Blood " for different elevations), those rows won't match the fixed
# `limits` used in scale_color_brewer() below, get coerced to NA color, and
# geom_point() silently drops them -- which looks like "the blood/nectar
# points are missing" at whichever elevation happens to have the mismatched
# spelling, even though the data is really there.
plot_data_diet <- plot_data_diet %>%
  mutate(Diet = trimws(Diet))
hull_df <- plot_data_diet %>%
  group_by(Elevation_num) %>%
  group_modify(~ {
    pts <- as.matrix(.x[, c("NMDS1", "NMDS2")])
    if (nrow(pts) < 3) return(tibble(X = numeric(0), Y = numeric(0)))
    idx <- chull(pts[, 1], pts[, 2])
    idx <- c(idx, idx[1])  # repeat first vertex to close the loop
    tibble(X = pts[idx, 1], Y = pts[idx, 2])
  }) %>%
  ungroup()
# Fixed diet -> color mapping, computed ONCE across the full dataset (not
# per-elevation), so a given diet category always gets the same color no
# matter which elevations happen to have that diet present. Without `limits`
# fixed here, scale_color_brewer() re-derives its palette from whichever
# diet levels are present in EACH facet's own data, which is why colors were
# shifting between elevation plots.
all_diets <- sort(unique(plot_data_diet$Diet))
# Diagnostic: confirm every elevation's Diet values are actually covered by
# `all_diets`. If this prints anything, there's a remaining label mismatch
# (e.g. a typo variant) upstream in plot_data_diet that trimws() alone won't
# fix -- worth checking unique(plot_data_diet$Diet) by eye in that case.
unmatched_diets <- plot_data_diet %>%
  filter(!(Diet %in% all_diets)) %>%
  distinct(Elevation_num, Diet)
if (nrow(unmatched_diets) > 0) {
  warning("Diet values not covered by all_diets -- these points will be dropped from plots:\n",
          paste(capture.output(print(unmatched_diets)), collapse = "\n"))
}
# --------------------------------------------------------------------------
# 2. Loop over elevations, save both versions separately
# --------------------------------------------------------------------------
out_dir <- "alpha_shape_by_elevation"
dir.create(out_dir, showWarnings = FALSE)
# Fixed physical size for the PANEL (data area) itself, in inches -- both
# versions use these same numbers for the panel. Version B additionally gets
# `legend_height` of extra canvas below the panel to hold its legend, so its
# saved file is taller overall but the panel occupies exactly the same area
# as Version A's.
panel_width  <- 5
panel_height <- 4.5
legend_height <- 0.9
elevations <- sort(unique(plot_data_diet$Elevation_num))
# ---- Build ONE shared diet legend, ONCE, from the FULL dataset -----------
# Using the complete plot_data_diet (rather than one elevation's subset)
# means every diet category has real, actual points backing its legend key
# -- no dependence on scale_color_brewer(drop = FALSE) conjuring a key for
# a category with zero points in a given elevation, which is what let
# "blood"/"nectar" go missing at 2500m before. This single legend object is
# then reused, unchanged, across every elevation's saved image, so the
# legend is guaranteed identical (same categories, same order, same colors)
# everywhere rather than being independently rebuilt (and potentially
# inconsistent) per elevation.
shared_legend_src <- ggplot(plot_data_diet, aes(x = NMDS1, y = NMDS2, color = Diet)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_brewer(palette = "Set2", limits = all_diets, breaks = all_diets) +
  labs(color = "Primary Diet") +
  theme_bw() +
  big_text_theme +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(
    nrow = 1, override.aes = list(size = 4, alpha = 1)
  ))
diet_legend <- cowplot::get_legend(shared_legend_src)
for (elev in elevations) {

  tri_e  <- tri_df       %>% filter(Elevation_num == elev)
  pts_e  <- plot_data_diet %>% filter(Elevation_num == elev)
  hull_e <- hull_df      %>% filter(Elevation_num == elev)

  # ---- Version A: no diet color-coding, dashed convex hull ----------------
  p_hull <- ggplot() +
    geom_polygon(
      data = tri_e, aes(x = X, y = Y, group = tri_id),
      fill = "steelblue", color = "steelblue4", alpha = 0.3, linewidth = 0.2
    ) +
    geom_path(
      data = hull_e, aes(x = X, y = Y),
      color = "black", linetype = "dashed", linewidth = 0.7
    ) +
    geom_point(
      data = pts_e, aes(x = NMDS1, y = NMDS2),
      color = "grey20", size = 2.5, alpha = 0.85
    ) +
    labs(
      title = paste0("Elevation: ", elev, " m"),
      x = "NMDS1", y = "NMDS2"
    ) +
    theme_bw() +
    big_text_theme +
    theme(legend.position = "none")

  fname_hull <- file.path(out_dir, sprintf("alpha_shape_elev_%d_hull_nogroup.png", elev))
  ggsave(fname_hull, p_hull, width = panel_width, height = panel_height, dpi = 300, bg = "white")

  # ---- Version B: diet color-coded, WITH convex hull ------------------------
  # Panel itself is built with NO legend, so its geometry is identical to
  # Version A's panel. The (shared, pre-built) legend is stacked below in
  # its own row, so it doesn't shrink the panel to make room for itself.
  p_diet_panel <- ggplot() +
    geom_polygon(
      data = tri_e, aes(x = X, y = Y, group = tri_id),
      fill = "steelblue", color = "steelblue4", alpha = 0.3, linewidth = 0.2
    ) +
    geom_path(
      data = hull_e, aes(x = X, y = Y),
      color = "black", linetype = "dashed", linewidth = 0.7
    ) +
    geom_point(
      data = pts_e, aes(x = NMDS1, y = NMDS2, color = Diet),
      size = 2.5, alpha = 0.85
    ) +
    scale_color_brewer(
      palette = "Set2", limits = all_diets, breaks = all_diets, drop = FALSE
    ) +
    labs(
      title = paste0("Elevation: ", elev, " m"),
      x = "NMDS1", y = "NMDS2", color = "Primary Diet"
    ) +
    theme_bw() +
    big_text_theme +
    theme(legend.position = "none")

  p_diet <- cowplot::plot_grid(
    p_diet_panel, diet_legend,
    ncol = 1, rel_heights = c(panel_height, legend_height)
  )

  fname_diet <- file.path(out_dir, sprintf("alpha_shape_elev_%d_diet_nohull.png", elev))
  ggsave(fname_diet, p_diet, width = panel_width, height = panel_height + legend_height,
         dpi = 300, bg = "white")
}
message(sprintf("Saved %d elevation x 2 version plots to '%s/'", length(elevations), out_dir))
# =============================================================================
# 3. Highlight plot for 1000m: alpha shape vs. convex hull "void" region
#
# This shades, in a distinct color, exactly the area that lies inside the
# convex hull but is NOT covered by the alpha shape -- i.e. the geometric
# counterpart of your void ratio (1 - alpha/convex_hull). Built with sf
# polygon differencing so the shaded area is exact, not an approximation.
# =============================================================================
target_elevs <- c(500, 1000)
# Nice, clearly distinct highlight color for the void region against the
# existing steelblue alpha shape fill
void_color  <- "#E4572E"   # warm burnt-orange/red -- reads clearly against steelblue
alpha_color <- "steelblue"
# Flatten an sf geometry (POLYGON or MULTIPOLYGON, possibly with holes) into
# a plain X/Y/group data frame for geom_polygon(). This avoids geom_sf() /
# coord_sf(), which silently forces a fixed 1:1 aspect ratio -- a different
# coordinate system than the plain Cartesian one used in the other panels.
# Using geom_polygon() here instead keeps the coordinate system identical
# across ALL plots, so panel size/shape is truly consistent, not just the
# same width/height passed to ggsave().
sf_to_polygon_df <- function(sfc_obj) {
  m <- sf::st_coordinates(sfc_obj)
  l_cols <- grep("^L", colnames(m), value = TRUE)
  grp <- apply(m[, l_cols, drop = FALSE], 1, paste, collapse = "_")
  data.frame(X = m[, "X"], Y = m[, "Y"], group = grp)
}
for (target_elev in target_elevs) {

  tri_e  <- tri_df        %>% filter(Elevation_num == target_elev)
  pts_e  <- plot_data_diet %>% filter(Elevation_num == target_elev)
  hull_e <- hull_df       %>% filter(Elevation_num == target_elev)

  stopifnot(
    "No tri_df rows found for target_elev -- check that this Elevation_num exists" =
      nrow(tri_e) > 0,
    "No hull_df rows found for target_elev" = nrow(hull_e) > 0
  )

  # ---- Build the alpha shape as a single (possibly multi-part) sf polygon --
  # Each tri_id becomes its own closed triangle polygon; st_union merges them
  # into one geometry so overlapping/adjacent triangles don't double-count.
  tri_polys <- tri_e %>%
    group_by(tri_id) %>%
    group_map(~ {
      coords <- as.matrix(.x[, c("X", "Y")])
      coords_closed <- rbind(coords, coords[1, ])  # close the ring
      st_polygon(list(coords_closed))
    })

  alpha_union_sf <- st_union(st_sfc(tri_polys))

  # ---- Build the convex hull as a single sf polygon -------------------------
  hull_coords <- as.matrix(hull_e[, c("X", "Y")])
  hull_sf <- st_sfc(st_polygon(list(hull_coords)))

  # ---- The difference: hull area not covered by the alpha shape ------------
  void_region_sf <- st_difference(hull_sf, alpha_union_sf)

  # Flatten both geometries to plain data frames for geom_polygon()
  alpha_shape_df <- sf_to_polygon_df(alpha_union_sf)
  void_region_df  <- sf_to_polygon_df(void_region_sf)

  p_void_highlight <- ggplot() +
    geom_polygon(
      data = alpha_shape_df, aes(x = X, y = Y, group = group),
      fill = alpha_color, color = "steelblue4", alpha = 0.5, linewidth = 0.2
    ) +
    geom_polygon(
      data = void_region_df, aes(x = X, y = Y, group = group),
      fill = void_color, color = NA, alpha = 0.65
    ) +
    geom_path(
      data = hull_e, aes(x = X, y = Y),
      color = "black", linetype = "dashed", linewidth = 0.7
    ) +
    geom_point(
      data = pts_e, aes(x = NMDS1, y = NMDS2),
      color = "grey10", size = 2.5, alpha = 0.9
    ) +
    labs(
      title = paste0("Elevation: ", target_elev, " m"),
      x = "NMDS1", y = "NMDS2"
    ) +
    theme_bw() +
    big_text_theme +
    theme(legend.position = "none")

  fname_void <- file.path(out_dir, sprintf("alpha_shape_elev_%d_hull_vs_alpha_void.png", target_elev))
  ggsave(fname_void, p_void_highlight, width = panel_width, height = panel_height,
         dpi = 300, bg = "white")

  message(sprintf("Saved hull-vs-alpha-shape void highlight plot for %dm to '%s'", target_elev, fname_void))
}
