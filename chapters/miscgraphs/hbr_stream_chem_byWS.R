# ============================================================
# HBR Stream Chemistry — 6-panel vertical stack by water year
# One figure per site, saved to working directory
# ============================================================

# ── Packages ----
pkgs <- c("ggplot2", "dplyr", "patchwork", "scales")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ── Fetch data ----
url <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/14/024b6acc5cb2e03a14fff5558bbffc0c"
dt  <- read.csv(url)

# Coerce DIC to numeric in case it loaded as logical (all-NA)
dt$DIC <- as.numeric(dt$DIC)

# ── Variables & y-axis labels ----
vars <- c("pH", "DOC", "DIC", "NH4", "SO4", "Ca")

ylabs <- c(
  pH  = "pH",
  DOC = "DOC (mg C/L)",
  DIC = "DIC (mg C/L)",
  NH4 = "NH4 (ueq/L)",
  SO4 = "SO4 (ueq/L)",
  Ca  = "Ca (ueq/L)"
)

# ── Shared theme (all black, large labels) ----
base_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(size = 13, colour = "black",
                                      margin = margin(r = 8)),
    axis.text.x        = element_text(size = 12, colour = "black"),
    axis.text.y        = element_text(size = 12, colour = "black"),
    axis.ticks         = element_line(colour = "grey60", linewidth = 0.3),
    plot.margin        = margin(t = 4, r = 12, b = 4, l = 4)
  )

# ── Single-variable panel builder ----
make_panel <- function(data_site, var, is_last = FALSE) {

  d <- data_site %>%
    filter(!is.na(.data[[var]]), !is.na(waterYr)) %>%
    group_by(waterYr) %>%
    summarise(
      med = median(.data[[var]], na.rm = TRUE),
      q25 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(d) == 0 || all(is.na(d$med))) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste("No data:", var),
               colour = "grey60", size = 4) +
      labs(y = ylabs[[var]]) +
      base_theme +
      theme(axis.text  = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    return(p)
  }

  p <- ggplot(d, aes(x = waterYr)) +
    geom_ribbon(aes(ymin = q25, ymax = q75),
                fill  = "black",
                alpha = 0.12) +
    geom_line(aes(y = med),
              colour    = "black",
              linewidth = 0.9) +
    geom_point(aes(y = med),
               colour = "black",
               fill   = "white",
               shape  = 21,
               size   = 1.8,
               stroke = 1.3) +
    scale_x_continuous(breaks = pretty(d$waterYr, n = 8),
                       expand = expansion(mult = 0.02)) +
    scale_y_continuous(labels = label_number(accuracy = NULL)) +
    labs(y = ylabs[[var]]) +
    base_theme

  if (!is_last) {
    p <- p + theme(axis.text.x  = element_blank(),
                   axis.ticks.x = element_blank())
  } else {
    p <- p + labs(x = "Water Year") +
      theme(axis.title.x = element_text(size = 13, colour = "black",
                                        margin = margin(t = 8)))
  }
  p
}

# ── Per-site figure builder ----
make_site_figure <- function(site_id, data) {

  d_site <- filter(data, site == site_id)

  panels <- lapply(seq_along(vars), function(i) {
    make_panel(d_site, vars[i], is_last = (i == length(vars)))
  })

  wrap_plots(panels, ncol = 1) +
    plot_annotation(
      title    = paste0("HBR Stream Chemistry  |  Site: ", site_id),
      subtitle = "Annual median (line) with interquartile range (ribbon) by water year",
      caption  = "Data: HBR LTER knb-lter-hbr.208.14",
      theme = theme(
        plot.background = element_rect(fill = "white", colour = NA),
        plot.title    = element_text(size = 15, face = "bold", colour = "black",
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(size = 11, colour = "grey40",
                                     margin = margin(b = 6)),
        plot.caption  = element_text(size = 9,  colour = "grey60",
                                     hjust = 1,  margin = margin(t = 8))
      )
    )
}

# ── Loop over sites, save PNGs to working directory ----
sites <- sort(unique(dt$site))
message("Sites found: ", paste(sites, collapse = ", "))
message("Saving to:   ", getwd())

for (s in c("W1","W3","W6","W9")) {
  fig  <- make_site_figure(s, dt)
  file <- paste0("site_", s, ".png")
  ggsave(filename = file,
         plot     = fig,
         width    = 10,
         height   = 14,
         dpi      = 180,
         bg       = "white")
  message("Saved -> ", file.path(getwd(), file))
}

message("\nDone! ", length(sites), " plots saved.")
