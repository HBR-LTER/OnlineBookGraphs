library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)

setwd("~/mnt/HBRIM/data/onlinebookgraphs/OnlineBookGraphs/chapters/forest_management")

# =========================================================
# READ DATA
# =========================================================

W2 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/4/19/a6aeef15070be913ee2f06f431b9b7a7"
) |>
  mutate(Watershed = "W2")

W4 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/6/19/54b3ae4a45a2bb6c7006c2ab45cf63b9"
) |>
  mutate(Watershed = "W4")

W5 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/7/19/c08ebaccab4fee5fb60f4eee77f06cb3"
) |>
  mutate(Watershed = "W5")

W6 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/8/19/3312389e77cc5fd06bc8a7c9019de0ed"
) |>
  mutate(Watershed = "W6")

All <- rbind(W2, W4, W5, W6) |>
  mutate(across(where(is.double), ~ na_if(., -888.88)))

# =========================================================
# DATE + WATER YEAR
# =========================================================

All$DATE <- paste0(All$Year_Month, "-01")
All$DATE <- ymd(All$DATE)

w_year <- as.numeric(format(All$DATE, "%Y"))

before_june <- as.numeric(format(All$DATE, "%m")) < 6
w_year[before_june] <- w_year[before_june] - 1

All$wyear <- w_year

# =========================================================
# KEEP COMPLETE WATER YEARS
# =========================================================

monchem <- as.data.frame(table(All$wyear))

monchem$wys <- paste(monchem$Var1)

monchem[monchem$Freq < 40, "Use"] <- "incomplete wyear"
monchem[is.na(monchem$Use), "Use"] <- "complete"

All$Use <- monchem$Use[match(All$wyear, monchem$wys)]

All_complete <- All[All$Use == "complete", ]

# =========================================================
# SUMMARIZE
# =========================================================

manage <- All_complete |>
  group_by(Watershed, wyear) |>
  summarize(
    Ca_flux = sum(Ca_flux, na.rm = TRUE),
    flow_mm = sum(flow_mm, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# UNIT CONVERSIONS
# =========================================================

manage <- manage |>
  mutate(
    flow_m = flow_mm / 1000,
    Ca_mg = Ca_flux * 1000
  )

Ca <- spread(manage, "Watershed", "Ca_mg")
Fl <- spread(manage, "Watershed", "flow_m")

# convert Ca to total mg
Ca <- Ca |>
  mutate(
    W2 = W2 * 15.6,
    W4 = W4 * 36.1,
    W5 = W5 * 21.9,
    W6 = W6 * 13.2
  )

# convert flow to m3
Fl <- Fl |>
  mutate(
    W2 = W2 * 15.6 * 10000,
    W4 = W4 * 36.1 * 10000,
    W5 = W5 * 21.9 * 10000,
    W6 = W6 * 13.2 * 10000
  )

# =========================================================
# CALCULATE CONCENTRATIONS
# =========================================================

cag <- gather(Ca, "Watershed", "Ca_mg", 5:8)

flag <- gather(Fl, "Watershed", "flow_m3", 5:8)

flag <- flag |>
  mutate(flow_L = flow_m3 * 1000)

cag <- cag |>
  mutate(
    flow_L = flag$flow_L,
    camgL = Ca_mg / flow_L
  )

fa <- spread(cag, "Watershed", "camgL")

# W2 vs W6
fa1 <- gather(fa, "Watershed", "camgL", c(7, 10))
fa1 <- fa1[, c(1, 9, 10)]
fa1 <- na.omit(fa1)
fa1 <- spread(fa1, "Watershed", "camgL")

# W4 vs W6
fa2 <- gather(fa, "Watershed", "camgL", c(8, 10))
fa2 <- fa2[, c(1, 9, 10)]
fa2 <- na.omit(fa2)
fa2 <- spread(fa2, "Watershed", "camgL")

# W5 vs W6
fa3 <- gather(fa, "Watershed", "camgL", c(9, 10))
fa3 <- fa3[, c(1, 9, 10)]
fa3 <- na.omit(fa3)
fa3 <- spread(fa3, "Watershed", "camgL")

# =========================================================
# STYLING
# =========================================================

theme_set(theme_bw())

point_size <- 2.6
point_stroke <- 0.7
line_size <- 0.6

base_theme <- theme(
  
  text = element_text(
    family = "Arial",
    color = "black"
  ),
  
  axis.text.x = element_text(
    size = 13
  ),
  
  axis.text.y = element_text(
    size = 13
  ),
  
  axis.title.x = element_text(
    size = 16,
    margin = margin(t = 10)
  ),
  
  axis.title.y = element_text(
    size = 16,
    margin = margin(r = 10)
  ),
  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  legend.text = element_text(
    size = 15
  ),
  
  plot.title = element_text(
    size = 18
  ),
  
  plot.margin = margin(10, 30, 10, 20)
)

# =========================================================
# PLOT 1
# =========================================================

g1 <- ggplot(fa1) +
  
  geom_line(
    aes(x = wyear, y = W6),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_line(
    aes(x = wyear, y = W2),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_point(
    aes(x = wyear, y = W2),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  base_theme +
  
  geom_text(
    aes(x = 2008, y = 7, label = "treatment"),
    size = 5
  ) +
  
  geom_text(
    aes(x = 2008, y = 6, label = "reference"),
    size = 5
  ) +
  
  ylab("Ca (mg/L)") +
  xlab("Water Year (June 1)") +
  
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  
  scale_y_continuous(
    limits = c(0, 9),
    breaks = seq(0, 8, 2)
  ) +
  
  annotate(
    "point",
    x = 2004,
    y = 7,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  annotate(
    "point",
    x = 2004,
    y = 6,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_text(
    aes(
      x = 1971,
      y = 8.5,
      label = "W2: devegetated in 1965-1968"
    ),
    size = 5.2
  )

# =========================================================
# PLOT 2
# =========================================================

g2 <- ggplot(fa2) +
  
  geom_line(
    aes(x = wyear, y = W6),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_line(
    aes(x = wyear, y = W4),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_point(
    aes(x = wyear, y = W4),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  base_theme +
  
  geom_text(
    aes(x = 2008, y = 3, label = "treatment"),
    size = 5
  ) +
  
  geom_text(
    aes(x = 2008, y = 2.6, label = "reference"),
    size = 5
  ) +
  
  ylab("Ca (mg/L)") +
  xlab("Water Year (June 1)") +
  
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  
  ylim(0, 3.5) +
  
  annotate(
    "point",
    x = 2004,
    y = 3,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  annotate(
    "point",
    x = 2004,
    y = 2.6,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_text(
    aes(
      x = 1972,
      y = 3.2,
      label = "W4: strip cut in 1970, 1972, 1974"
    ),
    size = 5.2
  )

# =========================================================
# PLOT 3
# =========================================================

g3 <- ggplot(fa3) +
  
  geom_line(
    aes(x = wyear, y = W6),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_line(
    aes(x = wyear, y = W5),
    linewidth = line_size,
    color = "black"
  ) +
  
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_point(
    aes(x = wyear, y = W5),
    shape = 21,
    size = point_size,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  base_theme +
  
  geom_text(
    aes(x = 2008, y = 3, label = "treatment"),
    size = 5
  ) +
  
  geom_text(
    aes(x = 2008, y = 2.6, label = "reference"),
    size = 5
  ) +
  
  ylab("Ca (mg/L)") +
  xlab("Water Year (June 1)") +
  
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  
  ylim(0, 3.5) +
  
  annotate(
    "point",
    x = 2004,
    y = 3,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "black"
  ) +
  
  annotate(
    "point",
    x = 2004,
    y = 2.6,
    size = point_size,
    shape = 21,
    stroke = point_stroke,
    color = "black",
    fill = "white"
  ) +
  
  geom_text(
    aes(
      x = 1973,
      y = 3.2,
      label = "W5: whole tree harvest in 1983, 1984"
    ),
    size = 5.2
  )

# =========================================================
# PLOTLY LAYOUT
# =========================================================

common_layout <- list(
  
  font = list(
    family = "Arial, Helvetica, sans-serif",
    size = 14,
    color = "black"
  ),
  
  modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ),
  
  margin = list(
    l = 70,
    r = 30,
    t = 30,
    b = 55
  )
)

p1 <- ggplotly(g1) |> layout(common_layout)

p2 <- ggplotly(g2) |> layout(common_layout)

p3 <- ggplotly(g3) |> layout(common_layout)

# =========================================================
# COMBINE PLOTS
# =========================================================

pfinal <- subplot(
  p1,
  p2,
  p3,
  nrows = 3,
  shareX = TRUE,
  titleY = TRUE,
  margin = 0.04,
  heights = c(0.33, 0.33, 0.34)
) |>
  
  layout(
    autosize = TRUE,
    height = 1400,
    showlegend = FALSE,
    
    margin = list(
      l = 80,
      r = 40,
      t = 40,
      b = 60
    )
  )

# =========================================================
# SAVE HTML
# =========================================================

output_file <- "StreamwaterCalcium.html"

tmp_html <- tempfile(fileext = ".html")

htmlwidgets::saveWidget(
  pfinal,
  file = tmp_html,
  selfcontained = TRUE
)

file.copy(tmp_html, output_file, overwrite = TRUE)

unlink(tmp_html)