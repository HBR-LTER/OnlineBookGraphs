library(tidyverse)
library(plotly)
library(dplyr)
library(webshot2)


# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
dt <- get_edi_table(identifier = "446", entity_seq = 1)
str(dt)

ws1_data <- dt 

# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
dt <- get_edi_table(identifier = "448", entity_seq = 1)
str(dt)

ws6_data <- dt 

df <- bind_rows(ws1_data, ws6_data)

df <- df |> 
  filter(status == "Live")

exclude_w1 <- c(1:7, 14:15, 23, 31, 32, 39, 41, 44, 48, 51, 55, 168)
exclude_w6 <- c(1, 2, 4, 8, 9, 15:18, 24:27, 34, 35, 42)

df <- df |>
  filter(status == "Live",
         sample_class == "tree",
         !(watershed == "W1" & plot %in% exclude_w1),
         !(watershed == "W6" & plot %in% exclude_w6),
         !(watershed == "W1" & year < 1996),
         species %in% c("ACSA","BEAL","FAGR"))

df <- df |>
  mutate(ba_m2 = pi * (dbh_cm / 200)^2 * exp_factor)

plot_ba <- df |>
  group_by(watershed, plot, year, species) |>
  summarise(ba = sum(ba_m2, na.rm=TRUE), .groups="drop")

plot_total <- plot_ba |>
  group_by(watershed, plot, year) |>
  summarise(total_ba = sum(ba), .groups="drop")

plot_frac <- plot_ba |>
  left_join(plot_total, by=c("watershed","plot","year")) |>
  mutate(frac = ba / total_ba)

summary_df <- plot_frac |>
  group_by(watershed, year, species) |>
  summarise(mean_frac = mean(frac, na.rm=TRUE),
            se = sd(frac, na.rm=TRUE)/sqrt(n()),
            .groups="drop")

summary_df <- summary_df |>
  mutate(watershed = factor(watershed, levels = c("W6","W1")))


colors <- c("ACSA" = "#F8766D",
            "BEAL" = "#00BA38",
            "FAGR" = "#619CFF")

fig_W1 <- summary_df |> 
  filter(watershed == "W1") |> 
  plot_ly(
    x = ~year,
    y = ~mean_frac,
    color = ~species,
    colors = colors,
    type = "scatter",
    mode = "lines+markers",
    error_y = ~list(type="data", array=se, visible=TRUE)
  ) |> 
  layout(
    xaxis = list(title = "Year", range = c(1990, NA)),
    yaxis = list(title = "Relative Dominance"),
    showlegend = TRUE
  )

fig_W6 <- summary_df |> 
  filter(watershed == "W6", year >= 1990) |> 
  plot_ly(
    x = ~year,
    y = ~mean_frac,
    color = ~species,
    colors = colors,
    type = "scatter",
    mode = "lines+markers",
    error_y = ~list(type="data", array=se, visible=TRUE)
  ) |> 
  layout(
    xaxis = list(title = "Year", range = c(1995, NA)),
    yaxis = list(title = "Relative Dominance")
  )


n_W6 <- length(fig_W6$x$data)


fig <- subplot(
  fig_W6, fig_W1,
  nrows = 1,
  shareY = FALSE,
  titleX = TRUE,
  titleY = TRUE,
  margin = 0.08) |> 
  layout(
    legend = list(title = list(text = "Species"), x = 0.47, y = 1, xanchor = "center", orientation = "r"),
    annotations = list(
      list(text = "W6", x = 0.19, y = 1.08, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14)),
      list(text = "W1", x = 0.8, y = 1.08, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14))),
    margin = list(
      l = 60,
      r = 60,
      t = 60,
      b = 60),
    xaxis = list(
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = FALSE
    ),
    xaxis2 = list(   # second subplot
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    yaxis2 = list(   # second subplot
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    )) |> 
  style(showlegend = TRUE) |> 
  style(showlegend = FALSE, traces = seq(3, length(fig_W1$x$data)*2))

fig


output_file <- "BasalArea-W1W6_Trends.html"
fname <- tools::file_path_sans_ext(basename(output_file))

p <- fig |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(p, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)

# Save static PNG from the HTML
Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium-browser")  # adjust path
webshot2::webshot(output_file, file ="BasalArea-W1W6.png" , delay = 2)