library(tidyverse)
library(plotly)
library(webshot2)


# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
dt <- get_edi_table(identifier = "220", entity_seq = 1)
str(dt)

data_sum <- dt |> 
  group_by(TreeSpecies, ElapsedDays) |> 
  summarise(
    mean_masslost = mean(PctMassLost, na.rm = TRUE),
    se = sd(PctMassLost, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

data_sum <- data_sum |> 
  mutate(SpeciesName = recode(TreeSpecies,
                              AB = "Beech",
                              SM = "Maple",
                              WA = "Ash",
                              YB = "Y Birch"
  ))

plot1 <- ggplot(data_sum,
                aes(ElapsedDays, mean_masslost,
                    linetype = SpeciesName,
                    shape = SpeciesName)) +
  
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(
    ymin = mean_masslost - se,
    ymax = mean_masslost + se
  ),
  width = 0) +
  labs(
    x = "Days of Incubation",
    y = "% Mass Lost",
    linetype = "Species",
    shape = "Species"
  ) +
  theme_classic()

plot1


fig <- ggplotly(plot1, margin=m) |> 
  layout(
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    ),
    title = list(
      text = "Reciprocal Litter Transplant - Mass Loss",
      x = 0.5,
      xanchor = "center",
      font = list(size = 24, color = "black")
    ),
    margin = list(t = 60)   # pixels of top margin
  )
fig

output_file <- "LongTerm_MassLoss.html"
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
webshot2::webshot(output_file, file ="LongTerm_MassLoss.png" , delay = 2)
