# install.packages("tidyverse", "Hmisc")

# magrittr::`%>%`, dplyr::filter, dplyr::group_by, dplyr::mutate,
# dplyr::summarise, dplyr::arrange, dplyr::n_distinct, ggplot2::ggplot,
# ggplot2::geom_tile, ggplot2::scale_fill_gradient2, ggplot2::ggsave
library(tidyverse)

# Calculate this easily to not use magic numbers
max.score <- (1 - 0.5) * 3 # most gendered for lower and upper bounds and estimate (3)

# Define a very small number to get just below a specified point
ε <- 10e-3 # smaller than this messes up the legend

# Go through all the files in the "data" directory
for (f in list.files("data", full.names = TRUE)) {
  # Only files ending in ".csv"
  if (endsWith(f, suffix = ".csv")) {
    # Data must be of the form:
    # name,sex,count,year,IPA
    # Brad,M,1,1991,bræd
    # Amanda,F,1,1995,əˈmændə
    # ...
    name.data <- read.csv(f)

    # One big tidyverse pipe (with `%>%`)
    heat.map <- name.data %>%
      # Remove unpronounceable names (ending with asterisks)
      filter(substring(IPA, first = nchar(IPA), last = nchar(IPA)) != "*") %>%
      # Group together same pronunciations for each year regardless of spelling/gender
      group_by(year, IPA) %>%
      # Make each observation have counts for both female and male
      summarise(
        name = list(name), # list of spellings (still with duplicates)
        female = sum(count[sex == "F"]), # female counts from adding up all Fs
        male = sum(count[sex == "M"]), # male counts from adding up all Ms
        .groups = "drop" # no more groups
      ) %>%
      # Perform operations with pronunciations of all years together
      group_by(IPA) %>%
      # Remove strictly gendered names (i.e., 0 female or 0 male)
      filter(sum(female) != 0 & sum(male) != 0) %>%
      # Apply all (unique) spellings for every pronunciation across years
      mutate(
        name = paste( # one string
          unique(unlist(name)), # vector from list with no duplicates
          collapse = ", " # commas between all of them
        )
      ) %>%
      # Put it back to normal
      ungroup() %>%
      # Made up gender-neutrality metric (0 being the most gender-neutral)
      mutate(
        sign = sign(female - male), # +/- for graphing female/male
        score = as.numeric( # Not a "named numeric"
          rowSums( # point estimate, lower bound, and upper bound
            abs( # positive differences so they compound
              Hmisc::binconf( # binary confidence interval
                x = female, # male would be the same
                n = female + male # total
              ) - 0.5 # distance from 50:50
            )
          )
        )
      ) %>%
      # Sort by score for next operation
      arrange(score) %>%
      # Reduce data set (by making x and y lengths of plot equal)
      filter(
        IPA %in% # select pronunciations that are in...
          unique(IPA) # ...unique list of pronunciations...
          [1:n_distinct(name.data$year)] # ...within the first {# of years} items
      ) %>%
      # Add these together to make the plot
      ggplot() +
      # Plot as a heat map
      geom_tile(aes(
        x = year, # independent variable along horizontal
        y = stats::reorder(name, score, FUN = min), # names sorted by minimum score
        fill = sign * score # colors based on signed score (+/- for female/male)
      )) +
      # Color the scale with arbitrary ranges
      scale_fill_gradientn(
        colors = c( # corresponding values below
          "#89CFF0", # baby blue
          "black", # more gender neutral
          "white", # exactly 50:50 (score is not plotted)
          "black", # more gender neutral
          "#FFC0CB" # pink
        ),
        values = scales::rescale( # scale from 0 to 1
          c( # corresponding colors above
            -max.score, # 100% male
            -ε, # lower than minimum score
            0, # exactly 50:50 (score is not plotted)
            ε, # lower than minimum score
            max.score # 100% female
          )
        ),
        labels = c( # corresponding values below
          "Male", # arbitrary minimum
          "Neutral", # actually "Unknown" but no room for extra labels
          "Female" # arbitrary maximum
        ),
        breaks = c( # corresponding labels above
          -(max.score - ε), # less a small amount to make it fit in the legend
          0, # actually "Unknown" but no room for extra labels
          max.score - ε # less a small amount to make it fit in the legend
        ),
        name = "Gender" # legend title
      ) +
      # Label axes
      labs(title = "Name Gender Neutrality", x = "Year", y = "Name") +
      # Set x-axis ticks
      scale_x_continuous(
        breaks = seq( # labeled ticks
          min(name.data$year), # first year
          max(name.data$year), # as close as possible to last year
          10 # one decade
        ),
        minor_breaks = seq( # unlabeled grid lines
          min(name.data$year), # first year
          max(name.data$year), # last year
          1 # yearly
        )
      ) +
      # Set legend ticks
      guides(fill = guide_colorbar(ticks.colour = NA)) +
      # Set zoom
      coord_cartesian(expand = FALSE) # no padding

    # Draw heat map
    ggsave(
      plot = heat.map, # defaults if one was plotted before
      filename = sub(pattern = "csv", replacement = "svg", f), # match with data set name
      device = svg, # `"svg"` (vs. `svg()`) requires `library(svglite)`
      units = "in", # this is default, but just to clarify
      width = 0.25 * n_distinct(name.data$year), # ~1/4" for each year (minus labels)
      height = 0.25 * n_distinct(heat.map$data$IPA), # ~1/4" for each pronunciation
      limitsize = FALSE # allow for a big plot
    )


    # A different sorting function
    boxplot.sum <- function(x) {
      return(sum(boxplot.stats(x)$stats)) # add median and ends of box and whiskers
    }

    # Make a box plot of variability in scores
    ggplot(heat.map$data) +
      geom_boxplot(aes(x = score, y = stats::reorder(name, score, FUN = boxplot.sum)))

    # Draw box plot
    ggsave(
      filename = sub(pattern = ".csv", replacement = "-variability.svg", f), # match with data set name
      device = svg, # `"svg"` (vs. `svg()`) requires `library(svglite)`
      units = "in", # this is default, but just to clarify
      width = 20, # number pulled out of my ass
      height = n_distinct(heat.map$data$name), # ~1" for each name
      limitsize = FALSE # allow for a big plot
    )
  }
}
