# Liam's Rubik's cube scores #

# Packages
library(tidyverse)
library(jpeg)
library(patchwork)

# Read Data and Pre-Process
df <- read.csv("liam.csv") %>%
      group_by(Type) %>%
      mutate(time = Minutes * 60 + Seconds,
             id = row_number()) %>%
      ungroup()

# Summarize Results
summary <- df %>%
           group_by(Type) %>%
           summarise(n = n(), avg = mean(time)) %>%
           mutate(min = trunc(avg/60),
                  sec = round(avg %% 60,0))

# Load Images for Plot
cube <- readJPEG('cube.jpg', native = TRUE)
pyraminx <- readJPEG('pyraminx.jpg', native = TRUE)

# Plot Times
ggplot(df, aes(x=id, y=time)) +
  geom_line(color = "#0099f9", size = 2) +
  geom_point(color = "#0099f9", size = 4) +
  facet_grid(rows = vars(Type)) +
  geom_text(
    data    = summary,
    mapping = aes(x = -Inf, y = -Inf, label = paste0(n," solves. Average time: ",min," minutes and ",sec," seconds")),
    hjust   = -2,
    vjust   = -1) +
  theme_classic(base_size = 15) + 
  labs(
    title = "Time to Solve 3x3 Cube and Pyraminx",
    x = "",
    y = "Time (s)"
  ) +
  theme(
    axis.title.y = element_text(size = 16, face = "bold")
  ) +
  inset_element(p = cube,
                left = 0.6,
                bottom = 0.55,
                right = 0.7,
                top = 0.65) +
  inset_element(p = pyraminx,
                left = 0.6,
                bottom = 0.05,
                right = 0.7,
                top = 0.15) 

