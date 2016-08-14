
# rotation.R
library(tourr)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2)

# Random Moving Points ----
aps <- 2
fps <- 30
mat <- rescale(as.matrix(flea[c(1,3,4)]))
tour <- new_tour(mat, grand_tour(), NULL)
start <- tour(0)
proj_data <- reactive({
  invalidateLater(1000 / fps, NULL);
  step <- tour(aps / fps)
  data.frame(center(mat %*% step$proj), species = flea$species)
})
proj_data %>% ggvis(~X1, ~X2, fill = ~species) %>%
  layer_points() %>%
  scale_numeric("x", domain = c(-1, 1)) %>%
  scale_numeric("y", domain = c(-1, 1)) %>%
  set_options(duration = 0)


# Interactive ggvis ----

sliderBox <- input_slider(.1, 2, value = 1, step = .1,
                          label = "Bandwidth adjustment")
selectBox <- input_select(c("Gaussian" = "gaussian",
                            "Epanechnikov" = "epanechnikov","Rectangular" = "rectangular",
                            "Triangular" = "triangular","Biweight" = "biweight",
                            "Cosine" = "cosine", "Optcosine" = "optcosine"),label = "Kernel")
mtcars %>%
  ggvis(x = ~wt) %>%
  layer_densities(adjust = sliderBox, kernel = selectBox)

# Hover Events -------------------

# This function receives information about the hovered
# point and returns an HTML string to display
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(fill.hover := "red") %>%
  add_tooltip(all_values, "hover") 

# Scale 1 Without colour scaling ----

iris %>%
  ggvis(x = ~Sepal.Width,
        y = ~Sepal.Length,
        fill = ~Species,
        size := 100) %>%
  layer_points()

# Scale 2 with colour scaling ----

iris %>%
  ggvis(x = ~Sepal.Width,
        y = ~Sepal.Length,
        fill = ~Species,
        size := 100) %>%
  layer_points() %>%
  scale_nominal("fill",
                range = c("yellow", "orange", "red"))

# Scale 3
iris %>%
  ggvis(x = ~Sepal.Width,
        y = ~Sepal.Length,
        fill = ~Species,
        size := 100) %>%
  layer_points() %>%
  scale_nominal("fill",
                range = c("lightblue", "steelblue",
                          "navy"))

# Histogram ----
img.width <- 450
img.height <- 300

# Use mtcars data
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
# Compute mean mpg per cyl and am
mtcars.mean <- mtcars %>% group_by(cyl, am) %>% 
  summarise(mpg_mean=mean(mpg)) %>% 
  select(cyl, am, mpg_mean) %>% ungroup()
summ
# options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
# opts_chunk$set(fig.width=6, fig.height=4)
hist.ggvis <- mtcars %>% ggvis(x = ~mpg) %>% layer_histograms(width=1) %>% 
  set_options(width = img.width, height = img.height)
hist.ggvis

# Scatterplot ----
scatter.ggvis <- mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>% 
  layer_points() %>% set_options(width = img.width, height = img.height)
scatter.ggvis

# Line charts
line.ggvis <- mtcars.mean %>% ggvis(x = ~cyl, y = ~mpg_mean, stroke = ~am) %>% 
  layer_lines() %>% set_options(width = img.width, height = img.height)
line.ggvis

# Sliders ----
mtcars %>%
  ggvis(~wt, ~mpg) %>%
  layer_smooths(span = input_slider(0.5, 1, value = 1)) %>%
  layer_points(size := input_slider(100, 1000, value = 100))

# Slider + Select box
mtcars %>% ggvis(x = ~wt) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )

# 1 slider - 2 outputs (size & outline I think!)
mtcars %>%
  ggvis(~wt, ~mpg, size := input_slider(10, 1000)) %>%
  layer_points(fill := "red") %>%
  layer_points(stroke := "black", fill := NA)

#2 - same
slider <- input_slider(10, 1000)
mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_points(fill := "red", size := slider) %>%
  layer_points(stroke := "black", fill := NA, size := slider)

# Similar ----
slider <- input_slider(10, 1000)
mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_points(size := slider) %>% 
  layer_points(fill := "red", opacity := 0.5, size := slider)

# 2
mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_points(size := input_slider(100, 1000, label = "black")) %>%
  layer_points(fill := "red", size := input_slider(100, 1000, label = "red"))
library(datasets)

library(help = "datasets")
cw <- ChickWeight

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
cw %>% ggvis(by_group(Diet), x = ~Time, y = ~weight, fill = ~Diet) %>%
  layer_points(fill.hover := "red") %>%
  add_tooltip(all_values, "hover") %>% 
  layer_smooths(method = "lm", props(Diet = ~factor(Diet)))

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}
cw %>% ggvis(x = ~Time, y = ~weight, fill = ~Diet) %>%
  layer_points(fill.hover := "red") %>%
  add_tooltip(all_values, "hover") %>% 
  layer_smooths(fill = ~Diet, se=TRUE)

cw %>% 
  ggplot(aes(x=Time, 
             y=weight, 
             group_by(Diet), 
             fill=Diet)
  ) + 
  geom_point(aes(colour=Diet)) + 
  geom_smooth()


cw %>% ggvis(x=~factor(Time), y=~weight) %>% group_by(factor(Diet)) %>% layer_boxplots()

# For consistent colours
col <- data.frame(Diet=factor(c(1:4)), Colour=c("blue","orange","green","red"))

cw <- left_join(cw, col, by="Diet")

# Boxplot with hover values
cw %>% 
  ggvis(~Diet, ~weight, fill=~Colour ) %>% 
  layer_boxplots() %>% 
  add_tooltip(all_values, "hover") %>% 
  hide_legend("fill")

# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
cw %>% ggplot(aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  geom_line() +
  ggtitle("Growth Curve for Each Chick")

# Growth Curve for each diet
cw %>% ggplot(aes(x=Time, y=weight, colour=Diet)) +
  geom_point(alpha=.4) +
  geom_smooth(method = "loess", alpha=.2, size=1) +
  ggtitle("Growth Curve for each Diet")

# Final Weight Density Plot by Diet
cw %>% 
  filter(Time==21) %>% 
  ggplot(aes(x=weight, colour=Diet)) +
  geom_density() +
  ggtitle("Final Weight by Diet")

# Final Weight Histogram by Diet
cw %>% 
  filter(Time>=21) %>% 
  ggplot(aes(x=weight, fill=Diet)) +
  geom_histogram(colour="black", binwidth=50) +
  facet_grid(Diet ~ .) +
  ggtitle("Final Weight by Diet") +
  theme(legend.position="none")

# Weight Density Plot by Diet
cw %>% 
  filter(Time>=10) %>% 
  ggplot(aes(x=weight, colour=Diet)) +
  geom_density() +
  ggtitle("Final Weight by Diet")

cw %>% 
  ggplot(aes(x=Time)) + geom_bar(aes(colour=Diet), 
                                 position = "dodge",
                                 binwidth = 0)

ggplot(cw, aes(x=Time, fill=as.factor(Diet)))+
  geom_bar(position="dodge")

cw2 <- cw %>% 
  group_by(Time, Diet) %>% 
  summarise(Num=n(), Percent=n()/10)
cw2$Percent <- ifelse(cw2$Diet==1, cw2$Percent/2, cw2$Percent)

cw2 %>% 
  ggplot(aes(x=Time, y=Percent, fill=Diet)) +
  geom_bar(position="dodge", stat="identity")
# More
# http://ggvis.rstudio.com/
# http://github.com/rstudio/ggvis/tree/master/demo


