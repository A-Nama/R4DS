library(ggplot2)

data(mtcars)
mtcars$carb_factor <- as.factor(mtcars$carb)

ggplot(mtcars, aes(x = wt, y = disp, color = carb_factor, size = carb)) +
  geom_point(alpha = 0.7) +                     
  geom_smooth(aes(x = wt, y = disp), method = "loess", se = TRUE, color = "black", inherit.aes = FALSE) +
  scale_color_brewer(palette = "Set1") +      
  scale_size_continuous(range = c(3, 8)) +                
  labs(
    title = "Displacement vs Weight in mtcars Dataset",
    x = "Weight (1000 lbs)",
    y = "Displacement (cu.in.)",
    color = "Number of Carburetors",
    size = "Number of Carburetors"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
