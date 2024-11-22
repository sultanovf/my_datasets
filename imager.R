
# Edit Pictures in R ===========================================================
# https://mfviz.com/r-image-art/
install.packages("imager")    # image loading and processing

library(tidyverse)
library(imager)

img <- load.image(file = "pics/habibme.png")
plot(img)

# Represent the image as a data frame
img_df <- as.data.frame(img)

# Show a table of the first 10 rows of the data frame
img_df %>% 
  arrange(x, y, cc) %>% # sort by columns for viewing
  filter(row_number() < 10) %>% # Select top 10 columns
  kable("html") %>%  # Display table in R Markdown
  kable_styling(full_width = F) # Don't take up full width

# Add more expressive labels to the colors
img_df <- img_df %>% 
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green", 
    cc == 3 ~ "Blue"
  ))

# Reshape the data frame so that each row is a point
img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )

# Plot points at each sampled location
ggplot(img_wide) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() # use the actual value in the `color` column


ggplot(img_wide) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  #theme_void() # Remove axes, background
  ggtitle("папа \U1F44A \U1F44A сыночек") +
  theme(plot.title = element_text(hjust = 0.5))


# Take a sample of rows from the data frame
sample_size <- 100000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() + # Remove axes, background
  ggtitle("папа \U1F9E1 \U1F91D \U1F9E1 сыночек") +
  theme(plot.title = element_text(hjust = 0.5))
  

# Create random weights for point size
img_sample$size <- runif(sample_size)

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  # theme_update(plot.title = element_text(hjust = 0.5)) +
  # ggtitle("папа \U1F9E1 дочка") 
  
  theme_void() + # Remove axes, background
  ggtitle("папа \U1F9E1 дочка") +
  theme(plot.title = element_text(hjust = 0.5))



library(magick)
pic <- image_read("pics/habibme.png")
image_background(pic, "hotpink", flatten = TRUE)
rotation <- image_rotate(pic, 30)
image_write(rotation, path = "C:/Users/sultanov/Documents/temp/habibchek.png", format = "png")





