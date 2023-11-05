library(sentimentr)
library(tidyverse)
install.packages('tidyverse')
data_senti <- read.csv("Example data.csv")
data_senti_r <- data_senti$Review.Text
data_senti %>%
  get_sentences() %>%
  sentiment() -> data_senti_V
data_senti_V %>%
  ggplot() + geom_density(aes(sentiment))
data_senti_V %>%
  mutate(polarity_level = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  count(Age,polarity_level) %>%
  ggplot() + geom_col(aes(y = n, x = Age, fill = polarity_level)) +
  theme_minimal()
