# Importing the required libraries
library(tidyverse)
library(tidytext)
library(syuzhet)

# Sentiment Analysis on book The Alchemist 

# Setting Working directory
setwd("C:\\Users\\ZAYD\\OneDrive\\Documents\\R studio")
data <- read.csv("reviews (1).csv") # Reading the dataset

# Making the dataframe with converting all text to lowercase
text.df <- tibble(text = str_to_lower(data$text))

# Analyzing the sentiments in the text
emotions <- get_nrc_sentiment(text.df$text)
emotions# Printing the values

emo_bar <- colSums(emotions) # Adding the values in the column 
emo_bar #Printing the added values
emo_sum <- data.frame(count = emo_bar, emotions = names(emo_bar))
emo_sum # Adding perticular sentiment value

# Creating barplot 
ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count)) + 
  geom_bar(stat = 'identity')

# Sentiment analysis using tidytext using bing lexicon
# Creating tokens, joining words and counting
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
bing_word_counts 
# Selecting top 10 words from sentiment
# First grouping them then ungrouping
bing_top_10_words <- bing_word_counts %>% 
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n))
bing_top_10_words
# Creating a bar plot for the final sentiment
bing_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = 'free_y') + 
  labs(y = "Contribution of sentiment on book", x = NULL) +
  coord_flip()


# Sentiment Analysis on Cooler
setwd("C:\\Users\\ZAYD\\OneDrive\\Documents\\R studio")
data <- read.csv("reviews (3).csv") # Reading the dataset
# Making the dataframe with converting all text to lowercase
text.df <- tibble(text = str_to_lower(data$text)) 
# Analyzing the sentiments in the text
emotions <- get_nrc_sentiment(text.df$text)
emotions
emo_bar <- colSums(emotions)
emo_bar # Adding all the sentiment values present in the column
emo_sum <- data.frame(count = emo_bar, emotions = names(emo_bar))
emo_sum # Adding perticular sentiment value
# Creating barplot 
ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count)) + 
  geom_bar(stat = 'identity')
# Sentiment analysis using tidytext using bing lexicon
# Creating tokens, joining words and counting
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
bing_word_counts 
# Selecting top 10 words from sentiment
# First grouping them then ungrouping
bing_top_10_words <- bing_word_counts %>% 
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n))
bing_top_10_words
# Creating a bar plot for the final sentiment
bing_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = 'free_y') + 
  labs(y = "Contribution of sentiment", x = NULL) +
  coord_flip()


# Sentiment Analysis on Boat Smart Watch with built in Alexa
setwd("C:\\Users\\ZAYD\\OneDrive\\Documents\\R studio")
data <- read.csv("reviews (4).csv") # Reading the dataset
# Making the dataframe with converting all text to lowercase
text.df <- tibble(text = str_to_lower(data$text)) 
# Analyzing the sentiments in the text
emotions <- get_nrc_sentiment(text.df$text)
emotions
emo_bar <- colSums(emotions)
emo_bar # Adding all the sentiment values present in the column
emo_sum <- data.frame(count = emo_bar, emotions = names(emo_bar))
emo_sum # Adding perticular sentiment value
# Creating barplot 
ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count)) + 
  geom_bar(stat = 'identity')
# Sentiment analysis using tidytext using bing lexicon
# Creating tokens, joining words and counting
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
bing_word_counts 
# Selecting top 10 words from sentiment
# First grouping them then ungrouping
bing_top_10_words <- bing_word_counts %>% 
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n))
bing_top_10_words
# Creating a bar plot for the final sentiment
bing_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = 'free_y') + 
  labs(y = "Contribution of sentiment", x = NULL) +
  coord_flip()


# Sentiment Analysis on Book IKigai
setwd("C:\\Users\\ZAYD\\OneDrive\\Documents\\R studio")
data <- read.csv("reviews.csv") # Reading the dataset
# Making the dataframe with converting all text to lowercase
text.df <- tibble(text = str_to_lower(data$text)) 
# Analyzing the sentiments in the text
emotions <- get_nrc_sentiment(text.df$text)
emotions
emo_bar <- colSums(emotions)
emo_bar # Adding all the sentiment values present in the column
emo_sum <- data.frame(count = emo_bar, emotions = names(emo_bar))
emo_sum # Adding perticular sentiment value
# Creating barplot 
ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count)) + 
  geom_bar(stat = 'identity')
# Sentiment analysis using tidytext using bing lexicon
# Creating tokens, joining words and counting
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
bing_word_counts 
# Selecting top 10 words from sentiment
# First grouping them then ungrouping
bing_top_10_words <- bing_word_counts %>% 
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n))
bing_top_10_words
# Creating a bar plot for the final sentiment
bing_top_10_words %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = 'free_y') + 
  labs(y = "Contribution of sentiment", x = NULL) +
  coord_flip()
