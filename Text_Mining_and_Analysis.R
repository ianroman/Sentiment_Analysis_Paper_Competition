# Load necessary packages for analysis
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(ggwordcloud)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)
library(topicmodels)
library(tm)

# Import data
threads <- read.csv("reddit_threads_data_final.csv")
comments <- read.csv("reddit_comments_data_final.csv")

# Some processing
threads$date <- as.Date(threads$date)
threads$year <- year(threads$date)
comments$date <- as.Date(comments$date)
comments$year <- year(comments$date)

# Exploratory Data Analysis (EDA)
## Aggregate number of threads and comments annually
threads_per_year <- threads %>%
  group_by(year) %>%
  count() %>%
  rename(count_threads = n)

comments_per_year <- comments %>%
  group_by(year) %>%
  count() %>%
  rename(count_comments = n)

threads_per_sub <- threads %>%
  group_by(subreddit) %>%
  count()

## Visualize the aggregate operations above and number of threads per subreddit analyzed
## Line graphs for yearly total threads and comments
ggplot(threads_per_year, aes(year, count_threads)) +
  geom_line(color = "#097969", linewidth = 1.5) +
  geom_point(size=3) + 
  labs(x = "Year", y = "Number of Threads (Posts)") +
  scale_x_continuous(breaks = seq(2009,2024,1), labels = seq(2009,2024,1)) +
  theme_bw()

ggplot(comments_per_year, aes(year, count_comments)) +
  geom_line(color = "#89cff0", linewidth = 1.5) +
  geom_point(size=3) + 
  labs(x = "Year", y = "Number of Comments") +
  scale_x_continuous(breaks = seq(2009,2024,1), labels = seq(2009,2024,1)) +
  theme_bw()

## Column graph showing the relative amount of threads per subreddit
ggplot(threads_per_sub, aes(factor(subreddit), n)) +
  geom_col(aes(fill=subreddit)) +
  labs(x = "Subreddit", y = "Thread (Post) Count") +
  theme_bw() +
  theme(legend.position = "none")
  
# Text Mining Techniques
## Tokenization
### First only get the ids and text content for each thread and column to form separate dfs
threads_text <- threads %>%
  select(post_id, content)

comments_text <- comments %>%
  select(text_id, comment)

### Tokenize both dfs and check both
tokenized_threads <- unnest_tokens(threads_text, input = "content", output = "word")
head(tokenized_threads)

tokenized_comments <- unnest_tokens(comments_text, input = "comment", output = "word")
head(tokenized_comments)

### Remove the stop words for each dataframe and plot the frequency bar plot for the most common words
tokenized_threads %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  rename(count = n) %>%
  filter(count > 50) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(count, word)) +
  geom_col() +
  labs(x = "Word Frequency", y = "Words") +
  theme_bw()

tokenized_comments %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  rename(count = n) %>%
  filter(count > 2000) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(count, word)) +
  geom_col() +
  labs(x = "Word Frequency", y = "Words") + 
  theme_bw()

## Sentiment Analysis
### Use the afinn and bing sentiment lexicon library to determine word polarity for each word
### Start with threads
tokenized_threads %>%
  group_by(post_id) %>%
  inner_join(get_sentiments("afinn")) %>%
  summarize(mean_sentiment = mean(value)) %>%
  ggplot(aes(post_id, mean_sentiment)) +
  geom_col(color = "blue") + 
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  labs(x = "Thread/Post ID", y = "Average Sentiment") +
  theme_bw()

tokenized_comments %>%
  group_by(text_id) %>%
  inner_join(get_sentiments("afinn")) %>%
  summarize(mean_sentiment = mean(value)) %>%
  ggplot(aes(text_id, mean_sentiment)) +
  geom_col(color = "red", width = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +
  labs(x = "Comment ID", y = "Average Sentiment") +
  theme_bw()

# Sentiment dfs
threads_sent_df <- tokenized_threads %>%
  group_by(post_id) %>%
  inner_join(get_sentiments("afinn")) %>%
  summarize(mean_sentiment = mean(value)) %>%
  mutate(
    polarity = case_when(
      mean_sentiment > 0 ~ "positive",
      mean_sentiment < 0 ~ "negative",
      mean_sentiment == 0 ~ "neutral"
    )
  ) %>%
  ungroup() %>%
  group_by(polarity) %>%
  count()

## Plot the polarity summary for threads above
ggplot(threads_sent_df, aes(factor(polarity), n)) +
  geom_col(aes(fill=polarity)) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(x = "Polarity", y = "Number of Threads/Posts") +
  theme_bw() +
  theme(legend.position = "none")
  
comments_sent_df <- tokenized_comments %>%
  group_by(text_id) %>%
  inner_join(get_sentiments("afinn")) %>%
  summarize(mean_sentiment = mean(value)) %>%
  mutate(
    polarity = case_when(
      mean_sentiment > 0 ~ "positive",
      mean_sentiment < 0 ~ "negative",
      mean_sentiment == 0 ~ "neutral"
    )
  ) %>%
  ungroup() %>%
  group_by(polarity) %>%
  count()

## Plot the polarity summary for threads above
ggplot(comments_sent_df, aes(factor(polarity), n)) +
  geom_col(aes(fill=polarity)) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(x = "Polarity", y = "Number of Comments") +
  theme_bw() +
  theme(legend.position = "none")

# Topic Modeling -- LDA
### Convert to tf_idf dataframe
threads_tf_idf <- tokenized_threads %>%
  count(word, post_id, sort = TRUE) %>%
  rename(count = n) %>%
  bind_tf_idf(word, post_id, count)
head(threads_tf_idf)

comments_tf_idf <- tokenized_comments %>%
  count(word, text_id, sort = TRUE) %>%
  rename(count = n) %>%
  bind_tf_idf(word, text_id, count)
head(comments_tf_idf)

### Modeling Proper
num_topics = 3
top_n_to_get = 15

threads_lda <- threads_tf_idf %>%
  anti_join(stop_words) %>%
  cast_dtm(document = post_id, term = word, value = count) %>%
  LDA(k = num_topics)
threads_topics <- tidy(threads_lda)
head(threads_topics)

### Top Terms 
threads_topics_top_terms <- threads_topics %>%
  group_by(topic) %>%
  top_n(top_n_to_get, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
head(threads_topics_top_terms)

### Visualize topics using the top terms
threads_topics_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + 
  labs(x = "Beta Value", y = "Words") +
  theme_bw()

### Repeat for Comments same as above
comments_lda <- comments_tf_idf %>%
  anti_join(stop_words) %>%
  cast_dtm(document = text_id, term = word, value = count) %>%
  LDA(k = num_topics)
comments_topics <- tidy(comments_lda)
head(comments_topics)

#### Top terms
comments_topics_top_terms <- comments_topics %>%
  group_by(topic) %>%
  top_n(top_n_to_get, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
head(comments_topics_top_terms)

#### Visualize topics
comments_topics_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + 
  labs(x = "Beta Value", y = "Words") +
  theme_bw()

######### Miscellaneous 
### Relationship between words -- ngrams

# Tokenized in trigrams
## Threads
threads_trigram <- threads_text %>%
  unnest_tokens(trigram, content, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
head(threads_trigram)

## Comments
comments_trigram <- comments_text %>%
  unnest_tokens(trigram, comment, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
head(comments_trigram)

# Count trigrams
## Threads
trigram_thread_counts <- threads_trigram %>%
  count(word1, word2, word3, sort = TRUE)
head(trigram_thread_counts)

## Comments
trigram_comment_counts <- comments_trigram %>%
  count(word1, word2, word3, sort = TRUE) %>%
  na.omit()
head(trigram_comment_counts)

# Word Associations Graph
## Threads
threads_tri_graph <- trigram_thread_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()
ggraph(threads_tri_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

## Comments
comments_tri_graph <- trigram_comment_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
ggraph(comments_tri_graph, layout = "fr")+
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)
