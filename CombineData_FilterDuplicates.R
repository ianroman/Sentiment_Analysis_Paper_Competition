# List all .csv files in your working directory
csv.files <- list.files(pattern = "\\.csv$")

# Create an empty data frame for the combined threads dataset
reddit_threads_data <- data.frame()

# Loop through each spreadsheet 
for (file in csv.files) {
  file_path <- file.path("E:/ITE Competition WATT/Threads Data", file)
  data <- read.csv(file_path, header = TRUE)
  reddit_threads_data <- rbind(reddit_threads_data, data)
}

# Check the combined dataset
head(reddit_threads_data)

# Write to a new csv file for more usability
write.csv(reddit_threads_data, "reddit_threads_data.csv", row.names = FALSE)

# Now do the same for the comments data
# List all .csv files in your working directory
csv.files_comments <- list.files(pattern = "\\.csv$")

# Create an empty data frame for the combined threads
reddit_comments_data <- data.frame()

# Loop through each spreadsheet
for (file2 in csv.files_comments) {
  file_path2 <- file.path(getwd(), file2)
  data2 <- read.csv(file_path2, header = TRUE)
  reddit_comments_data <- rbind(reddit_comments_data, data2)
}

# Check the combined dataset
head(reddit_comments_data)

# Write to a new csv file for more usability
write.csv(reddit_comments_data, "reddit_comments_data.csv", row.names = FALSE)

# Finalize Threads dataset
numPost <- reddit_threads_data %>%
  filter(author != 'AutoModerator') %>%
  nrow()

reddit_threads_data_final <- reddit_threads_data %>%
  filter(author != 'AutoModerator') %>%
  arrange(date) %>%
  mutate(content = paste(title, text, sep = " ")) %>%
  cbind(post_id = 1:numPost) %>%
  select(post_id, date, author, subreddit, content)

# Save the final reddit dataset
write.csv(reddit_threads_data_final, "reddit_threads_data_final.csv", row.names = FALSE)

# Finalize the reddit comments data
reddit_comments_data <- read.csv('reddit_comments_data.csv')

numComment <- reddit_comments_data %>%
  nrow()

reddit_comments_data_final <- reddit_comments_data %>%
  arrange(date) %>%
  mutate(text_id = 1:numComment) %>%
  select(text_id, date, author, comment)

# Write the final csv file for the comments data
write.csv(reddit_comments_data_final, "reddit_comments_data_final.csv", row.names = FALSE)
