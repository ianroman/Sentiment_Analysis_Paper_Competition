library(RedditExtractoR)
library(dplyr)
library(ggplot2)

# Store the post information and post url to respective variables below
CanadaPolitics_info <- find_thread_urls(subreddit = "CanadaPolitics",
                                        sort_by = "new",
                                        period = "all",
                                        keywords = "electric vehicle")
Canada_info <- find_thread_urls(subreddit = "Canada",
                                sort_by = "new",
                                period = "all",
                                keywords = "electric vehicle")
electric.vehicles_info <- find_thread_urls(subreddit = "electricvehicles",
                                           sort_by = "new",
                                           period = "all",
                                           keywords = "canada")
EVCanada_info <- find_thread_urls(subreddit = "EVCanada",
                                  sort_by = "new",
                                  period = "all")

# Extract threads and comments from posts for each information variables
CanadaPolitics_contents <- get_thread_content(CanadaPolitics_info$url)
Canada_contents <- get_thread_content(Canada_info$url)
electric.vehicles_contents <- get_thread_content(electric.vehicles_info$url)
EVCanada_contents <- get_thread_content(EVCanada_info$url)

# Convert to CSV for a more manageable dataset
# Threads
write.csv(CanadaPolitics_contents$threads, "Canada_Politics_threads.csv", row.names = F)
write.csv(Canada_contents$threads, "Canada_threads.csv", row.names = F)
write.csv(electric.vehicles_contents$threads, "electric_vehicles_threads.csv", row.names = F)
write.csv(EVCanada_contents$threads, "EVCanada_threads.csv", row.names = F)

# Comments
write.csv(CanadaPolitics_contents$comments, "Canada_Politics_comments.csv", row.names = F)
write.csv(Canada_contents$comments, "Canada_comments.csv", row.names = F)
write.csv(electric.vehicles_contents$comments, "electric_vehicles_comments.csv", row.names = F)
write.csv(EVCanada_contents$comments, "EVCanada_comments.csv", row.names = F)
