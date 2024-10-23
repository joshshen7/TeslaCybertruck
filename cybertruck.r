library(tuber)

client_id = ""
client_secret = ""
yt_oauth(client_id, client_secret, token = "")

# Load required libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

# Fetch channel stats
channelstats <- get_channel_stats("UC5WjFrtBdufl6CZojX3D8dQ")

# Get playlists from the channel
playlists_ids <- get_playlists(filter = c(channel_id = "UC5WjFrtBdufl6CZojX3D8dQ"))

playlistid <- playlists_ids$items[[1]]$id
playlist_videos<-get_playlist_items(filter= c(playlist_id=playlistid))

cybertruck_ids <- as.vector(playlist_videos$contentDetails.videoId)

# Function to scrape stats for all videos
get_all_stats <- function(id) {get_stats(id)}

# Get stats and convert results to data frame 
video_stats <- lapply(cybertruck_ids, get_all_stats)
video_stats_df <- do.call(rbind, lapply(video_stats, data.frame))
View(video_stats_df)

# Convert list to data frame
video_stats_df <- data.frame(video_stats_df)

# Convert text in the count columns to number
video_stats_df <- video_stats_df %>% mutate_at(c('viewCount','likeCount','favoriteCount','commentCount'), as.numeric)
View(video_stats_df)

# Save video stats to a csv file in working directory
write.csv(video_stats_df, file='TeslaCybertruckYoutube.csv')

# Stat plots displaying various counts (like, comments) against view count
p1 = ggplot(data = video_stats_df[, -1]) + 
  geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = video_stats_df[, -1]) + 
  geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2)

# Create wordcloud
library(tm)
library(wordcloud)
library(SnowballC)

get_video_comments <- function(id) {
  cat("Fetching comments for video ID:", id, "\n")
  comments <- get_all_comments(c(video_id = id), max_results = 200)
  if (is.null(comments) || nrow(comments) == 0) {
    return(NULL)
  }
  return(comments)
}
playlist_comments = lapply(as.character(cybertruck_ids), get_video_comments)

comments_text = lapply(playlist_comments, function(x){as.character(x$textOriginal)})

# Merge data lists of all comments into one variable
text <- Reduce(c, comments_text)

# Create text corpus, DTM, terms, and TDM to start creating word cloud
playlist_corp <- Corpus(VectorSource(text))
playlist_DTM <- DocumentTermMatrix(playlist_corp, control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))
playlist_terms <- colSums(as.matrix(playlist_DTM))
playlist_terms_metrix <- as.matrix(playlist_terms)

wordcloud(words = names(playlist_terms),
          freq = playlist_terms,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.35,
          scale = c(4, 0.5),
          colors = brewer.pal(8, "Dark2"))