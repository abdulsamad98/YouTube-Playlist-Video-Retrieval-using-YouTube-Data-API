# Install and load necessary packages
install.packages("googleAuthR")
install.packages("httr")

library(googleAuthR)
library(httr)

# Set API key
api_key <- "AIzaSyDceREUIhTNMXv6NK8QBoA8a1GaQNCvOhs"

# Define function to retrieve all playlist items
retrieve_all_playlist_items <- function(playlist_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/playlistItems"
  all_playlist_items <- list()
  next_page_token <- ""
  
  while (TRUE) {
    params <- list(
      part = "snippet",
      playlistId = playlist_id,
      maxResults = 50,
      key = api_key,
      pageToken = next_page_token
    )
    
    response <- GET(url = base_url, query = params)
    data <- content(response, "parsed")
    all_playlist_items <- c(all_playlist_items, data$items)
    
    next_page_token <- data$nextPageToken
    if (is.null(next_page_token)) {
      break
    }
  }
  
  return(all_playlist_items)
}

# Define function to extract video links
extract_video_links <- function(playlist_items) {
  video_links <- lapply(playlist_items, function(item) {
    video_id <- item$snippet$resourceId$videoId
    return(paste0("https://www.youtube.com/watch?v=", video_id))
  })
  return(video_links)
}

# Call function to retrieve all playlist items
playlist_id <- "PLu0W_9lII9agwh1XjRt242xIpHhPT2llg"
all_playlist_items <- retrieve_all_playlist_items(playlist_id, api_key)

# Call function to extract video links
video_links <- unlist(extract_video_links(all_playlist_items))

# Print video links with serial number
for (i in 1:length(video_links)) {
  cat("Video", i, ":",'"', video_links[i],'",', "\n")
}

