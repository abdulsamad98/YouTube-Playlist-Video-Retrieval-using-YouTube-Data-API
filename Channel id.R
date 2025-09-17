# Install and load necessary packages
install.packages("googleAuthR")
install.packages("httr")

library(googleAuthR)
library(httr)

# Set API key
api_key <- "AIzaSyDceREUIhTNMXv6NK8QBoA8a1GaQNCvOhs"  # Replace "YOUR_API_KEY_HERE" with your actual API key

# Define function to retrieve videos from a channel
retrieve_channel_videos <- function(channel_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/search"
  all_videos <- list()
  next_page_token <- ""
  
  while (TRUE) {
    params <- list(
      part = "snippet",
      channelId = channel_id,
      maxResults = 50,
      key = api_key,
      pageToken = next_page_token
    )
    
    response <- GET(url = base_url, query = params)
    data <- content(response, "parsed")
    all_videos <- c(all_videos, data$items)
    
    next_page_token <- data$nextPageToken
    if (is.null(next_page_token)) {
      break
    }
  }
  
  return(all_videos)
}

# Define function to search for topics in video titles and descriptions
search_topics_in_videos <- function(topics, videos) {
  matching_videos <- list()
  for (video in videos) {
    title <- video$snippet$title
    description <- video$snippet$description
    for (topic in topics) {
      topic_regex <- paste0("\\b", topic, "\\b", collapse = "|")
      if (grepl(topic_regex, tolower(title), ignore.case = TRUE) ||
          grepl(topic_regex, tolower(description), ignore.case = TRUE)) {
        video_id <- video$id$videoId
        video_link <- paste0("https://www.youtube.com/watch?v=", video_id)
        matching_videos[[topic]] <- c(matching_videos[[topic]], video_link)
        break  # Exit inner loop once a match is found
      }
    }
  }
  return(matching_videos)
}

# Define topics
topics_input <- c(
  "Basic Syntax in Python",
  "Variables and Data Types in Python",
  "Type Casting in Python",
  "Conditional Statements in Python",
  "Loops in Python",
  "List in Python, Tuples in Python, cines Strings Sets and dictionary in Python",
  "Functions in Python, lambdla in Python, HOF in Python",
  "Modules in Python",
  "Exception Handling in Python",
  "OoPs Concepts in Python",
  "File handling in Python",
  "List/Dictionary comprehensions in Python",
  "Generators in Python",
  "Iterators in Python",
  "Regex in Python",
  "List in list, Dictionary in Dictionary in Python",
  "Pytest in Python",
  "Doctest in Python",
  "Unittest in Python"
)

# Call function to retrieve videos from the channel
channel_id <- "UCeVMnSShP_Iviwkknt83cww"  # Replace "YOUR_CHANNEL_ID_HERE" with your actual channel ID
channel_videos <- retrieve_channel_videos(channel_id, api_key)

# Call function to search for topics in video titles and descriptions
matching_videos <- search_topics_in_videos(topics_input, channel_videos)

# Print matching video links
for (i in 1:length(topics_input)) {
  topic <- topics_input[i]
  videos <- matching_videos[[topic]]
  if (length(videos) > 0) {
    cat("Videos discussing topic '", topic, "':\n")
    for (j in 1:length(videos)) {
      cat("Video", j, ":",'"', videos[j],'",', "\n")
    }
  } else {
    cat("No videos found discussing topic '", topic, "'.\n")
  }
}

