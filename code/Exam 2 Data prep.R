library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(ggplot2)
library(readr)
library(lubridate)


plastic_hearts_lyrics <- data.frame(text = character(),
                           song = character(),
                           album = character(),
                           stringsAsFactors = F)
ph_songs <- c("Angels-Like-You.txt", "Bad-Karma.txt", "Gimme-What-I-Want.txt", "Golden-G-String.txt",
              "Hate-Me.txt", "High.txt", "Midnight-Sky.txt", "Never-Be-Me.txt", "Night-Crawling.txt", 
              "Plastic-Hearts.txt", "Prisoner.txt", "WTF-Do-I-Know.txt")

for (song in ph_songs) {
  lines <- scan(paste0("lyrics/Plastic_Hearts/", song), what=character(), sep="\n")
  lines <- lines[!grepl("^#", lines)]
  song_df <- data.frame(
    text=lines,
    song=tolower(gsub(".txt", "", song)),
    album="Plastic Hearts",
    stringsAsFactors = F
  )
  song_df$song <- str_replace_all(song_df$song, "-", " ")
  song_df$song <- str_replace_all(song_df$song, "_", " ")
  song_df$song <- str_replace_all(song_df$song, "\\(.*?\\)", "")
  song_df$song <- str_replace_all(song_df$song, "–", "-")
  song_df$song <- str_replace_all(song_df$song, "[[:punct:]]", "")
  song_df$song <- str_replace_all(song_df$song, "\\s+", " ")
  song_df$song <- str_trim(song_df$song)
  plastic_hearts_lyrics <- rbind(plastic_hearts_lyrics, song_df)
}



younger_now_lyrics <- data.frame(text = character(),
                                    song = character(),
                                    album = character(),
                                    stringsAsFactors = F)
yn_songs <- c("Bad-Mood.txt", "I-Would-Die-For-You.txt", "Inspired.txt", "Love-Someone.txt", "Malibu.txt", 
              "Miss-You-So-Much.txt", "Rainbowland.txt", "She's-Not-Him.txt", "Thinkin.txt", "Week-Without-You.txt", "Younger-Now.txt")

for (song in yn_songs) {
  lines <- scan(paste0("lyrics/Younger_Now/", song), what=character(), sep="\n")
  lines <- lines[!grepl("^#", lines)]
  song_df <- data.frame(
    text=lines,
    song=tolower(gsub(".txt", "", song)),
    album="Younger Now",
    stringsAsFactors = F
  )
  song_df$song <- str_replace_all(song_df$song, "-", " ")
  song_df$song <- str_replace_all(song_df$song, "_", " ")
  song_df$song <- str_replace_all(song_df$song, "\\(.*?\\)", "")
  song_df$song <- str_replace_all(song_df$song, "–", "-")
  song_df$song <- str_replace_all(song_df$song, "[[:punct:]]", "")
  song_df$song <- str_replace_all(song_df$song, "\\s+", " ")
  song_df$song <- str_trim(song_df$song)
  younger_now_lyrics <- rbind(younger_now_lyrics, song_df)
}


ttool_lyrics <- data.frame(text = character(),
                                    song = character(),
                                    album = character(),
                                    stringsAsFactors = F)
ttool_songs <- c("Before-the-Storm.txt", "Kicking-and-Screaming.txt", "Obsessed.txt", "Party-in-the-USA.txt",
                 "Talk-is-Cheap.txt", "The-Time-of-Our-Lives.txt", "When-I-Look-at-You.txt")

for (song in ttool_songs) {
  lines <- scan(paste0("lyrics/The_Time_Of_Our_Lives/", song), what=character(), sep="\n")
  lines <- lines[!grepl("^#", lines)]
  song_df <- data.frame(
    text=lines,
    song=tolower(gsub(".txt", "", song)),
    album="The Time of Our Lives",
    stringsAsFactors = F
  )
  song_df$song <- str_replace_all(song_df$song, "-", " ")
  song_df$song <- str_replace_all(song_df$song, "_", " ")
  song_df$song <- str_replace_all(song_df$song, "\\(.*?\\)", "")
  song_df$song <- str_replace_all(song_df$song, "–", "-")
  song_df$song <- str_replace_all(song_df$song, "[[:punct:]]", "")
  song_df$song <- str_replace_all(song_df$song, "\\s+", " ")
  song_df$song <- str_trim(song_df$song)
  ttool_lyrics <- rbind(ttool_lyrics, song_df)
}

miley_lyrics <- rbind(plastic_hearts_lyrics, younger_now_lyrics, ttool_lyrics)

miley_lyrics <- miley_lyrics %>%
  mutate(album = case_when(
    album == "Plastic Hearts" ~ "Plastic Hearts",
    album == "Younger Now" ~ "Younger Now",
    album == "The Time of Our Lives" ~ "The Time Of Our Lives",  # <- fix here
    TRUE ~ album
  ))

tidy_miley <-  miley_lyrics %>%
  unnest_tokens(word, text)
data("stop_words")

tidy_miley_clean <- tidy_miley %>%
  anti_join(stop_words)

ph_data <- tidy_miley_clean %>%
  filter(album == "Plastic Hearts") %>%
  count(word, sort=T) %>%
  slice_max(n, n=100)

ph_cloud <- ggplot(ph_data, aes(label=word, size=n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size=12) +
  theme_minimal() +
  labs(title="Plastic Hearts WordCloud")
ph_cloud

yn_data <- tidy_miley_clean %>%
  filter(album == "Younger Now") %>%
  count(word, sort=T) %>%
  slice_max(n, n=100)

yn_cloud <- ggplot(yn_data, aes(label=word, size=n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size=12) +
  theme_minimal() +
  labs(title="Younger Now WordCloud")
yn_cloud

ttool_data <- tidy_miley_clean %>%
  filter(album == "The Time Of Our Lives") %>%
  count(word, sort=T) %>%
  slice_max(n, n=100)

ttool_cloud <- ggplot(ttool_data, aes(label=word, size=n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size=12) +
  theme_minimal() +
  labs(title="The Time of Our Lives WordCloud")
ttool_cloud

all_songs <- miley_lyrics %>%
  distinct(album, song)

sentiment_counts <- tidy_miley_clean %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(album, song, sentiment)

sentiment_plot <- ggplot(sentiment_counts, aes(x=album, y=n, fill=sentiment)) +
  geom_col(position="dodge") +
  labs(title="Positive vs Negative Words by Album", y="Word Count", x=NULL) +
  theme_minimal()

all_songs <- all_songs %>%
  mutate(album = case_when(
    str_detect(album, "time of our lives") ~ "The Time Of Our Lives",
    str_detect(album, "younger now") ~ "Younger Now",
    str_detect(album, "plastic hearts") ~ "Plastic Hearts",
    TRUE ~ album))

sentiment_counts <- sentiment_counts %>%
  mutate(album = case_when(
    str_detect(album, "time of our lives") ~ "The Time Of Our Lives",
    str_detect(album, "younger now") ~ "Younger Now",
    str_detect(album, "plastic hearts") ~ "Plastic Hearts",
    TRUE ~ album))

song_sentiment <- all_songs %>%
  left_join(sentiment_counts, by = c("album", "song")) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)


metadata <- read_csv("C:/Users/pzog0/Documents/mileycyrus_metadata2.csv") %>%
  mutate(album = case_when(
    str_detect(album, "time of our lives") ~ "The Time Of Our Lives",
    str_detect(album, "younger now") ~ "Younger Now",
    str_detect(album, "plastic hearts") ~ "Plastic Hearts",
    TRUE ~ album),
    song = gsub(".txt", "", track_name),
    song = tolower(song),
    song = str_replace_all(song, "\\(.*?\\)", ""),
    song = str_replace_all(song, "–", "-"),
    song = str_replace_all(song, "[[:punct:]]", ""),
    song = str_replace_all(song, "\\s+", " "),
    song = str_trim(song)
  )

combined_data <- left_join(metadata, song_sentiment, by = c("album", "song"))

sentiment_popularity_plot <- ggplot(combined_data, aes(x = net_sentiment, y = popularity, color = album)) +
  geom_point(size = 3) +
  labs(title = "Song Sentiment vs Popularity", x = "Net Sentiment", y = "Spotify Popularity") +
  theme_minimal()

sentiment_duration_plot <- ggplot(combined_data, aes(x = duration_min, y = net_sentiment, color = album)) +
  geom_point(size = 3) +
  labs(title = "Net Sentiment vs Duration", x = "Duration (min)", y = "Net Sentiment") +
  theme_minimal()

combined_data <- combined_data %>%
  mutate(release_date = ymd(release_date))

popularity_over_time_plot <- ggplot(combined_data, aes(x = release_date, y = popularity, color = album)) +
  geom_line(aes(group = album)) +
  geom_point() +
  labs(title = "Popularity Over Time", x = "Release Date", y = "Spotify Popularity") +
  theme_minimal()

library(patchwork)
top_row <- ph_cloud + yn_cloud + ttool_cloud
bottom_row <- sentiment_popularity_plot + sentiment_duration_plot + popularity_over_time_plot
full_dashboard <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Miley Cyrus Sentiment Dashboard for 3 of her Albums")
full_dashboard

ggsave("dashboard/miley_dashboard.pdf", full_dashboard, width = 16, height = 10)
