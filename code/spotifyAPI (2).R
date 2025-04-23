install.packages("spotifyr")
library(spotifyr)

Sys.setenv(client_id = "fd1753b278a0452580b8ed78f4ce241d")
Sys.setenv(client_secret = "bb81952a9ca04d0d9ce04a57b010c400")

access_token <- get_spotify_access_token()

miley_cyrus <- get_artist("5YGY8feqx7naU7z4HrwZM6")
artist_id <- miley_cyrus$id

albums <- get_artist_albums(artist_id, include_groups="album") %>%
  filter(name %in% c("The Time Of Our Lives", "Younger Now", "Plastic Hearts")) %>%
  select(album_name = name, album_id = id)

get_albums_metadata <- function(album_id, album_name) {
  tracks <- get_album_tracks(album_id, limit=12)
  track_ids <- tracks$id
  track_data <- lapply(track_ids, function(track_id) {
    track <- get_track(track_id)
    data.frame(
      album=album_name,
      track_name=track$name,
      popularity=track$popularity,
      release_date=track$album$release_date,
      duration_min = round(track$duration_ms/60000, 2)
    )
  })
  bind_rows(track_data)
}

metadata_list <- lapply(1:nrow(albums), function(i) {
  get_albums_metadata(albums$album_id[i], albums$album_name[i])
})

miley_metadata <- bind_rows(metadata_list)

write_csv(miley_metadata, "postmalone_metadata.csv")