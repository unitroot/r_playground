require(geniusr)
require(tidyverse)
require(tidytext)
require(dplyr)

read_lines("secret.txt")
geniusr::genius_token(TRUE)
geniusr::search_artist("Linkin Park")

dfSongs <- get_artist_songs_df(1581) 

dfSongs$album_id <- 0
dfSongs$album_name <- "NA"
for (numI in c(1:NROW(dfSongs))) {
  dfTemp <- geniusr::get_song_df(dfSongs$song_id[numI])
  dfSongs[numI, c("album_id", "album_name")] <- dfTemp[,c("album_id", "album_name")]
}

dfSongs <- dfSongs[dfSongs$album_name %in% c("Hybrid Theory", "Meteora", 
                                         "Minutes to Midnight", "A Thousand Suns", 
                                         "LIVING THINGS", "The Hunting Party", 
                                         "One More Light"),]
sAlbum <- unique(dfSongs$album_id)

dfAlbum <- data.frame()
for (numI in c(1:length(sAlbum))) {
  dfTemp <- geniusr::get_album_tracklist_id(sAlbum[numI])
  dfAlbum <- rbind(dfTemp, dfAlbum)
}

dfSongs <- dplyr::left_join(dfAlbum, dfSongs[,c("song_lyrics_url", "song_id")], by = "song_lyrics_url") %>% 
  dplyr::select(song_id, song_number, song_title, album_name, song_lyrics_url) %>% 
  dplyr::mutate(album_name = factor(album_name, levels = c("Hybrid Theory", "Meteora", 
                                                           "Minutes to Midnight", "A Thousand Suns", 
                                                           "LIVING THINGS", "The Hunting Party", 
                                                           "One More Light")), 
                song_number = factor(song_number)) %>% 
  dplyr::arrange(album_name, song_number)


ids <- c(as.character(dfSongs$song_id))
dfLyrics <- data.frame()
while (length(ids) > 0) {
  for (id in ids) {
    tryCatch({
      dfLyrics <- rbind(geniusr::get_lyrics_id(id), dfLyrics)
      successful <- unique(dfLyrics$song_id)
      ids <- ids[!ids %in% successful]
      print(paste("done - ", id))
      print(paste("New length is ", length(ids)))
    }, error = function(e){})
  }
}

dfTokens <- dfLyrics %>% 
  dplyr::mutate(song_id = as.numeric(song_id)) %>% 
  dplyr::left_join(dfSongs[,c("song_id", "album_name")]) %>% 
  tidytext::unnest_tokens(word, line) %>%
  dplyr::anti_join(stop_words) %>% 
  dplyr::filter(word != "na") %>% 
  dplyr::filter(word != "eh") %>% 
  dplyr::filter(word != "ooh") %>%
  #count(word, sort = TRUE) %>%
  dplyr::group_by(album_name, word) %>%
  dplyr::mutate(n = row_number()) %>%
  ungroup()

dfTop <- dfTokens[,c("album_name", "word", "n")] %>% 
  dplyr::group_by(album_name, word) %>%
  dplyr::summarise(n = max(n))%>%
  ungroup() %>% 
  group_by(word) %>%
  mutate(total = sum(n)) %>%
  filter(total >= 30) %>%
  ungroup()

unique(dfTop$word)


# colours for each album
albumCol <- c("#394887",      # PTL
              "#9e5a47",      # OWT
              "#f9c784",      # Hot cakes
              "#cf57d4",      # Last
              "#e8b0a5",      # PINE
              "#d18943",      # Easter
              "#4C1A57")      # singles
names(albumCol) <- c("Hybrid Theory", "Meteora", 
                     "Minutes to Midnight", "A Thousand Suns", 
                     "LIVING THINGS", "The Hunting Party", 
                     "One More Light")

wordsPlot <- ggplot(dfTop) +
  
  geom_bar(aes(x = reorder(word, total), 
               y = n,
               fill = as.factor(album_name)),
           colour = "black",
           stat = "identity") +
  
  coord_flip() +
  
  labs(title = "'Linkin Park' most used words",
       subtitle = "The words that appear more than 30 times in 'Linkin Park' catalogue",
       caption = "Source: genius.com | by @un1t_r00t",
       y = "Number of appearances",
       x = "Word",
       fill = "Album")+
  
  scale_fill_manual(values = albumCol) +
  
  theme(title = element_text(face = "italic", size = 12), 
        
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title = element_text(face = "italic",size = 11, colour = "black"),
        axis.ticks.length = unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = unit(12,"pt"),
        legend.box.spacing = unit(5,"pt"),
        legend.text = element_text(size = 12),
        
        axis.text.y = element_text(size = 12))
wordsPlot
ggsave(filename = "DarknessWords.png", plot = wordsPlot, width = 30, height = 24, units = "cm",
       type = "cairo")

# Create Sentiment df
darknessSentiments <- tidyLyrics %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, song_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
# Factor as we did above
darknessSentiments$album <- factor(darknessSentiments$album, 
                                   levels = c("Hybrid Theory", "Meteora", 
                                              "Minutes to Midnight", "A Thousand Suns", 
                                              "LIVING THINGS", "The Hunting Party", 
                                              "One More Light"))
# sent plot
sentPlot <- ggplot(darknessSentiments,
                   aes(reorder(song_name, 
                               sentiment), 
                       sentiment, 
                       fill = album)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~album, 
             ncol = 3, 
             scales = "free")+
  
  scale_fill_manual(values = albumCol)+
  
  labs(title = "Linkin Park' songs ranked by sentiment",
       caption = "Source: genius.com | by @un1t_r00t",
       y = "Sentiment score",
       fill = "Album")+
  
  theme(title = element_text(face = "italic", size = 12), 
        
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title.x = element_text(face = "italic",size = 11, colour = "black"),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = unit(12,"pt"),
        legend.box.spacing = unit(5,"pt")) +
  
  coord_flip()
sentPlot
ggsave(filename = "DarknessSentiment.png", plot = sentPlot, width = 36, height = 24, units = "cm",
       type = "cairo")