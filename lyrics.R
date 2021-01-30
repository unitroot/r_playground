require(geniusr)
require(tidyverse)
require(tidytext)
require(dplyr)
require(vader)

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


# dominant colors - https://www.imgonline.com.ua/eng/get-dominant-colors.php
dfCol <- data.frame(color = 
  c("#9a867c",      # Hybrid Theory
    "#453827",      # Meteora
    "#d0cfcc",      # Minutes to Midnight
    "#000000",      # A Thousand Suns
    "#422929",      # LIVING THINGS
    "#494647",      # The Hunting Party
    "#a4514b"),     # One More Light
  album_name = c("Hybrid Theory", "Meteora", 
                 "Minutes to Midnight", "A Thousand Suns", 
                 "LIVING THINGS", "The Hunting Party", 
                 "One More Light"), stringsAsFactors = FALSE)

dfTop <- dplyr::left_join(dfTop, dfCol)
wordsPlot <- ggplot2::ggplot(dfTop) +
  ggplot2::geom_bar(aes(x = reorder(word, total), 
                        y = n,
                        fill = album_name),
                    colour = "black",
                    stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "'Linkin Park' most used words",
       subtitle = "The words that appear more than 30 times in 'Linkin Park' catalogue",
       caption = "Source: genius.com | by r/unit_root",
       y = "Number of appearances",
       x = "Word",
       fill = "Album")+
  
  ggplot2::scale_fill_manual(values = dfCol$color) +
  ggplot2::theme(title = ggplot2::element_text(face = "italic", size = 12), 
        
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
        panel.background = ggplot2::element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = ggplot2::element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title = ggplot2::element_text(face = "italic",size = 11, colour = "black"),
        axis.ticks.length = ggplot2::unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = ggplot2::unit(12,"pt"),
        legend.box.spacing = ggplot2::unit(5,"pt"),
        legend.text = ggplot2::element_text(size = 12),
        
        axis.text.y = ggplot2::element_text(size = 12))
wordsPlot
ggplot2::ggsave(filename = "LPWords.png", plot = wordsPlot, width = 30, height = 24, units = "cm",
       type = "cairo")

# Create Sentiment df
dfSent <- dfTokens %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"))%>% 
  dplyr::count(album_name, song_name, sentiment, song_id) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  dplyr::mutate(sentiment = positive - negative) %>% 
  dplyr::left_join(dfSongs[,c("song_id", "song_number")], by = "song_id") %>% 
  dplyr::arrange(album_name, song_number)

# sent plot
sentPlot <- ggplot2::ggplot(dfSent, ggplot2::aes(reorder(song_name, -as.numeric(song_number)), 
                       sentiment, fill = album_name)) +
  
  ggplot2::geom_col(show.legend = FALSE) +
  
  ggplot2::facet_wrap(~album_name, 
             ncol = 3, 
             scales = "free")+
  
  ggplot2::scale_fill_manual(values = dfCol$color) +
  
  ggplot2::labs(title = "Linkin Park' songs ranked by sentiment",
       caption = "Source: genius.com | by r/unit_root",
       y = "Sentiment score",
       fill = "Album")+
  
  ggplot2::theme(title = ggplot2::element_text(face = "italic", size = 12), 
        
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
        panel.background = ggplot2::element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = ggplot2::element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title.x = ggplot2::element_text(face = "italic",size = 11, colour = "black"),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = ggplot2::unit(12,"pt"),
        legend.box.spacing = ggplot2::unit(5,"pt")) +
  
  ggplot2::coord_flip()
sentPlot
ggplot2::ggsave(filename = "LPSentiment.png", plot = sentPlot, width = 36, height = 24, units = "cm",
       type = "cairo")

dfVader <- vader::vader_df(dfLyrics$line)

dfSent2 <- cbind(dfLyrics, dfVader[,c("compound"), drop = FALSE]) %>% 
  dplyr::group_by(song_id) %>% 
  dplyr::summarise(vader = mean(compound, na.rm = TRUE) * 100) %>% 
  dplyr::mutate(song_id = as.numeric(song_id)) %>% 
  dplyr::left_join(dfSent, ., by = "song_id")

# sent plot
sentPlot2 <- ggplot2::ggplot(dfSent2, ggplot2::aes(reorder(song_name, -as.numeric(song_number)), 
                                                 vader, fill = album_name)) +
  
  ggplot2::geom_col(show.legend = FALSE) +
  
  ggplot2::facet_wrap(~album_name, 
                      ncol = 3, 
                      scales = "free")+
  
  ggplot2::scale_fill_manual(values = dfCol$color) +
  
  ggplot2::labs(title = "Linkin Park' songs ranked by VADER sentiment",
                caption = "Source: genius.com | by r/unit_root",
                y = "Sentiment score",
                fill = "Album")+
  
  ggplot2::theme(title = ggplot2::element_text(face = "italic", size = 12), 
                 
                 panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
                 panel.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 panel.grid.major.x = ggplot2::element_line(colour="grey90",size = 1, linetype = 4),
                 
                 axis.title.x = ggplot2::element_text(face = "italic",size = 11, colour = "black"),
                 axis.title.y = ggplot2::element_blank(),
                 axis.ticks.length = ggplot2::unit(5, units = "pt"),
                 
                 legend.background = NULL,
                 legend.position = "top",
                 legend.key.size = ggplot2::unit(12,"pt"),
                 legend.box.spacing = ggplot2::unit(5,"pt")) +
  
  ggplot2::coord_flip()
sentPlot2
ggplot2::ggsave(filename = "LPSentiment2.png", plot = sentPlot2, width = 36, height = 24, units = "cm",
                type = "cairo")
