require(geniusr)
require(tidyverse)
require(tidytext)
require(dplyr)

readLines("secret.txt")
geniusr::genius_token(TRUE)
geniusr::search_artist("Linkin Park")

songs <- get_artist_songs_df(1581) 
# Get all song IDs
ids <- c(as.character(songs$song_id))

songs$album_id <- 0
songs$album_name <- "NA"
for (numI in c(1:NROW(songs))) {
  dfTemp <- geniusr::get_song_df(songs$song_id[numI])
  songs[numI, c("album_id", "album_name")] <- dfTemp[,c("album_id", "album_name")]
}

dfSongs <- songs[songs$album_name %in% c("Hybrid Theory", "Meteora", 
                                         "Minutes to Midnight", "A Thousand Suns", 
                                         "LIVING THINGS", "The Hunting Party", 
                                         "One More Light"),]

ids <- c(as.character(dfSongs$song_id))
# Create empty dataframe to house them
dfLyrics <- data.frame()
while (length(ids) > 0) {
  for (id in ids) {
    tryCatch({
      dfLyrics <- rbind(get_lyrics_id(id), dfLyrics)
      successful <- unique(dfLyrics$song_id)
      ids <- ids[!ids %in% successful]
      print(paste("done - ", id))
      print(paste("New length is ", length(ids)))
    }, error = function(e){})
  }
}

allIds <- data.frame(song_id = unique(dfLyrics$song_id))
allIds$album <- ""

for (song in allIds$song_id) {
  allIds[match(song,allIds$song_id),2] <- get_song_df(song)[12]
  print(allIds[match(song,allIds$song_id),])
}
dfLyrics <- full_join(allIds, dfLyrics)

head(allIds)
allIds$album[is.na(allIds$album)] <- "Single Only"
head(allIds)
allLyrics2 <- full_join(dfLyrics, allIds)

## Text Analysis
allLyricsTokenised <- allLyrics2 %>%
  #word is the new column, line the column to retrieve the information from
  unnest_tokens(word, line)
# Count each word - I guarantee love is top
allLyricsTokenised %>%
  count(word, sort = TRUE)

# Remove stopwords
tidyLyrics <- allLyricsTokenised %>%
  anti_join(stop_words)
# Top words again
tidyLyrics %>%
  count(word, sort = TRUE)

topFew <- tidyLyrics %>%
  group_by(album, word) %>%
  mutate(n = row_number()) %>%
  ungroup()

# Remove extra cols
topFew <- topFew[,c("album", "word", "n")]
# Take only max for each word by album
topFew <- topFew %>%
  group_by(album, word) %>%
  summarise(n = max(n))%>%
  ungroup()

# Subset
topFew <- topFew %>% 
  group_by(word) %>%
  mutate(total = sum(n)) %>%
  filter(total >= 40,
         word != "ooh") %>%
  ungroup()


topFew <- topFew2[topFew2$album %in% c(
  "Hybrid Theory", 
)]

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
# This ensures bars are stacked in order of release date
topFew$album <- factor(topFew$album, levels = c("Hybrid Theory", "Meteora", 
                                                "Minutes to Midnight", "A Thousand Suns", 
                                                "LIVING THINGS", "The Hunting Party", 
                                                "One More Light"))

wordsPlot <- ggplot(topFew) +
  
  geom_bar(aes(x = reorder(word, total), 
               y = n,
               fill = as.factor(album)),
           colour = "black",
           stat = "identity") +
  
  coord_flip() +
  
  labs(title = "The Darkness' most used words",
       subtitle = "The words that appear more than 40 times in The Darkness' catalogue",
       caption = "Source: genius.com | by @Statnamara",
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