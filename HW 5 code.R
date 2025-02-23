################################################################################################
#.    HW 05
################################################################################################
library(tidyverse)
library(stringr)
library(jsonlite)

######step 1
#step 1 part 1
current.filename <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"
split <- current.filename %>% str_split("-")
artist <- split[[1]][1]
track <-split[[1]][2]
filename <-str_sub(split[[1]][3],,-6)

#step 1 part 3
data <- fromJSON("EssentiaOutput/The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json")

#step 1 part 4
clean <- function(data){
  data %>%
    with(
      overall.loudness <- lowlevel$loudness_ebu128$integrated,
      spectral.energy <- lowlevel$spectral_energy$mean,
      dissonance <- lowlevel$dissonance$mean,
      pitch.salience <- lowlevel$pitch_salience$mean,
      bpm <- rhythm$bpm,
      beats.loudness <- rhythm$beats_loudness$mean,
      danceability <- rhythm$danceability,
      tuning.frequency <- tonal$tuning_frequency
    )
  
  to.return <- data.frame(overall.loudness, spectral.energy, dissonance, pitch.salience, bpm, beats.loudness, danceability, tuning.frequency)
}

###### Step 2
list.of.filenames <- list.files("EssentiaOutput")%>%
  .[!endsWith(., ".csv")]

frame <- data.frame(overall.loudness = rep(x=NA, times=length(list.of.filenames)),
                    spectral.energy = rep(x=NA, times=length(list.of.filenames)),
                    dissonance = rep(x=NA, times=length(list.of.filenames)),
                    pitch.salience = rep(x=NA, times=length(list.of.filenames)),
                    bpm = rep(x=NA, times=length(list.of.filenames)),
                    beats.loudness = rep(x=NA, times=length(list.of.filenames)),
                    danceability = rep(x=NA, times=length(list.of.filenames)),
                    tuning.frequency = rep(x=NA, times=length(list.of.filenames)),
                    artist = rep(x=NA, times=length(list.of.filenames)),
                    album = rep(x=NA, times=length(list.of.filenames)),
                    track = rep(x=NA, times=length(list.of.filenames)))

for (i in 1:length(list.of.filenames)){
  split = str_split(list.of.filenames[i], "-", simplify=TRUE)
  to.sort <- fromJSON(paste0("EssentiaOutput/",list.of.filenames[i])) %>%
    clean() %>%
    mutate(
      curr.artist = split[1],
      curr.album = split[2],
      curr.track = str_sub(split[3], end=-6) 
    )
  frame[i,] <- to.sort
}

######step 3
#step 3 part 1
info <- read.csv("EssentiaOutput/EssentiaModelOutput.csv")%>%
  mutate(
    valence <- rowMeans(.[,c("deam_valence", "emo_valence", "muse_valence")], na.rm=TRUE),
    arousal <- rowMeans(.[,c("deam_arousal", "emo_arousal", "muse_arousal")], na.rm=TRUE),
    aggressive <-rowMeans(.[,c("eff_aggressive","nn_aggressive")], na.rm=TRUE),
    happy <-rowMeans(.[,c("eff_happy","nn_happy")], na.rm=TRUE),
    party <-rowMeans(.[,c("eff_party","nn_party")], na.rm=TRUE),
    relaxed <-rowMeans(.[,c("eff_relax","nn_relax")], na.rm=TRUE),
    sad <-rowMeans(.[,c("eff_sad","nn_sad")], na.rm=TRUE),
    acoustic <-rowMeans(.[,c("eff_acoustic","nn_acoustic")], na.rm=TRUE),
    electric <-rowMeans(.[,c("eff_electronic","nn_electronic")], na.rm=TRUE),
    instrumental <-rowMeans(.[,c("eff_instrumental","nn_instrumental")], na.rm=TRUE)
  )%>%
  rename(timbreBright = eff_timbre_bright) %>%
  select(artist,album,track,valence,arousal,aggressive,happy,party,relaxed,sad,acoustic,electric,intstrumental,timbreBright)

######step 4
#step 4 part 1
lyrics <- read_csv("LIWCOutput/LIWCOutput.csv")

merged<- lyrics|>
  merge(info, by = c("artist", "album", "track"))|>
  merge(frame, by = c("artist", "album", "track"))|>
  {
    colnames(.)[colnames(.)=="function"]<- "funct"
    .
  }
