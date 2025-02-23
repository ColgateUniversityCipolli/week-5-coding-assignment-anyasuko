################################################################################################
#.    HW 05
################################################################################################
library(tidyverse)
library(stringr)
library(jsonlite)

######step 1
#step 1 part 1
current.filename <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

#step 1 part 2
artist <- str_split(current.filename, "-")[[1]][1]
track <-str_split(current.filename, "-")[[1]][2]
filename <-str_sub(str_split(current.filename, "-")[[1]][3],,-6)

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
list.of.filenames <- -which(endsWith(list.of.filenames, '.csv'))

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
  # read the ith file
  to.sort <- fromJSON(paste0("EssentiaOutput/",list.of.filenames[i])) %>%
    clean() %>%
    mutuate(
      curr.artist = str_split(list.of.filenames[i], "-", simplify=TRUE)[1],
      curr.album = str_split(list.of.filenames[i], "-", simplify=TRUE)[2],
      curr.track = str_sub(str_split(list.of.filenames[i], "-", simplify=TRUE)[3], end=-6) 
      
    )|>
  frame[i,] <- .
}

######step 3
#step 3 part 1
info <- read.csv("EssentiaOutput/EssentiaModelOutput.csv")%>%
  with(
    valence <- rowMeans(info[,c("deam_valence", "emo_valence", "muse_valence")], na.rm=TRUE),
    arousal <- rowMeans(info[,c("deam_arousal", "emo_arousal", "muse_arousal")], na.rm=TRUE),
    aggressive <-rowMeans(info[,c("eff_aggressive","nn_aggressive")], na.rm=TRUE),
    happy <-rowMeans(info[,c("eff_happy","nn_happy")], na.rm=TRUE),
    party <-rowMeans(info[,c("eff_party","nn_party")], na.rm=TRUE),
    relaxed <-rowMeans(info[,c("eff_relax","nn_relax")], na.rm=TRUE),
    sad <-rowMeans(info[,c("eff_sad","nn_sad")], na.rm=TRUE),
    acoustic <-rowMeans(info[,c("eff_acoustic","nn_acoustic")], na.rm=TRUE),
    electric <-rowMeans(info[,c("eff_electronic","nn_electronic")], na.rm=TRUE),
    instrumental <-rowMeans(info[,c("eff_instrumental","nn_instrumental")], na.rm=TRUE)
  )%>%
  colnames()=="eff_timbre_bright" <-"timbreBright"%>%
  info[, c("artist", "album", "track", "valence", "arousal", "aggressive", "happy", "party", "relaxed", "sad", "acoustic", "electric", "instrumental", "timbreBright")]


######step 4
#step 4 part 1
lyrics <- read_csv("LIWCOutput/LIWCOutput.csv") |>

#step 4 part 2
merged.two <- merge(. , info, by=c("artist", "album", "track"))|>
merged <- merge(., frame, by=c("artist", "album", "track"))%>%
  colnames() == "function" <- "funct"

