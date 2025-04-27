data_to_sonif_all <- function(data){
  freq_notes_all <- read.csv("note_freq_440_432.csv")
  freq_notes_all$note_names <- paste0(freq_notes_all$A,freq_notes_all$X0)
  ## filter to 2 octaves
  freq_notes <- freq_notes_all %>% filter(X0 %in% c(4, 5)) %>% add_row(freq_notes_all[63,])
  getnotenames <- freq_notes$note_names
  names(getnotenames) <- freq_notes$X27.5
  
  freqs <- freq_notes$X27.5
  #Convert to 5 number summary
  summ <- summary(data)[c(3,1:3,5:6)]
  
  #Convert to frequencies
  sonified <- (data- summ[2])/(summ[6] - summ[2])* (max(freqs) - min(freqs)) + min(freqs)
  
  #round to nearest semitone
  music_freqs <- sapply(1:length(sonified), function(i){freqs[which.min(abs(sonified[i]- freqs))]})
  
  #get note names
  notes <- unname(getnotenames[as.character(music_freqs)])
  
  #concatenate notes
  score <- c(notes, NA)
  return(score)
}

