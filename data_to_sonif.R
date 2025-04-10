data_to_sonif <- function(data){
  
  freqs <- freq_notes$X27.5
  #Convert to 5 number summary
  summ <- summary(data)[c(3,1:3,5:6)]
  
  #Convert to frequencies
  sonified <- (summ- summ[2])/(summ[6] - summ[2])* (max(freqs) - min(freqs)) + min(freqs)
  
  #round to nearest semitone
  music_freqs <- sapply(1:length(sonified), function(i){freqs[which.min(abs(sonified[i]- freqs))]})
  
  #get note names
  notes <- unname(getnotenames[as.character(music_freqs)])
  
  #concatenate notes
  score <- c(notes, NA)
  return(score)
}

