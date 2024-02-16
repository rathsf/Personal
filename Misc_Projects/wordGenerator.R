library('tidyverse')

vowels = c('A', 'E', 'I', 'O', 'U')
consonants = setdiff(LETTERS[seq(1, 26)], vowels)


for(i in 1:length(consonants)) {
        paste0(consonants[i], vowels[1]) 
    }


paste0(consonants[1], vowels[1])     
