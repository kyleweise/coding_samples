#####################################
# This R script complete 3 of the problems listed on
# https://rosalind.info/problems/list-view/
#####################################
library(tidyverse)
library(seqinr)
################################################################################
# Problem 1
# A string is simply an ordered collection of symbols selected from some alphabet and formed into a word; the length of a string is the number of symbols that it contains.
# An example of a length 21 DNA string (whose alphabet contains the symbols 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."

#Given: A DNA string s of length at most 1000 nt.
#Return: Four integers (separated by spaces) counting the respective number of times that the symbols 'A', 'C', 'G', and 'T' occur in s.

sample_dna <- "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

count_dna <- function(dna_string){
  #Save base nucleotides in a list
  nucleotides <- list("A", "C", "G", "T")
  #map the str_count function to the input dna_string
  return(purrr::map(dna_string, str_count, pattern = unlist(nucleotides)))
}
print(paste0("Input DNA string: ", sample_dna))
test_1 <- count_dna(sample_dna)
print(paste0("DNA count (A, C, G, T): ", test_1))

################################################################################
#Problem 2
# An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
# Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all occurrences of 'T' in t with 'U' in u.

# Given: A DNA string t having length at most 1000 nt.
# Return: The transcribed RNA string of t.

sample_dna_2 <- "GATGGAACTTGACTACGTAAATT"
transcribe_dna <- function(dna_string){
  return(stringr::str_replace_all(string = dna_string, pattern = "T", replacement = "U"))
}
print("------------")
print(paste0("Input DNA string: ", sample_dna_2))
test_2 <- transcribe_dna(sample_dna_2)
print(paste0("RNA transcription of DNA string: ", test_2))


################################################################################
#Problem 3
# In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
# The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").

# Given: A DNA string s of length at most 1000 bp.
# Return: The reverse complement s^c of s.

sample_dna_3 <- c("AAAACCCGGT")

rev_complement_dna <- function(dna_string){

  return(stringi::stri_reverse(glue::glue_collapse(toupper(stringi::stri_reverse(seqinr::comp(unlist(strsplit(dna_string, split = ""))))))))
}
print("------------")
print(paste0("Input DNA string: ", sample_dna_3))
test_5 <- rev_complement_dna(sample_dna_3)
print(paste0("Reverse complement of DNA string: ", test_5))