##'* Written: Morrigan Hughes *
##'* Written: 31-JULY-2024 *

##'* DATA ANALYST TECHNICAL INTERVIEW *
##'*        Timed data clean          *

## INSTRUCTIONS ----
# Take the raw dummydata.csv file and clean the data to prep for its use in mailings.
# Export the final dataframe as a .csv named "SDA_yourlastname.csv"
# Save the script as an .R named "SDA_yourlastname.R"

#'* IMPORTANT *
#' It is not expected that all flaws in the dummy data will be found within the time limit.
#' The goal is to gauge your understanding of R, your attention to detail, and how you
#' structure your code, not your speed. Relax and just do your best.

#'* Examples of things which may need cleaning *
#'  All names and addresses with proper capitalization and spacing
#'  All cities with proper capitalization, spelling, and spacing
#'  All zip codes with 5 digits only
#'  All states the two character abbreviation 
#'    i.e. MN for Minnesota
#'  All home_lang filled - assume "English" if no home_lang value
#'  NOT AN EXHAUSTIVE LIST

#'* Data Output Requirements: *
#'* label:  *
#'    A column for generating the "to" field on address labels, the label should be 
#'    EITHER "Parent/Guardian of student_first, student_last" if no guardian_1 or guardian_2 exists for the record, 
#'    OR "guardian_1 & guardian_2" without repeated last names if both guardian_1 and guardian_2 exists for the record
#'        i.e. "Morrigan & Madeleine Hughes" rather than "Morrigan Hughes & Madeleine Hughes"
#'    OR "guardian_1" if there is a guardian_1 for the record, but not a guardian_2
#'  
#'* student_first, student_last:  *
#'    Cleaned columns of student names
#'  
#'* address_1:  *
#'    Cleaned column of address_1 without the apartment number
#'  
#'* address_2:  *
#'    Cleaned column of address_2, apartment number only
#'  
#'* city: *
#'    Cleaned column of city; correct spelling, spacing, and format
#'  
#'* state:  *
#'    Generated column of state abbreviations matching the city and zip
#'  
#'* zip:  *
#'    Cleaned column of zip codes, 5 digits only
#'  
#'* student_id: *
#'    Untouched student_id column
#'  
#'* home_lang:  *
#'    Home language for the student, assume "English" if field is blank

## LOAD PACKAGES ----
library(stringr)
library(tidyverse)

## LOAD DATA ----
# Data from Communications & Engagement
data_raw <- read.csv("dummydata.csv")

# Data from SIS - guardian information
guardian_raw <- read.csv("dummydata_guardian.csv")

## DATA EXPLORATION ----
#'* Can be done in console if preferred*

## DATA CLEANING ----

#clean student first and last names
data_working <- data_raw |> mutate(
  student_first = str_to_title(str_squish(str_replace_all(student_first, " - ","-"))),
  student_last = str_to_title(str_squish(str_replace_all(student_last, " - ","-"))), 
  student_last = str_replace_all(student_last,"Iv$","IV"),
  student_last = str_replace_all(student_last,"Iii$","III"),
  student_last = str_replace_all(student_last,"Ii$","II"),
  student_last = case_when(
    str_starts(student_last,"Mc") 
    ~ paste("Mc",str_to_title(substring(student_last,3)), sep = ""), 
    TRUE ~ student_last))


#replace missing home language values
data_working$home_lang <- sub("^$", "English", data_working$home_lang)

#update zip and city
data_working <- data_working |> 
  mutate(
    zip = substr(zip, start = 1, stop = 5),
    zip = case_when(
      str_detect(city, "MN") ~ substr(city, nchar(city)-4,nchar(city)),
      TRUE ~ zip), 
    city = gsub("\\,.*","",city),
    city = str_to_title(str_squish(city)),
    city = case_when(city == "St.paul" ~ gsub("St.paul","Saint Paul",city),
                     (str_starts(city, 'St. ')~ gsub("St.","Saint",city)),
                     (str_starts(city, "St ")~gsub("St","Saint",city)),
                     (str_starts(city, "Mpls")~"Minneapolis"),
                     (str_starts(city, "Minnea")~"Minneapolis"), 
                      (str_starts(city, "Bloom")~"Bloomington"),
                      TRUE~city),
    address_1 = case_when(
      !is.na(address_2) ~ substr(address_1, 0, nchar(address_1)-(nchar(address_2))-1), 
      is.na(address_2) ~ address_1),
    address_1 = str_to_title(str_squish(address_1)),
    address_2 = case_when(!is.na(address_2) ~ str_to_title(address_2),TRUE~""),
    state = case_when(grepl("^55.*",zip) ~ "MN", 
                      grepl("^7.*",zip) ~"TX", 
                      TRUE ~ ""))

#label with guardian
guardian_working <- guardian_raw |> mutate(
  guardian_1 = str_to_title(str_squish(str_replace_all(guardian_1, " - ","-"))),
  guardian_1 = str_to_title(str_squish(str_replace_all(guardian_1, " - ","-"))), 
  guardian_1 = str_replace_all(guardian_1,"Iv$","IV"),
  guardian_1 = str_replace_all(guardian_1,"Iii$","III"),
  guardian_1 = str_replace_all(guardian_1,"Ii$","II"),
  guardian_2 = str_to_title(str_squish(str_replace_all(guardian_2, " - ","-"))),
  guardian_2 = str_to_title(str_squish(str_replace_all(guardian_2, " - ","-"))), 
  guardian_2 = str_replace_all(guardian_2,"Iv$","IV"),
  guardian_2 = str_replace_all(guardian_2,"Iii$","III"),
  guardian_2 = str_replace_all(guardian_2,"Ii$","II"))

guardian_working <-
  separate(guardian_working,guardian_1, into = c("guardian_1_first","guardian_1_last"), sep=" ", extra = "merge")

guardian_working <-
  separate(guardian_working,guardian_2, into = c("guardian_2_first","guardian_2_last"), sep=" ", extra = "merge")

guardian_working <- guardian_working |> mutate(
  is_match = ifelse(guardian_1_last == guardian_2_last,1,0))

guardian_working <- guardian_working |> mutate(
  guardian_1_last = case_when(
    str_starts(guardian_1_last, "Mc")    
    ~ paste("Mc",str_to_title(substring(guardian_1_last,3)), sep = ""), 
    TRUE ~ guardian_1_last),
  guardian_2_last = case_when(
    str_starts(guardian_2_last, "Mc")    
    ~ paste("Mc",str_to_title(substring(guardian_2_last,3)), sep = ""), 
    TRUE ~ guardian_2_last))

joined_set <- left_join(data_working, guardian_working)

joined_set <- joined_set |> mutate(label = case_when(
  is.na(guardian_1_first) ~ paste("Parent/Guardian of",student_first,student_last),
  is.na(guardian_2_first) ~ paste(guardian_1_first,guardian_1_last),
  is_match == 1 ~ paste(guardian_1_first,"&",guardian_2_first,guardian_1_last),
  TRUE ~ paste(guardian_1_first,guardian_1_last,"&",guardian_2_first,guardian_2_last)
))
## OUTPUT ----

output <- select(joined_set, c('label','student_first','student_last','address_1','address_2','city','state','zip','student_id','home_lang'))
write.csv(output, "MB-output.csv")

