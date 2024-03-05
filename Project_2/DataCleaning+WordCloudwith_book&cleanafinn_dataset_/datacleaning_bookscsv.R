#### Load the libraries we'll need

```{r, message = FALSE, warning = FALSE}
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(here)
```

#### Read in books saved to disk ####

book_raw <- read_csv(here("Data", "books.csv"))
book_raw <- read_csv("books.csv")

#### Tidy up the data ####

```
book_lines <- 
  book_raw %>% 
  filter(text != "") %>% 
  mutate(text = str_to_lower(text)) %>% 
  select(-gutenberg_id) %>% 
  group_by(title) %>% 
  mutate(line = row_number()) %>%
  ungroup()
book_lines
```
#### Find out how long each book is ####
```
book_length <- 
  book_lines %>% 
  group_by(title) %>% 
  summarise(length = n())
book_length 
```
#### Split into words ####

```
book_words <- 
  book_lines %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = str_replace_all(word, "_", ""))
book_words
```
#### If you want to check the frequency of the words been used ###

```
book_words %>% 
  count(word, sort = TRUE, name = "freq")
```
#### Removing Common Words and Generating Word Clouds ####
"The code removes very common words from the book_words dataset using the anti_join() function with a list of stop words. It then counts the frequency of each remaining word."
```
book_words %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE, name = "freq")
```
#### Now to generates word clouds for specific titles in the book_words dataset. ####
"It first removes common words using the anti_join() function, filters the dataset for a specific title (e.g., "Wuthering Heights"), 
counts the frequency of each word, filters out words with frequencies less than 10, and finally creates a word cloud using the wordcloud2() function."

```
book_words %>% 
  anti_join(stop_words) %>% 
  filter(title == "Wuthering Heights") %>% 
  count(word, sort = TRUE, name = "freq") %>% 
  filter(freq > 10) %>% 
  wordcloud2()
book_words
```
#### Now the next step is to performs sentiment analysis using the AFINN lexicon on the clean_afinn.csv dataset. ####
"It reads the CSV file into the book_raw dataframe, filters out rows where the word is not "miss", and assigns the result to the sentiment variable. 
The purpose of this code is to analyze sentiment scores associated with words in the dataset, providing insights into the sentiment expressed in the text data."

```
sentiment <- 
  book_raw <- read_csv("clean_afinn.csv", show_col_types = FALSE) %>% 
  #read_csv(here("Data", "clean_afinn.csv")) %>% 
  filter(word != "miss") # Why have I removed this?
sentiment
```
###Data Cleaning: Removing 'miss' Entries ####
"The code snippet reads the clean_afinn.csv dataset into the book_raw dataframe and filters out rows where the word is "miss". 
This step is performed as part of data cleaning to remove irrelevant or erroneous entries from the dataset.
The rationale for removing these entries is provided with a comment in the code."

```
book_words %>% 
  semi_join(sentiment %>% filter(value>0)) %>% 
  filter(title == "Wuthering Heights") %>%
  count(word, sort = TRUE, name = "freq") %>% 
  wordcloud2()
```






















