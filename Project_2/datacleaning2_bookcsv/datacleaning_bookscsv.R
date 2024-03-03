#### Load the libraries we'll need

```{r, message = FALSE, warning = FALSE}
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(here)
```

#### Read in books saved to disk

#book_raw <- read_csv(here("Data", "books.csv"))
book_raw <- read_csv("books.csv")

#### Tidy up the data.

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
#### Find out how long each book is.
```
book_length <- 
  book_lines %>% 
  group_by(title) %>% 
  summarise(length = n())
book_length 
```
#### Split into words.

```
book_words <- 
  book_lines %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = str_replace_all(word, "_", ""))
book_words
```
## If you want to check the frequency of the words been used ###

```
book_words %>% 
  count(word, sort = TRUE, name = "freq")
```

























