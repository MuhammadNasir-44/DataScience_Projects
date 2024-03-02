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


#### Step1:Tidy up the data.

```{r}
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

