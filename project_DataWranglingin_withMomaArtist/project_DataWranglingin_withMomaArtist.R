
'The data sets we are using this week are listings of the artists and works in the collection of the Museum of Modern Art (MoMA) in New York, together with a listing of artists in the UK s Tate Modern.
Details are here: https://data.world/moma/collection'
https://github.com/tategallery/collection


moma_artists<- read.csv(yourdatapathherefromcomputer)
moma_works <- read.csv(yourdatapathherefromcomputer)
tate_artists <- read.csv(yourdatapathherefromcomputer)

## Check all dataframe 
moma_artists 
moma_works  
tate_artists

### MoMA artists

'We are going to be using Nationality and Gender for our analysis.So we will convert the columns to a consistent capitalisation and make them factors.'

moma_artists_proc <- 
  moma_artists %>% 
  mutate(Nationality = factor(str_to_title(Nationality)),
         Nationality = fct_explicit_na(Nationality), 
         Gender = factor(str_to_title(Gender)))
num_artists <- nrow(moma_artists_proc)


### Exercise1. Use the `levels()` function to examine the levels for Nationality and Gender in the console.]_
'We can see the number in each Nationality group using the `group_by` and `summarise`.'


moma_artists_proc %>% 
  group_by(Nationality) %>% 
  summarise(Number = n()) %>% 
  arrange(desc(Number))

### Do the same for Gender.

moma_artists_proc %>% 
  group_by(Gender) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = Number/num_artists*100) %>%
  arrange(desc(Number))

'An [article](https://www.theguardian.com/artanddesign/2019/oct/10/moma-expansion-museum-new-york) in the Guardian quoted some figures on the diversity of the MoMA s  collection
#The museum said that works by women have increased fivefold since the early 2000s, although 59% of artists across their collectiThey have also increased works from outside North America and Europe.Does this seem right?
#The art works data set also list gender, so we could try to count the proportions of male and female artists listed there. 
#This time we will extract the `Gender` column as a vector, because some of the works have multiple artists listed. The following code does the trick.'

mw_artists <- 
  moma_works$Gender %>% 
  str_extract_all("(Male|Female|\\(\\))") %>% 
  unlist()

mw_artists

###[Exercise2. Use `table()` to produce a cross-tabulated summary of the gender split in this data set.]_

mw_artists %>% table()

###[Exercise3. Use `prop.table` to produce the same thing, but this time showing proportions]_

prop.table(mw_artists %>% table())
 
###We can also do this by Janitor library ###

library(janitor)
t <- mw_artists %>% tabyl()
t

`valid_percent` is the fraction with `NAs` removed from the denominator.


###[Exercise4. Investigate the rows with Nationality and/or Gender missing.]_

no_nat <- 
  moma_artists %>% 
  filter(is.na(Nationality))
no_nat

'Many of the "artists" with no nationality and gender seem to be organisations or groups, rather than individuals.'
###[Exercise5. Remove the rows with no Gender and those with no Nationality.]_

moma_artists_proc <- 
  moma_artists_proc %>% 
  filter(!is.na(Nationality)) %>% 
  filter(!is.na(Gender))

moma_artists_proc

###[Exercise6. Group by both nationality and gender and print the numbers in each group.]_

moma_artists_proc %>% 
  group_by(Nationality, Gender) %>% 
  count() %>% 
  arrange(desc(n))

###[Exercise7. Create a column called Alive, and a column giving Age. With `ifelse()` might be useful.]_

moma_artists_proc <- 
  moma_artists_proc %>% 
  mutate(Alive = ifelse(EndDate > 0,
                        FALSE,
                        ifelse(BeginDate == 0, NA,  TRUE))) %>% 
  mutate(Age = ifelse(Alive, 2020 - BeginDate, NA))             # Sloppy!
moma_artists_proc

###[Exercise8. Show average age by gender for living artists.]_

moma_artists_proc %>% 
  filter(Alive) %>% 
  group_by(Gender) %>% 
  summarise(Average_Age = mean(Age))

'Visualisation'

## Bar chart
'The key to plotting bar charts where the bars are in a particular order is to make sure that the variables you want to specify along the x axis are described 
by an ordered factor listed in the way you want it.'

Code show orders the Nationality factor by the total count for each country.

##[Challenge9: Achieving this with funtion,`forcats`]_


ord_nationalities <- 
  moma_artists_proc %>% 
  group_by(Nationality) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

moma_artists_proc <- 
  moma_artists_proc %>% 
  mutate(Nationality = ordered(Nationality, 
                               levels = rev(as.character(ord_nationalities$Nationality))))

'The following code produces a bar chart using `geom_col` 

1. When the labels are words, its often neater to plot the columns horizontally. This is achieved by `coord_flip()`.
2. If you need to process a data frame to get it into the form you need for plotting, 
   there is no need to create a separate variable. You can pipe the output from a string of pre-processing commands directly to `ggplot`.'

moma_artists_proc %>% 
  group_by(Nationality, Gender) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Nationality)) %>% 
  head(40) %>% 
  ggplot() +
  geom_col(aes(x = Nationality, y = Count, fill = Gender)) +
  coord_flip()

### A similar chart using geom_bar
The example above pre-processed the data, using `group_by` and `summarise` to produce the counts. We can use `geom_bar` to plot the counts automatically, 
but in this case I have had big problems with the labelled axis.

###[Challenge10. Can you write some better code using `geom_bar`?]_

rows <- length(levels(moma_artists_proc$Nationality))
ggplot(moma_artists_proc) +
  geom_bar(aes(x = Nationality, fill = Gender)) +
  coord_flip(xlim = c(rows - 19, rows))

##[Exercise11.]_

###Split location and country of birth for the Tate artists.

tate_artists_proc <- 
  tate_artists %>% 
  separate(placeOfBirth, into = c("locOfBirth", "countryOfBirth"), sep = ",")

###Try with three new columns

tate_artists_proc <- 
  tate_artists %>% 
  separate(placeOfBirth, into = c("loc2OfBirth", "locOfBirth", "countryOfBirth"),
           sep = ",",
           fill = "left") %>% 
  mutate(countryOfBirth = factor(str_trim(countryOfBirth)))
levels(tate_artists_proc$countryOfBirth)

View(tate_artists_proc)

###End####






