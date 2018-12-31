---
layout: post
title: "Geocoding Nostradamus' Prophecies"
date: "2018-12-30"
comments: true
maps: true
---
-   [Geocoding Nostradamus' Prophecies](#geocoding-nastradamus-prophecies)

Geocoding Nostradamus' Prophecies
=================================

In this post, I'll take a first shot at trying to go from unstructured to structured text data that's suitable for plotting or further scraping.

I took the liberty of scraping all of Nostradamus's English texts (his '10 Centuries' plus the two 'Almanacs') and I've already cleaned them up and placed them in a csv file that I'll simply import as a data frame.
If you're interested in reading the text files, you can view them [here](http://www.sacred-texts.com/nos/index.htm). 

If you've never heard of Nostradamus, here's a quick summary from [Wikipedia](https://en.wikipedia.org/wiki/Nostradamus):

>Michel de Nostredame (depending on the source, 14 or 21 December 1503 – 2 July 1566), usually Latinised as Nostradamus, was a French physician and reputed seer, who is best known for his book Les Propheties, a collection of 942 poetic quatrains allegedly predicting future events. The book was first published in 1555 and has rarely been out of print since his death.

Keep in mind that apparently Nostradamus was writing in Old French, and as such, the translations into English aren't always so reliable. So when we finally geocode the extracted locations, there's going to be a bit of noise.

Goal
==================
Our goal here will be to put all the locations on a world map and then use the text related to the location to infer keywords related to that location. This way, instead of just showing tons of lines of text that mentioned the location, we can just show some keywords that most summarize Nostradamus' prophecies surrounding that location. After all, the goal of data mining should be to make complicated things simple so that quick insights can be found.

Our end result should look something like this, but interactive:
![final result](./map/nas.jpg){:class="img-responsive"}

Here's what the CSV file looks like that will be the basis for the analysis.

``` r
df <- read.csv('nas_text.csv', stringsAsFactors = F)
head(df)
```

    ##                                                                                                                                                                                                              text
    ## 1                               Sitting alone at night in secret study it is placed on the brass tripod A slight flame comes out of the emptiness and makes successful that which should not be believed in vain 
    ## 2   The wand in the hand is placed in the middle of the tripod s legs With water he sprinkles both the hem of his garment and his foot A voice fear he trembles in his robes Divine splendor the God sits nearby 
    ## 3                        When the litters are overturned by the whirlwind and faces are covered by cloaks the new republic will be troubled by its people At this time the reds and the whites will rule wrongly 
    ## 4                                         In the world there will be made a king who will have little peace and a short life At this time the ship of the Papacy will be lost governed to its greatest detriment 
    ## 5         They will be driven away for a long drawn out fight The countryside will be most grievously troubled Town and country will have greater struggle Carcassonne and Narbonne will have their hearts tried 
    ## 6                         The eye of Ravenna will be forsaken when his wings will fail at his feet The two of Bresse will have made a constitution for Turin and Vercelli which the French will trample underfoot

Using OpenNLP to Extract Named Entities
=================================
In order to extract locations and (later) persons, we'll use the OpenNLP Annotator objects. Essentially these gather features from the text and then, using some algorithm (probably a neural network), assigns probabilities to each token of text as to whether it is a location or person. We're just focusing on locations for now.

``` r
#build instances
library(NLP)
persons <- Maxent_Entity_Annotator(kind='person')
locations <- Maxent_Entity_Annotator(kind='location', probs = T)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()


# Make one complete document
text = paste(df$text, collapse=' ' )
text = as.String(text)

#use person and location annotator
ann <- annotate(text, list(sent_token_annotator, 
                                   word_token_annotator,persons,
                                   locations))
```

Extracting people and places
=================================
Now that we have the annotations, we need to dig in there and extract what we want: locations and people. We do this by subsetting the original text and then keeping on the unique locations.

``` r
#[[ subsets. the k is looking for kind, then subsetting location kind and person kind
k <- sapply(ann$features, `[[`, "kind")
locs = text[ann[k == "location"]]
people = text[ann[k == "person"]]

#keep only unique locations
all_places = unique(locs)
```

Finding a location's context
=================================
Now it gets a little tricky. We need to write a function that extracts the "context" of the named location or person by looking at the previous 100 characters and the following 50. We also will remove stopwords.

``` r
library(tidytext)
library(tidyverse)
```

``` r
#try to get text from each location
get_context <- function(text_doc, a_place = 'Sicily'){
  #find alllocations of place in text
  locs <- str_locate_all(text_doc, a_place)
  save_text <- vector()
  for (i in 1:nrow(locs[[1]])){
  save_text[i] <- removeWords(str_sub(text_doc, locs[[1]][[i]]-100, locs[[1]][[i]]+50), stop_words$word)
  }
  save_text <- sapply(save_text, str_replace_all, '\\s+', ' ')
}
```

Cleaning up the results and getting the context of the locations
=================================

Now we apply this function to each unique location and it will make a list of lists: the first element of the list is the location, and the second element is another list of each time the word was mentioned, along with the previous 100 characters and following 50. This should help us to get a feel for the context in which the location came up.

``` r
#get context for all places mentioned
w <- all_places
for (i in 1:length(all_places)) {
  assign(w[i], get_context(text, all_places[i]))
}

names(w) <- all_places

#creates big list with sublist for each location
all.w = lapply(w, get)
```

Removing irrelevant entities
=================================
Now we get a bit hacky and re-extract the location names by making the rowname become a column and then keeping the letters before the first punctuation mark. There's probably a better way to do this, but this is what managed to work for me.

``` r
#create dataframe of texts and mentions
s <- data.frame(texts = unlist(all.w))
#get the rownames which contains serach term, later extract
s <- rownames_to_column(s, 'topics')


#extract the first word
s <- s%>%
  mutate(top = str_extract(topics, '[A-Za-z]*'))

#get rid of bs words like 'he'
s <- s%>%
  filter(nchar(top) > 2)

#remove places that are not actually locations
s <- s%>%
  filter(!top %in% c('This', 'Mars', 'San', 'Ark',
                     'Sea', 'Fort', 'Venus', 'Lake',
                     'Hope', 'Prey', 'Ocean', 'Moon',
                     'New', 'river', 'earth', 'Sun', 'Naked'))

#remove the column 
s$topics <- NULL
```

Summarizing the context using keywords
=================================
Now for each location, we are going to look for the words that most occur within this 150 character window. Then we rank top 5 most frequent after removing stopwords. We could have also used tf-idf here to rank the words, but simple frequency will do for now.

``` r
top_words <- s%>%
  mutate(texts = as.character(texts))%>%
  mutate(texts = str_to_lower(texts))%>%
  unnest_tokens(word, texts)%>%
  anti_join(stop_words)%>%
  filter(!word %in% c(tolower(top), 'one', 'ones', 'kings', 'princes'))%>%
  count(top, word, sort=T)%>%
  ungroup()
```

Geocode the locations 
=================================

``` r
#keep just the top 5 per topic
cts <- top_words%>%
  group_by(top)%>%
  arrange(desc(n), .by_group=TRUE)%>%
  slice(1:5)

#paste together into one string for easy display on map
cts <- cts%>%
  group_by(top)%>%
  mutate(full_words = paste(word, collapse= ' '))

library(ggmap)
#get geocodes for plotting
cts <- cts%>%
  ungroup()%>%
  mutate_geocode(top, source='dsk')
```

Making the map
=================================
Finally, we are ready to create a map of the locations. Notice that I've used the Data Science Toolkit geocoder instead of Google's because Google now requires that you register a credit card in order to use their Maps API. So in order to avoid that hassle, I'll use the free-but-not-as-good 'dsk' geocoder. Just please don't laugh when you see where some of these locations ended up on the map ;-)


``` r
library(leaflet)
library(htmltools)
library(htmlwidgets)
#This is a labeller function to display text using html tags
print_labels <- sprintf('<strong> Location: </strong>%s <br> <strong>Keywords:</strong> %s',
                        cts$top, cts$full_words)%>% lapply(htmltools::HTML)

#create coloring scheme: 9 degrees of red
word_pal = colorBin('Reds', domain = cts$n, bins=9) 

#finally plot the map
nas_map <- cts%>%
  leaflet()%>%
  addProviderTiles('CartoDB', group='Carto')%>%
  addCircleMarkers(radius = ~ n, label = print_labels,
                   fillOpacity = .5,
                    # popup = ~ all_text,
                   color= 'red',
                   fillColor = ~ word_pal(n))
```


``` r
saveWidget(nas_map, file="nas_map.html")
```


{% include nas_map.html %}

Conclusion
=================================
So that's it. Don't mind some of the crazy geocoding--that's mostly the fault of the dsk geocoder. Google maps did a better job of recognizing the locations. 

I'm not sure how enlightening the final result is, but I think this approach could be extended for a lot of other, more interesting analyses. For example, using the extracted locations to then scrape wikipedia and then filter the wikipedia text and display that on a map. You could input song lyrics and then you would get a map with all people mentioned in the song and a quick keyword summary of the person (based on say, the top tf-idf scores of words).


