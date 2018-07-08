---
layout: post
title: "Data Analysis: Visualizing Global Violence Since 1400 A.D. with ggplot2 & Plotly"
date: "2018-07-07"
---
For this post, I wanted to look at how global violent conflict has changed over the past few centuries. According to Steven Pinker in his *The Better Angels of Our Nature: Why Violence Has Declined*, we've never lived in a safer, less-violent time than today. Are there data to support this?

I managed to find an interesting dataset of world violent conflicts that has been painstakingly put together by professor Peter Brecke of the University of Utrecht. You can find the data on the "data" tab of the University of Utrecht's Centre for Global Economic History. <http://www.cgeh.nl/data#conflict>

**Regarding the completeness of the conflict data, Prof. Brecke has this to say:**

*It must be understood that the Conflict Catalog within that Excel file is an unfinished product. Taking into consideration the data I have in my office that is not yet in the Conflict Catalog, I can easily believe the catalog growing by 20% or more.*

*There are certainly errors in the data, especially as we go back in time and into particular regions of the world. I have strived mightily to make these data as correct and accurate as I can, but there are undoubtedly errors of some form or other in the file. I tried to eliminate errors and resolve inconsistencies by comparing as many sources as I could find for every datapoint in the file. I believe the number of mistakes in terms of typos is quite small.*

In short, it's a work in progress. For example, the US Civil War is not included.

This dataset contains approximately 3708 conflicts (rows) and 18 variables of interest. Note however that there are many many NA values. Of course, dealing with data from 1400-present day, one would expect some data sources to not be reliable. In some cases it may even be impossible to calculate accurate numbers. With these caveats in mind, we'll do our best.

**Our Plan of Attack**

We will start by cleaning up the data a little bit and adding some important missing country names to a new variable that we can then use to group our observations by. In order to do that, we will need to leverage another table that contains country names in many different languages: the countrycode package. Once we have those, it's a simple inner\_join to add the new columns (regions and continents) using our existing country names.

``` r
library(tidyverse)
library(countrycode)
library(stringr)
library(ggmap)
options(tibble.print_max = 50)

df <- read.csv('conflictcsv.csv', stringsAsFactors = FALSE)
df$Name <- tolower(df$Name)
names(df)
```

    ##  [1] "Common.Name"     "Name"            "CountryCode"    
    ##  [4] "NumberActors"    "MilFatalities"   "TotalFatalities"
    ##  [7] "StartYear"       "StartMonth"      "StartDay"       
    ## [10] "EndYear"         "EndMonth"        "EndDay"         
    ## [13] "Region"          "Century"         "Decade"         
    ## [16] "DurationD"       "DurationM"       "DurationY"

``` r
#great little package that includes regex examples for extracting countries
codes <- tolower(countrycode::codelist$country.name.en)

#add these because they are not the standard country names but will represent geographic regions in our plots
codes = append(codes, c('england', 'burma','korea','zaire', 'crimea', 'milan', 'america', 'west indies',
                        'guiana', 'guatamala', 'palestine',
                        'soviet union', 'ussr', 'congo', 'britain', 'persia', 'us',
                        'holland', 'ceylon','sumatrans', 'transylvania', 'united provinces',
                        'scotland', 'algiers','florence', 'burgundy', 'bohemia', 'naples',
                        'burmese', 'zurich', 'aragon', 'sicily', 'siam', 'tibet', 'venice',
                        'mongols', 'german', 'swiss', 'castile', 'emperor', 'papal',
                        'british', 'boers', 'bengal', 'portuguese', 'moghuls', 'zulu') , after = length(codes))

#turn it into a DF for joining
cl <- codelist

#convert all names to lowercase
cl <- cl %>%
  mutate_if(is.character, tolower)
  

#find name from description and add new column with name value. 430 missing this way
df <- df %>%
  mutate(country = (str_extract(df$Name, paste(codes, collapse="|"))))

#need continent and region labels for plotting. Have to join from other dataframe.
df <- df %>%
  rename('country.name.en' = country)%>%
  inner_join(cl[,c('region','continent', 'country.name.en')], by='country.name.en')%>%
  rename('country' = country.name.en)
```

Set up our maps with ggmap()
============================

I don't know why, but sometimes these don't work the first time. If I run them again, however, they will work.

``` r
#Map of EU
world_map_eur <- get_map('Europe', zoom=3)%>%ggmap()
world_map_eur
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-2-1.png)

Since the Google Maps API only allows 2500 geocoordinate queries per day, we will go old school and scrape the coordinates from this handy Google html table that lists the country name and the latitude and longitude. These coordinates will allow us to plot on a world map. Otherwise we could use the handy mutate\_geocode() function from gmaps.

``` r
#COLLECT LATITUDE AND LONGITUDE DATA FROM WEB
#html_table is super useful because it will format a dataframe from an html table
library(rvest)
url <- 'https://developers.google.com/public-data/docs/canonical/countries_csv'
tab <- html_table(read_html(url))[[1]]
tab <- tab %>%
  mutate(name = tolower(name))

#JOIN SCRAPED COORDS WITH DATA
#Now we have country, continent, region, and lat longitudes
df <- df %>%
  rename('name' = country)%>%
  inner_join(tab[,c('latitude','longitude','name')], by='name')
```

We have the data required for plotting, now we just need to compute some summary statistics to display. To start, let's look at the number of conflicts around the world since 1400.

``` r
#Latitudes for regions and countries for plotting. READY TO PLOT!
lat_long_conf <- df %>%
  group_by(name, region)%>%
  summarize(long = mean(longitude, na.rm=T),
            lat = mean(latitude, na.rm=T),
            num_conflicts = n())
head(lat_long_conf)
```
    ## # A tibble: 6 x 5
    ## # Groups:   name [6]
    ##          name          region       long       lat num_conflicts
    ##         <chr>           <chr>      <dbl>     <dbl>         <int>
    ## 1 afghanistan   southern asia  67.709953  33.93911            61
    ## 2     albania southern europe  20.168331  41.15333            10
    ## 3     algeria northern africa   1.659626  28.03389            11
    ## 4      angola   middle africa  17.873887 -11.20269            15
    ## 5   argentina   south america -63.616672 -38.41610            26
    ## 6     armenia    western asia  45.038189  40.06910             1

We're not done yet. We need a world map to plot on. We also need a dataframe with the latitude and longitude information. We obtain this using the maps library and the map\_data() function that creates a dataframe with region and longitude and latitude information. We pass these into the call to geom\_map().

``` r
library(maps)
```
``` r
world_map <- map_data('world')
```
``` r
head(world_map)
```
    ##        long      lat group order region subregion
    ## 1 -69.89912 12.45200     1     1  Aruba      <NA>
    ## 2 -69.89571 12.42300     1     2  Aruba      <NA>
    ## 3 -69.94219 12.43853     1     3  Aruba      <NA>
    ## 4 -70.00415 12.50049     1     4  Aruba      <NA>
    ## 5 -70.06612 12.54697     1     5  Aruba      <NA>
    ## 6 -70.05088 12.59707     1     6  Aruba      <NA>

Simply overlay these data on our ggplot world map. Notice the map=world\_map and aes(map\_id=region) arguments.

Let's look at conflicts by region.

``` r
  ggplot()+
  geom_map(data=world_map, map=world_map,aes(x=long, y=lat,map_id = region),
           fill="white", color="black")+
  geom_point(data=lat_long_conf,
             aes(long, lat, size=num_conflicts),color='red', alpha=.5)+
  theme_minimal()+
  labs(title='Violent conflicts since 1400 AD')+
  guides(size=FALSE)+
  labs(x='', y='')
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-6-1.png)

From this we can see that Europe is a hotbed, along with modern day China, the Middle East, and Central America. Africa has had conflicts in almostevery country, though the total number of conflicts is dwarfed by Europe and China.

Conflicts over the centuries
============================

Now we want to look more at the number of conflicts through the centuries. In order to do that we must turn our semi-continuous variable StartYear into a discrete variable with values for each century. To do this we will use the cut() function that takes two key arguments: the break values in the actual variable, and the labels for each break. Notice that there is one fewer label than break values.

``` r
#create copy of original df
df_cut <- df
df_cut$StartYear <-  cut(df_cut$StartYear, breaks = c(1400, 1500, 1600, 1700, 1800, 1900, 2001),
      labels = c('1400-1499', '1500-1599', '1600-1699', '1700-1799',
                 '1800-1899', '1900-2001'),
      right=F, ordered_result = T)

df_cut %>%
  select(Name, StartYear)%>%
  head(5)
```
    ##                                                           Name StartYear
    ## 1 china (minority in hebei, shandong, anhui, jiangsu), 1400-02 1400-1499
    ## 2                                                hungary, 1401 1400-1499
    ## 3                                    russia-lithuania, 1401-04 1400-1499
    ## 4                        mujapahit (india, civil war), 1401-06 1400-1499
    ## 5                      china-japanese pirates (zhejiang), 1401 1400-1499

We will then look at the number of conflicts by each present day country.

``` r
#get conflicts by continent
year_conf <- df_cut %>%
  group_by(StartYear, name, continent)%>%
  summarize(long = mean(longitude, na.rm=T),
            lat = mean(latitude, na.rm=T),
            num_conflicts = n())
head(year_conf)
```

    ## # A tibble: 6 x 6
    ## # Groups:   StartYear, name [6]
    ##   StartYear         name continent      long      lat num_conflicts
    ##       <ord>        <chr>     <chr>     <dbl>    <dbl>         <int>
    ## 1 1400-1499  afghanistan      asia 67.709953 33.93911             2
    ## 2 1400-1499      albania    europe 20.168331 41.15333             3
    ## 3 1400-1499      algeria    africa  1.659626 28.03389             1
    ## 4 1400-1499      austria    europe 14.550072 47.51623             3
    ## 5 1400-1499        benin    africa  2.315834  9.30769             1
    ## 6 1400-1499 burkina faso    africa -1.561593 12.23833             1

Now we take the computed numbers and overlay them, via ggplot, on our world map. To do this, we need to use the geom\_map function and pass in our world\_map as the data and as the map. Notice in the aes() function we pass in the map\_id = region argument. After that, it's like building any old ggplot2 chart.

``` r
  ggplot()+
  geom_map(data=world_map, map=world_map,aes(map_id = region),
           fill="white", color="black")+
  geom_point(data=year_conf,
             aes(long, lat, size=num_conflicts),color='red', alpha=.5)+
  theme_minimal()+
  labs(title='Violent conflicts since 1400 AD')+
  guides(size=FALSE)+
  facet_wrap(~ StartYear)+
  labs(x='', y='')
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-9-1.png)

We see that before Europe expanded to the Americas there were very few violent conflicts outside of Europe (besides China). As Europeans arrived in North and South America, more conflicts arose. Interestingly, in the 17th century, fewer new conflicts occurred in the New World, though Russia and West Africa experienced conflicts. Then in the 18th century, mroe and more violence broke out in North and South America. In the 19th century, the Americas became a powder keg for military conflict. At the same time, Europe and Africa blew up. Finally in the 20th century we see a pretty even distribution of conflicts all over the map. The Middle East also seems to have grown more violent (as measured by number of conflicts).

Notice that the increase in conflicts over the centuries is probably due to better recording of conflicts, not simply because people are more violent.

If we want a better map, we can use our Google map and limit the area to Europe.

``` r
eu_conf <- year_conf %>%
  filter(continent=='europe')

world_map_eur+
  geom_point(data=eu_conf,
             aes(long, lat, size=num_conflicts),color='red')+
  theme_minimal()+
  labs(title='Violent conflicts since 1400 AD')+
  guides(size=FALSE)+
  facet_wrap(~ StartYear)
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-10-1.png)

It looks like the 19th century was a particularly bloody time in Europe, with the Napoleonic wars probably contributing to most of that.

Closer look: Conflicts in the 20th century
==========================================

Let's say we are interested in examining conflicts in the 20th century. We will do the same process for cutting and relabeling the dates.

``` r
  df1 <- read.csv('conflictcsv.csv', stringsAsFactors = FALSE)
df1$Name <- tolower(df1$Name)
```
``` r
#great little package that includes regex examples for extracting countries
codes <- tolower(countrycode::codelist$country.name.en)

  
#FIRST CREATE A DF OF ONLY 1900+ EVENTS
#803 obs
df_1900 <- df1 %>%
  filter(StartYear >= 1900)

#Have to use 1 fewer label than the breaks. so binned from 1990+
df_1900$StartYear <-  cut(df_1900$StartYear, 
                          breaks = c(1900,1910, 1920,1930,1940,1950,1960,1970,1980,1990,2001),
      labels = c('1900-1909', '1910-1919', '1920-1929', '1930-1939',
                 '1940-1949', '1950-1959', '1960-1969', '1970-1979', '1980-1989',
                 '1990+'),
      right=F, ordered_result = T)
head(df_1900$StartYear)
```
    ## [1] 1900-1909 1900-1909 1900-1909 1900-1909 1900-1909 1900-1909
    ## 10 Levels: 1900-1909 < 1910-1919 < 1920-1929 < 1930-1939 < ... < 1990+

We do the same thing again: use regular expressions to find matches from our vector of country names and then create a new column with the matched value.

``` r
#First extract names from the list of Name field
#430 missing this wway
df_1900 <- df_1900 %>%
  mutate(country = (str_extract(df_1900$Name, paste(codes, collapse="|"))))


#need continent and region labels for plotting. Have to join from other dataframe.
df_1900 <- df_1900 %>%
  rename('country.name.en' = country)%>%
  inner_join(cl[,c('region','continent', 'country.name.en')], by='country.name.en')%>%
  rename('country' = country.name.en)

#Now append long and lat from google
#Now we have country, continent, region, and lat longitudes
df_1900 <- df_1900 %>%
  rename('name' = country)%>%
  inner_join(tab[,c('latitude','longitude','name')], by='name')
```

If we want to look at continent conflict numbers in the 20th century, we simply group by country and StartYear.

``` r
#get conflicts by country
conf_1900 <- df_1900 %>%
  group_by(StartYear, name)%>%
  summarize(long = mean(longitude, na.rm=T),
            lat = mean(latitude, na.rm=T),
            num_conflicts = n())
head(conf_1900)
```
    ## # A tibble: 6 x 5
    ## # Groups:   StartYear [1]
    ##   StartYear     name      long        lat num_conflicts
    ##       <ord>    <chr>     <dbl>      <dbl>         <int>
    ## 1 1900-1909   angola  17.87389 -11.202692             1
    ## 2 1900-1909   brazil -51.92528 -14.235004             1
    ## 3 1900-1909  burundi  29.91889  -3.373056             2
    ## 4 1900-1909 cameroon  12.35472   7.369722             1
    ## 5 1900-1909    china 104.19540  35.861660             3
    ## 6 1900-1909 colombia -74.29733   4.570868             1

``` r
#20th Century Conflicts
ggplot()+
  geom_map(data=world_map, map=world_map,aes(map_id = region),
           fill="white", color="black")+
  geom_point(data=conf_1900,
             aes(long, lat, size=num_conflicts),color='red', alpha=.5)+
  theme_minimal()+
  labs(title='Violent Conflicts in the 20th Century')+
  guides(size=FALSE)+
  facet_wrap(~ StartYear)+
  labs(x='', y='')
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-13-1.png)

This chart has a wealth of information. Starting in the first decade we see most conflicts in Europe, central Africa, and Russia. Then WWI breaks out in Europe and Northern Africa. At the same time, there's a civil war in China and Russia that will eventually leave the Communists in power and will influence international geopolitics for the next 70+ years. Note that in 1911 the Republic of China was established, ending the millenia of rule by emperor. In the 1920s we see relative calm, except in China where civil war broke out in the late 1920s and led to the KMT fleeing to Taiwan and the Communists ruling Chinese politics until the present day.

The outbreak of WWII is reflected in the 1930s by a fairly global distribution of conflicts. The 1950s saw the Korean war. At this point the Cold War was at its peak: you can see the emergence of the "Third World" in Africa--countries that were aligned with neither the USA nor the Soviet Union. Lots of conflicts cropped up in Africa at this time. In the 1970s East Africa became a hotbed, and unfortunately I don't know much about these events. Could it be related to the change of Rhodesia to Zimbabwe? It might be. 

The 1980s see an explosion of conflict in Central America: this is probably related to the USA's involvement in the region's politics, including the Iran-Contra scandal that Reagan resided over. In essence the US was arming the Contras in Nicaragua using money from arms sales to Iran, which at the time, was under embargo for the taking of US hostages in 1979. Check out the Ben Affleck movie *Argo* if you're interested in learning more about the event (or read a book!). Finally, the 1990s saw an explosion of conflict predominantly in the Middle East and what appears to be India. Unfortunately I don't know enough about Indian history to explain what was happening here.

So we've seen the number of conflicts, what about the total deaths from these conflicts?

``` r
library(ggrepel)
# Take top 30 to plot names
df_conflicts <- df %>%
  filter(!is.na(TotalFatalities))%>%
  filter(!Common.Name %in% c('', '?'))%>%
  select(Common.Name, latitude, longitude, TotalFatalities)%>%
  arrange(desc(TotalFatalities))%>%
  slice(1:50)
head(cbind(df_conflicts$Common.Name, df_conflicts$TotalFatalities))
```

    ##      [,1]                            [,2]     
    ## [1,] "Genocide of Jews"              "6000000"
    ## [2,] "Taiping Rebellion"             "2000000"
    ## [3,] "War of the Spanish Revolution" "2000000"
    ## [4,] "Sino-Japanese War"             "2000000"
    ## [5,] "Politicide of defeated"        "1900000"
    ## [6,] "Politicide of opponents"       "1900000"

``` r
ggplot()+
  geom_map(data=world_map, map=world_map,aes(map_id = region),
           fill="white", color="light grey")+
  geom_text_repel(data=df_conflicts,
             aes(longitude, latitude, label=Common.Name, size=TotalFatalities),
             segment.alpha = .6, segment.color = 'black',
             color='red')+
  expand_limits(x=-80, y=-30)+
  theme_minimal()+
  labs(title='Name and Location of Major Violent Conflicts since 1400 AD')+
  guides(size=FALSE)
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-14-1.png) 

We can see the genocide of the Jews, the Spanish Revolution, the Chinese Civil War, and a few other conflicts make up the worst conflicts in terms of total deaths. It's interesting to see the Indonesia genocide of Chinese and Communists, which started under Sukarno and led to the ascension of Suharto, who would control Indonesia for the next 30 years. I don't think this event gets enough attention in world history. And again, it shows how the Cold War shaped the 20th century.

Using regular expressions to find attackers and defenders in conflicts
======================================================================

One question we might ask is, Which countries seem to attack other countries? And, Which countries have lots of domestic conflicts?

To do this (and the method is not perfect, but good enough), we need to use regular expressions to extract out the names from the 'Name' column. A useful bit of info from the data curator's website says that if there was an aggressor, it is listed on the left side of the hyphen and the country/nation/state under attack is on the right. So for example, element three says that Russia attacked Lithuania in 1401. Here's a sample of the names:

``` r
head(df$Name)
```

    ## [1] "china (minority in hebei, shandong, anhui, jiangsu), 1400-02"
    ## [2] "hungary, 1401"                                               
    ## [3] "russia-lithuania, 1401-04"                                   
    ## [4] "mujapahit (india, civil war), 1401-06"                       
    ## [5] "china-japanese pirates (zhejiang), 1401"                     
    ## [6] "spain-canary islands, 1402-22"

You can already see some problems: not all elements have attackers and defenders (some are domestic conflicts). Also, if you look carefully, there are two hyphens: one for the countries and another used in the date. So we have to be careful about which hyphen we are extracting around.

With a bit of trial and error (and StackOverflow), I managed to come up with two regexes that work pretty well to capture the attacker and defender.

Basically the first regex is going to capture any characters in the range a-z up until it hits a non-alphabetic symbol, i.e., () or -.

The second uses what's called a "lookaround" and character classes (\\w). It is looking for one or more word characters (\\w+), preceeded by a hyphen and a word character. It might be easier to understand if you read it from right to left.

``` r
df <- df %>%
  mutate(attacker = str_extract(df$Name,'[a-z]+'))
df <- df %>%
  mutate(defender = str_extract(df$Name, '(?<=\\w-)\\w+'))
head(cbind(df$attacker, df$defender))
```

    ##      [,1]        [,2]       
    ## [1,] "china"     "02"       
    ## [2,] "hungary"   NA         
    ## [3,] "russia"    "lithuania"
    ## [4,] "mujapahit" "06"       
    ## [5,] "china"     "japanese" 
    ## [6,] "spain"     "canary"

The problem we have now is that some conflicts don't have foreign defenders. So we will need to first look for any NA values or numeric values that were incorrectly extracted from our regex and convert them to the string 'domestic.'

One useful technique is using lapply when you want to replace one value in one particular column or group of columns. If your function is more complicated than a one-liner, you can write the function then use lapply to target the particular column of interest.

First we replace NA values with 'domestic.' Then we replace digits with 'domestic' (indicating there was no 2nd country to extract a name from).

``` r
#HOW TO REPLACE NAs IN JUST ONE COLLUMN ? lapply
df$defender <- lapply(df[,'defender'], function(x){replace(x, is.na(x), 'domestic')})

#easier to just name them domestic
check_char <- function(x){
  if (str_detect(x, '\\d')==TRUE){
    x <- 'domestic'
  } else{
    x
  }
}

#use sapply to avoid getting a list for the column
df$defender <- sapply(df[,'defender'], check_char)

#Now this looks much better
head(cbind(df$attacker, df$defender))
```

    ##      [,1]        [,2]       
    ## [1,] "china"     "domestic" 
    ## [2,] "hungary"   "domestic" 
    ## [3,] "russia"    "lithuania"
    ## [4,] "mujapahit" "domestic" 
    ## [5,] "china"     "japanese" 
    ## [6,] "spain"     "canary"

``` r
#Another useful way to replace specific values: str_replace_all()
#df$defender <- str_replace_all(df$defender, 'None', 'domestic')
```

Which countries have the most domestic conflicts?

``` r
df %>%
  count(attacker, defender, sort=T)%>%
  head(5)
```

    ## # A tibble: 5 x 3
    ##   attacker defender     n
    ##      <chr>    <chr> <int>
    ## 1    china domestic   320
    ## 2   france domestic    62
    ## 3    japan domestic    62
    ## 4    spain domestic    54
    ## 5   turkey domestic    48

What about which countries have the biggest rivalries, measured in terms of the number of conflicts between them? And in which centuries did these rivalries take place?

``` r
df %>%
  filter(defender != 'domestic')%>%
  unite(title, 'attacker', 'defender', sep=' vs.')%>%
  count(title,  sort=T)%>%
  slice(1:40)%>%
  ggplot(aes(reorder(title,n),n, fill=n))+
  geom_col()+
  coord_flip()+ 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.y = element_text(size = 6), 
    plot.title = element_text(size = 11)) +labs(x = "Warring Countries", y = "# of Conflicts", 
    fill = "Century")+
    scale_fill_distiller(palette = 'Spectral', direction = -1)+
    theme_minimal()+
  guides(fill=FALSE) +
  labs(title='Most violent rivalries by # conflicts')
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-19-1.png) 

We can see China and the Tartars have had many conflicts over the years. We can also understand why there is hostility between Japan and China and France and England. There's definitely some lesser-known conflicts that would be worth learning more about, for example Oman and Portugal or Russia and the Buryats, which are a group of people indigenous to Siberia.

Let's see how the rankings change when we look at total fatalities. Keep in mind we don't have a lot of data on total fatalities. These rankings are only based on the non-NA values in the dataset.

``` r
#Same thing but by total fatalities
df %>%
  filter(!is.na(TotalFatalities))%>%
  filter(defender != 'domestic')%>%
  unite(title, 'attacker', 'defender', sep=' vs.')%>%
  group_by(title)%>%
  summarize(total_deaths = sum(TotalFatalities))%>%
  slice(1:40)%>%
  ggplot(aes(reorder(title,total_deaths),total_deaths, fill=total_deaths))+
  geom_col()+
  coord_flip()+ 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.y = element_text(size = 6), 
    plot.title = element_text(size = 11)) +labs(x = "Warring Countries", y = "# of Deaths", 
    fill = "Century")+
    scale_fill_distiller(palette = 'Spectral', direction = -1)+
    theme_minimal()+
  guides(fill=FALSE) +
  labs(title='Most violent rivalries by # deaths')
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-20-1.png)

Given our limited death data, we can see the effects of the Napoleonic wars on Europe with Austria and France being intense rivals. Lesser-known (to me at least) is China and the Dzoungars. According to Wikipedia, this was a mass genocide of Buddhist people living in Mongolia under the Manchu dynasty.

What about domestic conflicts: revolutions, civil wars, genocides, rebellions, etc.?

``` r
df %>%
  filter(defender == 'domestic')%>%
  filter(attacker != 'south')%>%
  count(attacker,defender, sort=T)%>%
  slice(1:40)%>%
  ggplot(aes(reorder(attacker,n),n, fill=n))+
  geom_col()+
  coord_flip() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.y = element_text(size = 7), 
    plot.title = element_text(size = 11)) +
    labs(x = "Country", y = "# of Domestic Conflicts")+
    scale_fill_distiller(palette = 'Spectral', direction = -1)+
    theme_minimal()+
    guides(fill=FALSE)
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-21-1.png) 

China and Japan have dealt with many domestic conflicts over the years.

Maybe we're interested in the countries whose conflicts have produced the most deaths. Keep in mind our regex was not perfect, so some of these results should be taken with a grain of salt.

``` r
#total deaths (all types)
df %>%
  filter(!attacker %in% c('south', 'north'))%>%
  filter(!is.na(TotalFatalities))%>%
  count(TotalFatalities,attacker,defender,StartYear, sort=T)%>%
  filter(TotalFatalities > 100000)%>%
  ggplot(aes(reorder(attacker,TotalFatalities),TotalFatalities, fill=StartYear))+
  geom_col()+
  coord_flip() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.y = element_text(size = 7), 
    plot.title = element_text(size = 11)) +labs(x = "Country", y = "Total Deaths (all types)", 
    fill = "Century")+
    scale_y_continuous(labels = scales::comma)+
    theme_minimal()
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-22-1.png) 

The reason Japan and Germany are listed so high is because they are being counted as "attackers" in WWII, which explains the massive amount of deaths in the 20th century. It's interesting to see that 400 years ago, Denmark was a pretty violent place, and so was France a couple hundred years ago.

The deadliest domestic conflicts?

``` r
#total deaths (domestic) : keep in mind we don't have numbers for a lot o conflicts.
df %>%
  filter(defender == 'domestic')%>%
  filter(!attacker %in% c('south', 'north', 'el'))%>%
  filter(!is.na(TotalFatalities))%>%
  group_by(attacker, defender, StartYear)%>%
  summarize(cts = sum(TotalFatalities))%>%
  filter(cts > 70000)%>%
  slice(1:40)%>%
  ggplot(aes(reorder(attacker,cts),cts, fill=StartYear))+
  geom_col()+
  coord_flip() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(size = 5), 
    plot.title = element_text(size = 11))+
    labs(x = "Country", y = "Total Deaths", 
    fill = "Century", title= 'Total Deaths in Domestic Conflicts')+
    scale_y_continuous(labels = scales::comma)+
    theme_minimal()
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-23-1.png) 

Poland had some domestic conflicts several hundred years ago, and same with the Moghuls in present day India. You can also see Portugal's violent role in the European colonization of Asia in the 15-17th centuries.

Using Plotly to make Interactive Visualizations
===============================================

To use plotly with ggplot2 is easy: you just create your normal ggplot object, save it to a variable, then pass it to ggplotly(). Voila, you have an interactive plot that can be scrolled through, zoomed in on, and filtered on using different variables.

I want to look at the number of conflicts over time for each continent. We'll use lubridate's ymd() and floor\_date() to aggregate into 10 year blocks.

The truncated parameter of ymd() is a very useful in that it checks for incomplete dates. Notice that it assumes January 1st for each year. That's good enough for us (sorry history buffs). Once we have the date integers converted into datetime objects, we can then use the floor\_date() function to aggregate them.

``` r
library(lubridate)
yr_conflicts <- df %>%
  mutate(StartYear = ymd(StartYear, truncated = 2))%>%
  mutate(StartYear = floor_date(StartYear, unit = '10 years'))%>%
  group_by(continent, StartYear)%>%
  summarize(cts = n())%>%
  ggplot(aes(StartYear, cts, group=continent, color=continent))+
  geom_line()+
  theme_minimal()
yr_conflicts
```

![](globalconf_files/figure-markdown_github/unnamed-chunk-25-1.png)

To show off plotly, I'll do the same plot but render it using plotly with the ggplotly() function. Take a minute to play around with the filtering (click on the legend to deactivate a category).

*I'm not sure why, but twice RStudio has crashed while I've been playing around with the interactive graph*

``` r
yr_conflicts <- df %>%
  mutate(StartYear = ymd(StartYear, truncated = 2))%>%
  mutate(StartYear = floor_date(StartYear, unit = '10 years'))%>%
  group_by(continent, StartYear)%>%
  summarize(cts = n())%>%
  ggplot(aes(StartYear, cts, group=continent, color=continent))+
  geom_line()+
  labs(x='Year of Conflict', y='Number of Conflicts')+
  theme_minimal()

ggplotly(yr_conflicts)
```

<!--html_preserve-->

<script type="application/json" data-for="6123419a9ec6">{"x":{"data":[{"x":[-17671910400000,-17356377600000,-17040758400000,-16725225600000,-16409606400000,-16094073600000,-15778454400000,-15462921600000,-15147302400000,-14831769600000,-14516236800000,-14200704000000,-13885084800000,-13569552000000,-13253932800000,-12938400000000,-12622780800000,-12307248000000,-11991628800000,-11676096000000,-11360476800000,-11044944000000,-10729324800000,-10413792000000,-10098172800000,-9782640000000,-9467020800000,-9151488000000,-8835868800000,-8520336000000,-7889270400000,-7573651200000,-7258118400000,-6942499200000,-6626966400000,-6311347200000,-5995814400000,-5680195200000,-5364662400000,-5049129600000,-4733596800000,-4417977600000,-4102444800000,-3786825600000,-3471292800000,-3155673600000,-2840140800000,-2524521600000,-2208988800000,-1893456000000,-1577923200000,-1262304000000,-946771200000,-631152000000,-315619200000,0,315532800000,631152000000,946684800000],"y":[3,4,3,1,1,2,4,2,4,4,5,3,1,2,6,7,4,3,3,4,2,2,4,3,2,3,4,3,6,2,7,4,3,2,3,8,6,3,7,10,12,12,7,12,15,16,22,17,10,14,4,2,4,15,42,20,21,22,1],"text":["StartYear: -204536<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -200884<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -197231<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -193579<br />cts:  1<br />continent: africa<br />continent: africa","StartYear: -189926<br />cts:  1<br />continent: africa<br />continent: africa","StartYear: -186274<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -182621<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -178969<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -175316<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -171664<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -168012<br />cts:  5<br />continent: africa<br />continent: africa","StartYear: -164360<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -160707<br />cts:  1<br />continent: africa<br />continent: africa","StartYear: -157055<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -153402<br />cts:  6<br />continent: africa<br />continent: africa","StartYear: -149750<br />cts:  7<br />continent: africa<br />continent: africa","StartYear: -146097<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -142445<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -138792<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -135140<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -131487<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -127835<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -124182<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -120530<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -116877<br />cts:  2<br />continent: africa<br />continent: africa","StartYear: -113225<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -109572<br />cts:  4<br />continent: africa<br />continent: africa","StartYear: -105920<br />cts:  3<br />continent: africa<br />continent: africa","StartYear: -102267<br />cts:  6<br />continent: africa<br />continent: africa","StartYear:  -98615<br />cts:  2<br />continent: africa<br />continent: africa","StartYear:  -91311<br />cts:  7<br />continent: africa<br />continent: africa","StartYear:  -87658<br />cts:  4<br />continent: africa<br />continent: africa","StartYear:  -84006<br />cts:  3<br />continent: africa<br />continent: africa","StartYear:  -80353<br />cts:  2<br />continent: africa<br />continent: africa","StartYear:  -76701<br />cts:  3<br />continent: africa<br />continent: africa","StartYear:  -73048<br />cts:  8<br />continent: africa<br />continent: africa","StartYear:  -69396<br />cts:  6<br />continent: africa<br />continent: africa","StartYear:  -65743<br />cts:  3<br />continent: africa<br />continent: africa","StartYear:  -62091<br />cts:  7<br />continent: africa<br />continent: africa","StartYear:  -58439<br />cts: 10<br />continent: africa<br />continent: africa","StartYear:  -54787<br />cts: 12<br />continent: africa<br />continent: africa","StartYear:  -51134<br />cts: 12<br />continent: africa<br />continent: africa","StartYear:  -47482<br />cts:  7<br />continent: africa<br />continent: africa","StartYear:  -43829<br />cts: 12<br />continent: africa<br />continent: africa","StartYear:  -40177<br />cts: 15<br />continent: africa<br />continent: africa","StartYear:  -36524<br />cts: 16<br />continent: africa<br />continent: africa","StartYear:  -32872<br />cts: 22<br />continent: africa<br />continent: africa","StartYear:  -29219<br />cts: 17<br />continent: africa<br />continent: africa","StartYear:  -25567<br />cts: 10<br />continent: africa<br />continent: africa","StartYear:  -21915<br />cts: 14<br />continent: africa<br />continent: africa","StartYear:  -18263<br />cts:  4<br />continent: africa<br />continent: africa","StartYear:  -14610<br />cts:  2<br />continent: africa<br />continent: africa","StartYear:  -10958<br />cts:  4<br />continent: africa<br />continent: africa","StartYear:   -7305<br />cts: 15<br />continent: africa<br />continent: africa","StartYear:   -3653<br />cts: 42<br />continent: africa<br />continent: africa","StartYear:       0<br />cts: 20<br />continent: africa<br />continent: africa","StartYear:    3652<br />cts: 21<br />continent: africa<br />continent: africa","StartYear:    7305<br />cts: 22<br />continent: africa<br />continent: africa","StartYear:   10957<br />cts:  1<br />continent: africa<br />continent: africa"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"africa","legendgroup":"africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-16725225600000,-14516236800000,-14200704000000,-13885084800000,-13569552000000,-11991628800000,-7889270400000,-7573651200000,-7258118400000,-6942499200000,-5995814400000,-5680195200000,-5364662400000,-5049129600000,-4733596800000,-4417977600000,-4102444800000,-3786825600000,-3471292800000,-3155673600000,-2840140800000,-2524521600000,-2208988800000,-1893456000000,-1577923200000,-1262304000000,-946771200000,-631152000000,-315619200000,0,315532800000,631152000000],"y":[1,1,1,1,2,1,1,1,1,1,1,1,4,12,9,11,13,17,16,10,7,12,10,10,6,13,9,11,17,7,12,3],"text":["StartYear: -193579<br />cts:  1<br />continent: americas<br />continent: americas","StartYear: -168012<br />cts:  1<br />continent: americas<br />continent: americas","StartYear: -164360<br />cts:  1<br />continent: americas<br />continent: americas","StartYear: -160707<br />cts:  1<br />continent: americas<br />continent: americas","StartYear: -157055<br />cts:  2<br />continent: americas<br />continent: americas","StartYear: -138792<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -91311<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -87658<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -84006<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -80353<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -69396<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -65743<br />cts:  1<br />continent: americas<br />continent: americas","StartYear:  -62091<br />cts:  4<br />continent: americas<br />continent: americas","StartYear:  -58439<br />cts: 12<br />continent: americas<br />continent: americas","StartYear:  -54787<br />cts:  9<br />continent: americas<br />continent: americas","StartYear:  -51134<br />cts: 11<br />continent: americas<br />continent: americas","StartYear:  -47482<br />cts: 13<br />continent: americas<br />continent: americas","StartYear:  -43829<br />cts: 17<br />continent: americas<br />continent: americas","StartYear:  -40177<br />cts: 16<br />continent: americas<br />continent: americas","StartYear:  -36524<br />cts: 10<br />continent: americas<br />continent: americas","StartYear:  -32872<br />cts:  7<br />continent: americas<br />continent: americas","StartYear:  -29219<br />cts: 12<br />continent: americas<br />continent: americas","StartYear:  -25567<br />cts: 10<br />continent: americas<br />continent: americas","StartYear:  -21915<br />cts: 10<br />continent: americas<br />continent: americas","StartYear:  -18263<br />cts:  6<br />continent: americas<br />continent: americas","StartYear:  -14610<br />cts: 13<br />continent: americas<br />continent: americas","StartYear:  -10958<br />cts:  9<br />continent: americas<br />continent: americas","StartYear:   -7305<br />cts: 11<br />continent: americas<br />continent: americas","StartYear:   -3653<br />cts: 17<br />continent: americas<br />continent: americas","StartYear:       0<br />cts:  7<br />continent: americas<br />continent: americas","StartYear:    3652<br />cts: 12<br />continent: americas<br />continent: americas","StartYear:    7305<br />cts:  3<br />continent: americas<br />continent: americas"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(163,165,0,1)","dash":"solid"},"hoveron":"points","name":"americas","legendgroup":"americas","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-17987443200000,-17671910400000,-17356377600000,-17040758400000,-16725225600000,-16409606400000,-16094073600000,-15778454400000,-15462921600000,-15147302400000,-14831769600000,-14516236800000,-14200704000000,-13885084800000,-13569552000000,-13253932800000,-12938400000000,-12622780800000,-12307248000000,-11991628800000,-11676096000000,-11360476800000,-11044944000000,-10729324800000,-10413792000000,-10098172800000,-9782640000000,-9467020800000,-9151488000000,-8835868800000,-8520336000000,-8204803200000,-7889270400000,-7573651200000,-7258118400000,-6942499200000,-6626966400000,-6311347200000,-5995814400000,-5680195200000,-5364662400000,-5049129600000,-4733596800000,-4417977600000,-4102444800000,-3786825600000,-3471292800000,-3155673600000,-2840140800000,-2524521600000,-2208988800000,-1893456000000,-1577923200000,-1262304000000,-946771200000,-631152000000,-315619200000,0,315532800000,631152000000,946684800000],"y":[20,23,18,17,21,25,30,27,16,18,21,33,34,17,25,29,25,28,29,21,5,11,11,8,5,6,5,8,6,4,3,10,9,5,9,7,6,7,7,10,10,6,14,13,8,14,19,12,11,9,8,19,26,27,25,37,41,27,30,29,1],"text":["StartYear: -208188<br />cts: 20<br />continent: asia<br />continent: asia","StartYear: -204536<br />cts: 23<br />continent: asia<br />continent: asia","StartYear: -200884<br />cts: 18<br />continent: asia<br />continent: asia","StartYear: -197231<br />cts: 17<br />continent: asia<br />continent: asia","StartYear: -193579<br />cts: 21<br />continent: asia<br />continent: asia","StartYear: -189926<br />cts: 25<br />continent: asia<br />continent: asia","StartYear: -186274<br />cts: 30<br />continent: asia<br />continent: asia","StartYear: -182621<br />cts: 27<br />continent: asia<br />continent: asia","StartYear: -178969<br />cts: 16<br />continent: asia<br />continent: asia","StartYear: -175316<br />cts: 18<br />continent: asia<br />continent: asia","StartYear: -171664<br />cts: 21<br />continent: asia<br />continent: asia","StartYear: -168012<br />cts: 33<br />continent: asia<br />continent: asia","StartYear: -164360<br />cts: 34<br />continent: asia<br />continent: asia","StartYear: -160707<br />cts: 17<br />continent: asia<br />continent: asia","StartYear: -157055<br />cts: 25<br />continent: asia<br />continent: asia","StartYear: -153402<br />cts: 29<br />continent: asia<br />continent: asia","StartYear: -149750<br />cts: 25<br />continent: asia<br />continent: asia","StartYear: -146097<br />cts: 28<br />continent: asia<br />continent: asia","StartYear: -142445<br />cts: 29<br />continent: asia<br />continent: asia","StartYear: -138792<br />cts: 21<br />continent: asia<br />continent: asia","StartYear: -135140<br />cts:  5<br />continent: asia<br />continent: asia","StartYear: -131487<br />cts: 11<br />continent: asia<br />continent: asia","StartYear: -127835<br />cts: 11<br />continent: asia<br />continent: asia","StartYear: -124182<br />cts:  8<br />continent: asia<br />continent: asia","StartYear: -120530<br />cts:  5<br />continent: asia<br />continent: asia","StartYear: -116877<br />cts:  6<br />continent: asia<br />continent: asia","StartYear: -113225<br />cts:  5<br />continent: asia<br />continent: asia","StartYear: -109572<br />cts:  8<br />continent: asia<br />continent: asia","StartYear: -105920<br />cts:  6<br />continent: asia<br />continent: asia","StartYear: -102267<br />cts:  4<br />continent: asia<br />continent: asia","StartYear:  -98615<br />cts:  3<br />continent: asia<br />continent: asia","StartYear:  -94963<br />cts: 10<br />continent: asia<br />continent: asia","StartYear:  -91311<br />cts:  9<br />continent: asia<br />continent: asia","StartYear:  -87658<br />cts:  5<br />continent: asia<br />continent: asia","StartYear:  -84006<br />cts:  9<br />continent: asia<br />continent: asia","StartYear:  -80353<br />cts:  7<br />continent: asia<br />continent: asia","StartYear:  -76701<br />cts:  6<br />continent: asia<br />continent: asia","StartYear:  -73048<br />cts:  7<br />continent: asia<br />continent: asia","StartYear:  -69396<br />cts:  7<br />continent: asia<br />continent: asia","StartYear:  -65743<br />cts: 10<br />continent: asia<br />continent: asia","StartYear:  -62091<br />cts: 10<br />continent: asia<br />continent: asia","StartYear:  -58439<br />cts:  6<br />continent: asia<br />continent: asia","StartYear:  -54787<br />cts: 14<br />continent: asia<br />continent: asia","StartYear:  -51134<br />cts: 13<br />continent: asia<br />continent: asia","StartYear:  -47482<br />cts:  8<br />continent: asia<br />continent: asia","StartYear:  -43829<br />cts: 14<br />continent: asia<br />continent: asia","StartYear:  -40177<br />cts: 19<br />continent: asia<br />continent: asia","StartYear:  -36524<br />cts: 12<br />continent: asia<br />continent: asia","StartYear:  -32872<br />cts: 11<br />continent: asia<br />continent: asia","StartYear:  -29219<br />cts:  9<br />continent: asia<br />continent: asia","StartYear:  -25567<br />cts:  8<br />continent: asia<br />continent: asia","StartYear:  -21915<br />cts: 19<br />continent: asia<br />continent: asia","StartYear:  -18263<br />cts: 26<br />continent: asia<br />continent: asia","StartYear:  -14610<br />cts: 27<br />continent: asia<br />continent: asia","StartYear:  -10958<br />cts: 25<br />continent: asia<br />continent: asia","StartYear:   -7305<br />cts: 37<br />continent: asia<br />continent: asia","StartYear:   -3653<br />cts: 41<br />continent: asia<br />continent: asia","StartYear:       0<br />cts: 27<br />continent: asia<br />continent: asia","StartYear:    3652<br />cts: 30<br />continent: asia<br />continent: asia","StartYear:    7305<br />cts: 29<br />continent: asia<br />continent: asia","StartYear:   10957<br />cts:  1<br />continent: asia<br />continent: asia"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,191,125,1)","dash":"solid"},"hoveron":"points","name":"asia","legendgroup":"asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-17987443200000,-17671910400000,-17356377600000,-17040758400000,-16725225600000,-16409606400000,-16094073600000,-15778454400000,-15462921600000,-15147302400000,-14831769600000,-14516236800000,-14200704000000,-13885084800000,-13569552000000,-13253932800000,-12938400000000,-12622780800000,-12307248000000,-11991628800000,-11676096000000,-11360476800000,-11044944000000,-10729324800000,-10413792000000,-10098172800000,-9782640000000,-9467020800000,-9151488000000,-8835868800000,-8520336000000,-8204803200000,-7889270400000,-7573651200000,-7258118400000,-6942499200000,-6626966400000,-6311347200000,-5995814400000,-5680195200000,-5364662400000,-5049129600000,-4733596800000,-4417977600000,-4102444800000,-3786825600000,-3471292800000,-3155673600000,-2840140800000,-2524521600000,-2208988800000,-1893456000000,-1577923200000,-1262304000000,-946771200000,-631152000000,-315619200000,0,315532800000,631152000000],"y":[18,8,2,9,11,9,15,14,7,19,25,32,23,20,11,13,21,21,14,24,24,29,27,35,27,26,23,26,26,18,23,16,11,9,14,10,9,11,11,16,15,8,21,26,31,24,23,25,25,44,39,56,18,22,14,4,3,2,1,7],"text":["StartYear: -208188<br />cts: 18<br />continent: europe<br />continent: europe","StartYear: -204536<br />cts:  8<br />continent: europe<br />continent: europe","StartYear: -200884<br />cts:  2<br />continent: europe<br />continent: europe","StartYear: -197231<br />cts:  9<br />continent: europe<br />continent: europe","StartYear: -193579<br />cts: 11<br />continent: europe<br />continent: europe","StartYear: -189926<br />cts:  9<br />continent: europe<br />continent: europe","StartYear: -186274<br />cts: 15<br />continent: europe<br />continent: europe","StartYear: -182621<br />cts: 14<br />continent: europe<br />continent: europe","StartYear: -178969<br />cts:  7<br />continent: europe<br />continent: europe","StartYear: -175316<br />cts: 19<br />continent: europe<br />continent: europe","StartYear: -171664<br />cts: 25<br />continent: europe<br />continent: europe","StartYear: -168012<br />cts: 32<br />continent: europe<br />continent: europe","StartYear: -164360<br />cts: 23<br />continent: europe<br />continent: europe","StartYear: -160707<br />cts: 20<br />continent: europe<br />continent: europe","StartYear: -157055<br />cts: 11<br />continent: europe<br />continent: europe","StartYear: -153402<br />cts: 13<br />continent: europe<br />continent: europe","StartYear: -149750<br />cts: 21<br />continent: europe<br />continent: europe","StartYear: -146097<br />cts: 21<br />continent: europe<br />continent: europe","StartYear: -142445<br />cts: 14<br />continent: europe<br />continent: europe","StartYear: -138792<br />cts: 24<br />continent: europe<br />continent: europe","StartYear: -135140<br />cts: 24<br />continent: europe<br />continent: europe","StartYear: -131487<br />cts: 29<br />continent: europe<br />continent: europe","StartYear: -127835<br />cts: 27<br />continent: europe<br />continent: europe","StartYear: -124182<br />cts: 35<br />continent: europe<br />continent: europe","StartYear: -120530<br />cts: 27<br />continent: europe<br />continent: europe","StartYear: -116877<br />cts: 26<br />continent: europe<br />continent: europe","StartYear: -113225<br />cts: 23<br />continent: europe<br />continent: europe","StartYear: -109572<br />cts: 26<br />continent: europe<br />continent: europe","StartYear: -105920<br />cts: 26<br />continent: europe<br />continent: europe","StartYear: -102267<br />cts: 18<br />continent: europe<br />continent: europe","StartYear:  -98615<br />cts: 23<br />continent: europe<br />continent: europe","StartYear:  -94963<br />cts: 16<br />continent: europe<br />continent: europe","StartYear:  -91311<br />cts: 11<br />continent: europe<br />continent: europe","StartYear:  -87658<br />cts:  9<br />continent: europe<br />continent: europe","StartYear:  -84006<br />cts: 14<br />continent: europe<br />continent: europe","StartYear:  -80353<br />cts: 10<br />continent: europe<br />continent: europe","StartYear:  -76701<br />cts:  9<br />continent: europe<br />continent: europe","StartYear:  -73048<br />cts: 11<br />continent: europe<br />continent: europe","StartYear:  -69396<br />cts: 11<br />continent: europe<br />continent: europe","StartYear:  -65743<br />cts: 16<br />continent: europe<br />continent: europe","StartYear:  -62091<br />cts: 15<br />continent: europe<br />continent: europe","StartYear:  -58439<br />cts:  8<br />continent: europe<br />continent: europe","StartYear:  -54787<br />cts: 21<br />continent: europe<br />continent: europe","StartYear:  -51134<br />cts: 26<br />continent: europe<br />continent: europe","StartYear:  -47482<br />cts: 31<br />continent: europe<br />continent: europe","StartYear:  -43829<br />cts: 24<br />continent: europe<br />continent: europe","StartYear:  -40177<br />cts: 23<br />continent: europe<br />continent: europe","StartYear:  -36524<br />cts: 25<br />continent: europe<br />continent: europe","StartYear:  -32872<br />cts: 25<br />continent: europe<br />continent: europe","StartYear:  -29219<br />cts: 44<br />continent: europe<br />continent: europe","StartYear:  -25567<br />cts: 39<br />continent: europe<br />continent: europe","StartYear:  -21915<br />cts: 56<br />continent: europe<br />continent: europe","StartYear:  -18263<br />cts: 18<br />continent: europe<br />continent: europe","StartYear:  -14610<br />cts: 22<br />continent: europe<br />continent: europe","StartYear:  -10958<br />cts: 14<br />continent: europe<br />continent: europe","StartYear:   -7305<br />cts:  4<br />continent: europe<br />continent: europe","StartYear:   -3653<br />cts:  3<br />continent: europe<br />continent: europe","StartYear:       0<br />cts:  2<br />continent: europe<br />continent: europe","StartYear:    3652<br />cts:  1<br />continent: europe<br />continent: europe","StartYear:    7305<br />cts:  7<br />continent: europe<br />continent: europe"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,176,246,1)","dash":"solid"},"hoveron":"points","name":"europe","legendgroup":"europe","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-5364662400000,315532800000],"y":[3,1],"text":["StartYear:  -62091<br />cts:  3<br />continent: oceania<br />continent: oceania","StartYear:    3652<br />cts:  1<br />continent: oceania<br />continent: oceania"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(231,107,243,1)","dash":"solid"},"hoveron":"points","name":"oceania","legendgroup":"oceania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-18934149600000,1893391200000],"ticktext":["1500","1700","1900"],"tickvals":[-14831769600000,-8520336000000,-2208988800000],"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"Year of Conflict","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-1.75,58.75],"ticktext":["0","20","40"],"tickvals":[0,20,40],"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Number of Conflicts","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.913385826771654},"annotations":[{"text":"continent","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest"},"source":"A","attrs":{"61234432e5012":{"x":{},"y":{},"colour":{},"type":"ggplotly"}},"cur_data":"61234432e5012","visdat":{"61234432e5012":["function (y) ","x"]},"config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script>
<!--/html_preserve-->
Finally, can we recreate our world map graph using plotly? Let's try. Note: Rendering to Plotly is super computationally intensive and I keep getting an error: "Cannot allocate vector of size 2.3 gb." My computer sounds like it's about to go into shock.


**Note:** *I've managed to at least decrease the size of the original map data frame from 99000 obs (which was too big to use Plotly with) to simply a few hundred. But now the underlying map is not displaying, though you can see the outline of the continents from the conflict locations.*

So that's it for now. The takeaway here is that for normal plotting needs, Plotly works great. But if you have big dataframes of coordinates for world maps, you might need to figure out another way to make interactive graphs.

``` r
world_map_small <- world_map %>%
  mutate(region = tolower(region))%>%
  filter(region %in% df$name)%>%
  group_by(region)%>%
  slice(1)

label_map <-  ggplot()+
  geom_map(data=world_map_small, map=world_map,aes(map_id = region),
           fill="white", color="black")+
  geom_point(data=lat_long_conf,
             aes(long, lat, size=num_conflicts),color='red', alpha=.5)+
  theme_minimal()+
  labs(title='Violent conflicts since 1400 AD')+
  guides(size=FALSE)+
  labs(x='', y='')

ggplotly(label_map)
```

<!--html_preserve-->

<script type="application/json" data-for="612345cc15c08">{"x":{"data":[{"visible":false,"showlegend":false,"xaxis":null,"yaxis":null,"hoverinfo":"text","frame":null},{"x":[67.709953,20.168331,1.659626,17.873887,-63.616672,45.038189,133.775136,14.550072,47.576927,90.356331,4.469936,2.315834,-63.588653,-51.92528,114.727669,25.48583,-1.561593,29.918886,104.990963,12.354722,-106.346771,18.732207,-71.542969,104.195397,-74.297333,43.872219,-83.753428,15.2,-77.781167,33.429859,9.501785,42.590275,-61.370976,-78.183406,30.802498,-88.89653,10.267895,25.013607,40.489673,25.748151,2.213749,11.609444,-15.310139,43.356892,10.451526,-1.023194,21.824312,-90.230759,-9.696645,-72.285215,-86.241905,19.503304,78.96288,113.921327,53.688046,43.679291,-8.24389,34.851612,12.56738,138.252924,36.238414,37.906193,102.495496,24.603189,35.862285,28.233608,-9.429499,17.228331,23.881275,46.869107,34.301525,73.22068,-3.996166,-10.940835,-102.552784,28.369885,103.846656,19.37439,-7.09262,35.529562,18.49041,84.124008,5.291266,-85.207229,8.081666,127.510093,8.468946,55.923255,69.345116,-80.782127,143.95555,-58.443832,-75.015152,121.774017,19.145136,-8.224454,-66.590149,24.96676,105.318756,29.873888,45.079162,-14.452362,21.005859,-11.779889,46.199616,22.937506,127.766922,-3.74922,80.771797,30.217636,18.643501,8.227512,38.996815,34.888822,100.992541,9.537499,35.243322,32.290275,31.16558,-95.712891,-55.765835,64.585262,-66.58973,108.277199,-12.885834,48.516388,27.849332,29.154857],"y":[33.93911,41.153332,28.033886,-11.202692,-38.416097,40.069099,-25.274398,47.516231,40.143105,23.684994,50.503887,9.30769,-16.290154,-14.235004,4.535277,42.733883,12.238333,-3.373056,12.565679,7.369722,56.130366,15.454166,-35.675147,35.86166,4.570868,-11.875001,9.748917,45.1,21.521757,35.126413,56.26392,11.825138,15.414999,-1.831239,26.820553,13.794185,1.650801,58.595272,9.145,61.92411,46.227638,-0.803689,13.443182,42.315407,51.165691,7.946527,39.074208,15.783471,9.945587,18.971187,15.199999,47.162494,20.593684,-0.789275,32.427908,33.223191,53.41291,31.046051,41.87194,36.204824,30.585164,-0.023559,19.85627,56.879635,33.854721,-29.609988,6.428055,26.3351,55.169438,-18.766947,-13.254308,3.202778,17.570692,21.00789,23.634501,47.411631,46.862496,42.708678,31.791702,-18.665695,-22.95764,28.394857,52.132633,12.865416,17.607789,40.339852,60.472024,21.512583,30.375321,8.537981,-6.314993,-23.442503,-9.189967,12.879721,51.919438,39.399872,18.220833,45.943161,61.52401,-1.940278,23.885942,14.497401,44.016521,8.460555,5.152149,-30.559482,35.907757,40.463667,7.873054,12.862807,60.128161,46.818188,34.802075,-6.369028,15.870032,33.886917,38.963745,1.373333,48.379433,37.09024,-32.522779,41.377491,6.42375,14.058324,24.215527,15.552727,-13.133897,-19.015438],"text":["long:   67.709953<br />lat:  33.939110<br />num_conflicts:  61","long:   20.168331<br />lat:  41.153332<br />num_conflicts:  10","long:    1.659626<br />lat:  28.033886<br />num_conflicts:  11","long:   17.873887<br />lat: -11.202692<br />num_conflicts:  15","long:  -63.616672<br />lat: -38.416097<br />num_conflicts:  26","long:   45.038189<br />lat:  40.069099<br />num_conflicts:   1","long:  133.775136<br />lat: -25.274398<br />num_conflicts:   3","long:   14.550072<br />lat:  47.516231<br />num_conflicts:  42","long:   47.576927<br />lat:  40.143105<br />num_conflicts:   1","long:   90.356331<br />lat:  23.684994<br />num_conflicts:   2","long:    4.469936<br />lat:  50.503887<br />num_conflicts:   7","long:    2.315834<br />lat:   9.307690<br />num_conflicts:  16","long:  -63.588653<br />lat: -16.290154<br />num_conflicts:   6","long:  -51.925280<br />lat: -14.235004<br />num_conflicts:  12","long:  114.727669<br />lat:   4.535277<br />num_conflicts:   2","long:   25.485830<br />lat:  42.733883<br />num_conflicts:   4","long:   -1.561593<br />lat:  12.238333<br />num_conflicts:   1","long:   29.918886<br />lat:  -3.373056<br />num_conflicts:   8","long:  104.990963<br />lat:  12.565679<br />num_conflicts:   8","long:   12.354722<br />lat:   7.369722<br />num_conflicts:   5","long: -106.346771<br />lat:  56.130366<br />num_conflicts:   5","long:   18.732207<br />lat:  15.454166<br />num_conflicts:   9","long:  -71.542969<br />lat: -35.675147<br />num_conflicts:  11","long:  104.195397<br />lat:  35.861660<br />num_conflicts: 428","long:  -74.297333<br />lat:   4.570868<br />num_conflicts:  15","long:   43.872219<br />lat: -11.875001<br />num_conflicts:   4","long:  -83.753428<br />lat:   9.748917<br />num_conflicts:   5","long:   15.200000<br />lat:  45.100000<br />num_conflicts:   2","long:  -77.781167<br />lat:  21.521757<br />num_conflicts:  12","long:   33.429859<br />lat:  35.126413<br />num_conflicts:   2","long:    9.501785<br />lat:  56.263920<br />num_conflicts:  35","long:   42.590275<br />lat:  11.825138<br />num_conflicts:   2","long:  -61.370976<br />lat:  15.414999<br />num_conflicts:   7","long:  -78.183406<br />lat:  -1.831239<br />num_conflicts:   5","long:   30.802498<br />lat:  26.820553<br />num_conflicts:  39","long:  -88.896530<br />lat:  13.794185<br />num_conflicts:   4","long:   10.267895<br />lat:   1.650801<br />num_conflicts:   2","long:   25.013607<br />lat:  58.595272<br />num_conflicts:   1","long:   40.489673<br />lat:   9.145000<br />num_conflicts:  51","long:   25.748151<br />lat:  61.924110<br />num_conflicts:   3","long:    2.213749<br />lat:  46.227638<br />num_conflicts: 257","long:   11.609444<br />lat:  -0.803689<br />num_conflicts:   2","long:  -15.310139<br />lat:  13.443182<br />num_conflicts:   1","long:   43.356892<br />lat:  42.315407<br />num_conflicts:   4","long:   10.451526<br />lat:  51.165691<br />num_conflicts:  37","long:   -1.023194<br />lat:   7.946527<br />num_conflicts:  16","long:   21.824312<br />lat:  39.074208<br />num_conflicts:   9","long:  -90.230759<br />lat:  15.783471<br />num_conflicts:   9","long:   -9.696645<br />lat:   9.945587<br />num_conflicts:  16","long:  -72.285215<br />lat:  18.971187<br />num_conflicts:  15","long:  -86.241905<br />lat:  15.199999<br />num_conflicts:   2","long:   19.503304<br />lat:  47.162494<br />num_conflicts:  27","long:   78.962880<br />lat:  20.593684<br />num_conflicts:  49","long:  113.921327<br />lat:  -0.789275<br />num_conflicts:  23","long:   53.688046<br />lat:  32.427908<br />num_conflicts:  14","long:   43.679291<br />lat:  33.223191<br />num_conflicts:  15","long:   -8.243890<br />lat:  53.412910<br />num_conflicts:   4","long:   34.851612<br />lat:  31.046051<br />num_conflicts:   5","long:   12.567380<br />lat:  41.871940<br />num_conflicts:  19","long:  138.252924<br />lat:  36.204824<br />num_conflicts:  92","long:   36.238414<br />lat:  30.585164<br />num_conflicts:   4","long:   37.906193<br />lat:  -0.023559<br />num_conflicts:   8","long:  102.495496<br />lat:  19.856270<br />num_conflicts:   8","long:   24.603189<br />lat:  56.879635<br />num_conflicts:   1","long:   35.862285<br />lat:  33.854721<br />num_conflicts:   4","long:   28.233608<br />lat: -29.609988<br />num_conflicts:   1","long:   -9.429499<br />lat:   6.428055<br />num_conflicts:   3","long:   17.228331<br />lat:  26.335100<br />num_conflicts:   2","long:   23.881275<br />lat:  55.169438<br />num_conflicts:   4","long:   46.869107<br />lat: -18.766947<br />num_conflicts:   4","long:   34.301525<br />lat: -13.254308<br />num_conflicts:   1","long:   73.220680<br />lat:   3.202778<br />num_conflicts:   1","long:   -3.996166<br />lat:  17.570692<br />num_conflicts:  17","long:  -10.940835<br />lat:  21.007890<br />num_conflicts:   2","long: -102.552784<br />lat:  23.634501<br />num_conflicts:  17","long:   28.369885<br />lat:  47.411631<br />num_conflicts:   1","long:  103.846656<br />lat:  46.862496<br />num_conflicts:   2","long:   19.374390<br />lat:  42.708678<br />num_conflicts:   4","long:   -7.092620<br />lat:  31.791702<br />num_conflicts:  37","long:   35.529562<br />lat: -18.665695<br />num_conflicts:   7","long:   18.490410<br />lat: -22.957640<br />num_conflicts:   5","long:   84.124008<br />lat:  28.394857<br />num_conflicts:   2","long:    5.291266<br />lat:  52.132633<br />num_conflicts:  14","long:  -85.207229<br />lat:  12.865416<br />num_conflicts:  12","long:    8.081666<br />lat:  17.607789<br />num_conflicts:  38","long:  127.510093<br />lat:  40.339852<br />num_conflicts:   1","long:    8.468946<br />lat:  60.472024<br />num_conflicts:   1","long:   55.923255<br />lat:  21.512583<br />num_conflicts:  24","long:   69.345116<br />lat:  30.375321<br />num_conflicts:   8","long:  -80.782127<br />lat:   8.537981<br />num_conflicts:   6","long:  143.955550<br />lat:  -6.314993<br />num_conflicts:   1","long:  -58.443832<br />lat: -23.442503<br />num_conflicts:  10","long:  -75.015152<br />lat:  -9.189967<br />num_conflicts:  20","long:  121.774017<br />lat:  12.879721<br />num_conflicts:   6","long:   19.145136<br />lat:  51.919438<br />num_conflicts:  70","long:   -8.224454<br />lat:  39.399872<br />num_conflicts: 136","long:  -66.590149<br />lat:  18.220833<br />num_conflicts:   2","long:   24.966760<br />lat:  45.943161<br />num_conflicts:   4","long:  105.318756<br />lat:  61.524010<br />num_conflicts: 206","long:   29.873888<br />lat:  -1.940278<br />num_conflicts:   4","long:   45.079162<br />lat:  23.885942<br />num_conflicts:   4","long:  -14.452362<br />lat:  14.497401<br />num_conflicts:   1","long:   21.005859<br />lat:  44.016521<br />num_conflicts:   4","long:  -11.779889<br />lat:   8.460555<br />num_conflicts:   6","long:   46.199616<br />lat:   5.152149<br />num_conflicts:   7","long:   22.937506<br />lat: -30.559482<br />num_conflicts:  20","long:  127.766922<br />lat:  35.907757<br />num_conflicts:   2","long:   -3.749220<br />lat:  40.463667<br />num_conflicts: 159","long:   80.771797<br />lat:   7.873054<br />num_conflicts:   3","long:   30.217636<br />lat:  12.862807<br />num_conflicts:  14","long:   18.643501<br />lat:  60.128161<br />num_conflicts:  41","long:    8.227512<br />lat:  46.818188<br />num_conflicts:  11","long:   38.996815<br />lat:  34.802075<br />num_conflicts:  12","long:   34.888822<br />lat:  -6.369028<br />num_conflicts:  10","long:  100.992541<br />lat:  15.870032<br />num_conflicts:   6","long:    9.537499<br />lat:  33.886917<br />num_conflicts:   2","long:   35.243322<br />lat:  38.963745<br />num_conflicts: 180","long:   32.290275<br />lat:   1.373333<br />num_conflicts:  18","long:   31.165580<br />lat:  48.379433<br />num_conflicts:   2","long:  -95.712891<br />lat:  37.090240<br />num_conflicts:   5","long:  -55.765835<br />lat: -32.522779<br />num_conflicts:   6","long:   64.585262<br />lat:  41.377491<br />num_conflicts:   3","long:  -66.589730<br />lat:   6.423750<br />num_conflicts:  10","long:  108.277199<br />lat:  14.058324<br />num_conflicts:  12","long:  -12.885834<br />lat:  24.215527<br />num_conflicts:   1","long:   48.516388<br />lat:  15.552727<br />num_conflicts:  16","long:   27.849332<br />lat: -13.133897<br />num_conflicts:   6","long:   29.154857<br />lat: -19.015438<br />num_conflicts:   6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,0,0,1)","opacity":0.5,"size":[10.863375144564,6.52308993162989,6.6714962257457,7.20135103152826,8.3521315133464,3.77952755905512,5.07285526455903,9.63531780063588,3.77952755905512,4.69404834991338,6.01963685582438,7.32145135180958,5.82445821425105,6.81264988531104,4.69404834991338,5.36352403339969,3.77952755905512,6.19912214046418,6.19912214046418,5.60856914077163,5.60856914077163,6.36618297006295,6.6714962257457,22.6771653543307,7.20135103152826,5.36352403339969,5.60856914077163,4.69404834991338,6.81264988531104,4.69404834991338,9.11205429738549,4.69404834991338,6.01963685582438,5.60856914077163,9.41701232822802,5.36352403339969,4.69404834991338,3.77952755905512,10.2461660865747,5.07285526455903,18.4118602127872,4.69404834991338,3.77952755905512,5.36352403339969,9.26665230420466,7.32145135180958,6.36618297006295,6.36618297006295,7.32145135180958,7.20135103152826,4.69404834991338,8.44268691722767,10.1155134564334,8.06901028918287,7.07687916297244,7.20135103152826,5.36352403339969,5.60856914077163,7.65951067556686,12.5034998881603,5.36352403339969,6.19912214046418,6.19912214046418,3.77952755905512,5.36352403339969,3.77952755905512,5.07285526455903,4.69404834991338,5.36352403339969,5.36352403339969,3.77952755905512,3.77952755905512,7.43761072248814,4.69404834991338,7.43761072248814,3.77952755905512,4.69404834991338,5.36352403339969,9.26665230420466,6.01963685582438,5.60856914077163,4.69404834991338,7.07687916297244,6.81264988531104,9.34234035886641,3.77952755905512,3.77952755905512,8.16541519657802,6.19912214046418,5.82445821425105,3.77952755905512,6.52308993162989,7.76583126817315,5.82445821425105,11.376107783533,14.4052989373185,4.69404834991338,5.36352403339969,16.8734726012096,5.36352403339969,5.36352403339969,3.77952755905512,5.36352403339969,5.82445821425105,6.01963685582438,7.76583126817315,4.69404834991338,15.2748756508746,5.07285526455903,7.07687916297244,9.56346489243628,6.6714962257457,6.81264988531104,6.52308993162989,5.82445821425105,4.69404834991338,16.0149818443381,7.55019337658711,4.69404834991338,5.60856914077163,5.82445821425105,5.07285526455903,6.52308993162989,6.81264988531104,3.77952755905512,7.32145135180958,5.82445821425105,5.82445821425105],"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":25.5707762557078,"l":28.4931506849315},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Violent conflicts since 1400 AD","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-118.86188705,156.47066605],"ticktext":["-100","-50","0","50","100","150"],"tickvals":[-100,-50,0,50,100,150],"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-43.43310735,66.94112035],"ticktext":["-40","-20","0","20","40","60"],"tickvals":[-40,-20,0,20,40,60],"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest"},"source":"A","attrs":{"612344def54f4":{"map_id":{},"type":"ggplotly"},"6123466d85747":{"x":{},"y":{},"size":{}}},"cur_data":"612344def54f4","visdat":{"612344def54f4":["function (y) ","x"],"6123466d85747":["function (y) ","x"]},"config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script>
<!--/html_preserve-->