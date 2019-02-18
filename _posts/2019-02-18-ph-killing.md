---
layout: post
title: "Visualizing Extra-judicial Killings in the Philippines"
date: "2019-02-18"
comments: true
maps: true
---
-   [After Scraping and Cleaning the Raw Text Data](#after-scraping-and-cleaning-the-raw-text-data)
-   [Killings over Time](#killings-over-time)
-   [Day of Week of Killings](#day-of-week-of-killings)
-   [Killings by Hour of Day](#killings-by-hour-of-day)
-   [Type of Killings](#type-of-killings)
-   [Mapping the killings](#mapping-the-killings)

Before heading to the Philippines a few weeks ago, I decided to read a bit about the current political environment. Jonathan Miller recently published a book called "Duterte Harry" that describes the current President's war on drugs and the social toll it's taking in the form of what the Filipinos call "Extra Judicial Killings," or EJKs. Some international organizations estimate that up to 10,000 Filipinos have been unlawfully killed in pursuit of Rodrigo Duterte's goal of eradicating the supposed "Shabu" (methamphetamine) epidemic in the Philippines.

Nevertheless, critics such as Jonathan Miller claim that he is essentially manufacturing consent for this operation by exaggerating the extent of the drug problem in the Philippines. It's really hard to know because finding official statistics is not easy. In fact, even trying to get informal data concerning these EJKs was quite difficult. In the end, I was able to pause a BBC Youtube documentary on the Philippine's drug war and copy down the website shown in a screenshot to find a list of EJKs from 2016-17. The list stems from a local newspaper called The Inquirer that was recording the killings as the happened, albeit in text form.

Here's [the website](https://newsinfo.inquirer.net/794598/kill-list-drugs-duterte) if you'd like to learn more. And here's a screenshot of the data in their original text form.
![kill list](./pics/dataexph.png){:class="img-responsive"}
![kill list](./pics/phkilling.png){:class="img-responsive"}

Given that these data were in a relatively unstructured format, I thought it might be interesting to scrape these records and turn them into a structured format suitable for visualization. I also hope these visualizations can better relay the gravity of the situation to people unfamiliar with Duterte's policy of "exterminating" the--mostly poor and urban--users of Shabu.

If you're interested in examining the dataset I created for these visualizations just email me.

After Scraping and Cleaning the Raw Text Data
=============================================

``` r
df <- read.csv('ph_drug_death.csv', stringsAsFactors = F)
glimpse(df)
```

    ## Observations: 528
    ## Variables: 12
    ## $ date     <chr> "2016-10-11", "2016-10-11", "2016-10-11", "2016-10-11...
    ## $ time     <chr> "5:00 a.m.", "4:00 a.m.", "12:30 a.m.", "12:30 a.m.",...
    ## $ name     <chr> "Alias Bong", "Dindo Piang", "Unidentified drug suspe...
    ## $ context  <chr> " suspected drug user", "former barangay councilor", ...
    ## $ location <chr> "Pasay City", "Pikit town", "Laguna", "Laguna", "Lagu...
    ## $ cause    <chr> "Killed by unknown hitmen", "Killed in police buy-bus...
    ## $ loc_2    <chr> "Metro Manila", "North Cotabato", "Laguna", "Laguna",...
    ## $ mo       <chr> "Oct", "Oct", "Oct", "Oct", "Oct", "Oct", "Oct", "Oct...
    ## $ wday     <chr> "Tue", "Tue", "Tue", "Tue", "Tue", "Tue", "Wed", "Wed...
    ## $ full_loc <chr> "Metro Manila Pasay City , Philippines", "North Cotab...
    ## $ lon      <dbl> 120.9999, 121.9998, 121.3332, 121.3332, 121.3333, 121...
    ## $ lat      <dbl> 14.54852, 13.00189, 14.16560, 14.16768, 14.16533, 16....

I should mention a couple caveats to these data. First, many of the times were given as "early morning," rather than a specific time. For any of these observations, I drew random samples with replacement from a uniform distribution of times from 4 a.m. to 7 a.m. So for visualizations regarding times of day, take these with a grain of salt since the exact times are unknown.

Second, rather than overplotting killings in the same city or Borangay, I've instead added a bit of jitter to the GPS coordinates so that we can see the individual cases on the map.

Killings over Time
==================

Notice there is a suspicious gap where no data were recorded from mid-December 2016 to February 2017. It's also curious that the data collection completely stopped after February 2017. It's been speculated the Duterte administration may have exerted some political pressure on The Inquirer to force it to stop publishing its "Kill List."

``` r
df%>%
  mutate(date = ymd(date))%>%
  count(date)%>%
  ggplot(aes(date, n))+
  geom_line()+
  scale_x_date(date_breaks = '1 month', date_labels = '%Y\n%b')+
  theme_minimal()+
  labs(title='Time series of killings from 2016-17')
```

![](ph_deaths_files/figure-markdown_github/unnamed-chunk-2-1.png)

Day of Week of Killings
=======================

FYI here I've aggregated Metro Manila data with Manila. We are just considering the 10 regions with the most killings.

Interestingly, it looks like most killings in Manila (which is \#1 for the number of killings) occur during the week, with most occuring on Tuesdays, Mondays, and Thursdays. Very few killings occur on the weekends. The pattern in Cebu is similar.

``` r
df%>%
  mutate(loc_2 = ifelse(str_detect(loc_2, 'Manila'), 'Metro Manila', loc_2))%>%
  count(loc_2 = fct_lump(loc_2,10), wday)%>%
  ggplot(aes(wday, n, fill=loc_2))+
  geom_col()+
  guides(fill=FALSE)+
  facet_wrap(~loc_2, ncol=3, scales='free_y')+
  theme_minimal()+
  labs(title='Which day of week do most killings happen?')
```

![](ph_deaths_files/figure-markdown_github/unnamed-chunk-3-1.png)

Let's aggregate all locations to see on which days of the week the killings tend to happen. It turns out that most tend to occur on Tuesdays and Thursdays. Not sure why this would be. It is surmised that many of the killings are done by off-duty police. Perhaps something about the nature of the job makes it easier to carry out on these days?

``` r
df%>%
  count(wday)%>%
  ggplot(aes(wday, n, fill=wday))+
  geom_col()+
  guides(fill=FALSE)+
  theme_minimal()+
  labs(title='Overall which day of week do most killings happen?')
```

![](ph_deaths_files/figure-markdown_github/unnamed-chunk-4-1.png)

Killings by Hour of Day
=======================

Here we will look at the times of day in which killings occur in the top two cities: Manila and Cebu. Again, I've aggregated "Metro Manila" and Manila and Cebu and Cebu City for ease of plotting. Remember also, many exact times were not given and I have randomly assigned (based on reasonable estimates) a time to observations that contained phrases like "early morning" or "late morning."

To me the overall pattern does seem to lend evidence to the idea that the killings are the work of off-duty police. Relatively speaking, very few killings occur during normal working hours.

``` r
df%>%
    mutate(loc_2 = ifelse(str_detect(loc_2, 'Manila'), 'Metro Manila', loc_2))%>%
  mutate(time = str_replace_all(time, c('a.m.'='AM', 'p.m.'='PM')))%>%
  mutate(time = format(strptime(time, "%I:%M %p"), "%H:%M"))%>%
  na.omit()%>%
  unite(dtime, date, time, sep= ' ')%>%
  mutate(dtime = ymd_hm(dtime))%>%
  mutate(dtime = floor_date(dtime, unit='1 hour'))%>%
  separate(dtime, into=c('date', 'time'), sep=' ')%>%
  count(loc = fct_lump(loc_2,2),time, sort=T)%>%
  mutate(time = str_sub(time, 1,2))%>%
  ggplot(aes(time,n,fill=loc))+
  geom_col()+
  theme_minimal()+
  guides(fill=FALSE)+
  facet_wrap(~loc, ncol=1, scales='free_y')+
  labs(title='Killings by Hour of Day', y='Number of Victims')
```

![](ph_deaths_files/figure-markdown_github/unnamed-chunk-5-1.png)

Type of Killings
================

The textual data from The Inquirer often contained interesting information regarding the killing. For example, many victims are left with a cardboard sign placed on their bodies saying something like "I am a drug user."

It looks like the most murders were done by "unknown hitmen" often "riding in tandem." It's also quite suspicious that many were killed in "buy-bust" operations by the police. It's often said that police will plant guns on the dead bodies to make it seem as if the killings were done in self-defense.

``` r
df%>%
  na.omit()%>%
  count(cause = fct_lump(str_sub(cause,1,60),15), sort=T)%>%
  ggplot(aes(reorder(cause,n),n, fill=cause))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  guides(fill=FALSE)+
  labs(title='Killings by Type', y='', x='')
```

![](ph_deaths_files/figure-markdown_github/unnamed-chunk-6-1.png)

Mapping the killings
====================

Take moment to zoom in to the individual killings. There you can see the victim's name and the circumstance regarding his killing. The location marker's colors are based on the top three types of killing visualized above.

``` r
c_death <- colorFactor('inferno', domain=fct_lump(df$cause, 3))

df%>%
  leaflet()%>%
  addProviderTiles('CartoDB.Positron')%>%
  addCircleMarkers(radius = 1,label = ~paste(name,':', fct_lump(df$cause, 3)),
                   color= ~ c_death(fct_lump(df$cause, 3)),
                   fillColor = ~c_death(fct_lump(df$cause, 3)),
                   fillOpacity = .5, 
                   clusterOptions = TRUE)%>%
  addControl('Philippine Drug War Killings 2016-2017', position = "bottomleft")
```


{% include ph_map.html %}
