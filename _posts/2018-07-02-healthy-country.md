---
layout: post
title: "Data Analysis: Which Country has the Best Health in the World?"
date: "2018-07-02"
---
The Mother of all Social Science Datasets: Quality of Government
================================================================

Dahlberg, Stefan, Sören Holmberg, Bo Rothstein, Natalia Alvarado Pachon & Richard Svensson. 2018. The Quality of Government Basic Dataset, version Jan18. University of Gothenburg: The Quality of Government Institute, <http://www.qog.pol.gu.se> <doi:10.18157/QoGBasJan18>

If you aren't familiar with this dataset, I suggest spending a few minutes looking through the 200-page data dictionary that comes along with it. It's basically a collection of collections of social science data: it's like a metadata repository of datasets from the World Health Organization, the UN, and many other independent researchers broken down into various "themes." The focus today will be on the theme of "Health." There are over 400 different variables recorded in here, so I'll definitely be continuing this series.

In order to determine which country is the overall best country for living a long happy life, I've decided to rate each country on 10 dimensions (based on the availability of such data in the QoG dataset). The 10 dimensions are:

1.  The HIV prevalance in %
2.  The degree of PM 2.5 in the air (100 is best air, 0 is worst air)
3.  The total tobacco use
4.  Infant mortality: deaths per 1,000 births
5.  The level of sanitation and public water cleanliness
6.  The degree of public services (10 meaning very unreliable, 1 being very stable)
7.  Alcohol consumption from 2000 onward
8.  Subjective happiness ratings 1-4 (very happy)
9.  Rate of tuberculosis
10. the UN's Human Development Index score (0-100) best
11. 2017 Obesity rates

**UPDATE** Upon suggestion of my cousin, I've added an 11th dimension: % of population that is obese. Data taken from <https://renewbariatrics.com/obesity-rank-by-countries/>

Notice that for some measures, low scores are good and for others high scores are good. We will take care of that issues in the ranking section.

Using these 11 dimensions: Which country in the world has the best Health Outcomes?
===================================================================================

First we read in the data and select our 11 variables. After omitting missing values, we end up with a sample of 39 countries from all over the globe.

``` r
library(tidyverse)
df_f <- read.csv('qog.csv')
df <- df_f %>%
  select(matches('cname'),matches('ccodealp'),matches('hiv'),matches('pm25'), matches('tobt'), matches('imort'),
         matches('alc2000'), matches('ehwater'), matches('wvs_hap'), matches('ffp_ps'),
         matches('wef_ct'), matches('hdi'))%>%
  na.omit()
rm(df_f)
head(df)
```
    ##         cname ccodealp wef_chiv epi_pm25 who_tobt wef_imort who_alc2000
    ## 7  Azerbaijan      AZE     0.20    94.30     23.5      30.8        2.14
    ## 8   Argentina      ARG     0.40   100.00     25.3      12.7        7.93
    ## 9   Australia      AUS     0.20   100.00     16.0       4.1        9.71
    ## 14    Armenia      ARM     0.20    85.76     28.0      14.7        3.91
    ## 21     Brazil      BRA     0.45   100.00     16.1      12.9        7.32
    ## 36      Chile      CHL     0.40   100.00     38.9       7.8        7.16
    ##    epi_ehwater  wvs_hap ffp_ps wef_ct undp_hdi
    ## 7        77.47 3.052584    5.4   95.0    0.758
    ## 8        92.52 3.199185    4.3   25.0    0.826
    ## 9       100.00 3.277956    2.0    6.5    0.937
    ## 14       83.21 3.091120    4.1   52.0    0.741
    ## 21       84.66 3.264543    5.9   46.0    0.754
    ## 36       94.25 3.084253    4.5   16.0    0.845

Getting the obesity data will require us to scrape the HTML tables from the rewewbariatrics website. We'll use the super easy-to-use Rvest package to do that.

``` r
library(rvest)
```

``` r
url <- 'https://renewbariatrics.com/obesity-rank-by-countries/'
codes_url <- read_html(url)
#Use the CSS selector to extract table data from the webiste
html_data <- html_nodes(codes_url,'td')
table_text <- html_text(html_data)

#easiest way is to recreate the table as a matrix and read in by row
mat_weight <- matrix(table_text, nrow = 192, ncol=6, byrow=TRUE)

weight_df <- data.frame(country = mat_weight[,2], perc_obese = mat_weight[,6])

#Get rid of percent signs and convert to decimal
weight_df$perc_obese <- as.numeric(gsub('%', '', weight_df$perc_obese))/100
head(weight_df)
```
    ##        country perc_obese
    ## 1 Cook Islands      0.508
    ## 2        Palau      0.476
    ## 3        Nauru      0.456
    ## 4        Samoa      0.434
    ## 5        Tonga      0.433
    ## 6         Niue      0.432

Some tedious but necessary changes we need to make in order to keep our original countries. The scraped table used slightly different names for the countries.

``` r
idex_num <- which(df$cname == 'Malaysia (1966-)')
df$cname <- as.character(df$cname)
df$cname[idex_num] <- 'Malaysia'

idex_num <- which(df$cname == 'Pakistan (1971-)')
df$cname[idex_num] <- 'Pakistan'

idex_num <- which(df$cname == 'United States')
df$cname[idex_num] <- 'United States of America'

idex_num <- which(df$cname == 'Russia')
df$cname[idex_num] <- 'Russian Federation'
```

Now the tricky part will be matching the names with our existing names. Because we've made sure to change names to make them identical, we shouldn't lose any observations (I hope). Let's try an inner\_join (mutating join) to only keep rows that have matches

``` r
#convert to character instead of factor
weight_df$country <- as.character(weight_df$country)

#Nice tip: use semi_join to see what will be joined before you use inner_join
#I was able to see that Russia was missing 
df %>%
  rename('country' = cname)%>%
  semi_join(weight_df, by='country')%>%
  head()
```

    ##      country ccodealp wef_chiv epi_pm25 who_tobt wef_imort who_alc2000
    ## 1 Azerbaijan      AZE     0.20    94.30     23.5      30.8        2.14
    ## 2  Argentina      ARG     0.40   100.00     25.3      12.7        7.93
    ## 3  Australia      AUS     0.20   100.00     16.0       4.1        9.71
    ## 4    Armenia      ARM     0.20    85.76     28.0      14.7        3.91
    ## 5     Brazil      BRA     0.45   100.00     16.1      12.9        7.32
    ## 6      Chile      CHL     0.40   100.00     38.9       7.8        7.16
    ##   epi_ehwater  wvs_hap ffp_ps wef_ct undp_hdi
    ## 1       77.47 3.052584    5.4   95.0    0.758
    ## 2       92.52 3.199185    4.3   25.0    0.826
    ## 3      100.00 3.277956    2.0    6.5    0.937
    ## 4       83.21 3.091120    4.1   52.0    0.741
    ## 5       84.66 3.264543    5.9   46.0    0.754
    ## 6       94.25 3.084253    4.5   16.0    0.845

``` r
#looks good so add together
df <- df %>%
  rename('country' = cname)%>%
  inner_join(weight_df, by = 'country')
```
OK, everything looks good: no NAs or crazy values. We can rerun our analysis using this new obesity dimension now.

Which 39 countries did we end up with?

``` r
#Which 39 countries? 
for (n in df$country){
  cat(paste(n, " "))
}
```
    ## Azerbaijan  Argentina  Australia  Armenia  Brazil  Chile  China  Colombia  Ecuador  Georgia  Germany  Ghana  India  Kazakhstan  Jordan  Kyrgyzstan  Lebanon  Malaysia  Mexico  Morocco  Netherlands  New Zealand  Nigeria  Pakistan  Philippines  Poland  Romania  Russian Federation  Singapore  Slovenia  South Africa  Spain  Sweden  Thailand  Turkey  Ukraine  Egypt  United States of America  Uruguay

Finally we are ready to re-run our analysis with these corrected names and the new obesity variable.

Let's check to make sure no crazy values here. All right, everything looks reasonable. Now we can start our comparisons.

First off, I'm interested in correlations between the various dimensions.

``` r
library(corrplot)
```
``` r
df %>%
  select_if(is.numeric)%>%
  cor()%>%
  corrplot(method='number')
```

![](health_files/figure-markdown_github/unnamed-chunk-6-1.png)

This is super interesting and it's worth spending some time examining.

1.  Not surprisingly tuberculosis and HIV rates are highly correlated. This suggest systematic problems in controlling contagious disease.

2.  Bad air is correlated with low happiness. That makes sense, but it worth reiterating. Your happiness may be determined by how clean your country's air is.

3.  Not surprisingly, high infant mortality is associated with low water sanitation.

4.  Essentially the Human Development Index is measuring the same thing as the public services variable.

5.  Higher alcohol consumption is associated with LOWER infant mortality and higher human development. Interesting. Perhaps this has something to do with the extra leisure time brought on by more development. Poorer countries do not have the luxury of sitting around and drinking beers with their buddies.

6.  Obesity is negatively correlated to happiness, but fairly strongly positively associated with the level of human development. Infant mortality and obesity are conversely related. Someting about being rich and modern is making us fat. Ever noticed how all the old British monarchs were fat? I would wager there's something about the diet of rich people that contributes to their higher rates of obesity (hint: it's probably the increased meat/animal product consumption). In a later post I'll look at timeseries of GDP growth and Body mass index changes to test this out.

There are plenty more interesting correlations, but these are just some of them.

Let's use a "Perceptual Map" to try to visualize the relationships among countries on these dimensions.
-------------------------------------------------------------------------------------------------------

If you're not familiar with perceptual maps, I suggest the very good book by Chris Chapman and Elea Feit called "R for Marketing Research and Analytics." Basically we will run PCA on the 10 dimensions, reduce to two dimensions, then visualize the groupings of the countries on the two dimensions.

``` r
#Keep row names for ID. Very useful bit of code
rownames(df) <- df[,2]

#Run PCA. Make sure to scale your data first. Otherwise the PCs will reflect the dimensions with huge units
ct_pc <- prcomp(df[,3:ncol(df)], scale. = TRUE)
summary(ct_pc)
```
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6
    ## Standard deviation     2.1693 1.3695 1.2621 1.00579 0.79305 0.74069
    ## Proportion of Variance 0.4278 0.1705 0.1448 0.09196 0.05717 0.04987
    ## Cumulative Proportion  0.4278 0.5983 0.7431 0.83508 0.89225 0.94213
    ##                            PC7     PC8    PC9   PC10   PC11
    ## Standard deviation     0.57025 0.41706 0.2482 0.1961 0.1934
    ## Proportion of Variance 0.02956 0.01581 0.0056 0.0035 0.0034
    ## Cumulative Proportion  0.97169 0.98750 0.9931 0.9966 1.0000

The summary() function is very useful in deciding how many PCs to keep. Here we can see that 5 PCs cover about 90% of the variance in our original 10 variables. We'll only keep the first two that explain 60% of variance.

If you're more of a visual person, you can use a screeplot to find the "elbow" where the marginal variance captured by the next PC seems to drop.

``` r
plot(ct_pc, type='l')
```

![](health_files/figure-markdown_github/unnamed-chunk-8-1.png)

If I were going to use this in a predictive model I might decide to keep 4-5 PCs, but here we will only keep the first two for easy visualization.

To visualize the results, we use the biplot() function. FYI South Africa is "ZAF."

``` r
biplot(ct_pc, cex=c(.7,1), expand=1.5,
       sub='Comparing countries on the 11 dimensions of Health')
```

![](health_files/figure-markdown_github/unnamed-chunk-9-1.png)

This biplot contains a ton of information, but its interpretation can be tricky. Let's break it down.

First notice that the x/y axes are PC1 and PC2, which cumulatively explain about 60% of the variation in the original variables. So already we know there is more to this story than just these two PC scores-- about 40% of the story is left untold.

Second, the red arrows have both a magnitude and an angle. The angle of these arrows relative to the x/y axes show you the degree to which these original variables are being "captured" by the PCs. So nearly vertical lines are well captured by PC2 (HIV and Tuberculosis rates), while PC1 seems to be capturing Infant mortality, public services, water sanitation and the Human Development Index.

The angles between the two original variables (red lines) represents the direction of correlation. So infant mortality and public services are very positively correlated, while HIV rates and Tobacco use are negatively correlated. These interpretations should match our results from the correlation matrix above. Finally, and this one I'm not sure about, but the length of the arrows represents the standard deviation in the original variables. So if you squared the length you'd represent that variable's variance.

Pitfalls in interpreting the biplot
===================================

Biplots are subtle creatures. If you don't believe me, check out some of the StackOverflow discussions about them. For now, we'll focus on just the most practical implications and pitfalls.

First, the positioning of the countries on the biplot is **relative** and so changing the number of variables and/or adding/removing countries will affect the relationships among them.

Second, remember I mentioned that PC1 & PC2 only account for about 60% of the variance of all original variables. This means there may be other relationships between countries that are not accounted for in this 60% of variance. In short, there is more to the story than what you see in this biplot. As Chapman and Feit explain, "When we look at PC1 and PC2 scores we are looking at the 'largest magnitude similarities,' which could hide other, smaller differences in the other PCs." With that proviso, let's quickly review what the biplot tells us.

1.  South Africa (ZAF) is way out in left field, probably due to infant mortality and HIV rates.

2.  Interestingly Jordan and Lebanon are clustered right next to each other mostly due to their levels of tobacco use. And now China and Egypt also seem to be clustered near each other again due to their high levels of tobacco use and low HIV/Tuberculosis rates.

3.  India and Pakistan have similar levels of infant mortality, tuberculosis (high numbers, which are bad), and unstable public services. They're surprisingly close. This makes sense considering they were essentially the same country until 1971.

4.  The African countries of Nigeria and Ghana seem to have the negative combo of both high infant mortality and tubercolosis coupled with HIV and bad public services.

The geographic clustering of countries suggests that PCA is working pretty well to figure out how different countries are similar and different. Paradoxically, any kind of statistical learning procedure should conform (roughly) to our expectations, but if it were 100% as we expected, we wouldn't be learning anything. So ideally our analysis would match our intution, but we hold out hope that there are a few wild cards thrown at us that force us to re-evaluate our assumptions--or even recheck our data to make sure they're correct. That's the beauty of data-driven methods: they can present us with a new perspective on a topic we thought we understood.

In some cases of many useful PCs, you might want to also visualize PC3 vs PC4
=============================================================================

You can use the choices=c(3,4) to remake the plot with PC3 and 4 as the axes. Just for fun we'll try it.

Because up to 5 PCs explain 90% of variance, it might be worthwhile to check this plot out as well. Think of this plot as showing how PC3 and 4 "Soaked up" (Thanks Prof. Soumya Ray!) the remaining variance not captured by PC1 and 2.

``` r
biplot(ct_pc, cex=c(.7,.8), expand=1.2, choices=c(3,4),
       sub='Comparing countries on the 11 dimensions of Health')
```

![](health_files/figure-markdown_github/unnamed-chunk-10-1.png)

Now we see that PC3 represents different variables (happiness) and PC4 captures a lot of variation in PM 2.5 and Obesity rates. Interestingly, by these measures, now Middle Eastern countries are grouped together by their higher obesity rates and low alcohol consumption.

Now we've looked at relationships among countries on the 11 dimensions. Let's try to answer the question of which country is the best for "health."
===================================================================================================================================================

In order to do this, we will rank countries by their scores on each variable and the compute which countries has the overall lowest (best) rank on all 11 dimensions. We will crown that country as "the best for health." But one problem we will have is that some dimensions have high scores for "good" and others have low scores for "good."

``` r
#Separate columns into those where higher is better
up_ranks <- df[,colnames(df) %in% c("epi_pm25","epi_ehwater",
                                              "undp_hdi", "wvs_hap")]

#Separate where lower scores are better
low_ranks <- df[,colnames(df) %in% c("who_alc2000", "ffp_ps", "wef_ct",
                        "wef_imort", "wef_chiv", "who_tobt", "perc_obese")]

#Now we rank 
df_low_rank <- apply(low_ranks, 2, rank)

#Use opposite ranking method
df_high_rank <- apply(up_ranks, 2, function(x) rank(-x))

#now assemble
df_ranked <- cbind(df_low_rank, df_high_rank)

#Here's what we end up with. 1 means best and 39 means worst.
head(df_ranked)
```
    ##     wef_chiv who_tobt wef_imort who_alc2000 ffp_ps wef_ct perc_obese
    ## AZE       18       19        34          10   25.0   29.0         22
    ## ARG       27       22        21          27   14.5   16.0         28
    ## AUS       18        7         7          35    6.5    5.0         33
    ## ARM       18       29        24          14   13.0   21.0         12
    ## BRA       30        8        22          25   29.0   19.0         14
    ## CHL       27       38        13          23   16.5   10.5         31
    ##     epi_pm25 epi_ehwater wvs_hap undp_hdi
    ## AZE       24          31      28     21.5
    ## ARG       11          13      18     12.0
    ## AUS       11           2      13      1.0
    ## ARM       27          24      25     25.5
    ## BRA       11          21      14     23.0
    ## CHL       11          12      26     11.0

Let's try some visualizations on these data to get a better feel. Visualizing ranked data is not something I do often, so I welcome any feedback on which methods are most suited for displaying these data.

First try a heatmap of ranks.

``` r
library(gplots)
```

``` r
heatmap.2(df_ranked,dendrogram = 'none', scale='none', key=FALSE, cexRow=.6,
          main='Red = low rank (Good), White = (Bad)')
```

![](health_files/figure-markdown_github/unnamed-chunk-12-1.png)

I kept the trace on there because it shows you where the colors fall (column-wise) relative to the average. For example, for HIV rates, you can see whitish colors mean higher scores and thus the trace pops out to the right. Dark red colors move the trace left. You can see that Jordan has the best ranking for HIV prevalence.

You can also see a big group of European countries that are dark red on all the basic life statistics, but do not do well on tobacco use or alcohol consumption--two strong predictors of early mortality.

The USA is white for "perc\_obese" due to their number 1 ranking out of the 39 countries.

We will finish off by crowning our winner and visualizing its performance
=========================================================================

``` r
#convert to Df
df_ranked <- as.data.frame(df_ranked)

#Find average ranks per country
score_mat <- as.matrix(apply(df_ranked, 1, mean))

#then rank by lowest average rank
final_ranks <- apply(score_mat, 2, rank)

#sort to get final list
sort_ranks <- apply(final_ranks, 2, sort)
sort_ranks
```

    ##     [,1]
    ## SGP  1.0
    ## SWE  2.0
    ## NZL  3.0
    ## AUS  4.0
    ## NLD  5.0
    ## MYS  6.0
    ## DEU  7.0
    ## USA  8.0
    ## SVN  9.0
    ## ESP 10.0
    ## URY 11.0
    ## ECU 12.0
    ## ARG 13.0
    ## POL 14.0
    ## MEX 15.0
    ## BRA 16.0
    ## COL 17.0
    ## CHL 18.0
    ## TUR 19.0
    ## JOR 20.0
    ## ARM 21.0
    ## KAZ 22.0
    ## PHL 23.0
    ## THA 24.0
    ## GHA 25.5
    ## LBN 25.5
    ## KGZ 27.0
    ## CHN 28.0
    ## EGY 29.0
    ## MAR 30.0
    ## ROU 31.0
    ## AZE 32.0
    ## GEO 33.0
    ## PAK 34.0
    ## UKR 35.0
    ## RUS 36.0
    ## NGA 37.0
    ## IND 38.0
    ## ZAF 39.0

So there you have it, according to our criteria the top 5 countries for "health" (as defined by yours truly) are: Singapore, Sweden, New Zealand, Australia and the Netherlands. The USA dropped from the 5th spot to the 8th spot when obesity rates were factored in.

The last 5 countries are: Ukraine, Russia, Nigeria, India, and South Africa. Wow, sorry my South African friends. If I had to guess why it's the HIV/Tuberculosis rates. Perhaps the situation has improved in recent years, as these data are several years old. Good news for Pakistan though, it is now ranked 34th out of 39 when obesity data were added. Russia took its spot.

I'd say these results match up pretty well with expectation, though I did not expect South Africa to fare so poorly. I think one key takeaway here is that countries that have some very poor populations (India and South Africa) are disproportionately weighed down by these inequalities. There are certainly parts of these countries that are every bit as modern (or more so) than countries in our top 10. However on the whole, those top 10 countries have a base-line level of public services/infrastructure/health care that the bottom 10 countries do not have.

What's the best way to visualize these results?
===============================================

My first idea was to use something like a parallel coordinate plot. But with 39 countries it is going to be ugly.

``` r
#useful function when converting from matrix to dfs
df_ranked <- rownames_to_column(df_ranked)

df_ranked %>%
  gather(-rowname, key='vars', value='scores')%>%
  ggplot(aes(vars, scores, group=rowname, color=rowname))+
  geom_path(size=1, alpha=.3)+
  theme_minimal() + theme(axis.text.x = element_text(size = 13, 
    angle = 35), legend.key = element_rect(fill = NA)) +labs(title = "Parallel Coordinates of Rankings", 
    x = "Variables", y = "Ranks")
```

![](health_files/figure-markdown_github/unnamed-chunk-14-1.png)

Yes, as I suspected it's chaos with 39 countries. But you can still see some useful relationships (denoted by the depth of the connecting lines between variables). For example, you can see that countries scoring well on obesity tend to also have high HIV/Tuberculosis rates. And countries that have bad water sanitation seem to have better air quality. Not a trade-off I'd like to make, but an interesting observation nonetheless.

My other idea was to facet by variable type and color by score and look for the countries with the smallest bars (indicating low=good ranks). I think this is better.

Don't mind the gargantuan chunk of code. But basically in order to do this you have to reshape the data into "long" form so you can facet by variable. If you haven't mastered this technique, it is well worth your time doing so. Use the gather() function from the tidyverse library.

``` r
#Try another one where on each var is a facet with ranks
df_ranked %>%
  gather(-rowname, key='vars', value='scores')%>%
  ggplot(aes(reorder(rowname, scores), scores, fill=scores))+
  geom_col()+
  scale_fill_gradient(low = "#0000F7",high="#FFDD00")+
  facet_wrap(~ vars, ncol= 11)+
  coord_flip() + theme(plot.subtitle = element_text(colour = NA, 
    vjust = 1), plot.caption = element_text(vjust = 1), 
    axis.ticks = element_line(colour = NA), 
    panel.grid.major = element_line(colour = NA), 
    panel.grid.minor = element_line(colour = NA), 
    axis.title = element_text(family = "serif", 
        colour = NA), axis.text = element_text(colour = "gray4"), 
    axis.text.x = element_text(size = 8), 
    axis.text.y = element_text(size = 7), 
    plot.title = element_text(hjust = 0.5), 
    panel.background = element_rect(fill = NA), 
    legend.key = element_rect(fill = NA)) +labs(title = "Ranking for each Health Dimension",
    x = "Country", y = "Ranks", fill = "Ranking") + theme(axis.title = element_text(size = 8), 
    axis.text = element_text(size = 5, colour = "gray14", 
        vjust = 0), plot.title = element_text(size = 8)) + theme(axis.title = element_text(size = 7), 
    axis.text = element_text(size = 1), plot.title = element_text(size = 13))
```

![](health_files/figure-markdown_github/unnamed-chunk-15-1.png)

Summary
=======

So there we go. We can see South Africa did poorly on water, public services, human development, HIV and tuberculosis, which contributed to its position as dead last.

Meanwhile, Singapore is top of the list in nearly every category except Tuberculosis. The ranks hide the fact that in absolute numbers, the cases per 100,000 in Singapore are like 50-60, vs. in the USA where they are 2.9. Not a huge absolute difference but a huge relative one.

Most of the European countries would have come out clearly on top had their alcohol and tobacco consumption levels been lower.

In the future I'll perform more analyses on world countries. I'm looking at comparing Education or Civil Society/Culture as my next blog post. Stay tuned.


