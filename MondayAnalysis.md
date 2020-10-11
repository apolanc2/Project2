Monday Analysis
================
Ariana Polanco
10/10/2020

# Data

Read in the data\! Filter for the weekday of interest\!

``` r
library(dplyr)
library(caret)
data <- readr::read_csv("C:/Users/nelso/Documents/NCSU/ST 558/Project2/OnlineNewsPopularity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
data <- data %>% filter(weekday_is_monday==1) %>% select(-starts_with("weekday"))
set.seed(123)
train <- sample(1:nrow(data), size = nrow(data)*0.7)
test <- setdiff(1:nrow(diamonds), train)

dataTrain <- data[train,]
dataTest <- data[test,]
```

# Data Exploration

``` r
library(gridExtra)
head(data)
```

    ## # A tibble: 6 x 54
    ##   url   timedelta n_tokens_title n_tokens_content n_unique_tokens n_non_stop_words n_non_stop_uniq~ num_hrefs
    ##   <chr>     <dbl>          <dbl>            <dbl>           <dbl>            <dbl>            <dbl>     <dbl>
    ## 1 http~       731             12              219           0.664             1.00            0.815         4
    ## 2 http~       731              9              255           0.605             1.00            0.792         3
    ## 3 http~       731              9              211           0.575             1.00            0.664         3
    ## 4 http~       731              9              531           0.504             1.00            0.666         9
    ## 5 http~       731             13             1072           0.416             1.00            0.541        19
    ## 6 http~       731             10              370           0.560             1.00            0.698         2
    ## # ... with 46 more variables: num_self_hrefs <dbl>, num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>, data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>, data_channel_is_world <dbl>, kw_min_min <dbl>,
    ## #   kw_max_min <dbl>, kw_avg_min <dbl>, kw_min_max <dbl>, kw_max_max <dbl>, kw_avg_max <dbl>, kw_min_avg <dbl>,
    ## #   kw_max_avg <dbl>, kw_avg_avg <dbl>, self_reference_min_shares <dbl>, self_reference_max_shares <dbl>,
    ## #   self_reference_avg_sharess <dbl>, is_weekend <dbl>, LDA_00 <dbl>, LDA_01 <dbl>, LDA_02 <dbl>, LDA_03 <dbl>,
    ## #   LDA_04 <dbl>, global_subjectivity <dbl>, global_sentiment_polarity <dbl>, global_rate_positive_words <dbl>,
    ## #   global_rate_negative_words <dbl>, rate_positive_words <dbl>, rate_negative_words <dbl>, avg_positive_polarity <dbl>,
    ## #   min_positive_polarity <dbl>, max_positive_polarity <dbl>, avg_negative_polarity <dbl>, min_negative_polarity <dbl>,
    ## #   max_negative_polarity <dbl>, title_subjectivity <dbl>, title_sentiment_polarity <dbl>, abs_title_subjectivity <dbl>,
    ## #   abs_title_sentiment_polarity <dbl>, shares <dbl>

``` r
summary(data)
```

    ##      url              timedelta     n_tokens_title  n_tokens_content n_unique_tokens  n_non_stop_words
    ##  Length:6661        Min.   : 10.0   Min.   : 2.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000  
    ##  Class :character   1st Qu.:164.0   1st Qu.: 9.00   1st Qu.: 249.0   1st Qu.:0.4742   1st Qu.:1.0000  
    ##  Mode  :character   Median :332.0   Median :10.00   Median : 400.0   Median :0.5420   Median :1.0000  
    ##                     Mean   :351.6   Mean   :10.42   Mean   : 543.9   Mean   :0.5314   Mean   :0.9715  
    ##                     3rd Qu.:542.0   3rd Qu.:12.00   3rd Qu.: 720.0   3rd Qu.:0.6087   3rd Qu.:1.0000  
    ##                     Max.   :731.0   Max.   :18.00   Max.   :7764.0   Max.   :1.0000   Max.   :1.0000  
    ##  n_non_stop_unique_tokens   num_hrefs      num_self_hrefs      num_imgs        num_videos     average_token_length
    ##  Min.   :0.0000           Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000       
    ##  1st Qu.:0.6289           1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.:4.479       
    ##  Median :0.6923           Median :  7.00   Median : 3.000   Median : 1.000   Median : 0.000   Median :4.656       
    ##  Mean   :0.6743           Mean   : 10.74   Mean   : 3.398   Mean   : 4.447   Mean   : 1.336   Mean   :4.547       
    ##  3rd Qu.:0.7551           3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.: 3.000   3rd Qu.: 1.000   3rd Qu.:4.841       
    ##  Max.   :1.0000           Max.   :162.00   Max.   :51.000   Max.   :93.000   Max.   :75.000   Max.   :8.042       
    ##   num_keywords    data_channel_is_lifestyle data_channel_is_entertainment data_channel_is_bus data_channel_is_socmed
    ##  Min.   : 1.000   Min.   :0.00000           Min.   :0.0000                Min.   :0.0000      Min.   :0.00000       
    ##  1st Qu.: 6.000   1st Qu.:0.00000           1st Qu.:0.0000                1st Qu.:0.0000      1st Qu.:0.00000       
    ##  Median : 7.000   Median :0.00000           Median :0.0000                Median :0.0000      Median :0.00000       
    ##  Mean   : 7.141   Mean   :0.04834           Mean   :0.2039                Mean   :0.1731      Mean   :0.05059       
    ##  3rd Qu.: 9.000   3rd Qu.:0.00000           3rd Qu.:0.0000                3rd Qu.:0.0000      3rd Qu.:0.00000       
    ##  Max.   :10.000   Max.   :1.00000           Max.   :1.0000                Max.   :1.0000      Max.   :1.00000       
    ##  data_channel_is_tech data_channel_is_world   kw_min_min       kw_max_min       kw_avg_min        kw_min_max    
    ##  Min.   :0.0000       Min.   :0.0000        Min.   : -1.00   Min.   :     0   Min.   :   -1.0   Min.   :     0  
    ##  1st Qu.:0.0000       1st Qu.:0.0000        1st Qu.: -1.00   1st Qu.:   440   1st Qu.:  136.0   1st Qu.:     0  
    ##  Median :0.0000       Median :0.0000        Median : -1.00   Median :   648   Median :  229.2   Median :  1300  
    ##  Mean   :0.1854       Mean   :0.2036        Mean   : 26.28   Mean   :  1240   Mean   :  318.8   Mean   : 11781  
    ##  3rd Qu.:0.0000       3rd Qu.:0.0000        3rd Qu.:  4.00   3rd Qu.:  1000   3rd Qu.:  354.1   3rd Qu.:  7100  
    ##  Max.   :1.0000       Max.   :1.0000        Max.   :318.00   Max.   :298400   Max.   :42827.9   Max.   :843300  
    ##    kw_max_max       kw_avg_max       kw_min_avg       kw_max_avg       kw_avg_avg    self_reference_min_shares
    ##  Min.   :     0   Min.   :     0   Min.   :  -1.0   Min.   :     0   Min.   :    0   Min.   :     0           
    ##  1st Qu.:843300   1st Qu.:173688   1st Qu.:   0.0   1st Qu.:  3531   1st Qu.: 2361   1st Qu.:   671           
    ##  Median :843300   Median :244636   Median : 975.4   Median :  4273   Median : 2841   Median :  1200           
    ##  Mean   :748525   Mean   :258792   Mean   :1074.8   Mean   :  5580   Mean   : 3076   Mean   :  3901           
    ##  3rd Qu.:843300   3rd Qu.:332686   3rd Qu.:1978.7   3rd Qu.:  5939   3rd Qu.: 3540   3rd Qu.:  2600           
    ##  Max.   :843300   Max.   :843300   Max.   :3602.1   Max.   :298400   Max.   :43568   Max.   :690400           
    ##  self_reference_max_shares self_reference_avg_sharess   is_weekend     LDA_00            LDA_01            LDA_02       
    ##  Min.   :     0            Min.   :     0             Min.   :0    Min.   :0.01818   Min.   :0.01819   Min.   :0.01819  
    ##  1st Qu.:  1100            1st Qu.:  1014             1st Qu.:0    1st Qu.:0.02520   1st Qu.:0.02504   1st Qu.:0.02857  
    ##  Median :  2900            Median :  2200             Median :0    Median :0.03345   Median :0.03337   Median :0.04000  
    ##  Mean   : 10074            Mean   :  6351             Mean   :0    Mean   :0.18954   Mean   :0.15297   Mean   :0.20648  
    ##  3rd Qu.:  8000            3rd Qu.:  5200             3rd Qu.:0    3rd Qu.:0.25776   3rd Qu.:0.17249   3rd Qu.:0.31581  
    ##  Max.   :843300            Max.   :690400             Max.   :0    Max.   :0.91999   Max.   :0.91997   Max.   :0.92000  
    ##      LDA_03            LDA_04        global_subjectivity global_sentiment_polarity global_rate_positive_words
    ##  Min.   :0.01819   Min.   :0.01818   Min.   :0.0000      Min.   :-0.38021          Min.   :0.00000           
    ##  1st Qu.:0.02857   1st Qu.:0.02857   1st Qu.:0.3949      1st Qu.: 0.05588          1st Qu.:0.02846           
    ##  Median :0.04000   Median :0.04005   Median :0.4516      Median : 0.11813          Median :0.03851           
    ##  Mean   :0.21816   Mean   :0.23285   Mean   :0.4413      Mean   : 0.11721          Mean   :0.03932           
    ##  3rd Qu.:0.35101   3rd Qu.:0.40040   3rd Qu.:0.5051      3rd Qu.: 0.17503          3rd Qu.:0.04985           
    ##  Max.   :0.92653   Max.   :0.92708   Max.   :1.0000      Max.   : 0.57551          Max.   :0.13636           
    ##  global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity min_positive_polarity
    ##  Min.   :0.000000           Min.   :0.0000      Min.   :0.0000      Min.   :0.0000        Min.   :0.00000      
    ##  1st Qu.:0.009749           1st Qu.:0.6000      1st Qu.:0.1864      1st Qu.:0.3056        1st Qu.:0.05000      
    ##  Median :0.015447           Median :0.7083      Median :0.2830      Median :0.3584        Median :0.10000      
    ##  Mean   :0.016821           Mean   :0.6806      Mean   :0.2907      Mean   :0.3545        Mean   :0.09525      
    ##  3rd Qu.:0.021818           3rd Qu.:0.8000      3rd Qu.:0.3846      3rd Qu.:0.4121        3rd Qu.:0.10000      
    ##  Max.   :0.092160           Max.   :1.0000      Max.   :1.0000      Max.   :1.0000        Max.   :1.00000      
    ##  max_positive_polarity avg_negative_polarity min_negative_polarity max_negative_polarity title_subjectivity
    ##  Min.   :0.0000        Min.   :-1.0000       Min.   :-1.000        Min.   :-1.0000       Min.   :0.0000    
    ##  1st Qu.:0.6000        1st Qu.:-0.3292       1st Qu.:-0.700        1st Qu.:-0.1250       1st Qu.:0.0000    
    ##  Median :0.8000        Median :-0.2531       Median :-0.500        Median :-0.1000       Median :0.1000    
    ##  Mean   :0.7603        Mean   :-0.2594       Mean   :-0.521        Mean   :-0.1058       Mean   :0.2754    
    ##  3rd Qu.:1.0000        3rd Qu.:-0.1861       3rd Qu.:-0.300        3rd Qu.:-0.0500       3rd Qu.:0.5000    
    ##  Max.   :1.0000        Max.   : 0.0000       Max.   : 0.000        Max.   : 0.0000       Max.   :1.0000    
    ##  title_sentiment_polarity abs_title_subjectivity abs_title_sentiment_polarity     shares      
    ##  Min.   :-1.00000         Min.   :0.0000         Min.   :0.0000               Min.   :     1  
    ##  1st Qu.: 0.00000         1st Qu.:0.1500         1st Qu.:0.0000               1st Qu.:   919  
    ##  Median : 0.00000         Median :0.5000         Median :0.0000               Median :  1400  
    ##  Mean   : 0.06549         Mean   :0.3408         Mean   :0.1509               Mean   :  3647  
    ##  3rd Qu.: 0.13636         3rd Qu.:0.5000         3rd Qu.:0.2500               3rd Qu.:  2700  
    ##  Max.   : 1.00000         Max.   :0.5000         Max.   :1.0000               Max.   :690400

``` r
# create a new variable "dataType" to be able to graph the frequency of the data channel type. 
data <- mutate(data,dataType = ifelse((data_channel_is_lifestyle + data_channel_is_lifestyle + data_channel_is_bus +                                   data_channel_is_socmed + data_channel_is_tech + data_channel_is_world) == 0, NA, 
                            ifelse((data_channel_is_lifestyle + data_channel_is_lifestyle + data_channel_is_bus +                                       data_channel_is_socmed + data_channel_is_tech + data_channel_is_world) != 1 , "Multi",
                            ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                            ifelse(data_channel_is_entertainment ==1, "Entertainment",
                            ifelse(data_channel_is_bus == 1, "Business", 
                            ifelse(data_channel_is_socmed==1, "Social Media", 
                            ifelse(data_channel_is_tech ==1, "Tech", 
                            ifelse(data_channel_is_world ==1, "World", NA)))))))))
table(data$dataType)
```

    ## 
    ##     Business        Multi Social Media         Tech        World 
    ##         1153          322          337         1235         1356

``` r
g <- ggplot(data=data, aes(x=dataType))
g + geom_bar()
```

![](MondayAnalysis_files/figure-gfm/eda-1.png)<!-- -->

``` r
p <- ggplot(data=data, aes(y=shares))
p1 <- p + geom_jitter(aes(x=n_tokens_title))
p2 <- p + geom_jitter(aes(x=num_imgs))
p3 <- p + geom_jitter(aes(x=num_videos))
grid.arrange(p1,p2,p3)
```

![](MondayAnalysis_files/figure-gfm/eda-2.png)<!-- -->

# Modeling

``` r
classification_tree  <- train(shares ~ . , data = dataTrain, method = "rpart", trControl = trainControl(method="cv"),
                 preProcess = c("center","scale"))
```

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/lego-taking-robotics-to-next-level-with-mindstorms-ev3/, urlhttp://mashable.com/
    ## 2013/01/07/panasonic-second-screen/, urlhttp://mashable.com/2013/01/07/power-matters-alliance-organization/, urlhttp://
    ## mashable.com/2013/01/14/branch-roundtable-opens/, urlhttp://mashable.com/2013/01/14/skittles-twitter/, urlhttp://
    ## mashable.com/2013/01/21/facebook-poke-one-month-later/, urlhttp://mashable.com/2013/01/21/inauguration-gifs/, urlhttp://
    ## mashable.com/2013/01/28/budweiser-twitter/, urlhttp://mashable.com/2013/01/28/check-out-this-full-body-tattoo-infographic-
    ## about-tattoos/, urlhttp://mashable.com/2013/01/28/crashlytics-joins-twitter/, urlhttp://mashable.com/2013/01/28/digital-
    ## jobs-hbo/, urlhttp://mashable.com/2013/01/28/grubwithus-grubtonight/, urlhttp://mashable.com/2013/01/28/soundrop-
    ## listening-rooms-facebook/, urlhttp://mashable.com/2013/01/28/twitter-data-warrant/, urlhttp://mashable.com/2013/01/28/
    ## youtube-super-bowl-ads-more-views/, urlhttp://mashable.com/2013/02/04/baby-boomer-apps/, urlhttp://mashable.com/
    ## 2013/02/04/geeks-civil-liberties/, urlhttp://mashable.com/2013/02/04/twitter-super-bowl-mentions/, urlhttp://mashable.com/
    ## 2013/02/04/work-at-fab/, urlhttp://mashable.com/2013/02/11/justin-timberlake-ad-bud-light/, urlhttp://mashable.com/
    ## 2013/02/11/taylor-swift-gets-death-threats-on-twitter-after-dissing-ex-at-grammys/, urlhttp://mashable.com/2013/02/18/
    ## grumpy-cat-etsy/, urlhttp://mashable.com/2013/02/18/reactions-to-burger-king-twitter-hacked/, urlhttp://mashable.com/
    ## 2013/02/25/alicia-keys-blackberry-project/, urlhttp://mashable.com/2013/02/25/camera-plus-tips-tricks/, urlhttp://
    ## mashable.com/2013/02/25/ghost-prank/, urlhttp://mashable.com/2013/02/25/guardian-wi-fi/, urlhttp://mashable.com/
    ## 2013/02/25/luxi/, urlhttp://mashable.com/2013/02/25/oscar-gifs-2/, urlhttp://mashable.com/2013/02/25/oscar-musical-
    ## numbers/, urlhttp://mashable.com/2013/02/25/oscars-jennifer-aniston-best-dressed/, urlhttp://mashable.com/2013/03/04/
    ## apple-market-cap-400-billion/, urlhttp://mashable.com/2013/03/04/dennis-rodman-north-korea-interview/, urlhttp://
    ## mashable.com/2013/03/04/how-hubble-space-telescope-works/, urlhttp://mashable.com/2013/03/04/landing-interview-startup/,
    ## urlhttp://mashable.com/2013/03/04/samsung-galaxy-s4-eye-scroll/, urlhttp://mashable.com/2013/03/04/super-smash-kittens/,
    ## urlhttp://mashable.com/2013/03/04/sxsw-panels/, urlhttp://mashable.com/2013/03/04/wacom-tablet/, urlhttp://mashable.com/
    ## 2013/03/11/facebook-kids-sale-arrested/, urlhttp://mashable.com/2013/03/11/game-of-thrones-high-school/, urlhttp://
    ## mashable.com/2013/03/11/instagram-companion-apps/, urlhttp://mashable.com/2013/03/11/jobs-mtv/, urlhttp://mashable.com/
    ## 2013/03/11/samsung-galaxy-s-iv-2/, urlhttp://mashable.com/2013/03/11/samsung-galaxy-tab-sprint-jelly-bean/, urlhttp://
    ## mashable.com/2013/03/11/sxsw-i-day-4/, urlhttp://mashable.com/2013/03/11/tweet-paintball-gun/, urlhttp://mashable.com/
    ## 2013/03/18/google-glass-backlash/, urlhttp://mashable.com/2013/03/18/google-maps-mountains/, urlhttp://mashable.com/
    ## 2013/03/18/huh-antelope-video/, urlhttp://mashable.com/2013/03/18/image-toaster/, urlhttp://mashable.com/2013/03/18/ipad-
    ## hacker-sentence/, urlhttp://mashable.com/2013/03/18/job-listings-the-onion/, urlhttp://mashable.com/2013/03/18/verizon-
    ## app-challenge/, urlhttp://mashable.com/2013/03/18/washington-post-paywall/, urlhttp://mashable.com/2013/03/25/backstreet-
    ## boys-harlem-shake/, urlhttp://mashable.com/2013/03/25/biggest-moments-photos/, urlhttp://mashable.com/2013/03/25/mtv-
    ## video-music-awards-vmas-brooklyn/, urlhttp://mashable.com/2013/03/25/viral-video-recap-26/, urlhttp://mashable.com/
    ## 2013/03/25/yahoo-acquires-summly/, urlhttp://mashable.com/2013/04/01/dancing-with-the-stars-abc-comedy/, urlhttp://
    ## mashable.com/2013/04/01/hulu-april-fools-day/, urlhttp://mashable.com/2013/04/01/monsters-university-hacked-fear-tech/,
    ## urlhttp://mashable.com/2013/04/01/pitch-from-baseball-perspective/, urlhttp://mashable.com/2013/04/08/facebook-home-leaks-
    ## online-but-dont-download-it-yet/, urlhttp://mashable.com/2013/04/08/hopstop-live/, urlhttp://mashable.com/2013/04/08/
    ## wikileaks-releases-1-7-million-u-s-diplomatic-records-in-the-kissinger-cables/, urlhttp://mashable.com/2013/04/15/
    ## samsung-galaxy-s4-canada/, urlhttp://mashable.com/2013/04/22/facebook-home-downloads/, urlhttp://mashable.com/2013/04/22/
    ## music-monday-spring-cleaning/, urlhttp://mashable.com/2013/04/22/seventh-woods-14-year-old/, urlhttp://mashable.com/
    ## 2013/04/22/white-house-first-vine/, urlhttp://mashable.com/2013/04/29/galaxy-s4-drop-test/, urlhttp://mashable.com/
    ## 2013/04/29/jason-collins-twitter-2/, urlhttp://mashable.com/2013/04/29/lg-curved-oled-tv/, urlhttp://mashable.com/
    ## 2013/04/29/shazam-ceo-ipo/, urlhttp://mashable.com/2013/04/29/social-media-advice-column-4-29/, urlhttp://mashable.com/
    ## 2013/05/06/cmo-data/, urlhttp://mashable.com/2013/05/06/knicks-jr-smith-haters-gonna-hate/, urlhttp://mashable.com/
    ## 2013/05/06/sports-twitter-sloane-stephens/, urlhttp://mashable.com/2013/05/06/startup-plateau/, urlhttp://mashable.com/
    ## 2013/05/06/textastrophe-pranks/, urlhttp://mashable.com/2013/05/06/your-phone-is-dirtier-than-these-5-objects/, urlhttp://
    ## mashable.com/2013/05/13/an-epic-space-exploration-playlist/, urlhttp://mashable.com/2013/05/13/music-video-from-space/,
    ## urlhttp://mashable.com/2013/05/13/quintet-game/, urlhttp://mashable.com/2013/05/13/samsung-5g/, urlhttp://mashable.com/
    ## 2013/05/20/dove-ad-most-watche/, urlhttp://mashable.com/2013/06/03/apple-ebook-publishers/, urlhttp://mashable.com/
    ## 2013/06/03/home-design-apps/, urlhttp://mashable.com/2013/06/03/ios-apps-you-never-use/, urlhttp://mashable.com/
    ## 2013/06/03/job-hunting/, urlhttp://mashable.com/2013/06/03/jordan-censorship-law/, urlhttp://mashable.com/2013/06/03/
    ## makerbot-factory/, urlhttp://mashable.com/2013/06/03/pinterest-dream-job/, urlhttp://mashable.com/2013/06/03/youtube-
    ## marketing/, urlhttp://mashable.com/2013/06/10/amazonfresh-los-angeles/, urlhttp://mashable.com/2013/06/10/pinball-
    ## rocks-game-app/, urlhttp://mashable.com/2013/06/10/tim-berners-lee-nsa-surveillance/, urlhttp://mashable.com/2013/06/17/
    ## openairplane/, urlhttp://mashable.com/2013/06/24/camp-grounded-digital-detox-campers/, urlhttp://mashable.com/2013/06/24/
    ## kid-attacks-dad-foul-ball/, urlhttp://mashable.com/2013/07/01/apple-iwatch-2/, urlhttp://mashable.com/2013/07/01/edward-
    ## snowden-movie/, urlhttp://mashable.com/2013/07/01/gross-food-names/, urlhttp://mashable.com/2013/07/01/nelson-mandela-
    ## vine/, urlhttp://mashable.com/2013/07/01/snowden-asylum-russia/, urlhttp://mashable.com/2013/07/08/cats-bacon-rule-
    ## internet/, urlhttp://mashable.com/2013/07/08/dublin-taxi-get-lucky/, urlhttp://mashable.com/2013/07/08/pitchfork-the-
    ## dissolve-launch/, urlhttp://mashable.com/2013/07/15/carly-rae-jepsen-baseball/, urlhttp://mashable.com/2013/07/15/elon-
    ## musk-hyperloop-design/, urlhttp://mashable.com/2013/07/15/kickstarter-human-powered-helicopter-sikorsky/, urlhttp://
    ## mashable.com/2013/07/15/microwave-barcode-hack/, urlhttp://mashable.com/2013/07/15/real-time-marketing/, urlhttp://
    ## mashable.com/2013/07/15/universal-music-uvinyl-the-vinyl-project/, urlhttp://mashable.com/2013/07/22/apples-developer-
    ## portal-hacked/, urlhttp://mashable.com/2013/07/22/cosplay-models/, urlhttp://mashable.com/2013/07/22/skydrive-
    ## windows81/, urlhttp://mashable.com/2013/07/29/spotify-comedy-app/, urlhttp://mashable.com/2013/07/29/swimming-pool-dunk-
    ## videos/, urlhttp://mashable.com/2013/08/05/branding-mistakes/, urlhttp://mashable.com/2013/08/05/jeff-bezos-investment-
    ## list/, urlhttp://mashable.com/2013/08/05/mlb-steroid-suspensions/, urlhttp://mashable.com/2013/08/12/currency-collage/,
    ## urlhttp://mashable.com/2013/08/12/easily-retrievable-asteroids/, urlhttp://mashable.com/2013/08/12/norways-prime-minister-
    ## taxi/, urlhttp://mashable.com/2013/08/12/socially-connected-country/, urlhttp://mashable.com/2013/08/12/spy-photos-from-
    ## space/, urlhttp://mashable.com/2013/08/12/wallpaper-iphone/, urlhttp://mashable.com/2013/08/12/who-would-buy-blackberry/,
    ## urlhttp://mashable.com/2013/08/19/linkedin-university-pages/, urlhttp://mashable.com/2013/08/19/twitter-related-
    ## headlines/, urlhttp://mashable.com/2013/08/26/mtv-2

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/creature-cups/, urlhttp://mashable.com/2013/01/07/lionel-messi-fashion-moments/,
    ## urlhttp://mashable.com/2013/01/07/living-room-pinterest-contest/, urlhttp://mashable.com/2013/01/07/philips-fidelio-
    ## p8-speaker/, urlhttp://mashable.com/2013/01/07/reeddit-reddit/, urlhttp://mashable.com/2013/01/14/darpa-design-a-tank/,
    ## urlhttp://mashable.com/2013/01/14/disney-wreck-it-ralph-8-bit-lane/, urlhttp://mashable.com/2013/01/14/illumiroom-
    ## microsoft/, urlhttp://mashable.com/2013/01/14/justin-timberlake-suit-and-tie-jay-z/, urlhttp://mashable.com/2013/01/14/
    ## top-twitter-pics-1-14/, urlhttp://mashable.com/2013/01/21/beyonce-national-anthem/, urlhttp://mashable.com/2013/01/21/
    ## hoteltonight-4-million/, urlhttp://mashable.com/2013/01/28/baby-snatching-eagle-scholarship/, urlhttp://mashable.com/
    ## 2013/01/28/disney-tomorrowland-1952/, urlhttp://mashable.com/2013/01/28/intel-mobile-chip-challenges/, urlhttp://
    ## mashable.com/2013/01/28/just-vined/, urlhttp://mashable.com/2013/01/28/mpaa-lobbying-for-drones/, urlhttp://mashable.com/
    ## 2013/01/28/pentagon-beefs-up-cybersecurity-force/, urlhttp://mashable.com/2013/01/28/tweet-seats-theater/, urlhttp://
    ## mashable.com/2013/01/28/winter-clothing-tech/, urlhttp://mashable.com/2013/02/04/beyonce-sloppy-swish/, urlhttp://
    ## mashable.com/2013/02/04/blackberry-keep-moving-tv-ad/, urlhttp://mashable.com/2013/02/04/nasas-crazy-robot-lab/,
    ## urlhttp://mashable.com/2013/02/04/will-ferrell-old-milwaukee-super-bowl-ad/, urlhttp://mashable.com/2013/02/04/women-
    ## in-science-infographic/, urlhttp://mashable.com/2013/02/04/world-cancer-day/, urlhttp://mashable.com/2013/02/04/youtube-
    ## not-live-streaming-fashion-week/, urlhttp://mashable.com/2013/02/11/justin-timberlake-mirrors/, urlhttp://mashable.com/
    ## 2013/02/11/lego-star-wars-2/, urlhttp://mashable.com/2013/02/11/super-mario-busters/, urlhttp://mashable.com/2013/02/18/
    ## blogologue-comedy/, urlhttp://mashable.com/2013/02/25/dkny-apologizes-via-tumblr-for-using-photos-without-permission/,
    ## urlhttp://mashable.com/2013/02/25/game-of-thrones-facebook/, urlhttp://mashable.com/2013/02/25/jennifer-lawrence-trips-
    ## oscars/, urlhttp://mashable.com/2013/02/25/nokia-lumia-520-and-720/, urlhttp://mashable.com/2013/02/25/six-strikes-
    ## internet-activists/, urlhttp://mashable.com/2013/03/04/pulp-xbox-live-movie/, urlhttp://mashable.com/2013/03/04/startups-
    ## mission-vine-contest/, urlhttp://mashable.com/2013/03/11/cat-camera-dinner/, urlhttp://mashable.com/2013/03/11/google-
    ## glass-bar/, urlhttp://mashable.com/2013/03/11/kids-harlem-shake/, urlhttp://mashable.com/2013/03/11/mashable-sxsw-keynote-
    ## video/, urlhttp://mashable.com/2013/03/11/rovio-corporate-transition/, urlhttp://mashable.com/2013/03/11/simcity-server-
    ## problems/, urlhttp://mashable.com/2013/03/11/social-tv-chart-3-11/, urlhttp://mashable.com/2013/03/11/split-screens/,
    ## urlhttp://mashable.com/2013/03/11/tivo-mini-now-available-for-99/, urlhttp://mashable.com/2013/03/18/simcity-free-game/,
    ## urlhttp://mashable.com/2013/03/18/weev-interview/, urlhttp://mashable.com/2013/03/25/coolest-things-coming-to-home/,
    ## urlhttp://mashable.com/2013/03/25/linkedin-improves-its-search/, urlhttp://mashable.com/2013/03/25/mad-men-season-6-
    ## trailer/, urlhttp://mashable.com/2013/03/25/michael-dell-takeover-bid/, urlhttp://mashable.com/2013/03/25/paintable-
    ## semiconductor/, urlhttp://mashable.com/2013/03/25/zuckerberg-immigration/, urlhttp://mashable.com/2013/04/01/google-
    ## treasure-map-brief/, urlhttp://mashable.com/2013/04/01/kevin-ware-injury/, urlhttp://mashable.com/2013/04/01/kickstarter-
    ## sensations-gaming/, urlhttp://mashable.com/2013/04/01/social-tv-chart-4-1/, urlhttp://mashable.com/2013/04/01/tesla-
    ## is-profitable/, urlhttp://mashable.com/2013/04/01/troll-appreciation-day-tickets-2/, urlhttp://mashable.com/2013/04/01/
    ## youtube-10-prank-videos/, urlhttp://mashable.com/2013/04/08/cher-nowthatchersdead-twitter-hashtag-dead/, urlhttp://
    ## mashable.com/2013/04/08/facebook-charging-brief/, urlhttp://mashable.com/2013/04/08/htc-profit-dwindles/, urlhttp://
    ## mashable.com/2013/04/08/ipad-help-mom-bond-with-baby/, urlhttp://mashable.com/2013/04/08/reasons-my-son-is-crying-
    ## tumblr/, urlhttp://mashable.com/2013/04/15/controversy-planet-naming-contest/, urlhttp://mashable.com/2013/04/15/ea-
    ## closing-facebook-games/, urlhttp://mashable.com/2013/04/15/google-glass-ready-to-ship/, urlhttp://mashable.com/2013/04/22/
    ## antares-rocket-launch-success/, urlhttp://mashable.com/2013/04/22/iphone-users-first-dates/, urlhttp://mashable.com/
    ## 2013/04/22/mashable-webby-nominations/, urlhttp://mashable.com/2013/04/22/reddit-cispa-blackouts/, urlhttp://mashable.com/
    ## 2013/04/22/yahoo-relaunches-iphone-app-summly/, urlhttp://mashable.com/2013/04/29/downton-abbey-remix/, urlhttp://
    ## mashable.com/2013/04/29/fandango-fox-network/, urlhttp://mashable.com/2013/04/29/google-bans-self-updating-apps/,
    ## urlhttp://mashable.com/2013/04/29/kobe-bryant-bill-clinton-tweet-support-as-jason-collins-comes-out-as-gay/, urlhttp://
    ## mashable.com/2013/04/29/programs-health-wellness/, urlhttp://mashable.com/2013/05/06/3d-printed-gun-fired-on-video/,
    ## urlhttp://mashable.com/2013/05/06/digital-cameras-bug-eyes/, urlhttp://mashable.com/2013/05/06/facebook-fortune-500/,
    ## urlhttp://mashable.com/2013/05/06/rob-delaney-mlb-twitter/, urlhttp://mashable.com/2013/05/06/tortilla-chip-joke/,
    ## urlhttp://mashable.com/2013/05/06/toxic-detector-gloves/, urlhttp://mashable.com/2013/05/13/google-unified-storage/,
    ## urlhttp://mashable.com/2013/05/13/linkedin-prostitution/, urlhttp://mashable.com/2013/05/13/tonight-show-gas-pump-fake/,
    ## urlhttp://mashable.com/2013/05/20/best-free-iphone-apps/, urlhttp://mashable.com/2013/05/20/in-google-breach-chinese-
    ## hackers-access-sensitive-u-s-government-data/, urlhttp://mashable.com/2013/05/20/videos-oklahoma-tornado/, urlhttp://
    ## mashable.com/2013/05/27/plus-social-good/, urlhttp://mashable.com/2013/05/27/reddit-thread-movie/, urlhttp://mashable.com/
    ## 2013/06/03/nba-hibbert-collins-twitter/, urlhttp://mashable.com/2013/06/03/trick-football-hikes/, urlhttp://mashable.com/
    ## 2013/06/03/twitter-replies-view/, urlhttp://mashable.com/2013/06/03/zenbook-inifinity-brief/, urlhttp://mashable.com/
    ## 2013/06/10/apple-2013-wwdc-keynote-analysis/, urlhttp://mashable.com/2013/06/10/apple-wwdc-2013-music-playlist/,
    ## urlhttp://mashable.com/2013/06/10/chinese-internet-edward-snowden/, urlhttp://mashable.com/2013/06/10/edward-snowden-
    ## internet-freedom/, urlhttp://mashable.com/2013/06/10/google-doodle-where-the-wild-things-are/, urlhttp://mashable.com/
    ## 2013/06/10/google-waze-brief-2/, urlhttp://mashable.com/2013/06/10/itunes-radio-compared-pandora/, urlhttp://mashable.com/
    ## 2013/06/10/sony-playstation-e3-press-conference-live/, urlhttp://mashable.com/2013/06/10/xbox-one-availabilty-november/,
    ## urlhttp://mashable.com/2013/06/16/twitter-lemmings/, urlhttp://mashable.com/2013/06/17/ad-blocker-helps-ad-industry/,
    ## urlhttp://mashable.com/2013/06/17/dream-job-vimeo/, urlhttp://mashable.com/2013/06/17/neflix-dreamworks-shows/,
    ## urlhttp://mashable.com/2013/06/24/amazon-lovefilm-cbs/, urlhttp://mashable.com/2013/06/24/aol-reader-launch/, urlhttp://
    ## mashable.com/2013/06/24/att-50-percent-smartphones/, urlhttp://mashable.com/2013/07/01/arizona-wildfire-video/, urlhttp://
    ## mashable.com/2013/07/01/egypt-protestors-laser-pointers/, urlhttp://mashable.com/2013/07/01/google-reader-last-day-
    ## brief/, urlhttp://mashable.com/2013/07/01/sly-and-arnie-join-forces-in-new-escape-plan-trailer/, urlhttp://mashable.com/
    ## 2013/07/01/social-good-summit-2013-announcement/, urlhttp://mashable.com/2013/07/08/ellie-goulding-burn-vine/, urlhttp://
    ## mashable.com/2013/07/08/facebook-home-update/, urlhttp://mashable.com/2013/07/08/nikon-camera-concept/, urlhttp://
    ## mashable.com/2013/07/08/ntsb-tweets-asiana-crash/, urlhttp://mashable.com/2013/07/08/spacex-grasshopper-hovers-lands/,
    ## urlhttp://mashable.com/2013/07/14/twitter-george-zimmerman/, urlhttp://mashable.com/2013/07/15/firefly-pick-kickstarter/,
    ## urlhttp://mashable.com/2013/07/15/mlb-commissioner-bud-selig-email/, urlhttp://mashable.com/2013/07/22/app-overload-
    ## clean-house/, urlhttp://mashable.com/2013/07/22/college-football-best-fans/, urlhttp://mashable.com/2013/07/22/comic-con-
    ## newbie/, urlhttp://mashab

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/att-u-verse-apps/, urlhttp://mashable.com/2013/01/07/iheartradio-app-perfect-
    ## for/, urlhttp://mashable.com/2013/01/07/polaroid-android-camera/, urlhttp://mashable.com/2013/01/07/targus-touch-
    ## pen-windows-8/, urlhttp://mashable.com/2013/01/14/aaron-swartz-sopa/, urlhttp://mashable.com/2013/01/14/getty-images-
    ## get-social-with-powerful-new-trending-tool/, urlhttp://mashable.com/2013/01/14/star-wars-light-speed/, urlhttp://
    ## mashable.com/2013/01/14/tina-fey-amy-poehler-jokes/, urlhttp://mashable.com/2013/01/14/tinto-river-martian-life/,
    ## urlhttp://mashable.com/2013/01/21/barack-obama-inauguration/, urlhttp://mashable.com/2013/01/21/eric-schmidt-north-
    ## korea/, urlhttp://mashable.com/2013/01/21/hulu-ad-blocking/, urlhttp://mashable.com/2013/01/21/inspiring-inauguration-
    ## photos/, urlhttp://mashable.com/2013/01/21/land-a-job-at-klout/, urlhttp://mashable.com/2013/01/21/obama-inauguration-
    ## twitter/, urlhttp://mashable.com/2013/01/21/stan-lee-youtube-shooting-victim/, urlhttp://mashable.com/2013/01/28/
    ## apple-maps-my-butt/, urlhttp://mashable.com/2013/01/28/facebook-down-1-28/, urlhttp://mashable.com/2013/01/28/rim-big-
    ## tech-comback/, urlhttp://mashable.com/2013/02/04/most-pirated-films/, urlhttp://mashable.com/2013/02/04/shaq-lipsyncs-
    ## beyonce/, urlhttp://mashable.com/2013/02/04/unicorn-accessories/, urlhttp://mashable.com/2013/02/11/barbie-makeup-
    ## mirror/, urlhttp://mashable.com/2013/02/11/facebook-is-down-for-some/, urlhttp://mashable.com/2013/02/11/flickr-bug-
    ## private-photos/, urlhttp://mashable.com/2013/02/11/report-twitter-now-charges-200000-for-promoted-trends/, urlhttp://
    ## mashable.com/2013/02/11/state-of-the-union-interactive/, urlhttp://mashable.com/2013/02/18/apple-ipad-ads/, urlhttp://
    ## mashable.com/2013/02/18/black-keys-drummer-justin-bieber/, urlhttp://mashable.com/2013/02/18/bungies-destiny-brief/,
    ## urlhttp://mashable.com/2013/02/18/jerry-buss-twitter/, urlhttp://mashable.com/2013/02/18/most-expensive-starbucks/,
    ## urlhttp://mashable.com/2013/02/25/cats-that-look-like/, urlhttp://mashable.com/2013/02/25/expensive-hotels-charge-more-wi-
    ## fi/, urlhttp://mashable.com/2013/02/25/jennifer-lawrence-oscars-press-interview/, urlhttp://mashable.com/2013/02/25/snow-
    ## moon/, urlhttp://mashable.com/2013/03/04/brands-harlem-shake-youtube-facebook-roi/, urlhttp://mashable.com/2013/03/04/
    ## game-of-thrones-season-3-extended-trailer/, urlhttp://mashable.com/2013/03/04/grammar/, urlhttp://mashable.com/2013/03/04/
    ## michelle-obama-google-hangout/, urlhttp://mashable.com/2013/03/04/simcity-review/, urlhttp://mashable.com/2013/03/11/
    ## equala/, urlhttp://mashable.com/2013/03/11/lg-optimus-l5ii-global-rollout/, urlhttp://mashable.com/2013/03/18/marriage-
    ## equality-letter/, urlhttp://mashable.com/2013/03/18/spacex-grasshopper-ring-of-fire/, urlhttp://mashable.com/2013/03/18/
    ## t-mobile-mystery-event/, urlhttp://mashable.com/2013/03/25/angry-birds-kennedy/, urlhttp://mashable.com/2013/03/25/
    ## campbells-hack-the-kitchen/, urlhttp://mashable.com/2013/03/25/cfaa-amendment/, urlhttp://mashable.com/2013/03/25/netflix-
    ## television-buzz-infographic/, urlhttp://mashable.com/2013/03/25/smartphone-recycling/, urlhttp://mashable.com/2013/03/25/
    ## windows-blue-brief/, urlhttp://mashable.com/2013/04/01/aereo-injunction/, urlhttp://mashable.com/2013/04/01/events-
    ## listing-sony/, urlhttp://mashable.com/2013/04/01/greatest-moments-mlb/, urlhttp://mashable.com/2013/04/01/ipad-mini-
    ## denied-trademark-in-u-s/, urlhttp://mashable.com/2013/04/01/sf-da-iphone/, urlhttp://mashable.com/2013/04/08/brad-paisley-
    ## accidental-racist/, urlhttp://mashable.com/2013/04/08/makeup-pet-peeves-reddit/, urlhttp://mashable.com/2013/04/08/the-
    ## beatles-let-it-be-naked-itunes/, urlhttp://mashable.com/2013/04/15/boston-marathon-room-offers/, urlhttp://mashable.com/
    ## 2013/04/15/google-glass-api/, urlhttp://mashable.com/2013/04/15/job-openings-social-media/, urlhttp://mashable.com/
    ## 2013/04/15/simpsons-opener-breaking-bad/, urlhttp://mashable.com/2013/04/15/why-mozilla-building-mobile-os/, urlhttp://
    ## mashable.com/2013/04/15/windows-8-boot-to-desktop/, urlhttp://mashable.com/2013/04/15/youtube-look-your-best/, urlhttp://
    ## mashable.com/2013/04/22/a-dance-to-the-music-of-earth/, urlhttp://mashable.com/2013/04/22/boston-police-social-media/,
    ## urlhttp://mashable.com/2013/04/22/iphones-returned-problems/, urlhttp://mashable.com/2013/04/22/lyrid-meteor-shower-
    ## webcast/, urlhttp://mashable.com/2013/04/22/mars-one-reality-tv-show/, urlhttp://mashable.com/2013/04/22/startup-pitch-
    ## tips/, urlhttp://mashable.com/2013/04/29/barry-diller-newsweek-mistake/, urlhttp://mashable.com/2013/04/29/chat-apps-
    ## surpass-sms/, urlhttp://mashable.com/2013/04/29/dog-friend-cat-friend/, urlhttp://mashable.com/2013/04/29/evernote-food-
    ## android/, urlhttp://mashable.com/2013/04/29/gymnast-viral-video/, urlhttp://mashable.com/2013/04/29/kenny-powers-htc-one/,
    ## urlhttp://mashable.com/2013/04/29/social-media-wiretap/, urlhttp://mashable.com/2013/04/29/yahoo-introduces-two-new-ad-
    ## formats/, urlhttp://mashable.com/2013/05/06/coolest-hot-dog-vendor-kayak/, urlhttp://mashable.com/2013/05/06/job-listings-
    ## digital/, urlhttp://mashable.com/2013/05/13/amazon-coins-launches/, urlhttp://mashable.com/2013/05/13/bloomberg-editor-
    ## in-chief-apologizes-for-snooping/, urlhttp://mashable.com/2013/05/13/newt-gingrich-cellphone/, urlhttp://mashable.com/
    ## 2013/05/20/boeing-787-dreamliner-returns/, urlhttp://mashable.com/2013/05/20/oklahoma-tornado-social-media/, urlhttp://
    ## mashable.com/2013/05/20/wikileaks-documentary-soundtrack-listen/, urlhttp://mashable.com/2013/05/20/xbox-through-the-
    ## ages/, urlhttp://mashable.com/2013/05/27/screenshots-of-despair/, urlhttp://mashable.com/2013/06/03/soviet-union-domain-
    ## hackers/, urlhttp://mashable.com/2013/06/03/turkish-protesters-ad-new-york-times/, urlhttp://mashable.com/2013/06/10/
    ## hillary-clinton-twitter/, urlhttp://mashable.com/2013/06/10/move-out-of-cloud/, urlhttp://mashable.com/2013/06/10/ref-
    ## cam-wnba/, urlhttp://mashable.com/2013/06/10/xbox-e3-keynote/, urlhttp://mashable.com/2013/06/17/apple-statement-nsa/,
    ## urlhttp://mashable.com/2013/06/17/black-hole-computer-simulation/, urlhttp://mashable.com/2013/06/17/ford-robots-crash/,
    ## urlhttp://mashable.com/2013/06/17/landing-page-tips/, urlhttp://mashable.com/2013/06/17/longest-book-chain/, urlhttp://
    ## mashable.com/2013/06/24/celebrity-facemashups/, urlhttp://mashable.com/2013/06/24/interactive-cutting-board/, urlhttp://
    ## mashable.com/2013/06/24/missing-red-panda-twitter/, urlhttp://mashable.com/2013/07/01/icorns-2/, urlhttp://mashable.com/
    ## 2013/07/01/installation-virtual-snowstorm/, urlhttp://mashable.com/2013/07/01/nokia-siemens-buyout/, urlhttp://
    ## mashable.com/2013/07/01/wearable-tech-quantified-self/, urlhttp://mashable.com/2013/07/01/wedding-livestream/, urlhttp://
    ## mashable.com/2013/07/08/george-rr-martin-comic/, urlhttp://mashable.com/2013/07/08/google-doodle-roswell/, urlhttp://
    ## mashable.com/2013/07/08/ios-games-apps-free/, urlhttp://mashable.com/2013/07/08/nasa-kid-letter-mars/, urlhttp://
    ## mashable.com/2013/07/15/report-microsoft-surface-watch-will-launch-in-2014/, urlhttp://mashable.com/2013/07/22/cranston-
    ## disguised-comic-con/, urlhttp://mashable.com/2013/07/22/leap-motion-apps/, urlhttp://mashable.com/2013/07/22/security-
    ## researcher-apple-hack/, urlhttp://mashable.com/2013/07/28/virus-child-pornography/, urlhttp://mashable.com/2013/07/29/
    ## best-workout-headphones/, urlhttp://mashable.com/2013/07/29/digital-dating-survey/, urlhttp://mashable.com/2013/07/29/
    ## fema-app-disaster-relief/, urlhttp://mashable.com/2013/07/29/gangnam-style-stock-boost/, urlhttp://mashable.com/
    ## 2013/07/29/homeland-season-3-teaser-trailer/, urlhttp://mashable.com/2013/07/29/iphone-5c-labor-report/, urlhttp://
    ## mashable.com/2013/07/29/social-makeover-bug-out-results/, urlhttp://mashable.com/2013/08/05/23andme-tv-campaign/,
    ## urlhttp://mashable.com/2013/08/05/jay-z-performance-art-film/, urlhttp://mashable.com/2013/08/05/obama-zillow/, urlhttp://
    ## mashable.com/2013/08/05/oc-high-school-romance/, urlhttp://mashable.com/2013/08/05/rooster-space/, urlhttp://mashable.com/
    ## 2013/08/05/sex-robots-comic/, urlhttp://mashable.com/2013/08/12/breaking-bad-itunes-season-pass-complaints/, urlhttp://
    ## mashable.com/2013/08/

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/youtube-vs-cable/, urlhttp://mashable.com/2013/01/14/aaron-swartz-plea-deal/, urlhttp://
    ## mashable.com/2013/01/14/cnet-writer-resigns/, urlhttp://mashable.com/2013/01/14/do-you-need-voice-plan/, urlhttp://
    ## mashable.com/2013/01/14/hacker-olympics/, urlhttp://mashable.com/2013/01/14/kid-bulldozer/, urlhttp://mashable.com/
    ## 2013/01/14/rha-sci-fi-short/, urlhttp://mashable.com/2013/01/21/blackberry-live/, urlhttp://mashable.com/2013/01/21/
    ## bodymedia-sparkpeople-partnership/, urlhttp://mashable.com/2013/01/21/brief-sony-tablet/, urlhttp://mashable.com/
    ## 2013/01/21/lupe-fiasco-escorted/, urlhttp://mashable.com/2013/01/21/rim-license-blackberry10/, urlhttp://mashable.com/
    ## 2013/01/28/mining-asteroids-gold-platinum/, urlhttp://mashable.com/2013/01/28/office-2013-launch/, urlhttp://mashable.com/
    ## 2013/01/28/ray-lewis-social-media-sentiment/, urlhttp://mashable.com/2013/01/28/recover-from-interview-disaster/,
    ## urlhttp://mashable.com/2013/01/28/woman-eats-cat-hair/, urlhttp://mashable.com/2013/02/04/beyonce-world-tour/, urlhttp://
    ## mashable.com/2013/02/04/beyonces-super-bowl-spectacle-lights-up-instagram-facebook-and-twitter/, urlhttp://mashable.com/
    ## 2013/02/04/discount-iphone-apps-revenue/, urlhttp://mashable.com/2013/02/04/little-caesers-robot/, urlhttp://mashable.com/
    ## 2013/02/04/richard-iii-tech/, urlhttp://mashable.com/2013/02/04/twitter-made-super-bowl-awesome/, urlhttp://mashable.com/
    ## 2013/02/11/bieber-grammys/, urlhttp://mashable.com/2013/02/11/bill-gates-things/, urlhttp://mashable.com/2013/02/11/
    ## companies-outsource-kickstarter/, urlhttp://mashable.com/2013/02/11/grammys-winners-list-twitter-tweets/, urlhttp://
    ## mashable.com/2013/02/11/lapd-drones-dorner/, urlhttp://mashable.com/2013/02/11/tim-cook-state-of-the-union/, urlhttp://
    ## mashable.com/2013/02/18/6-minutes-of-cute-animals/, urlhttp://mashable.com/2013/02/18/bill-clinton-cat/, urlhttp://
    ## mashable.com/2013/02/18/sea-world-harlem-shake/, urlhttp://mashable.com/2013/02/18/zombies-in-movies/, urlhttp://
    ## mashable.com/2013/02/25/aaron-swartz-targeted-for-activism/, urlhttp://mashable.com/2013/02/25/brands-editorial-
    ## calendars/, urlhttp://mashable.com/2013/02/25/firefox-mobile-os/, urlhttp://mashable.com/2013/02/25/galaxy-note-8-
    ## hands-on/, urlhttp://mashable.com/2013/02/25/nokia-20-dollar-phone/, urlhttp://mashable.com/2013/02/25/six-strikes/,
    ## urlhttp://mashable.com/2013/02/25/vulcan-cerberus-pluto-moons/, urlhttp://mashable.com/2013/03/04/andrew-mason-memo-
    ## marc-andreessen/, urlhttp://mashable.com/2013/03/04/congressional-app-challenge/, urlhttp://mashable.com/2013/03/04/
    ## cooking-app-spices-meal/, urlhttp://mashable.com/2013/03/04/food-combos-instagram/, urlhttp://mashable.com/2013/03/04/
    ## samsung-teases-galaxy-4-video/, urlhttp://mashable.com/2013/03/04/wickr/, urlhttp://mashable.com/2013/03/11/danish-tv-
    ## syria-assassins-creed/, urlhttp://mashable.com/2013/03/11/firefox-add-ons-developers/, urlhttp://mashable.com/2013/03/11/
    ## samsung-galaxy-s-iv-leak/, urlhttp://mashable.com/2013/03/18/be-your-own-vc-how-to-vet-a-startup-before-you-join/,
    ## urlhttp://mashable.com/2013/03/18/facebook-god/, urlhttp://mashable.com/2013/03/18/facebook-ios-cover-photo/, urlhttp://
    ## mashable.com/2013/03/18/government-scientists-tweet/, urlhttp://mashable.com/2013/03/18/jack-dorsey-mayor/, urlhttp://
    ## mashable.com/2013/03/25/evernote-vs-google-keep/, urlhttp://mashable.com/2013/03/25/game-of-thrones-contest/, urlhttp://
    ## mashable.com/2013/03/25/kids-react-to-grumpy-cat/, urlhttp://mashable.com/2013/03/25/rebelmouse-powered-sites/, urlhttp://
    ## mashable.com/2013/03/25/star-trek-logo-london/, urlhttp://mashable.com/2013/04/01/b2b-marketing/, urlhttp://mashable.com/
    ## 2013/04/01/facebook-phone-leak/, urlhttp://mashable.com/2013/04/01/grumpy-cat-april-fools/, urlhttp://mashable.com/
    ## 2013/04/01/sony-animalia/, urlhttp://mashable.com/2013/04/01/space-prank-iss-pizza/, urlhttp://mashable.com/2013/04/08/
    ## google-hangouts-product-placements-glamour/, urlhttp://mashable.com/2013/04/08/reddit-summer-playlist/, urlhttp://
    ## mashable.com/2013/04/08/sony-4k-nab/, urlhttp://mashable.com/2013/04/15/71-of-facebook-users-engage-in-self-censorship/,
    ## urlhttp://mashable.com/2013/04/15/boston-marathon-explosion/, urlhttp://mashable.com/2013/04/15/dish-network-sprint/,
    ## urlhttp://mashable.com/2013/04/22/recycle-old-smartphone/, urlhttp://mashable.com/2013/04/22/time-lapse-earth-day/,
    ## urlhttp://mashable.com/2013/04/29/betaworks-buying-online-news/, urlhttp://mashable.com/2013/04/29/rob-delaney-national-
    ## anthem/, urlhttp://mashable.com/2013/04/29/san-francisco-stolen-iphones-sting/, urlhttp://mashable.com/2013/05/06/
    ## billboard-message-children-lenticular/, urlhttp://mashable.com/2013/05/06/budget-iphone/, urlhttp://mashable.com/
    ## 2013/05/06/google-palestine-israel/, urlhttp://mashable.com/2013/05/06/government-lab-quantum-internet/, urlhttp://
    ## mashable.com/2013/05/06/internet-sales-tax/, urlhttp://mashable.com/2013/05/06/israels-syria-attack-video/, urlhttp://
    ## mashable.com/2013/05/06/nba-best-dance-moves/, urlhttp://mashable.com/2013/05/06/star-wars-infomercials/, urlhttp://
    ## mashable.com/2013/05/06/startup-venture-capital/, urlhttp://mashable.com/2013/05/13/barbara-walters-retirement-video-
    ## the-view/, urlhttp://mashable.com/2013/05/20/3d-printed-liberator-handgun/, urlhttp://mashable.com/2013/05/20/flickr-pro-
    ## changes/, urlhttp://mashable.com/2013/05/20/playstation-4-teaser-video/, urlhttp://mashable.com/2013/05/20/quiz-geocities-
    ## or-tumblr/, urlhttp://mashable.com/2013/05/20/the-doors-ray-manzarek-dies/, urlhttp://mashable.com/2013/05/20/wordpress-
    ## yahoo-tumblr/, urlhttp://mashable.com/2013/05/20/yahoo-flickr-event/, urlhttp://mashable.com/2013/05/27/gawker-rob-ford-
    ## crackstarter/, urlhttp://mashable.com/2013/05/27/google-glass-accessories/, urlhttp://mashable.com/2013/05/27/memorial-
    ## day-2013-images/, urlhttp://mashable.com/2013/05/27/skyranger-drone/, urlhttp://mashable.com/2013/06/03/malicious-
    ## charger-iphone-hack/, urlhttp://mashable.com/2013/06/03/taco-bell-taco-licker/, urlhttp://mashable.com/2013/06/09/
    ## yahoo-prism-denial/, urlhttp://mashable.com/2013/06/10/apple-store-down-wwdc/, urlhttp://mashable.com/2013/06/10/kings-
    ## best-sports-tweet/, urlhttp://mashable.com/2013/06/10/viral-video-recap-33/, urlhttp://mashable.com/2013/06/17/friends-
    ## apartment-paper-model/, urlhttp://mashable.com/2013/06/17/interview-mistakes-2/, urlhttp://mashable.com/2013/06/17/
    ## jack-black-yahoo/, urlhttp://mashable.com/2013/06/17/outlast-playstation-4/, urlhttp://mashable.com/2013/06/17/question-
    ## bridge-kickstarter/, urlhttp://mashable.com/2013/06/17/radio-host-steve-gleason-nfl/, urlhttp://mashable.com/2013/06/24/
    ## dresses-respond-stares/, urlhttp://mashable.com/2013/06/24/galaxy-tab-3/, urlhttp://mashable.com/2013/06/24/hearst-
    ## dr-oz-magazine/, urlhttp://mashable.com/2013/06/24/snapchat-kidz/, urlhttp://mashable.com/2013/07/08/garmin-portable-
    ## hud/, urlhttp://mashable.com/2013/07/08/google-doodle-ufo-brief/, urlhttp://mashable.com/2013/07/08/ted-talks-change-
    ## your-life/, urlhttp://mashable.com/2013/07/08/top-tech-stories-china/, urlhttp://mashable.com/2013/07/15/superman-star-
    ## explosion/, urlhttp://mashable.com/2013/07/22/canary/, urlhttp://mashable.com/2013/07/22/dan-loeb-yahoo/, urlhttp://
    ## mashable.com/2013/07/22/dennis-farina-dead/, urlhttp://mashable.com/2013/07/22/nvidia-shield-shipping/, urlhttp://
    ## mashable.com/2013/07/22/united-kingdom-blocks-porn/, urlhttp://mashable.com/2013/07/29/douwe-egberts-yawn-machine/,
    ## urlhttp://mashable.com/2013/07/29/eve-online-huge-battle/, urlhttp://mashable.com/2013/08/05/feedly-pro/, urlhttp://
    ## mashable.com/2013/08/05/how-entrepreneurs-use-linkedin/, urlhttp://mashable.com/2013/08/05/most-used-smartphone-apps/,
    ## urlhttp://mashable.com/2013/08/05/nancy-pelosi-google-glass/, urlhttp://mashable.com/2013/08/05/obama-apple-ban-brief/,
    ## urlhttp://mashable.com/2013/08/12/disney-legend-steve-jobs/, urlhttp://mashable.com/2013/08/12/diva-impressions/,
    ## urlhttp://mashable.com/2013/08/12/girl-power-songs/, urlhttp://mashable.com/2013/08/12/lady-gaga-applause-leak-emergency/,
    ## urlhttp://mashable.com/2013/08/12/tweetdeck-makeover-2/, urlhttp://mashable.com/2013/08/12/viral-video-recap-42/,
    ## urlhttp://mashable.com/2013/08/19/coca-cola-move

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/felt-audio-pulse-speaker/, urlhttp://mashable.com/2013/01/07/land-a-job-at-spotify/,
    ## urlhttp://mashable.com/2013/01/07/nvidia-project-shield/, urlhttp://mashable.com/2013/01/07/papertab/, urlhttp://
    ## mashable.com/2013/01/07/pitchfork-advance-album-stream/, urlhttp://mashable.com/2013/01/07/sharp-intros-worlds-largest-
    ## led-hdtv/, urlhttp://mashable.com/2013/01/07/social-tv-movies-getglue-2012/, urlhttp://mashable.com/2013/01/07/toshiba-4k-
    ## tv/, urlhttp://mashable.com/2013/01/14/golden-globes-gifs/, urlhttp://mashable.com/2013/01/14/golden-globes-list-of-
    ## winners/, urlhttp://mashable.com/2013/01/14/instagram-changes/, urlhttp://mashable.com/2013/01/14/paint-fireworks-slow-
    ## mo/, urlhttp://mashable.com/2013/01/14/rim-stock-up-10-percent/, urlhttp://mashable.com/2013/01/21/hussein-twitter-
    ## inauguration/, urlhttp://mashable.com/2013/01/21/online-coupons-business/, urlhttp://mashable.com/2013/01/21/speed-up-
    ## your-pc/, urlhttp://mashable.com/2013/01/21/temple-run-2-downloads/, urlhttp://mashable.com/2013/01/21/youtube-fitness-
    ## videos/, urlhttp://mashable.com/2013/01/28/college-degrees-most-money/, urlhttp://mashable.com/2013/01/28/facebook-
    ## app-update/, urlhttp://mashable.com/2013/01/28/facebook-suicide-prevention/, urlhttp://mashable.com/2013/01/28/google-
    ## government-data/, urlhttp://mashable.com/2013/01/28/news-bloopers-january/, urlhttp://mashable.com/2013/01/28/pinterest-
    ## design-look/, urlhttp://mashable.com/2013/01/28/simple-life-hacks/, urlhttp://mashable.com/2013/01/28/surprising-
    ## billboard-hits/, urlhttp://mashable.com/2013/02/04/cool-vine-art-videos/, urlhttp://mashable.com/2013/02/04/jawbone-
    ## buys-massivehealth/, urlhttp://mashable.com/2013/02/04/job-listings-stylecaster/, urlhttp://mashable.com/2013/02/04/
    ## tomb-raider-first-impressions/, urlhttp://mashable.com/2013/02/04/valentine-tees/, urlhttp://mashable.com/2013/02/11/
    ## amazon-cbs-under-the-dome/, urlhttp://mashable.com/2013/02/11/cat-new-york-fashion-week/, urlhttp://mashable.com/
    ## 2013/02/11/craigslist-next-pope/, urlhttp://mashable.com/2013/02/11/justin-timberlake-joins-instagram-kelly-clarkson-
    ## madonna/, urlhttp://mashable.com/2013/02/11/name-pluto-moons/, urlhttp://mashable.com/2013/02/11/shape-shifting-sculpture-
    ## gifs/, urlhttp://mashable.com/2013/02/18/dick-vitale/, urlhttp://mashable.com/2013/02/18/ubuntu-tablet/, urlhttp://
    ## mashable.com/2013/02/25/oscars-acceptance-speeches/, urlhttp://mashable.com/2013/02/25/samsung-to-announce-the-galaxy-
    ## s-iv-on-march-14/, urlhttp://mashable.com/2013/02/25/seth-macfarlane-oscars/, urlhttp://mashable.com/2013/02/25/spotify-
    ## ford-sync-applink/, urlhttp://mashable.com/2013/02/25/worst-imadeface/, urlhttp://mashable.com/2013/03/04/amazons-bezos-
    ## forbes-billionaires/, urlhttp://mashable.com/2013/03/04/sheland-pony-viral-ad/, urlhttp://mashable.com/2013/03/11/angry-
    ## birds-cartoon/, urlhttp://mashable.com/2013/03/11/facts-chile-radio-telescope/, urlhttp://mashable.com/2013/03/11/movie-
    ## theater-homeless-winter/, urlhttp://mashable.com/2013/03/11/pope-linkedin/, urlhttp://mashable.com/2013/03/11/ranger-
    ## dance-moves/, urlhttp://mashable.com/2013/03/11/sxsw-posters/, urlhttp://mashable.com/2013/03/18/amazon-literary-fiction-
    ## imprint-little-a/, urlhttp://mashable.com/2013/03/18/march-madness-instagram/, urlhttp://mashable.com/2013/03/18/sony-
    ## xperia-sp-xperia-l/, urlhttp://mashable.com/2013/03/25/freedompop/, urlhttp://mashable.com/2013/03/25/march-madness-
    ## marketing/, urlhttp://mashable.com/2013/03/25/reddit-ama-tips/, urlhttp://mashable.com/2013/03/25/tech-industry-obnoxious-
    ## tweeters/, urlhttp://mashable.com/2013/04/01/biz-stone-startup-jelly/, urlhttp://mashable.com/2013/04/01/darpa-robot-
    ## change-tires/, urlhttp://mashable.com/2013/04/01/game-of-thrones-remix/, urlhttp://mashable.com/2013/04/01/google-
    ## nose/, urlhttp://mashable.com/2013/04/01/music-festivals-april/, urlhttp://mashable.com/2013/04/01/onerepublic-feel-
    ## again-heartbeats-2/, urlhttp://mashable.com/2013/04/08/antares-rocket-test-flight/, urlhttp://mashable.com/2013/04/08/
    ## fox-tv-threat/, urlhttp://mashable.com/2013/04/08/htc-one-review/, urlhttp://mashable.com/2013/04/15/boston-marathon-
    ## police-tweet-video/, urlhttp://mashable.com/2013/04/15/genachowski-aspen-institute/, urlhttp://mashable.com/2013/04/15/
    ## simon-schuster-ebooks-nypl/, urlhttp://mashable.com/2013/04/15/turkey-islam-twitter/, urlhttp://mashable.com/2013/04/15/
    ## youtube-vhs-mode/, urlhttp://mashable.com/2013/04/22/ibeetle-volkswagen/, urlhttp://mashable.com/2013/04/22/jobs-digital-
    ## media-sales/, urlhttp://mashable.com/2013/04/22/kobe-bryant-tweeting-lakers-games/, urlhttp://mashable.com/2013/04/22/
    ## storm-halle-berry-x-men-days-future-past/, urlhttp://mashable.com/2013/04/29/fish-life-jacket/, urlhttp://mashable.com/
    ## 2013/04/29/samsung-galaxy-tab-3/, urlhttp://mashable.com/2013/04/29/social-media-censorship-china/, urlhttp://
    ## mashable.com/2013/04/29/stfu-parents-overshare/, urlhttp://mashable.com/2013/04/29/worst-job-2013-reporter/, urlhttp://
    ## mashable.com/2013/05/06/is-downloading-3d-printed-gun-design-illegal/, urlhttp://mashable.com/2013/05/06/mask-superhuman/,
    ## urlhttp://mashable.com/2013/05/13/adobe-creative-cloud-top-comments/, urlhttp://mashable.com/2013/05/13/google-glass-
    ## facial-recognition/, urlhttp://mashable.com/2013/05/13/reddit-subreddit-graph/, urlhttp://mashable.com/2013/05/13/spotify-
    ## tips/, urlhttp://mashable.com/2013/05/20/cbs-mike-molly-oklahoma-tornado/, urlhttp://mashable.com/2013/05/20/sony-x-
    ## headphones-maxim/, urlhttp://mashable.com/2013/05/20/star-wars-rebels-disney/, urlhttp://mashable.com/2013/05/20/what-
    ## to-expect-xbox/, urlhttp://mashable.com/2013/05/20/world-without-youtube/, urlhttp://mashable.com/2013/06/03/3d-scanning-
    ## bioshock-infinite/, urlhttp://mashable.com/2013/06/03/sports-illustrated-debuts-daily-talk-show/, urlhttp://mashable.com/
    ## 2013/06/03/twitter-technical-issues-down/, urlhttp://mashable.com/2013/06/03/viral-video-recap-32/, urlhttp://
    ## mashable.com/2013/06/03/vonage-crazy-generous/, urlhttp://mashable.com/2013/06/10/alyssa-milano-twitter-mistresses/,
    ## urlhttp://mashable.com/2013/06/10/apple-macbook-air-new/, urlhttp://mashable.com/2013/06/10/ibooks-apple-maps-mac/,
    ## urlhttp://mashable.com/2013/06/10/ios-7-perfecting-mobile-os/, urlhttp://mashable.com/2013/06/10/plants-vs-zombies-
    ## xbox-one/, urlhttp://mashable.com/2013/06/10/wwdc-stream-apple-tv/, urlhttp://mashable.com/2013/06/17/snowden-treason/,
    ## urlhttp://mashable.com/2013/06/17/superman-is-a-super-advertiser/, urlhttp://mashable.com/2013/06/17/volvo-electric-road-
    ## car-charge/, urlhttp://mashable.com/2013/06/24/monsters-university-mobile-game-review/, urlhttp://mashable.com/2013/06/24/
    ## nik-wallenda/, urlhttp://mashable.com/2013/07/01/groupon-restaurant-reservations/, urlhttp://mashable.com/2013/07/01/
    ## prism-game-data-dealer/, urlhttp://mashable.com/2013/07/08/collegehumor-film/, urlhttp://mashable.com/2013/07/08/google-
    ## search-results-design/, urlhttp://mashable.com/2013/07/08/icracked/, urlhttp://mashable.com/2013/07/08/iran-government-
    ## email-address/, urlhttp://mashable.com/2013/07/08/mit-gmail-immersion-breakdown/, urlhttp://mashable.com/2013/07/08/
    ## nhl-player-twitter-homophic-hack/, urlhttp://mashable.com/2013/07/08/windows-81-rtm/, urlhttp://mashable.com/2013/07/15/
    ## child-tracking-apps/, urlhttp://mashable.com/2013/07/15/report-apple-aggressively-hiring-for-iwatch-project/, urlhttp://
    ## mashable.com/2013/07/15/smart-diapers/, urlhttp://mashable.com/2013/07/15/social-good-summit-ron-garan/, urlhttp://
    ## mashable.com/2013/07/21/weird-ice-cream-flavors/, urlhttp://mashable.com/2013/07/22/celebrate-royal-baby/, urlhttp://
    ## mashable.com/2013/07/22/engagement-photos-movie-posters/, urlhttp://mashable.com/2013/07/22/psfk-home-innovations/,
    ## urlhttp://mashable.com/2013/07/22/uk-porn-crackdown-brief/, urlhttp://mashable.com/2013/07/29/canadian-robotic-arm/,
    ## urlhttp://mashable.com/2013/07/29/hipsters-and-hashtags-and-falcons-oh-my/, urlhttp://mashable.com/2013/07/29/kenyan-
    ## orphans-sports-videos/, urlhttp://mashable.com/2013/07/29/new-wireless-upgrade-plans/, urlhttp://mashable.com/2013/07/29/
    ## pose-advertising-revenue-model/, urlhttp://mashable.com/2013/08/05/curiosity-rover-anniversary/, urlhttp://mashable.com/
    ## 2013/08/05/doctor

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/htc-q1/, urlhttp://mashable.com/2013/01/07/monster-katana-bluetooth-speake/, urlhttp://
    ## mashable.com/2013/01/07/rage-comics-dying/, urlhttp://mashable.com/2013/01/14/chartburst-musicians/, urlhttp://
    ## mashable.com/2013/01/14/dublin-radio-show-on-air-suicide/, urlhttp://mashable.com/2013/01/14/iphone-cases-fashion/,
    ## urlhttp://mashable.com/2013/01/14/twitter-tv-mystery-hawaii-5-0/, urlhttp://mashable.com/2013/01/21/first-2014-corvette-
    ## sells-for-1-1-million-at-auction/, urlhttp://mashable.com/2013/01/21/gopro-dog/, urlhttp://mashable.com/2013/01/21/
    ## guardian-australia/, urlhttp://mashable.com/2013/01/21/total-boox/, urlhttp://mashable.com/2013/01/21/white-house-twitter-
    ## inauguration/, urlhttp://mashable.com/2013/01/28/jennifer-lawrence-mtv-sweet-sixteen/, urlhttp://mashable.com/2013/01/28/
    ## mother-daughter-pics/, urlhttp://mashable.com/2013/01/28/nfl-commissioner-reddit-ama/, urlhttp://mashable.com/2013/01/28/
    ## snl-yolo-song-itunes/, urlhttp://mashable.com/2013/02/04/dyson-hand-dryers-future/, urlhttp://mashable.com/2013/02/04/hp-
    ## chromebook/, urlhttp://mashable.com/2013/02/04/super-bowl-twitter/, urlhttp://mashable.com/2013/02/11/fashion-hackathon/,
    ## urlhttp://mashable.com/2013/02/11/microsoft-surface-pro-commercial/, urlhttp://mashable.com/2013/02/11/social-media-
    ## tracking/, urlhttp://mashable.com/2013/02/11/tumblr-dream-job/, urlhttp://mashable.com/2013/02/18/burger-king-twitter-
    ## account-hacked/, urlhttp://mashable.com/2013/02/25/kobe-bryant-mark-cuban-twitter/, urlhttp://mashable.com/2013/02/25/
    ## michelle-obama-week/, urlhttp://mashable.com/2013/03/04/aaron-swartz-2/, urlhttp://mashable.com/2013/03/04/amazon-fashion-
    ## tv-commercial/, urlhttp://mashable.com/2013/03/04/googleplex-rap/, urlhttp://mashable.com/2013/03/04/guardian-u-s-first-
    ## ad-campaign/, urlhttp://mashable.com/2013/03/04/microsoft-next-windows-phone/, urlhttp://mashable.com/2013/03/04/twitter-
    ## instagram-infographic/, urlhttp://mashable.com/2013/03/04/umphreys-mcgee/, urlhttp://mashable.com/2013/03/04/weathercube-
    ## weather-app/, urlhttp://mashable.com/2013/03/11/augment-app/, urlhttp://mashable.com/2013/03/11/iphone-prototype-
    ## pictures/, urlhttp://mashable.com/2013/03/11/napster-documentary-downloaded-alex-winter/, urlhttp://mashable.com/
    ## 2013/03/11/plant-prank/, urlhttp://mashable.com/2013/03/11/trending-terms-sxsw/, urlhttp://mashable.com/2013/03/18/
    ## beyonce-tumblr-song/, urlhttp://mashable.com/2013/03/25/bioshock-infinite-review/, urlhttp://mashable.com/2013/03/25/fair-
    ## use/, urlhttp://mashable.com/2013/03/25/steve-jobs-atari-book-exclusive/, urlhttp://mashable.com/2013/03/25/windows-8-
    ## apps-update/, urlhttp://mashable.com/2013/04/01/easy-pranks/, urlhttp://mashable.com/2013/04/01/golfer-no-pants-shot/,
    ## urlhttp://mashable.com/2013/04/01/silent-film-instagram/, urlhttp://mashable.com/2013/04/01/the-freedom-bay/, urlhttp://
    ## mashable.com/2013/04/08/jcpenney-fires-ron-johnson/, urlhttp://mashable.com/2013/04/15/astronauts-sleep-in-space/,
    ## urlhttp://mashable.com/2013/04/15/boston-marathon-explosions-news-reddit/, urlhttp://mashable.com/2013/04/15/facebook-
    ## teen-privacy-program/, urlhttp://mashable.com/2013/04/15/ipad-easel/, urlhttp://mashable.com/2013/04/15/sony-internet-
    ## japan/, urlhttp://mashable.com/2013/04/22/acer-star-trek-ad/, urlhttp://mashable.com/2013/04/22/earth-day/, urlhttp://
    ## mashable.com/2013/04/22/eventbrite-funding-ipo/, urlhttp://mashable.com/2013/04/22/soccer-suarez-mike-tyson/, urlhttp://
    ## mashable.com/2013/04/22/syrian-hackers-world-cup-twitter/, urlhttp://mashable.com/2013/04/22/touch-bionics-i-limb-
    ## prosthetic-hand/, urlhttp://mashable.com/2013/04/29/espn-broussard-jason-collins/, urlhttp://mashable.com/2013/04/29/
    ## iron-man-flaws/, urlhttp://mashable.com/2013/04/29/puking-space-chris-hadfield/, urlhttp://mashable.com/2013/04/29/
    ## twitter-music-ranks/, urlhttp://mashable.com/2013/05/06/blackberry-r10-leak/, urlhttp://mashable.com/2013/05/06/digital-
    ## skills-college/, urlhttp://mashable.com/2013/05/06/the-national-6-hours-straight/, urlhttp://mashable.com/2013/05/06/toe-
    ## touch-photos/, urlhttp://mashable.com/2013/05/06/zuckerberg-game-of-thrones/, urlhttp://mashable.com/2013/05/13/chris-
    ## hadfield-top-moments-space/, urlhttp://mashable.com/2013/05/13/doctor-who-spoilers/, urlhttp://mashable.com/2013/05/13/
    ## great-gatsby-game/, urlhttp://mashable.com/2013/05/13/phonesuit-iphone-5-charger/, urlhttp://mashable.com/2013/05/13/
    ## pichai-google-io-2013/, urlhttp://mashable.com/2013/05/13/runtastic/, urlhttp://mashable.com/2013/05/13/social-media-
    ## advice-column-5-13/, urlhttp://mashable.com/2013/05/20/baby-scores-goal/, urlhttp://mashable.com/2013/05/20/samsungs-13-
    ## inch-3200x1800/, urlhttp://mashable.com/2013/05/20/social-media-advice-column-5-20/, urlhttp://mashable.com/2013/05/20/
    ## super-skinny-ipad-5/, urlhttp://mashable.com/2013/05/20/tornado-tv-anchor-takes-shelter/, urlhttp://mashable.com/
    ## 2013/05/20/yahoo-answers-tumblr/, urlhttp://mashable.com/2013/05/27/social-media-advice-column-5-27/, urlhttp://
    ## mashable.com/2013/06/03/ducklings-cross-indycar-racetrack/, urlhttp://mashable.com/2013/06/03/htc-one-verizon/, urlhttp://
    ## mashable.com/2013/06/03/patton-oswalt-the-coup-video/, urlhttp://mashable.com/2013/06/03/vine-for-android-hands-
    ## on/, urlhttp://mashable.com/2013/06/10/apple-ios-7/, urlhttp://mashable.com/2013/06/10/karmin-nasdaq-bell-dreambig/,
    ## urlhttp://mashable.com/2013/06/10/nba-mean-tweets/, urlhttp://mashable.com/2013/06/10/wwdc-2013-apple-keynote/, urlhttp://
    ## mashable.com/2013/06/17/andrew-wk-oma-world-record/, urlhttp://mashable.com/2013/06/17/apple-statement-privacy-brief/,
    ## urlhttp://mashable.com/2013/06/17/edward-snowden-questions/, urlhttp://mashable.com/2013/06/17/iran-president-twitter/,
    ## urlhttp://mashable.com/2013/06/17/mad-max-game-preview-video/, urlhttp://mashable.com/2013/06/24/e-la-carte/, urlhttp://
    ## mashable.com/2013/06/24/mywater-water-usage/, urlhttp://mashable.com/2013/06/24/social-chrome-extensions/, urlhttp://
    ## mashable.com/2013/06/24/twitter-joel-lunenfeld/, urlhttp://mashable.com/2013/06/24/wikileaks-edward-snowden/, urlhttp://
    ## mashable.com/2013/07/01/bet-awards-performances/, urlhttp://mashable.com/2013/07/01/canadian-flag-emoji/, urlhttp://
    ## mashable.com/2013/07/01/eu-slashes-roaming-prices/, urlhttp://mashable.com/2013/07/01/google-reader-final-countdown/,
    ## urlhttp://mashable.com/2013/07/01/mozilla-firefox-os-phones/, urlhttp://mashable.com/2013/07/01/san-francisco-fog-time-
    ## lapse/, urlhttp://mashable.com/2013/07/01/teacher-yearbook-outfit/, urlhttp://mashable.com/2013/07/08/battery-powered-
    ## plane/, urlhttp://mashable.com/2013/07/08/brain-science-social-media/, urlhttp://mashable.com/2013/07/08/ios-7-beta-3/,
    ## urlhttp://mashable.com/2013/07/08/running-of-the-bulls-crazy-pics/, urlhttp://mashable.com/2013/07/08/twitter-dm-syncing/,
    ## urlhttp://mashable.com/2013/07/15/allrecipes-print-magazine/, urlhttp://mashable.com/2013/07/15/apple-iwatch-brief/,
    ## urlhttp://mashable.com/2013/07/15/best-table-desk-fans/, urlhttp://mashable.com/2013/07/15/edward-snowden-nobel-prize-
    ## winner/, urlhttp://mashable.com/2013/07/15/facemash-family-resemblances/, urlhttp://mashable.com/2013/07/15/matt-harvey-
    ## interviews-fans/, urlhttp://mashable.com/2013/07/15/samsung-ativ-book-9-lite/, urlhttp://mashable.com/2013/07/15/samsung-
    ## chips-apple/, urlhttp://mashable.com/2013/07/15/sharknado-nielsen-social-tv-vh1/, urlhttp://mashable.com/2013/07/15/
    ## shopping-delivery/, urlhttp://mashable.com/2013/07/15/steve-jobs-instagram-trailer/, urlhttp://mashable.com/2013/07/15/
    ## trayvon-martin-tumblr/, urlhttp://mashable.com/2013/07/22/boeing-capsule-reveal/, urlhttp://mashable.com/2013/07/22/nate-
    ## silver-espn/, urlhttp://mashable.com/2013/07/22/ron-burgundy-kevin-love/, urlhttp://mashable.com/2013/07/22/royal-baby-
    ## revealed-via-twitter-its-a-boy/, urlhttp://mashable.com/2013/07/22/wedding-photo/, urlhttp://mashable.com/2013/07/29/
    ## camera-sutra/, urlhttp://mashable.com/2013/07/29/datarank/, urlhttp://mashable.com/2013/07/29/johnny-manziel-twitter/,
    ## urlhttp://mashable.com/2013/07/29/lady-gaga-hillary-clinton-star-wars/, urlhttp://mashable.com/2013/08/04/viral-video-
    ## recap-41/, urlhttp://mashable.com/2013/08/05/grumpy-cat-weather-app/, urlhttp://mashable.com/2013/08/05/mlb-

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero
    ## variances: urlhttp://mashable.com/2013/01/07/ap-samsung-sponsored-tweets/, urlhttp://mashable.com/2013/01/07/echo-
    ## game/, urlhttp://mashable.com/2013/01/07/entrepreneur-trends-2013/, urlhttp://mashable.com/2013/01/07/ford-glympse/,
    ## urlhttp://mashable.com/2013/01/07/monster-ea-gaming-headphones/, urlhttp://mashable.com/2013/01/07/traffic-signals-
    ## bugs/, urlhttp://mashable.com/2013/01/14/ad-agency-linkedin-bomb/, urlhttp://mashable.com/2013/01/14/samsung-100-million-
    ## galaxy-s-devices/, urlhttp://mashable.com/2013/01/14/social-media-tv-chart-1-14/, urlhttp://mashable.com/2013/01/14/
    ## tebow-brother-tweet/, urlhttp://mashable.com/2013/01/21/job-listings-digitas/, urlhttp://mashable.com/2013/01/21/music-
    ## group-says-myspace-is-using-songs-without-permission/, urlhttp://mashable.com/2013/01/21/oscar-nominees-best-picture-
    ## mashup/, urlhttp://mashable.com/2013/01/21/rim-blackberry-world/, urlhttp://mashable.com/2013/01/21/top-comments-jan18/,
    ## urlhttp://mashable.com/2013/01/21/watch-kelly-clarkson-my-country-tis-of-thee-inauguration/, urlhttp://mashable.com/
    ## 2013/01/28/astronauts-food-photos/, urlhttp://mashable.com/2013/01/28/mars-at-night-photos/, urlhttp://mashable.com/
    ## 2013/01/28/nba-white-house-photobomb/, urlhttp://mashable.com/2013/01/28/twitter-monitored-inauguration/, urlhttp://
    ## mashable.com/2013/02/04/3-super-bowl-ads-other-ads/, urlhttp://mashable.com/2013/02/04/blackberry-super-bowl-oped/,
    ## urlhttp://mashable.com/2013/02/04/google-flights/, urlhttp://mashable.com/2013/02/04/iron-man-3-super-bowl-trailer/,
    ## urlhttp://mashable.com/2013/02/04/mccain-ahmadinejad/, urlhttp://mashable.com/2013/02/04/minecraft-earnings-brief/,
    ## urlhttp://mashable.com/2013/02/04/nasa-curiosity-rover-mars-rock/, urlhttp://mashable.com/2013/02/04/old-flight-fails/,
    ## urlhttp://mashable.com/2013/02/04/router-faq/, urlhttp://mashable.com/2013/02/04/walking-dead-80s-sitcom/, urlhttp://
    ## mashable.com/2013/02/04/why-startups-fail/, urlhttp://mashable.com/2013/02/11/bluetooth-mercedes-is-cool/, urlhttp://
    ## mashable.com/2013/02/11/obama-google-hangout-state-union/, urlhttp://mashable.com/2013/02/11/pope-benedict-xvi-to-
    ## resign-on-february-28/, urlhttp://mashable.com/2013/02/11/ron-paul-website/, urlhttp://mashable.com/2013/02/11/video-
    ## game-romances/, urlhttp://mashable.com/2013/02/18/geeky-keychains/, urlhttp://mashable.com/2013/02/18/sex-on-mars/,
    ## urlhttp://mashable.com/2013/02/25/after-work-apps/, urlhttp://mashable.com/2013/02/25/big-data-self-regulation/,
    ## urlhttp://mashable.com/2013/02/25/lg-acquires-webos-oscars/, urlhttp://mashable.com/2013/02/25/milky-way-volcanoes-
    ## time-lapse/, urlhttp://mashable.com/2013/02/25/volkswagen-hybrid-261-mpg/, urlhttp://mashable.com/2013/03/04/kate-upton-
    ## doppelganger/, urlhttp://mashable.com/2013/03/04/social-media-ffect-on-writing/, urlhttp://mashable.com/2013/03/04/sxsw-
    ## promotions/, urlhttp://mashable.com/2013/03/04/the-hobbit-an-unexpected-journey-passes-1-billion-worldwide/, urlhttp://
    ## mashable.com/2013/03/04/twitter-killing-tweetdeck/, urlhttp://mashable.com/2013/03/04/yahoo-work-from-home/, urlhttp://
    ## mashable.com/2013/03/11/2013-sxsw-music-playlist/, urlhttp://mashable.com/2013/03/11/arduino-compatible-board-wireless-
    ## sensors/, urlhttp://mashable.com/2013/03/11/astronaut-ron-garan-sxsw/, urlhttp://mashable.com/2013/03/11/fake-fire-prank-
    ## is-amusingly-cruel/, urlhttp://mashable.com/2013/03/11/hooters-ball-girl/, urlhttp://mashable.com/2013/03/18/bike-theft-
    ## tracker-bikespike/, urlhttp://mashable.com/2013/03/18/cnn-rape-apologist-steubenville/, urlhttp://mashable.com/2013/03/18/
    ## google-spring-cleaning-rumor/, urlhttp://mashable.com/2013/03/25/comcast-watchathon/, urlhttp://mashable.com/2013/03/25/
    ## human-powered-helicopter/, urlhttp://mashable.com/2013/04/01/imessages-crash-iphone/, urlhttp://mashable.com/2013/04/08/
    ## facebook-pay-to-message-celebs/, urlhttp://mashable.com/2013/04/08/g-tech-evolution/, urlhttp://mashable.com/2013/04/08/
    ## human-chair-youtube-video/, urlhttp://mashable.com/2013/04/08/human-genome/, urlhttp://mashable.com/2013/04/08/margaret-
    ## thatcher-dies/, urlhttp://mashable.com/2013/04/08/steroid-ferrets-as-dogs/, urlhttp://mashable.com/2013/04/08/thin-
    ## reads/, urlhttp://mashable.com/2013/04/15/revolights-bike-wheel-lights/, urlhttp://mashable.com/2013/04/22/giving-vine/,
    ## urlhttp://mashable.com/2013/04/22/media-failed-coverage-boston-bombings/, urlhttp://mashable.com/2013/04/29/digital-
    ## hieroglyphics/, urlhttp://mashable.com/2013/04/29/samsung-style/, urlhttp://mashable.com/2013/04/29/tornado-week-weather-
    ## channel-interns/, urlhttp://mashable.com/2013/05/06/algorithm-financial-disaster/, urlhttp://mashable.com/2013/05/06/
    ## best-movie-soundtracks/, urlhttp://mashable.com/2013/05/06/snl-google-glass-brief/, urlhttp://mashable.com/2013/05/13/
    ## business-deals-2/, urlhttp://mashable.com/2013/05/13/what-to-expect-blackberry-live/, urlhttp://mashable.com/2013/05/20/
    ## mobile-marketing-tools/, urlhttp://mashable.com/2013/05/20/thought-leadership/, urlhttp://mashable.com/2013/06/03/acer-
    ## launches-8-1-inch-windows-tablet-5-7-inch-android-smartphone/, urlhttp://mashable.com/2013/06/03/alien-planet-caught-on-
    ## camera/, urlhttp://mashable.com/2013/06/03/porn-app-google-glass/, urlhttp://mashable.com/2013/06/03/vine-for-android-
    ## finally-arrives/, urlhttp://mashable.com/2013/06/10/apple-wwdc-ad/, urlhttp://mashable.com/2013/06/10/ea-press-conference-
    ## e3-2013/, urlhttp://mashable.com/2013/06/10/gdex-spike-e3/, urlhttp://mashable.com/2013/06/10/new-airport-extreme-time-
    ## capsule/, urlhttp://mashable.com/2013/06/10/renault-led-electric-car/, urlhttp://mashable.com/2013/06/17/citi-bikes-
    ## convenience/, urlhttp://mashable.com/2013/06/24/asus-cube-review/, urlhttp://mashable.com/2013/06/24/summer-songs-2013/,
    ## urlhttp://mashable.com/2013/06/24/vine-hashtags-cool/, urlhttp://mashable.com/2013/06/24/world-war-z-social-media-
    ## reviews/, urlhttp://mashable.com/2013/07/01/flashlight-powered-hands-heat/, urlhttp://mashable.com/2013/07/01/gourmet-
    ## summer-recipes/, urlhttp://mashable.com/2013/07/01/viral-video-recap-36/, urlhttp://mashable.com/2013/07/01/xbox-music-
    ## web/, urlhttp://mashable.com/2013/07/08/3d-printer-video/, urlhttp://mashable.com/2013/07/08/8-twists-on-just-girly-
    ## things/, urlhttp://mashable.com/2013/07/08/facebook-launch-graph-search/, urlhttp://mashable.com/2013/07/08/facebook-
    ## promoted-posts-business/, urlhttp://mashable.com/2013/07/08/harpers-magazine-digital-revolution/, urlhttp://mashable.com/
    ## 2013/07/15/disney-horror/, urlhttp://mashable.com/2013/07/15/home-run-derby-all-star-game/, urlhttp://mashable.com/
    ## 2013/07/15/imdb-app-update/, urlhttp://mashable.com/2013/07/15/internet-association-new-website/, urlhttp://mashable.com/
    ## 2013/07/22/ebay-now-desktop/, urlhttp://mashable.com/2013/07/22/facebook-feature-phones/, urlhttp://mashable.com/
    ## 2013/07/22/game-of-thrones-comic-con/, urlhttp://mashable.com/2013/07/22/guardian-royal-baby/, urlhttp://mashable.com/
    ## 2013/07/22/honda-mean-mower/, urlhttp://mashable.com/2013/07/22/hopstop-windows-phone/, urlhttp://mashable.com/2013/07/22/
    ## how-i-met-your-mother-trailer/, urlhttp://mashable.com/2013/07/22/sad-gifs/, urlhttp://mashable.com/2013/07/29/flat-
    ## design-ui-kits/, urlhttp://mashable.com/2013/07/29/hobbit-peter-jackson-facebook/, urlhttp://mashable.com/2013/07/29/
    ## taylor-swift-carly-simon-duet/, urlhttp://mashable.com/2013/08/05/google-chrome-touch-control/, urlhttp://mashable.com/
    ## 2013/08/05/kanye-west-mtv-vmas-record/, urlhttp://mashable.com/2013/08/05/lab-grown-burger-meat/, urlhttp://mashable.com/
    ## 2013/08/05/verizon-nfl-app/, urlhttp://mashable.com/2013/08/12/audi-car-enders-game/, urlhttp://mashable.com/2013/08/12/
    ## ios-top-smartphone/, urlhttp://mashable.com/2013/08/12/kick-ass-8-bit/, urlhttp://mashable.com/2013/08/12/missouri-
    ## obama-rodeo-clown/, urlhttp://mashable.com/2013/08/12/schrodingers-cat-plays-starring-role-in-google-doodle/, urlhttp://
    ## mashable.com/2013/08/12/sigmo-language-translator/, urlhttp://mashable.com/2013/08/19/action-sports-trace/, urlhttp://
    ## mashable.com/2013/08/19/apple-iphone-5s-launch-date-september/, urlhttp://mashable.com/2013/08/19/guardian-hard-drives/,
    ## urlhttp://mashable.com/2013/08/19/lastpass-password-bug/, urlhttp://mashable.com/2013/08/19/pantone-accessories/,
    ## urlhttp://mashable.c

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/earth-size-planets-milky-way/, urlhttp://mashable.com/2013/01/07/roku-adds-channels-
    ## device-partners-ces/, urlhttp://mashable.com/2013/01/07/sharp-unveils-next-gen-igo-hdtvs/, urlhttp://mashable.com/
    ## 2013/01/07/social-media-iran/, urlhttp://mashable.com/2013/01/07/stolen-iphone-dating/, urlhttp://mashable.com/2013/01/07/
    ## vh1-new-logo-plus-sign/, urlhttp://mashable.com/2013/01/14/beijing-air-pollution/, urlhttp://mashable.com/2013/01/14/
    ## hottest-golden-globes-fashion-facebook-instyle/, urlhttp://mashable.com/2013/01/14/maltreated-girls-risky-online-
    ## behavior/, urlhttp://mashable.com/2013/01/14/sleep-phones-bluetooth/, urlhttp://mashable.com/2013/01/14/spreecast-live-
    ## video/, urlhttp://mashable.com/2013/01/21/flu-season-germs-off-your-phone/, urlhttp://mashable.com/2013/01/21/mlk-art/,
    ## urlhttp://mashable.com/2013/01/28/apple-android-smartphone-shipments/, urlhttp://mashable.com/2013/01/28/pay-to-pin-
    ## tumblr/, urlhttp://mashable.com/2013/01/28/republican-digital-twitter-chat/, urlhttp://mashable.com/2013/01/28/sag-awards-
    ## winners-list-videos-best-moments/, urlhttp://mashable.com/2013/01/28/speed-up-mac/, urlhttp://mashable.com/2013/01/28/
    ## vine-gif-mac-app/, urlhttp://mashable.com/2013/01/28/vw-super-bowl-ad-jamaican/, urlhttp://mashable.com/2013/02/04/bones-
    ## king-richard-iii/, urlhttp://mashable.com/2013/02/04/joe-flacco-f-bomb/, urlhttp://mashable.com/2013/02/04/super-bowl-
    ## halftime-songs/, urlhttp://mashable.com/2013/02/11/bill-gates-ama/, urlhttp://mashable.com/2013/02/11/game-of-thrones-
    ## season-3/, urlhttp://mashable.com/2013/02/11/google-now-nexus-4-ad/, urlhttp://mashable.com/2013/02/11/google-solve-for-
    ## x-event/, urlhttp://mashable.com/2013/02/11/grammy-nominees-you-forgot/, urlhttp://mashable.com/2013/02/11/grammys-who-
    ## should-have-won/, urlhttp://mashable.com/2013/02/11/htc-one-image-leak/, urlhttp://mashable.com/2013/02/11/rihanna-stay-
    ## music-video-mikky-ekko/, urlhttp://mashable.com/2013/02/11/tesla-new-york-times/, urlhttp://mashable.com/2013/02/11/us-
    ## victim-cyber-espionage/, urlhttp://mashable.com/2013/02/18/lg-officially-announces-optimus-g-pro/, urlhttp://mashable.com/
    ## 2013/02/25/best-celebrity-wipeouts/, urlhttp://mashable.com/2013/02/25/erin-andrews-50-cent-kiss/, urlhttp://mashable.com/
    ## 2013/02/25/mobile-world-congress-supercharge-phone/, urlhttp://mashable.com/2013/02/25/oscars-winners-2013-best-moments/,
    ## urlhttp://mashable.com/2013/02/25/switchcam-director/, urlhttp://mashable.com/2013/02/25/watch-oscars-online-full-
    ## show/, urlhttp://mashable.com/2013/03/04/cryface-iphone-app/, urlhttp://mashable.com/2013/03/04/mashable-house-sxswi/,
    ## urlhttp://mashable.com/2013/03/04/teen-workshops/, urlhttp://mashable.com/2013/03/04/twitter-public-opinion/, urlhttp://
    ## mashable.com/2013/03/11/darrell-issa-sxsw/, urlhttp://mashable.com/2013/03/11/linkedin-to-acquire-pulse/, urlhttp://
    ## mashable.com/2013/03/11/self-powered-emergency-smartphone-charger/, urlhttp://mashable.com/2013/03/11/sxsw-social-buzz-3/,
    ## urlhttp://mashable.com/2013/03/11/veti-gel/, urlhttp://mashable.com/2013/03/18/craigslist-spring-cleaning/, urlhttp://
    ## mashable.com/2013/03/18/lil-bub-friendz-movie-trailer-debuts/, urlhttp://mashable.com/2013/03/18/personal-finance-experts-
    ## twitter/, urlhttp://mashable.com/2013/03/18/ryan-seacrest-draw-something-2/, urlhttp://mashable.com/2013/03/18/strokes-
    ## comedown-machine/, urlhttp://mashable.com/2013/03/18/worlds-fastest-agency/, urlhttp://mashable.com/2013/03/25/canadian-
    ## ad-dangers-of-sexting/, urlhttp://mashable.com/2013/03/25/spotify-ad-campaign/, urlhttp://mashable.com/2013/04/01/
    ## best-indie-games/, urlhttp://mashable.com/2013/04/01/digital-tools-diabetes/, urlhttp://mashable.com/2013/04/08/new-
    ## xbox-announcement/, urlhttp://mashable.com/2013/04/08/nomad-espresso/, urlhttp://mashable.com/2013/04/08/samsung-mega/,
    ## urlhttp://mashable.com/2013/04/15/boston-marathon-person-finder/, urlhttp://mashable.com/2013/04/15/dove-ad-beauty-
    ## sketches/, urlhttp://mashable.com/2013/04/15/facebook-home-dinner-commercial/, urlhttp://mashable.com/2013/04/15/foxconn-
    ## hiring-iphone/, urlhttp://mashable.com/2013/04/22/evian-babies-back/, urlhttp://mashable.com/2013/04/22/nest-thermostat-
    ## energy/, urlhttp://mashable.com/2013/04/22/repairable-cell-phones/, urlhttp://mashable.com/2013/04/22/rookie-news-anchor-
    ## fired-gaffe/, urlhttp://mashable.com/2013/04/22/social-media-advice-column-4-22/, urlhttp://mashable.com/2013/04/22/
    ## social-media-day-june-30-2013/, urlhttp://mashable.com/2013/04/29/911-plane-wreckage/, urlhttp://mashable.com/2013/04/29/
    ## international-travel-apps/, urlhttp://mashable.com/2013/04/29/kevin-ware-quits-twitter/, urlhttp://mashable.com/
    ## 2013/04/29/samsung-galaxy-s-4-video/, urlhttp://mashable.com/2013/04/29/tlc-reunion-mixtape-festival-hologram/, urlhttp://
    ## mashable.com/2013/05/06/anonymous-islamist-hackers-may-7/, urlhttp://mashable.com/2013/05/06/mariah-carey-beautiful-
    ## miguel-listen/, urlhttp://mashable.com/2013/05/06/omega-watch-astronauts/, urlhttp://mashable.com/2013/05/13/astronaut-
    ## return-watch-online/, urlhttp://mashable.com/2013/05/13/geography-of-hate/, urlhttp://mashable.com/2013/05/13/seattle-
    ## man-drone-spying-woman/, urlhttp://mashable.com/2013/05/20/game-of-thrones-company/, urlhttp://mashable.com/2013/05/20/
    ## video-game-without-video/, urlhttp://mashable.com/2013/05/27/apple-messages-brief/, urlhttp://mashable.com/2013/05/27/
    ## child-lego-school/, urlhttp://mashable.com/2013/05/27/fallon-game-of-thrones/, urlhttp://mashable.com/2013/05/27/need-for-
    ## speed-rivals-trailer/, urlhttp://mashable.com/2013/05/27/theodore-chalfen-commencement-speech/, urlhttp://mashable.com/
    ## 2013/06/03/apple-streaming-music/, urlhttp://mashable.com/2013/06/03/arya-stark-game-of-thrones-reaction/, urlhttp://
    ## mashable.com/2013/06/03/frank-lautenberg-farewell/, urlhttp://mashable.com/2013/06/03/funniest-game-of-thrones-red-
    ## wedding/, urlhttp://mashable.com/2013/06/10/apple-has-paid-developers-10-billion/, urlhttp://mashable.com/2013/06/10/
    ## apple-itunes-radio/, urlhttp://mashable.com/2013/06/10/fathers-day-tech/, urlhttp://mashable.com/2013/06/10/google-street-
    ## view-embarrassing/, urlhttp://mashable.com/2013/06/10/high-tech-home-gadgets/, urlhttp://mashable.com/2013/06/10/job-
    ## hunting-2/, urlhttp://mashable.com/2013/06/10/pe-tablet-study-2013/, urlhttp://mashable.com/2013/06/10/startup-mistakes/,
    ## urlhttp://mashable.com/2013/06/10/warp-speed-dogs/, urlhttp://mashable.com/2013/06/17/facebook-mobile-ads-price/,
    ## urlhttp://mashable.com/2013/06/17/sports-fans-online-study/, urlhttp://mashable.com/2013/06/17/tearaway-playstation-vita-
    ## hands-on/, urlhttp://mashable.com/2013/06/24/google-ftc/, urlhttp://mashable.com/2013/06/24/illicit-google-street-view/,
    ## urlhttp://mashable.com/2013/06/24/leap-motion-airspace/, urlhttp://mashable.com/2013/07/01/edward-snowden-statement-
    ## letter-from-russia/, urlhttp://mashable.com/2013/07/01/lucas-cultural-arts-museum/, urlhttp://mashable.com/2013/07/01/
    ## reddit-google-reader/, urlhttp://mashable.com/2013/07/08/blurred-lines-cosby/, urlhttp://mashable.com/2013/07/08/social-
    ## discovery-apps/, urlhttp://mashable.com/2013/07/08/supercut-one-man-trailers/, urlhttp://mashable.com/2013/07/15/marissa-
    ## mayer-yahoo-first-year/, urlhttp://mashable.com/2013/07/15/nfl-star-free-hernandez/, urlhttp://mashable.com/2013/07/15/
    ## san-francisco-trayvon-martin-fake/, urlhttp://mashable.com/2013/07/22/comic-con-cosplay/, urlhttp://mashable.com/
    ## 2013/07/22/comic-con-recap/, urlhttp://mashable.com/2013/07/22/free-image-editors/, urlhttp://mashable.com/2013/07/22/
    ## leap-motion-review/, urlhttp://mashable.com/2013/07/29/baby-magic-bullet/, urlhttp://mashable.com/2013/07/29/nexus-7-
    ## review-2013/, urlhttp://mashable.com/2013/07/29/sony-panasonic-300gb-optical-discs/, urlhttp://mashable.com/2013/07/29/
    ## xbox-one-controller-price/, urlhttp://mashable.com/2013/08/05/dine-and-dasher-facebook/, urlhttp://mashable.com/
    ## 2013/08/05/google-glass-hearing-impaired/, urlhttp://mashable.com/2013/08/05/handheld-electronic-games/, urlhttp://
    ## mashable.com/2013/08/05/quick-draw-hulu-original-watch/, urlhttp://mashable.com/2013/08/05/spotify-browse-themed-
    ## playlists-smartphone/, urlhttp://mashable.com/2013/08/12/elon-musk-hyperloop-reveal/, urlhttp://mashable.c

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/huawei-ascend-mate/, urlhttp://mashable.com/2013/01/07/jobs-contently/, urlhttp://
    ## mashable.com/2013/01/07/nvidia-tegra-4/, urlhttp://mashable.com/2013/01/07/samsung-85-inch-uhd-tv/, urlhttp://
    ## mashable.com/2013/01/07/samsung-uhd-blu-ray/, urlhttp://mashable.com/2013/01/14/best-musical-comebacks/, urlhttp://
    ## mashable.com/2013/01/14/delta-airlines-ipad-app-glass-bottom-jet/, urlhttp://mashable.com/2013/01/14/fan-half-court-
    ## shot/, urlhttp://mashable.com/2013/01/14/job-searching-onion/, urlhttp://mashable.com/2013/01/14/sinofsky-iphone/,
    ## urlhttp://mashable.com/2013/01/14/straight-talk-iphone-5-data-plan/, urlhttp://mashable.com/2013/01/14/top-comments-
    ## jan13/, urlhttp://mashable.com/2013/01/14/trombonist-skis/, urlhttp://mashable.com/2013/01/14/twitter-mexico-violence/,
    ## urlhttp://mashable.com/2013/01/21/baclberryz10-versus-iphone-5/, urlhttp://mashable.com/2013/01/21/financial-times-
    ## digital-first/, urlhttp://mashable.com/2013/01/21/james-franco-poem/, urlhttp://mashable.com/2013/01/21/mars-crater-
    ## ancient-lake/, urlhttp://mashable.com/2013/01/21/obama-gay-rights-inauguration/, urlhttp://mashable.com/2013/01/21/
    ## obama-inauguration-speech/, urlhttp://mashable.com/2013/01/21/samsung-blackberry-ad/, urlhttp://mashable.com/2013/01/21/
    ## sony-xperia-tablet-z/, urlhttp://mashable.com/2013/01/28/128gb-ipad/, urlhttp://mashable.com/2013/01/28/ashton-kutcher-
    ## hospitalized-steve-jobs-fruitarian-diet/, urlhttp://mashable.com/2013/01/28/eve-online-asakai/, urlhttp://mashable.com/
    ## 2013/01/28/reasons-deactivate-facebook/, urlhttp://mashable.com/2013/01/28/star-trek-into-darkness-iphone/, urlhttp://
    ## mashable.com/2013/01/28/super-bowl-survey-second-screen-big-game/, urlhttp://mashable.com/2013/01/28/twitter-videoads-are-
    ## here/, urlhttp://mashable.com/2013/01/28/vine-art/, urlhttp://mashable.com/2013/01/28/vine-porn/, urlhttp://mashable.com/
    ## 2013/02/04/facebook-location-sharing/, urlhttp://mashable.com/2013/02/04/hitchhiker-news-viral/, urlhttp://mashable.com/
    ## 2013/02/04/obama-pre-emptive-cyberstrike/, urlhttp://mashable.com/2013/02/11/american-express-twitter-hashtag-purchase/,
    ## urlhttp://mashable.com/2013/02/11/apple-wristwatch-brief/, urlhttp://mashable.com/2013/02/11/microsoft-la-studios-
    ## xobox/, urlhttp://mashable.com/2013/02/11/obama-cybersecurity-order/, urlhttp://mashable.com/2013/02/11/robot-fish-
    ## looks-real/, urlhttp://mashable.com/2013/02/11/worst-romantic-comedies/, urlhttp://mashable.com/2013/02/18/simplcase-
    ## iphone-travelers/, urlhttp://mashable.com/2013/02/25/andrew-sullivan-the-dish/, urlhttp://mashable.com/2013/02/25/elon-
    ## musk-tesla-new-york-times/, urlhttp://mashable.com/2013/02/25/screaming-goats-songs/, urlhttp://mashable.com/2013/03/04/
    ## beatles-covers-minute/, urlhttp://mashable.com/2013/03/04/microsoft-denmark-1-billion/, urlhttp://mashable.com/2013/03/11/
    ## clock-art/, urlhttp://mashable.com/2013/03/11/grocery-shopping-google-glass/, urlhttp://mashable.com/2013/03/11/simcity-
    ## brief/, urlhttp://mashable.com/2013/03/18/cory-booker-reddit/, urlhttp://mashable.com/2013/03/18/gears-of-war-judgement-
    ## review/, urlhttp://mashable.com/2013/03/18/google-play-store-newspapers/, urlhttp://mashable.com/2013/03/18/heins-slams-
    ## iphone/, urlhttp://mashable.com/2013/03/18/insurance-against-cyberattacks/, urlhttp://mashable.com/2013/03/18/more-
    ## solar-companies-fail/, urlhttp://mashable.com/2013/03/18/pinteret-redesign/, urlhttp://mashable.com/2013/03/18/supreme-
    ## court-refuses-music-piracy-case/, urlhttp://mashable.com/2013/03/18/tiger-woods-lindsey-vonn/, urlhttp://mashable.com/
    ## 2013/03/18/web-design-tools/, urlhttp://mashable.com/2013/03/25/carly-rae-jepsen-american-idol/, urlhttp://mashable.com/
    ## 2013/03/25/facebook-replies/, urlhttp://mashable.com/2013/03/25/google-glass-driving/, urlhttp://mashable.com/2013/03/25/
    ## music-monday-spring/, urlhttp://mashable.com/2013/04/01/april-fools-day-origin/, urlhttp://mashable.com/2013/04/01/
    ## april-fools-day-roundup/, urlhttp://mashable.com/2013/04/01/avatar-project/, urlhttp://mashable.com/2013/04/01/htc-
    ## one-2/, urlhttp://mashable.com/2013/04/01/nasa-robonaut-challenge/, urlhttp://mashable.com/2013/04/01/tim-cook-china-
    ## apology/, urlhttp://mashable.com/2013/04/01/troll-appreciation-day/, urlhttp://mashable.com/2013/04/08/canalys-report/,
    ## urlhttp://mashable.com/2013/04/08/google-whatsapp-acquisition/, urlhttp://mashable.com/2013/04/15/crashalert-texting-
    ## app/, urlhttp://mashable.com/2013/04/15/iron-man-3-footage-video/, urlhttp://mashable.com/2013/04/22/jason-derulo-
    ## vine-promotion/, urlhttp://mashable.com/2013/04/29/jerry-seinfeld-girlfriends/, urlhttp://mashable.com/2013/04/29/
    ## speakingphoto/, urlhttp://mashable.com/2013/04/29/virgin-galactic-spaceshiptwo-launch/, urlhttp://mashable.com/
    ## 2013/05/06/bill-gates-ipad-apple-surface-windows/, urlhttp://mashable.com/2013/05/06/mars-panoramas-curiosity-kremer/,
    ## urlhttp://mashable.com/2013/05/06/nike-dream-job/, urlhttp://mashable.com/2013/05/06/samsung-galaxy-core/, urlhttp://
    ## mashable.com/2013/05/13/best-vine-users/, urlhttp://mashable.com/2013/05/20/abercrombie-fitch-skinny-ads/, urlhttp://
    ## mashable.com/2013/05/20/heywire-debuts-landline-texting-for-businesses/, urlhttp://mashable.com/2013/05/20/nba-dwyane-
    ## wade-prom/, urlhttp://mashable.com/2013/05/20/pinterest-brands-upgrade/, urlhttp://mashable.com/2013/05/20/taylor-swift-
    ## justin-bieber-kiss/, urlhttp://mashable.com/2013/05/20/vc-value-add/, urlhttp://mashable.com/2013/05/20/yahoo-tumblr-
    ## acquisition-ads/, urlhttp://mashable.com/2013/05/20/yahoo-tumblr-comic/, urlhttp://mashable.com/2013/06/03/bradley-
    ## manning-trial/, urlhttp://mashable.com/2013/06/03/man-proposes-google-glass/, urlhttp://mashable.com/2013/06/10/insiders-
    ## hackers-cybersecurity/, urlhttp://mashable.com/2013/06/10/kate-upton-birthday/, urlhttp://mashable.com/2013/06/10/
    ## snowden-timeline/, urlhttp://mashable.com/2013/06/10/twitter-chase-1-million-small-businesses/, urlhttp://mashable.com/
    ## 2013/06/17/batman-arkham-origins-preview-video/, urlhttp://mashable.com/2013/06/17/cynthia-rowley-eyewear/, urlhttp://
    ## mashable.com/2013/06/17/retail-workers-free-time/, urlhttp://mashable.com/2013/06/24/facebook-flipboard-reader/,
    ## urlhttp://mashable.com/2013/06/24/game-of-thrones-most-pirated/, urlhttp://mashable.com/2013/06/24/kids-reacting-to-gifs/,
    ## urlhttp://mashable.com/2013/06/24/yeah-yeah-yeahs-empire-state-building-despair/, urlhttp://mashable.com/2013/07/01/work-
    ## from-home-tips/, urlhttp://mashable.com/2013/07/08/champs-battlegrounds-mobile-game/, urlhttp://mashable.com/2013/07/08/
    ## exoskeleton-mimics-moves/, urlhttp://mashable.com/2013/07/08/mobileye-self-driving-cars/, urlhttp://mashable.com/
    ## 2013/07/08/viral-video-recap-37/, urlhttp://mashable.com/2013/07/15/pacific-rim-tech/, urlhttp://mashable.com/2013/07/15/
    ## reddit-pizza-cancer/, urlhttp://mashable.com/2013/07/22/3d-printing-patents/, urlhttp://mashable.com/2013/07/22/diy-
    ## projects/, urlhttp://mashable.com/2013/07/22/north-korea-instagram-videos/, urlhttp://mashable.com/2013/07/22/royal-baby-
    ## twitter-record/, urlhttp://mashable.com/2013/07/22/sim-card-hack/, urlhttp://mashable.com/2013/07/22/work-happiness/,
    ## urlhttp://mashable.com/2013/07/29/14-weirdest-craigslist-job-postings-on-the-internet/, urlhttp://mashable.com/2013/07/29/
    ## awkward-years-project-tumblr/, urlhttp://mashable.com/2013/07/29/intel-microsoft-oracle-iphone-4-ban/, urlhttp://
    ## mashable.com/2013/07/29/miley-cyrus-directors-cut/, urlhttp://mashable.com/2013/07/29/tlc-sings-waterfalls-in-emotional-
    ## tribute-to-the-late-left-eye/, urlhttp://mashable.com/2013/08/05/kill-your-darlings-daniel-radcliffe/, urlhttp://
    ## mashable.com/2013/08/05/mifare-ultralights-hack-def-con/, urlhttp://mashable.com/2013/08/12/google-glass-investors/,
    ## urlhttp://mashable.com/2013/08/12/kim-dotcom-mega-encrypted-email/, urlhttp://mashable.com/2013/08/12/robert-downey-
    ## brief/, urlhttp://mashable.com/2013/08/12/versavid-winners/, urlhttp://mashable.com/2013/08/19/deodorant-rocky-theme/,
    ## urlhttp://mashable.com/2013/08/19/gold-iphone-rumor/, urlhttp://mashable.com/2013/08/26/facebook-shared-photo-albums/,
    ## urlhttp://mashable.com/2013/08/26/no-more-microsoft-points/, urlhttp://mashable.com/2013/08/26/one-direction-spoof/,
    ## urlhttp://mashable.com/2013/08/26/summe

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## urlhttp://mashable.com/2013/01/07/livio-connect-expands/, urlhttp://mashable.com/2013/01/07/samsung-2013-ces-press-
    ## conference/, urlhttp://mashable.com/2013/01/14/arianna-huffington-gps-for-the-soul/, urlhttp://mashable.com/2013/01/14/
    ## avoid-getting-flu-at-work/, urlhttp://mashable.com/2013/01/14/coldplay-safe-road/, urlhttp://mashable.com/2013/01/14/dvf-
    ## pinterest-giveaway/, urlhttp://mashable.com/2013/01/14/exclusive-download-party-kids-by-sallie-ford-the-sound-outside/,
    ## urlhttp://mashable.com/2013/01/14/iphone-cases-fun/, urlhttp://mashable.com/2013/01/14/no-pants-subway-ride-2013/,
    ## urlhttp://mashable.com/2013/01/14/nra-shooter-game/, urlhttp://mashable.com/2013/01/21/amazon-woos-advertisers-with-
    ## what-it-knows-about-consumers/, urlhttp://mashable.com/2013/01/21/bette-midler-twitter-inauguration/, urlhttp://
    ## mashable.com/2013/01/21/storify-private-facebook-updates/, urlhttp://mashable.com/2013/01/28/community-management/,
    ## urlhttp://mashable.com/2013/01/28/iran-space-monkey/, urlhttp://mashable.com/2013/01/28/manti-teo-autotune/, urlhttp://
    ## mashable.com/2013/01/28/prank-on-mom/, urlhttp://mashable.com/2013/01/28/reporter-head-butt/, urlhttp://mashable.com/
    ## 2013/01/28/video-basketball-coach-pass/, urlhttp://mashable.com/2013/01/28/yahoo-earnings-2/, urlhttp://mashable.com/
    ## 2013/02/04/ramen-bowl/, urlhttp://mashable.com/2013/02/04/samsung-global-engine/, urlhttp://mashable.com/2013/02/04/super-
    ## bowl-on-facebook/, urlhttp://mashable.com/2013/02/04/super-bowl-social-media/, urlhttp://mashable.com/2013/02/04/syria-
    ## skype/, urlhttp://mashable.com/2013/02/04/when-will-we-get-google-fiber/, urlhttp://mashable.com/2013/02/11/blackberry-
    ## z10-solavei/, urlhttp://mashable.com/2013/02/11/helicopter-crash-top-gear-korea/, urlhttp://mashable.com/2013/02/11/
    ## mashable-job-board-popsugar/, urlhttp://mashable.com/2013/02/25/asus-padfone-infinity/, urlhttp://mashable.com/2013/02/25/
    ## instagram-oscars/, urlhttp://mashable.com/2013/02/25/lg-optimus-barcelona/, urlhttp://mashable.com/2013/02/25/oscars-
    ## facebook-chatter/, urlhttp://mashable.com/2013/02/25/real-calvin-and-hobbes/, urlhttp://mashable.com/2013/02/25/the-
    ## onion-apologizes-tweet-quvenzhane-wallis/, urlhttp://mashable.com/2013/03/04/linkedin-endorsements-weird/, urlhttp://
    ## mashable.com/2013/03/04/mwc-scotevest-trench-coat/, urlhttp://mashable.com/2013/03/04/oncologist-watson-brief/, urlhttp://
    ## mashable.com/2013/03/11/game-of-thrones-recap/, urlhttp://mashable.com/2013/03/11/yabbly/, urlhttp://mashable.com/
    ## 2013/03/18/canada-social-farting/, urlhttp://mashable.com/2013/03/18/google-keep/, urlhttp://mashable.com/2013/03/18/
    ## homeland-walking-dead/, urlhttp://mashable.com/2013/03/18/soccer-player-banned-nazi-salute/, urlhttp://mashable.com/
    ## 2013/03/25/drone-strikes-infograhic/, urlhttp://mashable.com/2013/03/25/nike-tiger-woods/, urlhttp://mashable.com/
    ## 2013/03/25/the-vamp-old-speakers/, urlhttp://mashable.com/2013/03/25/tildaing/, urlhttp://mashable.com/2013/04/01/
    ## iplifier-iphone-speaker-amplifier/, urlhttp://mashable.com/2013/04/01/mixbit-youtube/, urlhttp://mashable.com/2013/04/01/
    ## new-york-times-haiku/, urlhttp://mashable.com/2013/04/01/toshiba-shibasphere/, urlhttp://mashable.com/2013/04/01/twttr/,
    ## urlhttp://mashable.com/2013/04/08/promposals/, urlhttp://mashable.com/2013/04/15/bitcoin-cryptocurrency/, urlhttp://
    ## mashable.com/2013/04/15/foursquare-dennis-crowley-boston-marathon/, urlhttp://mashable.com/2013/04/15/google-doodle-
    ## leonhard-euler/, urlhttp://mashable.com/2013/04/15/hybrid-animals/, urlhttp://mashable.com/2013/04/15/mtv-movie-awards-
    ## gifs/, urlhttp://mashable.com/2013/04/15/van-gogh-starry-night-google/, urlhttp://mashable.com/2013/04/22/google-earth-
    ## leap-motion/, urlhttp://mashable.com/2013/04/22/kohler-numi-toilet/, urlhttp://mashable.com/2013/04/22/prescription-
    ## drug-bottle-regulator/, urlhttp://mashable.com/2013/04/29/apple-os-x-wwdc/, urlhttp://mashable.com/2013/04/29/coffee-
    ## apps/, urlhttp://mashable.com/2013/04/29/hurricane-on-saturn/, urlhttp://mashable.com/2013/04/29/vintage-psas/, urlhttp://
    ## mashable.com/2013/05/06/lg-no-3-smartphone-maker/, urlhttp://mashable.com/2013/05/06/new-york-times-gif-homepage/,
    ## urlhttp://mashable.com/2013/05/06/social-media-advice-column-5-6/, urlhttp://mashable.com/2013/05/06/tweetdeck-facebook/,
    ## urlhttp://mashable.com/2013/05/13/casino-royale-lego/, urlhttp://mashable.com/2013/05/13/entertainment-news-recap/,
    ## urlhttp://mashable.com/2013/05/13/native-advertising-buzzword/, urlhttp://mashable.com/2013/05/13/netflix-dream-job/,
    ## urlhttp://mashable.com/2013/05/13/twitter-acquires-lucky-sort/, urlhttp://mashable.com/2013/05/13/ugly-cute-animals/,
    ## urlhttp://mashable.com/2013/05/20/can-yahoo-ignore-tumblr-prn/, urlhttp://mashable.com/2013/05/20/esquire-weekly-ipad/,
    ## urlhttp://mashable.com/2013/05/20/hologram-michael-jackson-will-i-am/, urlhttp://mashable.com/2013/05/20/yahoo-officially-
    ## acquires-tumblr-promises-not-to-screw-it-up/, urlhttp://mashable.com/2013/05/27/cat-boarding/, urlhttp://mashable.com/
    ## 2013/05/27/flirting-animals/, urlhttp://mashable.com/2013/05/27/samsung-to-unveil-new-galaxy-and-ativ-devices-on-
    ## june-20/, urlhttp://mashable.com/2013/05/27/wordpress-10-years/, urlhttp://mashable.com/2013/06/03/red-wedding-game-
    ## of-thrones/, urlhttp://mashable.com/2013/06/03/twitter-traditional-media/, urlhttp://mashable.com/2013/06/03/yankees-
    ## spooked-thunder/, urlhttp://mashable.com/2013/06/10/charging-electric-buses/, urlhttp://mashable.com/2013/06/10/lebron-
    ## james-block-viral/, urlhttp://mashable.com/2013/06/10/mike-tyson-tony-awards/, urlhttp://mashable.com/2013/06/10/new-mac-
    ## pro/, urlhttp://mashable.com/2013/06/10/ps4-used-games/, urlhttp://mashable.com/2013/06/10/share-red-campaign/, urlhttp://
    ## mashable.com/2013/06/10/xbox-360-updated/, urlhttp://mashable.com/2013/06/17/10-unbelievable-ballpoint-pen-drawing-
    ## time-lapses/, urlhttp://mashable.com/2013/06/17/beyonce-congratulates-kanye-kim-kardashian/, urlhttp://mashable.com/
    ## 2013/06/17/mario-kart-8-hands-on/, urlhttp://mashable.com/2013/06/17/supermans-life-drawing/, urlhttp://mashable.com/
    ## 2013/06/24/apple-stock-400/, urlhttp://mashable.com/2013/06/24/game-of-thrones-zooey-deschanel-aubrey-plaza/, urlhttp://
    ## mashable.com/2013/06/24/lebron-james-danny-green-twitter/, urlhttp://mashable.com/2013/06/24/snapchat-funding/, urlhttp://
    ## mashable.com/2013/06/24/statue-in-manchester-museum-spins-eerily-by-itself/, urlhttp://mashable.com/2013/07/01/edward-
    ## snowden-lingerie-ad/, urlhttp://mashable.com/2013/07/01/interactive-gettysburg-map/, urlhttp://mashable.com/2013/07/01/
    ## microsoft-mood-technology/, urlhttp://mashable.com/2013/07/08/hut-internet/, urlhttp://mashable.com/2013/07/15/apps-for-
    ## team-collaboration/, urlhttp://mashable.com/2013/07/15/cool-running-clothes/, urlhttp://mashable.com/2013/07/15/first-
    ## vine-album/, urlhttp://mashable.com/2013/07/15/golf-stars-happy-gilmore-impressions/, urlhttp://mashable.com/2013/07/15/
    ## hp-ray-ozzie/, urlhttp://mashable.com/2013/07/15/sharknado-viral/, urlhttp://mashable.com/2013/07/15/summer-camp-letters/,
    ## urlhttp://mashable.com/2013/07/15/tumblr-fandoms-glee-cory-monteith/, urlhttp://mashable.com/2013/07/22/absurd-missed-
    ## connections/, urlhttp://mashable.com/2013/07/22/apple-larger-ipad-iphone/, urlhttp://mashable.com/2013/07/22/state-of-
    ## digital-rights-google-hangout/, urlhttp://mashable.com/2013/07/29/colorful-ink-video/, urlhttp://mashable.com/2013/07/29/
    ## grumpy-cat-cappuccino/, urlhttp://mashable.com/2013/07/29/hydradeck-shooting-game/, urlhttp://mashable.com/2013/07/29/
    ## kmart-da-rich-kidzz/, urlhttp://mashable.com/2013/07/29/newt-gingrich-google-glass/, urlhttp://mashable.com/2013/07/29/
    ## viral-video-recap-40/, urlhttp://mashable.com/2013/08/05/jeff-bezos-washington-post-opinion/, urlhttp://mashable.com/
    ## 2013/08/05/monday-gifs/, urlhttp://mashable.com/2013/08/05/samsung-note-event/, urlhttp://mashable.com/2013/08/12/gap-
    ## mobile-ad-takeover/, urlhttp://mashable.com/2013/08/12/hyperloop-op-ed/, urlhttp://mashable.com/2013/08/12/ipad-design-
    ## ipad-mini/, urlhttp://mashable.com/2013/08/12/michael-jordan-50-dunk/, urlhttp://mashable.com/2013/08/12/paranoia-movie-
    ## harrison-ford/, urlhttp://mashable.com/2013/08/12/roar-katy-perry-sara-bareilles-brave/, urlhttp://mashable.com/2013/08/1

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, : There were missing values in resampled
    ## performance measures.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances:
    ## is_weekend
