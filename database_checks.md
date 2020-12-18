Tonogenesis database checks
================
Steven Moran

18 December, 2020

# Overview

This is an [R markdown](https://rmarkdown.rstudio.com/) report that does
things like check fields in the database, creates tables and graphs of
the data, and integrates [Glottolog
metadata](https://glottolog.org/meta/downloads).

It’s also set up so that we can easily cite references in the [BibTeX
biblography](referencs.bib). For example: Kristoffersen (2007) will now
appear below in the references section.

First, let’s load some useful R libraries.

``` r
library(tidyverse)
library(knitr)
```

We’ve downloaded the data from the private [Google
sheets](https://docs.google.com/spreadsheets/d/1qkmyyNiTWilIxG6bZRDJ8eUef8ox7N_Q7seKCV0WGRo/edit#gid=0)
document and placed it in the [data](data) folder.

Let’s load the index.

``` r
index <- read.csv('data/Tonogenesis - Index.csv')
```

And let’s have a look at it.

``` r
kable(index) %>% head()
```

    ## [1] "| ID|LanguageVariety                             |Glottocode  |Family                         |Area          |Reference                                         |"
    ## [2] "|--:|:-------------------------------------------|:-----------|:------------------------------|:-------------|:-------------------------------------------------|"
    ## [3] "|  1|Proto-Nordic                                |(nort3160)  |Indoeuropean                   |Europe        |Kristoffersen (2007)                              |"
    ## [4] "|  2|Yabem                                       |yabe1254    |Austronesian                   |Papunesia     |Kingston (2011)                                   |"
    ## [5] "|  3|Kammu                                       |khmu1256    |Austroasiatic                  |Asia          |Kingston (2011)                                   |"
    ## [6] "|  4|Phan Rang Cham                              |east2563    |Austronesian                   |Asia          |Kingston (2011)                                   |"

Currently, we have parentheses in the `Glottocode` column that denote
non-leaf nodes. These will be changed in the future, but for now let’s
strip those out, so that we can merge our index with the Glottolog
metadata.

``` r
index$Glottocode <- index$Glottocode %>% str_replace("\\(", "")
index$Glottocode <- index$Glottocode %>% str_replace("\\)", "")
index$Glottocode
```

    ##  [1] "nort3160"  "yabe1254"  "khmu1256"  "east2563"  "viet1252"  "sind1278" 
    ##  [7] "nucl1310"  "lahu1253"  "midd1344"  "cher1273"  "atha1247"  "atha1247" 
    ## [13] "nort3160"  "cemu1238"  "utsa1239"  "baim1244"  "uuuu1243"  "lugb1240" 
    ## [19] "khal1275"  "kurt1248"  "moha1257"  "heil1246"  "iraq1241"  "podo1243" 
    ## [25] "bila1255"  ""          "dzon1239"  "kohu1244"  "kore1280"  "pwon1235" 
    ## [31] "hmon1333"  "hopi1249"  "mobw1234"  "huuu1240"  "esto1258"  "benc1235" 
    ## [37] "sout2746"  ""          "shan1277"  ""          "midd1319"  "newc1243" 
    ## [43] "extr1245"  "taik1256"  "samo1305"  " dani1285" " dani1285" "scot1245" 
    ## [49] "limb1263"  "tian1238"  "tsat1238"  "thai1261?" "slav1255"  "yeni1252" 
    ## [55] "bwek1238 " "chey1247"  "kick1244"  "boro1277"  "pwoo1239"  "sgaw1245" 
    ## [61] "prus1238"  ""          "zhuo1234"  ""          "cent2346"  ""         
    ## [67] "tuuu1240"  "raja1258"  "matb1237"  "morm1235"  "ires1239"  "east2280" 
    ## [73] "latv1249"  "lith1251"  "auks1239"  "waig1244"  "can1236"   "nupe1254" 
    ## [79] "arap1274"  "moha1258"  "cadd1256"  "kere1287"  "take1257"  "quil1240" 
    ## [85] "geba1237"  "coas1300"  "tlin1245"

We’ve now removed the trailing parentheses, but we can see 1) that some
Glottocodes have spaces, e.g. " dani1285“, and some are empty strings.
We need to fix the spaces in the input data (another reason why we don’t
want to have to maintain multiple columns in different sheets with the
same information!). For the time being, we will simply strip the
whitespace here, because otherwise when we join in the metadata,”
dani1285" will not match Glottolog metadata “dani1285.”

``` r
index$Glottocode <- index$Glottocode %>% str_trim()
index$Glottocode
```

    ##  [1] "nort3160"  "yabe1254"  "khmu1256"  "east2563"  "viet1252"  "sind1278" 
    ##  [7] "nucl1310"  "lahu1253"  "midd1344"  "cher1273"  "atha1247"  "atha1247" 
    ## [13] "nort3160"  "cemu1238"  "utsa1239"  "baim1244"  "uuuu1243"  "lugb1240" 
    ## [19] "khal1275"  "kurt1248"  "moha1257"  "heil1246"  "iraq1241"  "podo1243" 
    ## [25] "bila1255"  ""          "dzon1239"  "kohu1244"  "kore1280"  "pwon1235" 
    ## [31] "hmon1333"  "hopi1249"  "mobw1234"  "huuu1240"  "esto1258"  "benc1235" 
    ## [37] "sout2746"  ""          "shan1277"  ""          "midd1319"  "newc1243" 
    ## [43] "extr1245"  "taik1256"  "samo1305"  "dani1285"  "dani1285"  "scot1245" 
    ## [49] "limb1263"  "tian1238"  "tsat1238"  "thai1261?" "slav1255"  "yeni1252" 
    ## [55] "bwek1238"  "chey1247"  "kick1244"  "boro1277"  "pwoo1239"  "sgaw1245" 
    ## [61] "prus1238"  ""          "zhuo1234"  ""          "cent2346"  ""         
    ## [67] "tuuu1240"  "raja1258"  "matb1237"  "morm1235"  "ires1239"  "east2280" 
    ## [73] "latv1249"  "lith1251"  "auks1239"  "waig1244"  "can1236"   "nupe1254" 
    ## [79] "arap1274"  "moha1258"  "cadd1256"  "kere1287"  "take1257"  "quil1240" 
    ## [85] "geba1237"  "coas1300"  "tlin1245"

Now it looks good.

Let’s load the Glottolog metadata.

``` r
glottolog <- read_csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/languages_and_dialects_geo.csv')
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   glottocode = col_character(),
    ##   name = col_character(),
    ##   isocodes = col_character(),
    ##   level = col_character(),
    ##   macroarea = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double()
    ## )

And have a look if it downloaded and loaded correctly.

``` r
glottolog %>% head() %>% kable()
```

| glottocode | name       | isocodes | level    | macroarea | latitude | longitude |
|:-----------|:-----------|:---------|:---------|:----------|---------:|----------:|
| 3adt1234   | 3Ad-Tekles | NA       | dialect  | Africa    |       NA |        NA |
| aala1237   | Aalawa     | NA       | dialect  | Papunesia |       NA |        NA |
| aant1238   | Aantantara | NA       | dialect  | Papunesia |       NA |        NA |
| aari1239   | Aari       | aiw      | language | Africa    |  5.95034 |   36.5721 |
| aari1240   | Aariya     | aay      | language | Eurasia   |       NA |        NA |
| aasa1238   | Aasax      | aas      | language | Africa    | -4.00679 |   36.8648 |

Let’s merge it into our index. We have to use the `by` parameter because
the column names are different.

``` r
index_glottolog <- left_join(index, glottolog, by=c("Glottocode"="glottocode"))
```

And have a look.

``` r
index_glottolog %>% head() %>% kable()
```

|  ID | LanguageVariety | Glottocode | Family        | Area      | Reference            | name         | isocodes | level    | macroarea | latitude | longitude |
|----:|:----------------|:-----------|:--------------|:----------|:---------------------|:-------------|:---------|:---------|:----------|---------:|----------:|
|   1 | Proto-Nordic    | nort3160   | Indoeuropean  | Europe    | Kristoffersen (2007) | NA           | NA       | NA       | NA        |       NA |        NA |
|   2 | Yabem           | yabe1254   | Austronesian  | Papunesia | Kingston (2011)      | Yabem        | jae      | language | Papunesia | -6.67052 |  147.8100 |
|   3 | Kammu           | khmu1256   | Austroasiatic | Asia      | Kingston (2011)      | Khmu         | kjg      | language | Eurasia   | 20.24630 |  101.6710 |
|   4 | Phan Rang Cham  | east2563   | Austronesian  | Asia      | Kingston (2011)      | Eastern Cham | cjm      | language | Eurasia   | 11.28530 |  108.4900 |
|   5 | Vietnamese      | viet1252   | Austroasiatic | Asia      | Kingston (2011)      | Vietnamese   | vie      | language | Eurasia   | 20.68119 |  105.7741 |
|   6 | Punjabi         | sind1278   | Indoeuropean  | Asia      | Yip (2002)           | NA           | NA       | NA       | NA        |       NA |        NA |

Glottolog has two different metadata files. This one is for the language
name, level, macroareas, and geo-corrdinates. It only contains present
day languages, i.e. leaf nodes in the tree, and hence not reconstructed
or intermediate level nodes like “Proto-Nordic.”

The other Glottolog metadata file is the [languoids
file](https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/glottolog_languoid.csv.zip),
which needs to be downloaded and unzipped. We have put it in the `data`
file for the time being.

Let’s have a look at it.

``` r
glottolog_languoids <- read_csv('data/languoid.csv')
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   id = col_character(),
    ##   family_id = col_character(),
    ##   parent_id = col_character(),
    ##   name = col_character(),
    ##   bookkeeping = col_logical(),
    ##   level = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   iso639P3code = col_character(),
    ##   description = col_logical(),
    ##   markup_description = col_logical(),
    ##   child_family_count = col_double(),
    ##   child_language_count = col_double(),
    ##   child_dialect_count = col_double(),
    ##   country_ids = col_character()
    ## )

``` r
glottolog_languoids %>% glimpse()
```

    ## Rows: 25,439
    ## Columns: 15
    ## $ id                   <chr> "3adt1234", "aala1237", "aant1238", "aari1238", …
    ## $ family_id            <chr> "afro1255", "aust1307", "nucl1709", "sout2845", …
    ## $ parent_id            <chr> "nort3292", "ramo1244", "nort2920", "ahkk1235", …
    ## $ name                 <chr> "3Ad-Tekles", "Aalawa", "Aantantara", "Aari-Gayi…
    ## $ bookkeeping          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, …
    ## $ level                <chr> "dialect", "dialect", "dialect", "family", "lang…
    ## $ latitude             <dbl> NA, NA, NA, NA, 5.950340, NA, -4.006790, NA, NA,…
    ## $ longitude            <dbl> NA, NA, NA, NA, 36.57210, NA, 36.86480, NA, NA, …
    ## $ iso639P3code         <chr> NA, NA, NA, "aiz", "aiw", "aay", "aas", NA, NA, …
    ## $ description          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ markup_description   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ child_family_count   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0,…
    ## $ child_language_count <dbl> 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 22, 0, 0, 0, 0,…
    ## $ child_dialect_count  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 29, 0, 0, 0, 0,…
    ## $ country_ids          <chr> NA, NA, NA, NA, "ET", "IN", "TZ", NA, NA, NA, NA…

We can see that it also contains information such as the language family
ID, ISO code, etc. It use to contain more information, such as
endangerment status, so for now let’s just work with our index.

Let’s see how our language names differ.

``` r
index_glottolog %>% filter(LanguageVariety != name) %>% select(ID, LanguageVariety, name) %>% kable()
```

|  ID | LanguageVariety                   | name               |
|----:|:----------------------------------|:-------------------|
|   3 | Kammu                             | Khmu               |
|   4 | Phan Rang Cham                    | Eastern Cham       |
|  14 | Cem                               | Cemuhî             |
|  16 | Baima Tibetan                     | Baima              |
|  18 | Western Lugbara                   | Lugbara            |
|  20 | Kurtöp                            | Kurtokha           |
|  22 | Heiltsuk                          | Heiltsuk-Oowekyala |
|  30 | Phlong                            | Pwo Northern Karen |
|  31 | White Hmong                       | Hmong Daw          |
|  33 | Blimaw                            | Mobwa Karen        |
|  36 | Gimira                            | Bench              |
|  37 | Nakhon Si Thammarat Thai          | Southern Thai      |
|  46 | Zealand Danish                    | Danish             |
|  47 | East Slesvig                      | Danish             |
|  48 | Scottish gaelic (Bernera)         | Scottish Gaelic    |
|  49 | Limburgish                        | Limburgan          |
|  50 | T’ientsin                         | Tianjin Mandarin   |
|  51 | Utsat                             | Tsat               |
|  55 | Chitabu (bwe)                     | Bwe Karen          |
|  58 | Shinasha                          | Boro (Ethiopia)    |
|  60 | Sgaw Karen                        | S’gaw Karen        |
|  61 | West Baltic (Prussian             | Old Prussian       |
|  63 | Zhuoni Tibetan                    | Zhuoni             |
|  67 | Mongour                           | Tu                 |
|  69 | Magey Matbat                      | Matbat             |
|  70 | Moor                              | Mor (Mor Islands)  |
|  75 | Auktaitian dialects of Lithuanian | Aukshtaitish       |
|  76 | Ambel                             | Waigeo             |
|  78 | Nupe                              | Nupe-Nupe-Tako     |
|  85 | Geba                              | Geba Karen         |
|  87 | Sanya-Henya Tlingit               | Tlingit            |

Or how our areas differ.

``` r
index_glottolog %>% filter(Area != macroarea) %>% select(Area, macroarea) %>% distinct() %>% kable()
```

| Area   | macroarea |
|:-------|:----------|
| Asia   | Eurasia   |
| Europe | Eurasia   |

Not bad.

Let’s have a look at the area distribution. The parameter
`exclude = FALSE` will return any NAs in the data.

``` r
table(index_glottolog$Area, exclude = FALSE)
```

    ## 
    ##        Africa          Asia        Europe North America     Papunesia 
    ##             9            38            14            16            10

Since we’re missing some connections with Glottolog, we’ll have a few
NAs here.

``` r
table(index_glottolog$macroarea, exclude = FALSE)
```

    ## 
    ##        Africa       Eurasia North America     Papunesia          <NA> 
    ##             7            35            12             7            26

Given the data that do overlap, we can create a quick map. There will be
a warning for where we’re missing geo-coordinates.

``` r
ggplot(data=index_glottolog, aes(x=longitude,y=latitude)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point()
```

    ## Warning: Removed 30 rows containing missing values (geom_point).

![](database_checks_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

We can also write some tests to make sure that our input data are valid.
For example, do all of our Glottocodes follow their format?

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
glottocode <- "([a-z0-9]{4})([0-9]{4})"
which(!(str_detect(index$Glottocode, glottocode)))
```

    ## [1] 26 38 40 62 64 66 77

``` r
# When the above codes are fixed, then we can uncomment this test -- otherwise the code fails here.
# expect_equal(length(which(!(str_detect(index$Glottocode, glottocode)))), 0)
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Kristoffersen2007" class="csl-entry">

Kristoffersen, Gjert. 2007. “Dialect Variation in East Norwegian Tone.”
In *Typological Studies in Word and Sentence Prosody*, edited by Tomas
Riad and Carlos Gussenhoven, 91–112. Berlin, New York: Mouton de
Gruyter. <https://doi.org/10.1515/9783110207569.91>.

</div>

</div>
