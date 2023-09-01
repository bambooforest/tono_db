TonoDB analyses
================
Steven Moran and Lilja Maria Sæbø

01 September, 2023

- [Setup](#setup)
- [Descriptive stats](#descriptive-stats)
- [Analyses](#analyses)

# Setup

Load the libraries.

``` r
library(tidyverse)
library(knitr)
library(kableExtra)
library(xtable)
```

Load the [CLDF data](https://github.com/cldf-datasets/tonodb/).

``` r
values <- 
  read_csv(url('https://raw.githubusercontent.com/cldf-datasets/tonodb/main/cldf/values.csv'))
languages <- 
  read_csv(url('https://raw.githubusercontent.com/cldf-datasets/tonodb/main/cldf/languages.csv'))
contributions <- 
  read_csv(url('https://raw.githubusercontent.com/cldf-datasets/tonodb/main/cldf/contributions.csv'))
parameters <- 
  read_csv(url('https://raw.githubusercontent.com/cldf-datasets/tonodb/main/cldf/parameters.csv'))
```

# Descriptive stats

We have this many languages in our sample.

``` r
nrow(languages)
```

    ## [1] 96

And this many observations.

``` r
nrow(values)
```

    ## [1] 250

Let’s map our data points. We note some rows are removed because the
lat/long figures are NA due to them being listed as dialects or language
families.

``` r
ggplot(data=languages, aes(x=Longitude, y=Latitude)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point() +
  theme_bw()
```

    ## Warning: Removed 24 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

These are the missing data points for geographic location.

``` r
languages %>% filter(is.na(Latitude)) %>% select(ID, Name, Macroarea, Latitude, Longitude) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
ID
</th>
<th style="text-align:left;">
Name
</th>
<th style="text-align:left;">
Macroarea
</th>
<th style="text-align:right;">
Latitude
</th>
<th style="text-align:right;">
Longitude
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
atha1247
</td>
<td style="text-align:left;">
Athabaskan
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
auks1239
</td>
<td style="text-align:left;">
Aukshtaitish
</td>
<td style="text-align:left;">
Eurasia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
cant1236
</td>
<td style="text-align:left;">
Cantonese
</td>
<td style="text-align:left;">
Eurasia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
cent2346
</td>
<td style="text-align:left;">
Central Tibetan
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
coas1300
</td>
<td style="text-align:left;">
Coast Tsimshian
</td>
<td style="text-align:left;">
North America
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
east2280
</td>
<td style="text-align:left;">
Eastern Baltic
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
extr1245
</td>
<td style="text-align:left;">
Extreme Southern New Caledonian
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
kere1287
</td>
<td style="text-align:left;">
Keresan
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
mang1393
</td>
<td style="text-align:left;">
Mangbetu-Asua
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
metn1237
</td>
<td style="text-align:left;">
Metnyo
</td>
<td style="text-align:left;">
Papunesia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
midd1319
</td>
<td style="text-align:left;">
Middle Franconian
</td>
<td style="text-align:left;">
Eurasia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
moha1257
</td>
<td style="text-align:left;">
Mohawk-Oneida
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
newc1243
</td>
<td style="text-align:left;">
New Caledonian
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
nort3160
</td>
<td style="text-align:left;">
North Germanic
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
podo1243
</td>
<td style="text-align:left;">
Podoko
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
pwoo1239
</td>
<td style="text-align:left;">
Pwo
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
raja1258
</td>
<td style="text-align:left;">
Raja Ampat Maya
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
sind1278
</td>
<td style="text-align:left;">
Sindhi-Lahnda
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
slav1255
</td>
<td style="text-align:left;">
Slavic
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
taik1256
</td>
<td style="text-align:left;">
Tai-Kadai
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
tere1281
</td>
<td style="text-align:left;">
Terena
</td>
<td style="text-align:left;">
South America
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
utsa1239
</td>
<td style="text-align:left;">
Lhasa Tibetan
</td>
<td style="text-align:left;">
Eurasia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
yeni1252
</td>
<td style="text-align:left;">
Yeniseian
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
zhuo1234
</td>
<td style="text-align:left;">
Zhuoni
</td>
<td style="text-align:left;">
Eurasia
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

We’ve gone through by hand and added approximate geocoordinates for
visualization purposes, e.g., using Glottolog’s Swedish latitude and
longitude for North Germanic.

Merge in the hand attributed geocoordinates.

``` r
# There must be a saner way to do this!
hc <- read_csv('hand_coordinates.csv')
tmp <- left_join(languages, hc, by=c("ID"="ID", "Name"="Name"))
tmp <- tmp %>% mutate(Latitude.x = coalesce(Latitude.x, Latitude.y))
tmp <- tmp %>% mutate(Longitude.x = coalesce(Longitude.x, Longitude.y))
tmp <- tmp %>% select(-Latitude.y, Longitude.y)
tmp <- tmp %>% rename(Latitude = Latitude.x)
tmp <- tmp %>% rename(Longitude = Longitude.x)
languages <- tmp
```

Redo the map.

``` r
ggplot(data=languages, aes(x=Longitude, y=Latitude)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Here we can add some color by language family.

``` r
ggplot(data=languages, aes(x=Longitude, y=Latitude, color=family_id)) + 
  borders("world", colour="gray50", fill="gray50") + 
  geom_point() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

How many data points per macroarea? (Note again several NAs.)

``` r
table(languages$Area, exclude=FALSE)
```

    ## Warning: Unknown or uninitialised column: `Area`.

    ## < table of extent 0 >

Some Glottolog macroareas are missing, e.g., languages that don’t have
Glottocodes or are family level codes.

``` r
languages %>% filter(is.na(Macroarea))
```

    ## # A tibble: 16 × 18
    ##    ID       Name  Macroarea Latitude Longitude Glottocode ISO639P3code family_id
    ##    <chr>    <chr> <chr>        <dbl>     <dbl> <chr>      <chr>        <chr>    
    ##  1 atha1247 Atha… <NA>        60.5      -151.  atha1247   <NA>         atha1245 
    ##  2 cent2346 Cent… <NA>        28.4        90.2 cent2346   <NA>         sino1245 
    ##  3 east2280 East… <NA>        56.8        24.3 east2280   <NA>         indo1319 
    ##  4 extr1245 Extr… <NA>       -22.1       167.  extr1245   <NA>         aust1307 
    ##  5 kere1287 Kere… <NA>        35.5      -106.  kere1287   <NA>         <NA>     
    ##  6 mang1393 Mang… <NA>         0.268      27.3 mang1393   <NA>         cent2225 
    ##  7 moha1257 Moha… <NA>        43.7       -74.7 moha1257   <NA>         iroq1247 
    ##  8 newc1243 New … <NA>       -20.9       167.  newc1243   <NA>         aust1307 
    ##  9 nort3160 Nort… <NA>        59.8        17.4 nort3160   <NA>         indo1319 
    ## 10 podo1243 Podo… <NA>        10.9        14.0 podo1243   <NA>         afro1255 
    ## 11 pwoo1239 Pwo   <NA>        18.0        99.6 pwoo1239   <NA>         sino1245 
    ## 12 raja1258 Raja… <NA>        -0.173     130.  raja1258   <NA>         aust1307 
    ## 13 sind1278 Sind… <NA>        30.1        75.3 sind1278   <NA>         indo1319 
    ## 14 slav1255 Slav… <NA>        49.9        15.1 slav1255   <NA>         indo1319 
    ## 15 taik1256 Tai-… <NA>        24.1       110.  taik1256   <NA>         <NA>     
    ## 16 yeni1252 Yeni… <NA>        63.8        87.5 yeni1252   <NA>         <NA>     
    ## # ℹ 10 more variables: parent_id <chr>, bookkeeping <lgl>, level <chr>,
    ## #   description <lgl>, markup_description <lgl>, child_family_count <dbl>,
    ## #   child_language_count <dbl>, child_dialect_count <dbl>, country_ids <chr>,
    ## #   Longitude.y <dbl>

``` r
# tmp <- languages %>% filter(is.na(Macroarea)) %>% select(ID, Name, Macroarea)
# write_csv(tmp, 'get_macroareas.csv')

# There must be a saner way to do this!
hc <- read_csv('hand_macroareas.csv')
tmp <- left_join(languages, hc, by=c("ID"="ID", "Name"="Name"))
tmp <- tmp %>% mutate(Macroarea.x = coalesce(Macroarea.x, Macroarea.y))
tmp <- tmp %>% select(-Macroarea.y)
tmp <- tmp %>% rename(Macroarea = Macroarea.x)
languages <- tmp
table(languages$Macroarea, exclude = FALSE)
```

    ## 
    ##        Africa       Eurasia North America     Papunesia South America 
    ##            13            48            19            10             6

And a quick look at our areas.

``` r
table(contributions$Area, exclude=FALSE)
```

    ## 
    ##        Africa          Asia        Europe North America     Papunesia 
    ##            13            39            14            20            10 
    ## South America 
    ##             6

# Analyses

Recreate some of the tables.

First merge the tonodb tables.

``` r
tonodb <- left_join(values, languages, by=c("Language_ID"="ID"))

# Reduce the Contributor table and get the TonoDB Area column
tmp <- contributions %>% select(ID, Family, Area)
tonodb <- left_join(tonodb, tmp, by=c("Inventory_ID"="ID"))

# tonodb %>% filter(is.na(family_id))
```

Distribution of the languages, families and cases of tonogenesis across
different areas.

``` r
x <- tonodb %>% select(Area, Language_ID) %>% distinct() %>% group_by(Area) %>% summarise(Languages = n())
y <- tonodb %>% select(Area, family_id) %>% distinct() %>% group_by(Area) %>% summarize(Families = n())
z <- tonodb %>% select(Area, TriggeringContext) %>% group_by(Area) %>% summarize(`Cases of tonogenesis` = n())

tmp <- left_join(x, y)
```

    ## Joining with `by = join_by(Area)`

``` r
tmp <- left_join(tmp, z)
```

    ## Joining with `by = join_by(Area)`

``` r
tmp %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Area
</th>
<th style="text-align:right;">
Languages
</th>
<th style="text-align:right;">
Families
</th>
<th style="text-align:right;">
Cases of tonogenesis
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
154
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
North America
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
Papunesia
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8
</td>
</tr>
</tbody>
</table>

``` r
# Still getting some NAs, let's drop them
table(tonodb$family_id, exclude = FALSE)
```

    ## 
    ## afro1255 algi1248 araw1281 atha1245 atla1278 aust1305 aust1307 cadd1255 
    ##        3        5        1        5        6       15       25        1 
    ## cent2225 chim1311 gong1255 hmon1336 indo1319 iroq1247 koma1264 kore1284 
    ##        4        1        2       10       23        7        6        2 
    ## maya1287 mong1349 nada1235 sino1245 taik1256 tsim1258 tuca1253 ural1272 
    ##        4        1        3       58       28        1        4        1 
    ## utoa1244 waka1280     <NA> 
    ##        1        2       31

``` r
tmp <- tmp %>% filter(!is.na(Area))
tmp %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Area
</th>
<th style="text-align:right;">
Languages
</th>
<th style="text-align:right;">
Families
</th>
<th style="text-align:right;">
Cases of tonogenesis
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
154
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
North America
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
Papunesia
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
South America
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8
</td>
</tr>
</tbody>
</table>

``` r
# TODO: comment this out for the SI
print(xtable(tmp, type = "latex", caption="Distribution of the languages, families and cases of tonogenesis across different areas"), include.rownames=FALSE)
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{lrrr}
    ##   \hline
    ## Area & Languages & Families & Cases of tonogenesis \\ 
    ##   \hline
    ## Africa &  13 &   5 &  21 \\ 
    ##   Asia &  37 &   9 & 154 \\ 
    ##   Europe &  12 &   2 &  22 \\ 
    ##   North America &  19 &  10 &  29 \\ 
    ##   Papunesia &  10 &   1 &  16 \\ 
    ##   South America &   6 &   3 &   8 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Distribution of the languages, families and cases of tonogenesis across different areas} 
    ## \end{table}

Number of languages in different families.

``` r
tmp <- tonodb %>% select(family_id, LanguageVariety) %>% distinct() %>% arrange(family_id, LanguageVariety) %>% group_by(family_id) %>% summarize(`Number of varieties` = n(), Languages = str_c(LanguageVariety, collapse=", "))

# We need the Glottolog family names
glottolog <- read_csv('data/languoid.csv')
families <- glottolog %>% filter(id %in% tmp$family_id) %>% select(id, name)
tmp <- left_join(tmp, families, by=c("family_id"="id"))
tmp <- tmp %>% select(name, `Number of varieties`, Languages)
tmp <- tmp %>% rename(Family = name)

tmp %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Family
</th>
<th style="text-align:right;">
Number of varieties
</th>
<th style="text-align:left;">
Languages
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Afro-Asiatic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Iraqw, Podoko
</td>
</tr>
<tr>
<td style="text-align:left;">
Algic
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Arapaho, Cheyenne, Kickapoo
</td>
</tr>
<tr>
<td style="text-align:left;">
Arawakan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Terena
</td>
</tr>
<tr>
<td style="text-align:left;">
Athabaskan-Eyak-Tlingit
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Proto-Athabaskan (tonal dialects) group one, Proto-Athabaskan (tonal
dialects) group two, Sanya-Henya Tlingit
</td>
</tr>
<tr>
<td style="text-align:left;">
Atlantic-Congo
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Bantu D30, Bila, Kohumono, Moba, Nupe
</td>
</tr>
<tr>
<td style="text-align:left;">
Austroasiatic
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Hu, Kammu, U, Vietnamese
</td>
</tr>
<tr>
<td style="text-align:left;">
Austronesian
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Cem, Central North New Caledonian languages, Far South New Caledonian
langauges, Magey Matbat, Metnyo Ambel, Moor, Phan Rang Cham,
Proto-Maˈya, Samoan, Utsat, Yabem, Yerisiam
</td>
</tr>
<tr>
<td style="text-align:left;">
Caddoan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Caddo
</td>
</tr>
<tr>
<td style="text-align:left;">
Central Sudanic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Languages of the Mangbetu-Asua subgroup with three tones, Western
Lugbara
</td>
</tr>
<tr>
<td style="text-align:left;">
Chimakuan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Quileute
</td>
</tr>
<tr>
<td style="text-align:left;">
Ta-Ne-Omotic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Gimira, Shinasha
</td>
</tr>
<tr>
<td style="text-align:left;">
Hmong-Mien
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
White Hmong
</td>
</tr>
<tr>
<td style="text-align:left;">
Indo-European
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Auktaitian dialects of Lithuanian, Central Franconian, Central
Scandinavian, East Baltic (Latvian and Lithuanian), East Slesvig, Late
Proto-Slavic, Latvian, Limburgish, Lithuanian, Proto-Nordic, Punjabi,
Scottish gaelic (Bernera), West Baltic (Prussian, Zealand Danish
</td>
</tr>
<tr>
<td style="text-align:left;">
Iroquoian
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Cherokee, Mohawk, Proto-Mohawk-Oneida
</td>
</tr>
<tr>
<td style="text-align:left;">
Koman
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Proto-Gwama, Proto-Opo
</td>
</tr>
<tr>
<td style="text-align:left;">
Koreanic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Korean
</td>
</tr>
<tr>
<td style="text-align:left;">
Mayan
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Mocho’, San Bartolo Tzotzil, Uspanteko, Yucatec
</td>
</tr>
<tr>
<td style="text-align:left;">
Mongolic-Khitan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Mongour
</td>
</tr>
<tr>
<td style="text-align:left;">
Naduhup
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Eastern Naduhup
</td>
</tr>
<tr>
<td style="text-align:left;">
Sino-Tibetan
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Baima Tibetan, Brokpa, Burmese, Cantonese, Chitabu (bwe), Dzongkha,
Geba, Khaling, Kurtöp, Lahu, Lhasa Tibetan, Middle Chinese, Phlong, Pwo
Karen, Rikeze Tibetan, Sgaw Karen, Tokpe Gola (Tibetan), T’ientsin,
Zhibo Tibetan, Zhuoni Tibetan
</td>
</tr>
<tr>
<td style="text-align:left;">
Tai-Kadai
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Nakhon Si Thammarat Thai, Proto-Tai, Shan, Yung Chiang Kam
</td>
</tr>
<tr>
<td style="text-align:left;">
Tsimshian
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Coast Tsimshian
</td>
</tr>
<tr>
<td style="text-align:left;">
Tucanoan
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Barasana, Kubeo, Máíhɨ̃ki, Tatuyo
</td>
</tr>
<tr>
<td style="text-align:left;">
Uralic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Estonian
</td>
</tr>
<tr>
<td style="text-align:left;">
Uto-Aztecan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Hopi
</td>
</tr>
<tr>
<td style="text-align:left;">
Wakashan
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Heiltsuk
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Blimaw, Dong, Keres, Kra-Dai languages, Pre-proto-Yeniseian, Szu ta
Chai, Takelma
</td>
</tr>
</tbody>
</table>

``` r
# TODO: comment this out for the SI
print(xtable(tmp, type = "latex", caption="Number of languages in different language families"), include.rownames=FALSE)
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{lrl}
    ##   \hline
    ## Family & Number of varieties & Languages \\ 
    ##   \hline
    ## Afro-Asiatic &   2 & Iraqw, Podoko \\ 
    ##   Algic &   3 & Arapaho, Cheyenne, Kickapoo \\ 
    ##   Arawakan &   1 & Terena \\ 
    ##   Athabaskan-Eyak-Tlingit &   3 & Proto-Athabaskan (tonal dialects) group one, Proto-Athabaskan (tonal dialects) group two, Sanya-Henya Tlingit \\ 
    ##   Atlantic-Congo &   5 & Bantu D30, Bila, Kohumono, Moba, Nupe \\ 
    ##   Austroasiatic &   4 & Hu, Kammu, U, Vietnamese \\ 
    ##   Austronesian &  12 & Cem, Central North New Caledonian languages, Far South New Caledonian langauges, Magey Matbat, Metnyo Ambel, Moor, Phan Rang Cham, Proto-Maˈya, Samoan, Utsat, Yabem, Yerisiam \\ 
    ##   Caddoan &   1 & Caddo \\ 
    ##   Central Sudanic &   2 & Languages of the  Mangbetu-Asua subgroup with three tones, Western Lugbara \\ 
    ##   Chimakuan &   1 & Quileute \\ 
    ##   Ta-Ne-Omotic &   2 & Gimira, Shinasha \\ 
    ##   Hmong-Mien &   1 & White Hmong \\ 
    ##   Indo-European &  14 & Auktaitian dialects of Lithuanian, Central Franconian, Central Scandinavian, East Baltic (Latvian and Lithuanian), East Slesvig, Late Proto-Slavic, Latvian, Limburgish, Lithuanian, Proto-Nordic, Punjabi, Scottish gaelic (Bernera), West Baltic (Prussian, Zealand Danish \\ 
    ##   Iroquoian &   3 & Cherokee, Mohawk, Proto-Mohawk-Oneida \\ 
    ##   Koman &   2 & Proto-Gwama, Proto-Opo \\ 
    ##   Koreanic &   1 & Korean \\ 
    ##   Mayan &   4 & Mocho', San Bartolo Tzotzil, Uspanteko, Yucatec \\ 
    ##   Mongolic-Khitan &   1 & Mongour \\ 
    ##   Naduhup &   1 & Eastern Naduhup \\ 
    ##   Sino-Tibetan &  20 & Baima Tibetan, Brokpa, Burmese, Cantonese, Chitabu (bwe), Dzongkha, Geba, Khaling, Kurtöp, Lahu, Lhasa Tibetan, Middle Chinese, Phlong, Pwo Karen, Rikeze Tibetan, Sgaw Karen, Tokpe Gola (Tibetan), T’ientsin, Zhibo Tibetan, Zhuoni Tibetan \\ 
    ##   Tai-Kadai &   4 & Nakhon Si Thammarat Thai, Proto-Tai, Shan, Yung Chiang Kam \\ 
    ##   Tsimshian &   1 & Coast Tsimshian \\ 
    ##   Tucanoan &   4 & Barasana, Kubeo, Máíhɨ̃ki, Tatuyo \\ 
    ##   Uralic &   1 & Estonian \\ 
    ##   Uto-Aztecan &   1 & Hopi \\ 
    ##   Wakashan &   1 & Heiltsuk \\ 
    ##    &   7 & Blimaw, Dong, Keres, Kra-Dai languages, Pre-proto-Yeniseian, Szu ta Chai, Takelma \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Number of languages in different language families} 
    ## \end{table}

Cases of tonogenesis sorted by triggering context.

``` r
x <- tonodb %>% group_by(Type) %>% summarize(`Cases of tonogenesis` = n()) %>% arrange()
y <- tonodb %>% select(Type, LanguageVariety) %>% distinct() %>% group_by(Type) %>% summarize(`Number of languages` = n()) %>% arrange()
tmp <- left_join(x, y)
```

    ## Joining with `by = join_by(Type)`

``` r
tmp %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Type
</th>
<th style="text-align:right;">
Cases of tonogenesis
</th>
<th style="text-align:right;">
Number of languages
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
coda
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
39
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, nucleus
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, onset
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, wordtype
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
nucleus, onset
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
onset
</td>
<td style="text-align:right;">
128
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
onset, other
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
stress
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
wordtype
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
wordtype, nucleus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
# Remove NAs
tmp <- tmp %>% filter(!is.na(Type))
tmp %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Type
</th>
<th style="text-align:right;">
Cases of tonogenesis
</th>
<th style="text-align:right;">
Number of languages
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
coda
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
39
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, nucleus
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, onset
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
coda, wordtype
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
nucleus, onset
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
onset
</td>
<td style="text-align:right;">
128
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
onset, other
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
stress
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
wordtype
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
wordtype, nucleus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
# TODO: comment this out for the SI
print(xtable(tmp, type = "latex", caption="Cases of tonogenesis by category"), include.rownames=FALSE)
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{lrr}
    ##   \hline
    ## Type & Cases of tonogenesis & Number of languages \\ 
    ##   \hline
    ## coda &  58 &  39 \\ 
    ##   coda, nucleus &   2 &   1 \\ 
    ##   coda, onset &   2 &   2 \\ 
    ##   coda, wordtype &   4 &   4 \\ 
    ##   nucleus &  18 &  11 \\ 
    ##   nucleus, onset &   1 &   1 \\ 
    ##   onset & 128 &  36 \\ 
    ##   onset, other &   2 &   1 \\ 
    ##   other &   3 &   3 \\ 
    ##   stress &  11 &   8 \\ 
    ##   wordtype &  20 &  15 \\ 
    ##   wordtype, nucleus &   1 &   1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Cases of tonogenesis by category} 
    ## \end{table}

Tonogenesis conditioned by voiced and voiceless (unaspirated)
obstruents.

``` r
tmp <- tonodb %>% select(OnsetVoicing, EffectOnPitch)
table(tmp)
```

    ##                    EffectOnPitch
    ## OnsetVoicing        elevating falling level lowering lowering, elevating mid
    ##   sonorant                  1       1     0        0                   0   0
    ##   Voiced                    9       2     0       35                   0   0
    ##   Voiced, voiceless         4       0     0        3                   1   2
    ##   Voiceless                32       1     0        9                   0   7
    ##                    EffectOnPitch
    ## OnsetVoicing        no change rising rising-falling rising, elevating
    ##   sonorant                  0      0              0                 0
    ##   Voiced                    0      2              0                 0
    ##   Voiced, voiceless         0      0              0                 0
    ##   Voiceless                 0      0              0                 0
    ##                    EffectOnPitch
    ## OnsetVoicing        rising, lowering
    ##   sonorant                         0
    ##   Voiced                           0
    ##   Voiced, voiceless                0
    ##   Voiceless                        0

``` r
tmp <- tonodb %>% select(OnsetVoicing, EffectOnPitch) %>% filter(OnsetVoicing != "") %>% filter(EffectOnPitch != "")
table(tmp)
```

    ##                    EffectOnPitch
    ## OnsetVoicing        elevating falling lowering lowering, elevating mid rising
    ##   sonorant                  1       1        0                   0   0      0
    ##   Voiced                    9       2       35                   0   0      2
    ##   Voiced, voiceless         4       0        3                   1   2      0
    ##   Voiceless                32       1        9                   0   7      0

``` r
tmp <- tonodb %>% select(OnsetVoicing, EffectOnPitch) %>% 
  filter(OnsetVoicing != "") %>% 
  filter(EffectOnPitch != "") %>%
  filter(OnsetVoicing %in% c("Voiced", "Voiceless"))
table(tmp)
```

    ##             EffectOnPitch
    ## OnsetVoicing elevating falling lowering mid rising
    ##    Voiced            9       2       35   0      2
    ##    Voiceless        32       1        9   7      0

``` r
tmp <- tonodb %>% select(OnsetVoicing, EffectOnPitch) %>% 
  filter(OnsetVoicing != "") %>% 
  filter(EffectOnPitch != "") %>%
  filter(OnsetVoicing %in% c("Voiced", "Voiceless"))
table(tmp)
```

    ##             EffectOnPitch
    ## OnsetVoicing elevating falling lowering mid rising
    ##    Voiced            9       2       35   0      2
    ##    Voiceless        32       1        9   7      0

``` r
tmp <- tonodb %>% select(OnsetVoicing, EffectOnPitch) %>% 
  filter(OnsetVoicing != "") %>% 
  filter(EffectOnPitch != "") %>%
  filter(OnsetVoicing %in% c("Voiced", "Voiceless"))
table(tmp)
```

    ##             EffectOnPitch
    ## OnsetVoicing elevating falling lowering mid rising
    ##    Voiced            9       2       35   0      2
    ##    Voiceless        32       1        9   7      0

``` r
print(xtable(table(tmp), type = "latex", caption="Tonogenesis conditioned by voiced and voiceless (unaspirated) obstruents"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & mid & rising \\ 
    ##   \hline
    ## Voiced &   9 &   2 &  35 &   0 &   2 \\ 
    ##   Voiceless &  32 &   1 &   9 &   7 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenesis conditioned by voiced and voiceless (unaspirated) obstruents} 
    ## \end{table}

Tonogenesis triggered by coda consonants.

``` r
tmp <- tonodb %>% select(CodaGlottal, EffectOnPitch) %>%
  filter(!is.na(CodaGlottal)) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "") %>%
  filter(EffectOnPitch %in% c("level", "rising", "falling"))
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Tonogenesis triggered by coda consonants"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & falling & rising \\ 
    ##   \hline
    ## /h/ &   1 &   0 \\ 
    ##   /h/, glottal stop &   1 &   1 \\ 
    ##   glottal stop &   2 &   3 \\ 
    ##   glottalized &   1 &   2 \\ 
    ##   laryngeal &   6 &   0 \\ 
    ##   non-glottalized &   1 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenesis triggered by coda consonants} 
    ## \end{table}

Tonogenesis based on vowel length.

``` r
table(tonodb$Nucleus, tonodb$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
-ATR
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
-ATR and non-high vowel
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR and high vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
high vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
long vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
low vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long, glottalic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% select(Nucleus, EffectOnPitch) %>%
  filter(Nucleus != "") %>%
  filter(EffectOnPitch != "") %>%
  filter(Nucleus %in% c("long vowel", "short vowel"))

print(xtable(table(tmp), type = "latex", caption="Tonogenesis based on vowel length"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## long vowel &   1 &   1 \\ 
    ##   short vowel &   3 &   1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenesis based on vowel length} 
    ## \end{table}

Tonogenesis based on vowel length – high/low is relative.

``` r
table(tonodb$Nucleus, tonodb$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
-ATR
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
-ATR and non-high vowel
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR and high vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
high vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
long vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
low vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long, glottalic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% select(Nucleus, EffectOnPitch) %>%
  filter(Nucleus != "") %>%
  filter(EffectOnPitch != "") %>%
  filter(Nucleus %in% c("high vowel", "low vowel"))

print(xtable(table(tmp), type = "latex", caption="Tonogenesis based on vowel height – high/low is relative"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## high vowel &   3 &   1 \\ 
    ##   low vowel &   1 &   2 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenesis based on vowel height – high/low is relative} 
    ## \end{table}

Tonogenesis based on ATR – high/low is relative.

``` r
table(tonodb$Nucleus, tonodb$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
-ATR
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
-ATR and non-high vowel
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR and high vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
high vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
long vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
low vowel
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short vowel
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
short, long, glottalic
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% select(Nucleus, EffectOnPitch) %>%
  filter(Nucleus != "") %>%
  filter(EffectOnPitch != "") %>%
  filter(Nucleus %in% c("+ATR", "-ATR"))

print(xtable(table(tmp), type = "latex", caption="Tonogenesis based on ATR – high/low is relative"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## -ATR &   0 &   1 \\ 
    ##   +ATR &   1 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenesis based on ATR – high/low is relative} 
    ## \end{table}

The effect of voicing on tone in the DoTE (number of languages).

``` r
tmp <- tonodb %>% filter(Onset %in% c('voiceless', 'voiced'))
table(tmp$Onset, tmp$EffectOnPitch)
```

    ##            
    ##             elevating falling lowering rising
    ##   voiced            2       1       19      1
    ##   voiceless        15       0        1      0

``` r
print(xtable(table(tmp$Onset, tmp$EffectOnPitch), type = "latex", caption="The effect of voicing on tone"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & rising \\ 
    ##   \hline
    ## voiced &   2 &   1 &  19 &   1 \\ 
    ##   voiceless &  15 &   0 &   1 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of voicing on tone} 
    ## \end{table}

Tonogenesis triggered by codas in the DoTE (number of cases of
tonogenesis).

``` r
table(tonodb$Coda, tonodb$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/ + consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
absence of glottalization
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
breathy voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
creaky
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal constriction
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic, non-glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalization
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
no glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
not glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open, nasal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open, semivowel, sonorant, other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
preaspirated, /h/
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop, glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative + voiceless sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% select(Coda, EffectOnPitch) %>% filter_at(vars(Coda, EffectOnPitch),any_vars(!is.na(.)))
table(tmp$Coda, tmp$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/ + consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
absence of glottalization
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
breathy voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
creaky
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal constriction
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic, non-glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalization
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
no glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
not glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open, nasal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open, semivowel, sonorant, other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
preaspirated, /h/
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop, glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative + voiceless sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% select(Coda, EffectOnPitch) %>% filter_at(vars(Coda, EffectOnPitch),all_vars(!is.na(.)))
table(tmp$Coda, tmp$EffectOnPitch) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/ + consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
absence of glottalization
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
breathy voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
creaky
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal consonant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal constriction
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalic, non-glottalic
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalization
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
no glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open, nasal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
preaspirated, /h/
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop, glottal stop
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless fricative + voiceless sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

Onset Voicing by effect on pitch.

``` r
tmp <- tonodb %>% select(OnsetAspiration, EffectOnPitch) %>%
  filter(OnsetAspiration != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Aspirated
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Aspirated, unaspirated
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Breathy
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Unaspirated
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of voicing on tone"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & mid & rising \\ 
    ##   \hline
    ## Aspirated &   5 &   0 &   6 &   3 &   0 \\ 
    ##   Aspirated, unaspirated &   5 &   1 &   1 &   0 &   0 \\ 
    ##   Breathy &   0 &   0 &   0 &   0 &   1 \\ 
    ##   Unaspirated &   4 &   0 &   3 &   6 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of voicing on tone} 
    ## \end{table}

The effect of voicing on pitch.

``` r
tmp <- tonodb %>% select(CodaManner, EffectOnPitch) %>%
  filter(CodaManner != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
cluster
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
fricative
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent, sonorant
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of voicing on tone"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrr}
    ##   \hline
    ##  & elevating & falling & level & lowering & rising \\ 
    ##   \hline
    ## cluster &   0 &   0 &   0 &   1 &   0 \\ 
    ##   fricative &   1 &   3 &   0 &   0 &   0 \\ 
    ##   obstruent &   2 &   3 &   0 &   1 &   1 \\ 
    ##   obstruent, sonorant &   1 &   1 &   0 &   1 &   0 \\ 
    ##   open &   0 &   0 &   1 &   0 &   0 \\ 
    ##   sonorant &   0 &   1 &   1 &   0 &   0 \\ 
    ##   sonorant, open &   0 &   0 &   2 &   0 &   0 \\ 
    ##   stop &   3 &   2 &   0 &   5 &   3 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of voicing on tone} 
    ## \end{table}

The effect of voice on pitch.

``` r
tmp <- tonodb %>% select(CodaPhonation, EffectOnPitch) %>%
  filter(CodaPhonation != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
breathy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
creaky
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
preaspirated
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of voice on pitch"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrr}
    ##   \hline
    ##  & falling & lowering & rising \\ 
    ##   \hline
    ## breathy &   1 &   0 &   0 \\ 
    ##   creaky &   2 &   0 &   0 \\ 
    ##   preaspirated &   0 &   1 &   0 \\ 
    ##   voiced &   1 &   1 &   0 \\ 
    ##   voiceless &   2 &   0 &   1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of voice on pitch} 
    ## \end{table}

The effect of coda glottal on pitch.

``` r
tmp <- tonodb %>% select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized, non-glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of coda glottal on pitch"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & rising \\ 
    ##   \hline
    ## /h/ &   1 &   1 &   1 &   0 \\ 
    ##   /h/, glottal stop &   1 &   1 &   0 &   1 \\ 
    ##   glottal stop &   3 &   2 &   4 &   3 \\ 
    ##   glottalized &   3 &   1 &   1 &   2 \\ 
    ##   glottalized, non-glottalized &   1 &   0 &   1 &   0 \\ 
    ##   laryngeal &   0 &   6 &   0 &   0 \\ 
    ##   non-glottalized &   0 &   1 &   0 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of coda glottal on pitch} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized, non-glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

The effect of vowel height on pitch.

``` r
tmp <- tonodb %>% select(Height, EffectOnPitch) %>%
  filter(Height != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of vowel height on pitch"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrrrrrrr}
    ##   \hline
    ##  & elevating & falling & level & lowering & lowering, elevating & mid & no change & rising & rising, elevating & rising, lowering \\ 
    ##   \hline
    ## high &  51 &   0 &   0 &   4 &   0 &   0 &   1 &   1 &   1 &   0 \\ 
    ##   low &   0 &   1 &   0 &  47 &   0 &   0 &   0 &   0 &   0 &   1 \\ 
    ##   mid &   8 &   1 &   1 &   5 &   1 &   2 &   0 &   1 &   0 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of vowel height on pitch} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

The effect of nucleus length on pitch.

``` r
tmp <- tonodb %>% select(NucleusLength, EffectOnPitch) %>%
  filter(NucleusLength != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
long
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
short
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of nucleus length on pitch"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## long &   1 &   1 \\ 
    ##   short &   3 &   1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of nucleus length on pitch} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
long
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
short
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

The effect of nuclear +/iATR on pitch.

``` r
tmp <- tonodb %>% select(NucleusATR, EffectOnPitch) %>%
  filter(NucleusATR != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
-ATR
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="The effect of nuclear +/iATR on pitch"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## -ATR &   0 &   2 \\ 
    ##   +ATR &   2 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{The effect of nuclear +/iATR on pitch} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
-ATR
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
+ATR
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

For the second case study, the one about area, we will need:

Number of cases/varieties of different types for each region.

Africa.

``` r
tmp <- tonodb %>% filter(Area == "Africa") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "Africa") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<tbody>
<tr>
</tr>
</tbody>
</table>

``` r
# Nothing here
# print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for Africa"))
```

Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
# print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for Asia"))
```

Europe.

``` r
tmp <- tonodb %>% filter(Area == "Europe") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
no change
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising-falling
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "Europe") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
non-glottalized
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for Europe"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrr}
    ##   \hline
    ##  & elevating & falling & rising \\ 
    ##   \hline
    ## glottalized &   1 &   0 &   2 \\ 
    ##   non-glottalized &   0 &   1 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Number of cases/varieties of different tonogenesis types for Europe} 
    ## \end{table}

North America.

``` r
tmp <- tonodb %>% filter(Area == "North America") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
<th style="text-align:right;">
rising, elevating
</th>
<th style="text-align:right;">
rising, lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized, non-glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "North America") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
/h/, glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized, non-glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
laryngeal
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for North America"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & rising \\ 
    ##   \hline
    ## /h/ &   0 &   0 &   1 &   0 \\ 
    ##   /h/, glottal stop &   1 &   1 &   0 &   1 \\ 
    ##   glottalized &   1 &   1 &   1 &   0 \\ 
    ##   glottalized, non-glottalized &   1 &   0 &   1 &   0 \\ 
    ##   laryngeal &   0 &   6 &   0 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Number of cases/varieties of different tonogenesis types for North America} 
    ## \end{table}

Papunesia.

``` r
tmp <- tonodb %>% filter(Area == "Papunesia") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "Papunesia") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<tbody>
<tr>
</tr>
</tbody>
</table>

``` r
# No results
# print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for Papunesia"))
```

``` r
tmp <- tonodb %>% filter(Area == "South America") %>% select(CodaGlottal, EffectOnPitch)
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
tmp <- tonodb %>% filter(Area == "South America") %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Number of cases/varieties of different tonogenesis types for South America"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## glottal stop &   1 &   3 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Number of cases/varieties of different tonogenesis types for South America} 
    ## \end{table}

Onset aspiration in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(OnsetAspiration, EffectOnPitch) %>% 
  select(OnsetAspiration, EffectOnPitch) %>%
  filter(OnsetAspiration != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Aspirated
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Aspirated, unaspirated
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Breathy
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Unaspirated
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Onset aspiration in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & mid & rising \\ 
    ##   \hline
    ## Aspirated &   3 &   0 &   5 &   3 &   0 \\ 
    ##   Aspirated, unaspirated &   5 &   1 &   1 &   0 &   0 \\ 
    ##   Breathy &   0 &   0 &   0 &   0 &   1 \\ 
    ##   Unaspirated &   4 &   0 &   1 &   6 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Onset aspiration in Asia} 
    ## \end{table}

Coda glottal in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(CodaGlottal, EffectOnPitch) %>% 
  select(CodaGlottal, EffectOnPitch) %>%
  filter(CodaGlottal != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
/h/
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
glottal stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glottalized
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Coda glottal in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & rising \\ 
    ##   \hline
    ## /h/ &   1 &   1 &   0 &   0 \\ 
    ##   glottal stop &   2 &   2 &   1 &   3 \\ 
    ##   glottalized &   1 &   0 &   0 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Coda glottal in Asia} 
    ## \end{table}

Coda manner in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(CodaManner, EffectOnPitch) %>% 
  select(CodaManner, EffectOnPitch) %>%
  filter(CodaManner != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
fricative
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Coda manner in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrr}
    ##   \hline
    ##  & elevating & falling & level & lowering & rising \\ 
    ##   \hline
    ## fricative &   1 &   3 &   0 &   0 &   0 \\ 
    ##   obstruent &   1 &   2 &   0 &   0 &   0 \\ 
    ##   open &   0 &   0 &   1 &   0 &   0 \\ 
    ##   sonorant &   0 &   1 &   1 &   0 &   0 \\ 
    ##   sonorant, open &   0 &   0 &   2 &   0 &   0 \\ 
    ##   stop &   2 &   2 &   0 &   2 &   3 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Coda manner in Asia} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
level
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
fricative
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
obstruent
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
sonorant, open
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
stop
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

Coda phonation type in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(CodaPhonation, EffectOnPitch) %>% 
  select(CodaPhonation, EffectOnPitch) %>%
  filter(CodaPhonation != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
breathy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Coda phonation type in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & falling & lowering \\ 
    ##   \hline
    ## breathy &   1 &   0 \\ 
    ##   voiced &   0 &   1 \\ 
    ##   voiceless &   1 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Coda phonation type in Asia} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
breathy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
voiced
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
voiceless
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

Nucleus height in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(NucleusHeight, EffectOnPitch) %>% 
  select(NucleusHeight, EffectOnPitch) %>%
  filter(NucleusHeight != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
High
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Nucleus height in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrr}
    ##   \hline
    ##  & elevating & lowering \\ 
    ##   \hline
    ## High &   1 &   0 \\ 
    ##   Low &   0 &   1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Nucleus height in Asia} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
lowering
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
High
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

Onset voicing in Asia.

``` r
tmp <- tonodb %>% filter(Area == "Asia") %>% select(OnsetVoicing, EffectOnPitch) %>% 
  select(OnsetVoicing, EffectOnPitch) %>%
  filter(OnsetVoicing != "") %>%
  filter(EffectOnPitch != "")
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiced
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiced, voiceless
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiceless
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
print(xtable(table(tmp), type = "latex", caption="Onset voicing in Asia"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrrrrr}
    ##   \hline
    ##  & elevating & falling & lowering & lowering, elevating & mid & rising \\ 
    ##   \hline
    ## sonorant &   1 &   0 &   0 &   0 &   0 &   0 \\ 
    ##   Voiced &   9 &   2 &  30 &   0 &   0 &   2 \\ 
    ##   Voiced, voiceless &   4 &   0 &   3 &   1 &   2 &   0 \\ 
    ##   Voiceless &  28 &   1 &   9 &   0 &   7 &   0 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Onset voicing in Asia} 
    ## \end{table}

``` r
table(tmp) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
elevating
</th>
<th style="text-align:right;">
falling
</th>
<th style="text-align:right;">
lowering
</th>
<th style="text-align:right;">
lowering, elevating
</th>
<th style="text-align:right;">
mid
</th>
<th style="text-align:right;">
rising
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sonorant
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiced
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiced, voiceless
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Voiceless
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

Tonogenetic events by macroarea.

Worldwide.

``` r
tmp <- tonodb %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rllllll}
    ##   \hline
    ##  & 1 & 2 & 3 & 4 & 5 & 6 \\ 
    ##   \hline
    ## Type & coda & nucleus & onset & other & stress & wordtype \\ 
    ##   Number of cases &  66 &  22 & 133 &   5 &  11 &  25 \\ 
    ##   Number of varieties & 42 & 13 & 40 &  4 &  8 & 19 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
onset
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
stress
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
66
</td>
<td style="text-align:left;">
22
</td>
<td style="text-align:left;">
133
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
40
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
19
</td>
</tr>
</tbody>
</table>

Africa.

``` r
tmp <- tonodb  %>% filter(Area == "Africa") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in Africa in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rllll}
    ##   \hline
    ##  & 1 & 2 & 3 & 4 \\ 
    ##   \hline
    ## Type & nucleus & onset & other & wordtype \\ 
    ##   Number of cases & 7 & 7 & 1 & 6 \\ 
    ##   Number of varieties & 4 & 5 & 1 & 4 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in Africa in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
onset
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
4
</td>
</tr>
</tbody>
</table>

Asia.

``` r
tmp <- tonodb  %>% filter(Area == "South America") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in Asia in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:09 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlll}
    ##   \hline
    ##  & 1 & 2 & 3 \\ 
    ##   \hline
    ## Type & coda & nucleus & wordtype \\ 
    ##   Number of cases & 6 & 3 & 1 \\ 
    ##   Number of varieties & 5 & 1 & 1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in Asia in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
</tr>
</tbody>
</table>

Europe.

``` r
tmp <- tonodb  %>% filter(Area == "South America") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in Europe in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:10 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlll}
    ##   \hline
    ##  & 1 & 2 & 3 \\ 
    ##   \hline
    ## Type & coda & nucleus & wordtype \\ 
    ##   Number of cases & 6 & 3 & 1 \\ 
    ##   Number of varieties & 5 & 1 & 1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in Europe in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
</tr>
</tbody>
</table>

North America.

``` r
tmp <- tonodb  %>% filter(Area == "North America") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in North America in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:10 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rllllll}
    ##   \hline
    ##  & 1 & 2 & 3 & 4 & 5 & 6 \\ 
    ##   \hline
    ## Type & coda & nucleus & onset & other & stress & wordtype \\ 
    ##   Number of cases & 18 &  4 &  1 &  1 &  4 &  3 \\ 
    ##   Number of varieties & 16 &  3 &  1 &  1 &  2 &  2 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in North America in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
onset
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
stress
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
18
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
16
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
</tr>
</tbody>
</table>

South America.

``` r
tmp <- tonodb  %>% filter(Area == "South America") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in South America in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:10 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlll}
    ##   \hline
    ##  & 1 & 2 & 3 \\ 
    ##   \hline
    ## Type & coda & nucleus & wordtype \\ 
    ##   Number of cases & 6 & 3 & 1 \\ 
    ##   Number of varieties & 5 & 1 & 1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in South America in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
</tr>
</tbody>
</table>

Papunesia.

``` r
tmp <- tonodb  %>% filter(Area == "South America") %>% select(LanguageVariety, Type) %>% separate_rows(Type)
cases <- tmp %>% group_by(Type) %>% summarize(`Number of cases` = n())
varieties <- tmp %>% distinct() %>% group_by(Type) %>% summarize(`Number of varieties` = n())
t <- left_join(cases, varieties)
```

    ## Joining with `by = join_by(Type)`

``` r
print(xtable(t(t), type = "latex", caption="Tonogenetic events in Papunesia in the DTE"))
```

    ## % latex table generated in R 4.0.5 by xtable 1.8-4 package
    ## % Fri Sep  1 12:48:10 2023
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlll}
    ##   \hline
    ##  & 1 & 2 & 3 \\ 
    ##   \hline
    ## Type & coda & nucleus & wordtype \\ 
    ##   Number of cases & 6 & 3 & 1 \\ 
    ##   Number of varieties & 5 & 1 & 1 \\ 
    ##    \hline
    ## \end{tabular}
    ## \caption{Tonogenetic events in Papunesia in the DTE} 
    ## \end{table}

``` r
t(t) %>% kable()
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Type
</td>
<td style="text-align:left;">
coda
</td>
<td style="text-align:left;">
nucleus
</td>
<td style="text-align:left;">
wordtype
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of cases
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of varieties
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
</tr>
</tbody>
</table>
