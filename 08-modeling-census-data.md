# Modeling US Census data



The previous chapter included a range of examples illustrating methods for analyzing and exploring spatial datasets. Census data can also be used to derive models for explaining patterns that occur across regions or within cities. These models draw from concepts introduced in prior chapters, but can also be used as part of explanatory frameworks or within broader analytic pipelines for statistical inference or machine learning. This chapter introduces a series of such frameworks. The first section looks at *segregation and diversity indices* which are widely used across the social sciences to explain demographic patterns. The second section explores topics in statistical modeling, including methods for *spatial regression* that take into account the spatial autocorrelation inherent in most Census variables. The third and final section explores concepts such as *classification*, *clustering*, and *regionalization* which are common in both unsupervised and supervised machine learning. Examples will illustrate how to use Census data to generate neighborhood typologies, which are widely used for business and marketing applications, and how to generate spatially coherent sales territories from Census data with regionalization.

## Indices of segregation and diversity

A large body of research in the social sciences is concerned with neighborhood *segregation* and *diversity*. Segregation as addressed here generally refers to the measurement of the extent to which two or more groups live apart from each other; diversity as a companion metric measures neighborhood heterogeneity among groups. A wide range of indices have been developed by social scientists to measure segregation and diversity, and in many cases are inherently linked with spatial Census data which are often the best way to measure these concepts. Segregation and diversity indices are implemented in a variety of different R packages; the package recommended by this book is the **segregation** package [@elbers2021], which includes R functions for a variety of regional and local indices.

### Data setup with spatial analysis

Much of the segregation and diversity literature focuses on race and ethnicity, which will be explored in the example below. The data setup code uses spatial methods covered in the previous three chapters to acquire Census tract-level data on population estimates for non-Hispanic white, non-Hispanic black, non-Hispanic Asian, and Hispanic populations in California, then filters those Census tracts those that intersect the largest urbanized areas by population in the state using an inner spatial join. In turn, it is an illustrative example of how spatial analysis tools can be important parts of data setup workflows for analysis. As urbanized areas for the 2020 Census are not yet defined at the time of this writing, we'll be using urbanized areas for 2019 and data from the 2015-2019 ACS.


```r
library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

# Get California tract data by race/ethnicity
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "CA",
  geometry = TRUE,
  year = 2019
) 

# Use tidycensus to get urbanized areas by population with geometry, 
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", CA Urbanized Area (2010)")))

# Compute an inner spatial join between the California tracts and the 
# urbanized areas, returning tracts in the largest California urban 
# areas with the urban_name column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()
```

To summarize, the spatial analysis workflow detailed above uses the following steps:

1.  Data on race & ethnicity from the 2015-2019 5-year ACS for the four largest demographic groups in California is acquired with **tidycensus**'s `get_acs()` at the Census tract level with feature geometry included. Depending on the goals of the study, other racial/ethnic groups (e.g. native American, native Hawaiian/Pacific Islander) should be added or removed as needed.
2.  As urban areas as defined by the Census Bureau often cross state boundaries, urban areas must be obtained for the entire US with `get_acs()`. Once obtained, urban areas are filtered to only those areas with populations of 750,000 or greater, and then `transmute()` is used to retain only a new column representing the area name (along with the simple feature geometry column).
3.  A spatial join between the Census tract data and the urban area data is computed with `st_join()`. The argument `left = FALSE` computes an *inner spatial join*, which retains only those Census tracts that intersect the urban area boundaries, and appends the corresponding `urban_name` column to each Census tract.

The data structure appears as follows:

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:show-ca-urban-data)Prepared data for segregation analysis</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GEOID </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> variable </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> estimate </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> moe </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> urban_name </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 06013370000 </td>
   <td style="text-align:left;"> white </td>
   <td style="text-align:right;"> 1235 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06013370000 </td>
   <td style="text-align:left;"> black </td>
   <td style="text-align:right;"> 371 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06013370000 </td>
   <td style="text-align:left;"> asian </td>
   <td style="text-align:right;"> 540 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06013370000 </td>
   <td style="text-align:left;"> hispanic </td>
   <td style="text-align:right;"> 557 </td>
   <td style="text-align:right;"> 121 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06001442301 </td>
   <td style="text-align:left;"> white </td>
   <td style="text-align:right;"> 969 </td>
   <td style="text-align:right;"> 197 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06001442301 </td>
   <td style="text-align:left;"> black </td>
   <td style="text-align:right;"> 214 </td>
   <td style="text-align:right;"> 129 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06001442301 </td>
   <td style="text-align:left;"> asian </td>
   <td style="text-align:right;"> 3000 </td>
   <td style="text-align:right;"> 259 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06001442301 </td>
   <td style="text-align:left;"> hispanic </td>
   <td style="text-align:right;"> 1010 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:left;"> San Francisco--Oakland </td>
  </tr>
</tbody>
</table>

The data are in long (tidy) form, the default used by **tidycensus**; this data structure is ideal for computing indices in the **segregation** package.

### The dissimilarity index

The dissimilarity index is widely used to assess neighborhood segregation between two groups within a region. It is computed as follows:

$$
D = \frac{1}{2}\sum\limits_{i=1}^{N}\left |\frac{a_i}{A}-\frac{b_i}{B}  \right |
$$

where $a_i$ represents the population of group $A$ in a given areal unit $i$; $A$ is the total population of that group in the study region (e.g. a metropolitan area); and $b_i$ and $B$ are the equivalent metrics for the second group. The index ranges from a low of 0 to a high of 1, where 0 represents perfect integration between the two groups and 1 represents complete segregation. This index is implemented in the **segregation** package with the `dissimilarity()` function.

The example below computes the dissimilarity index between non-Hispanic white and Hispanic populations for the San Francisco/Oakland urbanized area. The data are filtered for only those rows that represent the target populations in the San Francisco/Oakland area, which is then piped to the `dissimilarity()` function. The function requires identification of a group column, for which we'll use `variable`; a unit column representing the neighborhood unit, for which we'll use `GEOID` to represent the Census tract; and a weight column that tells the function how many people are in each group.


```r
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "San Francisco--Oakland") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )
```

```
##    stat       est
## 1:    D 0.5135526
```

The $D$ index of segregation between non-Hispanic white and Hispanic populations in the San Francisco-Oakland area is 0.51. This statistic, however, is more meaningful in comparison with other cities. To compute dissimilarity for each urban area, we can creatively apply tidyverse techniques covered in earlier chapters and introduce a new function, `group_modify()`, for group-wise calculation. This example follows the recommended workflow [in the **segregation** package documentation](https://elbersb.github.io/segregation/articles/faq.html#how-can-i-compute-indices-for-different-areas-at-once-). The code below filters the data for non-Hispanic white and Hispanic populations by Census tract, then groups the dataset by values in the `urban_name` column. The `group_modify()` function from **dplyr** then allows for the calculation of dissimilarity indices *by group*, which in this example is Census tracts within each respective urban area. It returns a combined dataset that is sorted in descending order with `arrange()` to make comparisons.


```r
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>% 
  arrange(desc(est))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:compare-dissim-show)Dissimilarity indices for Hispanic and non-Hispanic white populations, large California urbanized areas</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> urban_name </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> stat </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> est </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Los Angeles--Long Beach--Anaheim </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.5999229 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Francisco--Oakland </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.5135526 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Jose </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.4935633 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Diego </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.4898184 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Riverside--San Bernardino </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.4079863 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sacramento </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.3687927 </td>
  </tr>
</tbody>
</table>

The Los Angeles area is the most segregated of the large urbanized areas in California with respect to non-Hispanic white and Hispanic populations at the Census tract level, followed by San Francisco/Oakland. Riverside/San Bernardino and Sacramento are the least segregated of the large urban areas in the state.

### Multi-group segregation indices

One disadvantage of the dissimilarity index is that it only measures segregation between two groups. For a state as diverse as California, we may be interested in measuring segregation and diversity between multiple groups at a time. The **segregation** package implements two such indices: the Mutual Information Index $M$, and Theil's Entropy Index $H$ [@mora2011]. Following @elbers2021, $M$ is computed as follows for a dataset $T$:

$$
M(\mathbf{T})=\sum_{u=1}^U\sum_{g=1}^Gp_{ug}\log\frac{p_{ug}}{p_{u}p_{g}}
$$

where $U$ is the total number of units $u$, $G$ is the total number of groups $g$, and $p_{ug}$ is the joint probability of being in unit $u$ and group $g$, with $p_u$ and $p_g$ referring to unit and group probabilities. Theil's $H$ for the same dataset $T$ can then be written as:

$$
H(\mathbf{T})=\frac{M(\mathbf{T})}{E(\mathbf{T})}
$$

where $E(T)$ is the entropy of $T$, normalizing $H$ to range between values of 0 and 1.

Computing these indices is straightforward with the **segregation** package. The `mutual_total()` function computes both indices; when different regions are to be considered (like multiple urban areas, as in this example) the `mutual_within()` function will compute $M$ and $H$ by urban area with the `within` argument appropriately specified. We'll be using the full `ca_urban_data` dataset, which includes population estimates for non-Hispanic white, non-Hispanic Black, non-Hispanic Asian, and Hispanic populations.


```r
mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:multi-entropy-show)Multi-group segregation results for California urban areas</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> urban_name </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> M </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> p </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> H </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> ent_ratio </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Los Angeles--Long Beach--Anaheim </td>
   <td style="text-align:right;"> 0.3391033 </td>
   <td style="text-align:right;"> 0.5016371 </td>
   <td style="text-align:right;"> 0.2851662 </td>
   <td style="text-align:right;"> 0.9693226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Riverside--San Bernardino </td>
   <td style="text-align:right;"> 0.1497129 </td>
   <td style="text-align:right;"> 0.0867808 </td>
   <td style="text-align:right;"> 0.1408461 </td>
   <td style="text-align:right;"> 0.8664604 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sacramento </td>
   <td style="text-align:right;"> 0.1658898 </td>
   <td style="text-align:right;"> 0.0736948 </td>
   <td style="text-align:right;"> 0.1426804 </td>
   <td style="text-align:right;"> 0.9477412 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Diego </td>
   <td style="text-align:right;"> 0.2290891 </td>
   <td style="text-align:right;"> 0.1256072 </td>
   <td style="text-align:right;"> 0.2025728 </td>
   <td style="text-align:right;"> 0.9218445 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Francisco--Oakland </td>
   <td style="text-align:right;"> 0.2685992 </td>
   <td style="text-align:right;"> 0.1394522 </td>
   <td style="text-align:right;"> 0.2116127 </td>
   <td style="text-align:right;"> 1.0346590 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> San Jose </td>
   <td style="text-align:right;"> 0.2147445 </td>
   <td style="text-align:right;"> 0.0728278 </td>
   <td style="text-align:right;"> 0.1829190 </td>
   <td style="text-align:right;"> 0.9569681 </td>
  </tr>
</tbody>
</table>

When multi-group segregation is considered using these indices, Los Angeles remains the most segregated urban area, whereas Riverside/San Bernardino is the least segregated.

The **segregation** package also offers a function for local segregation analysis, `mutual_local()`, which decomposes $M$ into unit-level segregation scores, represented by `ls`. In the example below, we will use `mutual_local()` to examine patterns of segregation across the most segregated urban area, Los Angeles.


```r
la_local_seg <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GEOID </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> ls </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 06037101110 </td>
   <td style="text-align:right;"> 0.2821846 </td>
   <td style="text-align:right;"> 0.0003363 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037101122 </td>
   <td style="text-align:right;"> 0.7790480 </td>
   <td style="text-align:right;"> 0.0002690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037101210 </td>
   <td style="text-align:right;"> 0.1012193 </td>
   <td style="text-align:right;"> 0.0005088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037101220 </td>
   <td style="text-align:right;"> 0.1182334 </td>
   <td style="text-align:right;"> 0.0002917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037101300 </td>
   <td style="text-align:right;"> 0.6538220 </td>
   <td style="text-align:right;"> 0.0003094 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037101400 </td>
   <td style="text-align:right;"> 0.3951408 </td>
   <td style="text-align:right;"> 0.0002655 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037102103 </td>
   <td style="text-align:right;"> 0.3039904 </td>
   <td style="text-align:right;"> 0.0001384 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037102104 </td>
   <td style="text-align:right;"> 0.4462187 </td>
   <td style="text-align:right;"> 0.0002836 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037102105 </td>
   <td style="text-align:right;"> 0.1284913 </td>
   <td style="text-align:right;"> 0.0001496 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 06037102107 </td>
   <td style="text-align:right;"> 0.2389721 </td>
   <td style="text-align:right;"> 0.0003454 </td>
  </tr>
</tbody>
</table>

The results can be mapped by joining the data to a dataset of Census tracts from **tigris**; the `inner_join()` function is used to retain tracts for the Los Angeles area only.


```r
la_tracts_seg <- tracts("CA", cb = TRUE) %>%
  inner_join(la_local_seg, by = "GEOID") 

la_tracts_seg %>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26946) + 
  scale_fill_viridis_c(option = "inferno") + 
  theme_void() + 
  labs(fill = "Local\nsegregation index")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/local-seg-map-1.png" alt="Map of local multi-group segregation scores in Los Angeles" width="100%" />
<p class="caption">(\#fig:local-seg-map)Map of local multi-group segregation scores in Los Angeles</p>
</div>

### Visualizing the diversity gradient

The *diversity gradient* is a concept that uses scatterplot smoothing to visualize how neighborhood diversity varies by distance or travel-time from the core of an urban region [@walker2016b]. Historically, literature on suburbanization in the social sciences assumes a more heterogeneous urban core relative to segregated and homogeneous suburban neighborhoods. The diversity gradient is a visual heuristic used to evaluate the validity of this demographic model.

The entropy index for a given geographic unit is calculated as follows:

$$
E = \sum\limits_{r=1}^{n}Q_rln\frac{1}{Q_r}
$$

$Q_r$ in this calculation represents group $r$'s proportion of the population in the geographic unit.

This statistic is implemented in the `entropy()` function in the **segregation** package. As the `entropy()` function calculates this statistic for a specific unit at a time, we will group the data by tract, and then use `group_modify()` to calculate the entropy for each tract separately. The argument `base = 4` is set by convention to the number of groups in the calculation; this sets the maximum value of the statistic to 1, which represents perfect evenness between the four groups in the area. Once computed, the indices are joined to a dataset of Census tracts from California; `inner_join()` is used to retain only those tracts in the Los Angeles urbanized area.


```r
la_entropy <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
      data = .x,
      group = "variable",
      weight = "estimate",
      base = 4)))

la_entropy_geo <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_entropy, by = "GEOID")
```

Visualization of the diversity gradient then requires a relative measurement of how far each Census tract is from the urban core. The travel-time methods available in the **mapboxapi** package introduced in Chapter \@ref(spatial-analysis-with-us-census-data) are again used here to calculate driving distance to Los Angeles City Hall for all Census tracts in the Los Angeles urbanized area.


```r
library(mapboxapi)

la_city_hall <- mb_geocode("City Hall, Los Angeles CA")

minutes_to_downtown <- mb_matrix(la_entropy_geo, la_city_hall)
```

Once computed, the travel times are stored in a vector `minutes_to_downtown`, then assigned to a new column `minutes` in the entropy data frame. The tract diversity index is visualized using **ggplot2** relative to its travel time to downtown Los Angeles, with a LOESS smoother superimposed over the scatterplot to represent the diversity gradient.


```r
la_entropy_geo$minutes <- as.numeric(minutes_to_downtown)

ggplot(la_entropy_geo, aes(x = minutes_to_downtown, y = entropy)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 80)) + 
  labs(title = "Diversity gradient, Los Angeles urbanized area",
       x = "Travel-time to downtown Los Angeles in minutes, Census tracts",
       y = "Entropy index")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/visualize-diversity-gradient-1.png" alt="Diversity gradient visualization for the Los Angeles, CA urbanized area" width="100%" />
<p class="caption">(\#fig:visualize-diversity-gradient)Diversity gradient visualization for the Los Angeles, CA urbanized area</p>
</div>

The visualization of the diversity gradient shows that neighborhood diversity increases with driving time from the urban core in Los Angeles, peaking at about 35 minutes in free-flowing traffic from the urban core then leveling off after that. The structure of the diversity gradient suggests that Census tracts near to downtown tend to be segregated, and suburban tracts more likely to be integrated.

## Regression modeling with US Census data

*Regression modeling* is widely used in industry and the social sciences to understand social processes. In the social sciences, the goal of regression modeling is commonly to understand the relationships between a variable under study, termed an *outcome variable*, and one or more *predictors* that are believed to have some influence on the outcome variable. Following @james2013c, a model can be represented with the following general notation:

$$
Y = f(X) + \epsilon
$$

where $Y$ represents the outcome variable; $X$ represents one or more predictors hypothesized to have some influence on the outcome variable; $f$ is a function that represents the relationships between $X$ and $Y$; and $\epsilon$ represents the *error terms* or residuals, the differences between the modeled values of $Y$ and the actual values. The function $f$ will be estimated using a method appropriate for the structure of the data and selected by the analyst.

A complete treatment of regression modeling is beyond the scope of this book; recommended resources include @james2013c, @boehmke2019j, @cetinkaya2021, and @matloff2017a. The purpose of this section is to illustrate an example workflow using regression modeling to analyze data from the American Community Survey. The section will start with a simple linear model and extend its discussion from there. In doing so, some problems with the application of the linear model to aggregated Census data will be discussed. First, demographic statistics are often highly correlated with one another, meaning that Census data-based models risk *collinearity* where predictors are not independent of one another. Second, spatial demographic data commonly exhibit *spatial autocorrelation*, which may lead to a violation of the assumption of independent and identically distributed error terms ($i.i.d$) in the linear model. Suggested approaches for addressing these problems discussed in this section include dimension reduction and spatial regression.

### Data setup and exploratory data analysis

The topic of study in this illustrative applied workflow will be median home value by Census tract in the Dallas-Fort Worth metropolitan area. To get started, we'll define several counties in north Texas that we'll use to represent the DFW region, and use a named vector of variables to acquire data that will represent both our outcome variable and predictors. Data are returned from **tidycensus** with the argument `output = "wide"`, giving one column per variable. The geometry is also transformed to an appropriate coordinate reference system for North Texas, EPSG code 32138 (NAD83 / Texas North Central with meters for measurement units).


```r
library(tidycensus)
library(sf)

dfw_counties <- c("Collin County", "Dallas", "Denton", 
                  "Ellis", "Hunt", "Kaufman", "Rockwall", 
                  "Johnson", "Parker", "Tarrant", "Wise")

variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

dfw_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "TX",
  county = dfw_counties,
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(32138) # NAD83 / Texas North Central
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:show-variables)Data acquired from tidycensus for regression modeling</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GEOID </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_valueE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_valueM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_roomsE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_roomsM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> total_populationE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> total_populationM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_ageE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_ageM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_year_builtE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_year_builtM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_incomeE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_incomeM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_collegeE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_collegeM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_foreign_bornE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_foreign_bornM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_whiteE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_whiteM </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> percent_oohE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> percent_oohM </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> geometry </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 48085030101 </td>
   <td style="text-align:right;"> 183600 </td>
   <td style="text-align:right;"> 11112 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 2296 </td>
   <td style="text-align:right;"> 420 </td>
   <td style="text-align:right;"> 31.9 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 1994 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 63036 </td>
   <td style="text-align:right;"> 25030 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 65.4 </td>
   <td style="text-align:right;"> 12.6 </td>
   <td style="text-align:right;"> 85.5 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((787828.3 21... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030102 </td>
   <td style="text-align:right;"> 198500 </td>
   <td style="text-align:right;"> 88036 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 2720 </td>
   <td style="text-align:right;"> 524 </td>
   <td style="text-align:right;"> 45.5 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 1995 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 65234 </td>
   <td style="text-align:right;"> 19074 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 68.8 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 78.6 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((784027.9 21... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030201 </td>
   <td style="text-align:right;"> 324200 </td>
   <td style="text-align:right;"> 47203 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 3653 </td>
   <td style="text-align:right;"> 509 </td>
   <td style="text-align:right;"> 35.1 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 85938 </td>
   <td style="text-align:right;"> 12350 </td>
   <td style="text-align:right;"> 41.3 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 77.0 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 79.2 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((774843.6 21... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030202 </td>
   <td style="text-align:right;"> 366900 </td>
   <td style="text-align:right;"> 20014 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 3530 </td>
   <td style="text-align:right;"> 418 </td>
   <td style="text-align:right;"> 42.1 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 134097 </td>
   <td style="text-align:right;"> 24830 </td>
   <td style="text-align:right;"> 40.9 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 72.5 </td>
   <td style="text-align:right;"> 8.5 </td>
   <td style="text-align:right;"> 88.3 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((764886.2 21... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030204 </td>
   <td style="text-align:right;"> 217200 </td>
   <td style="text-align:right;"> 14152 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 6592 </td>
   <td style="text-align:right;"> 1193 </td>
   <td style="text-align:right;"> 32.7 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 98622 </td>
   <td style="text-align:right;"> 6630 </td>
   <td style="text-align:right;"> 51.8 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 63.0 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 63.6 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((778099.2 21... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030205 </td>
   <td style="text-align:right;"> 231200 </td>
   <td style="text-align:right;"> 41519 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 5257 </td>
   <td style="text-align:right;"> 1273 </td>
   <td style="text-align:right;"> 30.4 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 75382 </td>
   <td style="text-align:right;"> 41296 </td>
   <td style="text-align:right;"> 31.7 </td>
   <td style="text-align:right;"> 12.2 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 45.9 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 82.2 </td>
   <td style="text-align:right;"> 11.7 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((777818.4 21... </td>
  </tr>
</tbody>
</table>

The ACS estimates we've acquired include:

-   `median_valueE`: The median home value of the Census tract (our outcome variable);

-   `median_roomsE`: The median number of rooms for homes in the Census tract;

-   `total_populationE`: The total population;

-   `median_ageE`: The median age of the population in the Census tract;

-   `median_year_builtE`: The median year built of housing structures in the tract;

-   `median_incomeE`: The median income of households in the Census tract;

-   `pct_collegeE`: The percentage of the population age 25 and up with a four-year college degree;

-   `pct_foreign_bornE`: The percentage of the population born outside the United States;

-   `pct_whiteE`: The percentage of the population that identifies as non-Hispanic white;

-   `percent_oohE`: The percentage of housing units in the tract that are owner-occupied.

### Inspecting the outcome variable with visualization

To get started, we will examine both the geographic and data distributions of our outcome variable, median home value, with a quick map with `geom_sf()` and a histogram.


```r
library(tidyverse)
library(patchwork)

mhv_map <- ggplot(dfw_data, aes(fill = median_valueE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median home value ")

mhv_histogram <- ggplot(dfw_data, aes(x = median_valueE)) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::label_number_si(accuracy = 0.1)) + 
  labs(x = "Median home value")

mhv_map + mhv_histogram
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/visualize-home-value-1.png" alt="Median home value charts" width="100%" />
<p class="caption">(\#fig:visualize-home-value)Median home value charts</p>
</div>

As is common with home values in metropolitan regions, the data distribution is right-skewed with a clustering of Census tracts on the lower end of the distribution of values and a long tail of very expensive areas, generally located north of downtown Dallas. This can lead to downstream violations of normality in model residuals. In turn, we might consider log-transforming our outcome variable, which will make its distribution closer to normal and will better capture the geographic variations in home values that we are trying to model.


```r
library(tidyverse)
library(patchwork)

mhv_map_log <- ggplot(dfw_data, aes(fill = log(median_valueE))) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Median home\nvalue (log)")

mhv_histogram_log <- ggplot(dfw_data, aes(x = log(median_valueE))) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous() + 
  labs(x = "Median home value (log)")

mhv_map_log + mhv_histogram_log
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/visualize-logged-home-value-1.png" alt="Logged median home value charts" width="100%" />
<p class="caption">(\#fig:visualize-logged-home-value)Logged median home value charts</p>
</div>

The expensive areas of north Dallas still stand out, but the log-transformation makes the distribution of values more normal and better shows geographic variation of home values on the map. This suggests that we require some data preparation prior to fitting the model.

### "Feature engineering"

A common term used when preparing data for regression modeling is "feature engineering," which refers to the transformation of predictors in ways that better represent the relationships between those predictors and the outcome variable. Many of the variables acquired from the ACS in the steps above are already "pre-engineered" as they were returned as percentages from the ACS data profile, saving some steps. However, some variables would benefit from additional transformation.

The code below creates two new variables: `pop_density`, which represents the number of people in each Census tract per square kilometer, and `median_structure_age`, which represents the median age of housing structures in the tract.


```r
library(sf)
library(units)

dfw_data_for_model <- dfw_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>% 
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()
```

The calculation of the `pop_density` column appears more complicated, so it is helpful to read it from the inside out. The `st_area()` function from the sf package calculates the area of the Census tract; by default this will be in square meters, using the base measurement unit of the data's coordinate reference system. The `total_population` column is then divided by the area of the tract. Next, the `set_units()` function is used to convert the measurement to population per square kilometer using `"1/km2"`. Finally, the calculation is converted from a units vector to a numeric vector with `as.numeric()`. Calculating median structure age is more straightforward, as the `median_year_builtE` column is subtracted from 2017, the mid-point of the 5-year ACS period from which our data are derived. Finally, to simplify the dataset, margin of error columns are dropped, the `E` at the end of the estimate columns is removed with `rename_with()`, and tracts with `NA` values are dropped as well with `na.omit()`.

We can then examine our modified dataset:

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:inspect-dfw)Engineered predictors for regression modeling</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GEOID </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_value </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_rooms </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> total_population </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_age </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_year_built </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_income </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_college </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_foreign_born </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pct_white </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> percent_ooh </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> geometry </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> pop_density </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> median_structure_age </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 48085030101 </td>
   <td style="text-align:right;"> 183600 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 2296 </td>
   <td style="text-align:right;"> 31.9 </td>
   <td style="text-align:right;"> 1994 </td>
   <td style="text-align:right;"> 63036 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 65.4 </td>
   <td style="text-align:right;"> 85.5 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((787828.3 21... </td>
   <td style="text-align:right;"> 19.05013 </td>
   <td style="text-align:right;"> 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030102 </td>
   <td style="text-align:right;"> 198500 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 2720 </td>
   <td style="text-align:right;"> 45.5 </td>
   <td style="text-align:right;"> 1995 </td>
   <td style="text-align:right;"> 65234 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 68.8 </td>
   <td style="text-align:right;"> 78.6 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((784027.9 21... </td>
   <td style="text-align:right;"> 13.43726 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030201 </td>
   <td style="text-align:right;"> 324200 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 3653 </td>
   <td style="text-align:right;"> 35.1 </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> 85938 </td>
   <td style="text-align:right;"> 41.3 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 77.0 </td>
   <td style="text-align:right;"> 79.2 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((774843.6 21... </td>
   <td style="text-align:right;"> 39.45911 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030202 </td>
   <td style="text-align:right;"> 366900 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 3530 </td>
   <td style="text-align:right;"> 42.1 </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 134097 </td>
   <td style="text-align:right;"> 40.9 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 72.5 </td>
   <td style="text-align:right;"> 88.3 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((764886.2 21... </td>
   <td style="text-align:right;"> 22.95549 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48085030204 </td>
   <td style="text-align:right;"> 217200 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 6592 </td>
   <td style="text-align:right;"> 32.7 </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:right;"> 98622 </td>
   <td style="text-align:right;"> 51.8 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 63.0 </td>
   <td style="text-align:right;"> 63.6 </td>
   <td style="text-align:left;"> MULTIPOLYGON (((778099.2 21... </td>
   <td style="text-align:right;"> 417.34182 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
</tbody>
</table>

### A first regression model

After inspecting the distribution of the outcome variable and completing feature engineering with respect to the predictors, we are ready to fit a first linear model. Our linear model with a log-transformed outcome variable can be written as follows:

$$
\begin{align*}\operatorname{log(median\_value)} &= \alpha + \beta_{1}(\operatorname{median\_rooms}) + \beta_{2}(\operatorname{median\_income})\ + \\&\quad \beta_{3}(\operatorname{pct\_college}) + \beta_{4}(\operatorname{pct\_foreign\_born}) + \\&\quad\beta_{5}(\operatorname{pct\_white})\ + \beta_{6}(\operatorname{median\_age}) + \\&\quad\beta_{7}(\operatorname{median\_structure\_age}) + \beta_{8}(\operatorname{percent\_ooh})\ + \\&\quad \beta_{9}(\operatorname{pop\_density}) + \beta_{10}(\operatorname{total\_population}) + \epsilon\end{align*}
$$ where $\alpha$ is the model intercept, $\beta_{1}$ is the change in the log of median home value with a 1-unit increase in the median number of rooms (and so forth for all the model predictors) while holding all other predictors constant, and $\epsilon$ is the error term.

Model formulas in R are generally written as `outcome ~ predictor_1 + predictor_2 + ... + predictor_k`, where `k` is the number of model predictors. The formula can be supplied as a character string to the model function (as shown below) or supplied unquoted in the call to the function. We use `lm()` to fit the linear model, and then check the results with `summary()`.


```r
formula <- "log(median_value) ~ median_rooms + median_income + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model1 <- lm(formula = formula, data = dfw_data_for_model)

summary(model1)
```

```
## 
## Call:
## lm(formula = formula, data = dfw_data_for_model)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.03015 -0.14250  0.00033  0.14794  1.45712 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           1.123e+01  6.199e-02 181.093  < 2e-16 ***
## median_rooms          8.800e-03  1.058e-02   0.832 0.405711    
## median_income         5.007e-06  4.202e-07  11.915  < 2e-16 ***
## pct_college           1.325e-02  5.994e-04  22.108  < 2e-16 ***
## pct_foreign_born      2.877e-03  8.005e-04   3.594 0.000336 ***
## pct_white             3.961e-03  4.735e-04   8.365  < 2e-16 ***
## median_age            4.782e-03  1.372e-03   3.485 0.000507 ***
## median_structure_age  1.202e-05  2.585e-05   0.465 0.642113    
## percent_ooh          -4.761e-03  5.599e-04  -8.504  < 2e-16 ***
## pop_density          -7.946e-06  6.160e-06  -1.290 0.197216    
## total_population      8.960e-06  4.460e-06   2.009 0.044733 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2695 on 1548 degrees of freedom
## Multiple R-squared:  0.7818,	Adjusted R-squared:  0.7804 
## F-statistic: 554.6 on 10 and 1548 DF,  p-value: < 2.2e-16
```

The printed summary gives us information about the model fit. The `Estimate` column represents the model parameters (the $\beta$ values), followed by the standard error, the t-value test statistic, and the *p*-value which helps us assess relative statistical significance. @james2013c (p. 67) provides a concise summary of how *p*-values should be interpreted:

> Roughly speaking, we interpret the *p-*value as follows: a small *p*-value indicates that it is unlikely to observe such a substantial association between the predictor and the response (outcome variable) due to chance, in the absence of any real association between the predictor and the response. Hence, if we see a small *p*-value, then we can infer that there is an association between the predictor and the response. We *reject the null hypothesis* -- that is, we declare a relationship to exist between X and Y -- if the *p-*value is small enough.

By convention, researchers will use *p*-value cutoffs of 0.05, 0.01, or 0.001, depending on the topic under study; these values are highlighted with asterisks in the model summary printout. Examining the model parameters and *p*-values suggests that median household income, bachelor's degree attainment, the percentage non-Hispanic white population, and median age are positively associated with median home values, whereas the percentage owner-occupied housing is negatively associated with median home values. The R-squared value is 0.78, suggesting that our model explains around 78 percent of the variance in `median_value`.

Somewhat surprisingly, `median_rooms` does not appear to have a significant relationship with median home value as per the model. On the one hand, this can be interpreted as "the effect of median rooms on median home value with all other predictors held constant" - but also could be suggestive of model mis-specification. As mentioned earlier, models using ACS data as predictors are highly vulnerable to *collinearity*. Collinearity occurs when two or more predictors are highly correlated with one another, which can lead to misinterpretation of the actual relationships between predictors and the outcome variable.

One way to inspect for collinearity is to visualize the *correlation matrix* of the predictors, in which correlations between all predictors are calculated with one another. The `correlate()` function in the **corrr** package [@kuhn2020] offers a straightforward method for calculating a correlation matrix over a rectangular data frame.


```r
library(corrr)

dfw_estimates <- dfw_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()

correlations <- correlate(dfw_estimates, method = "pearson")
```

One calculated, the correlation matrix can be visualized with `network_plot()`:


```r
network_plot(correlations)
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/network-plot-1.png" alt="Network plot of correlations between model predictors" width="100%" />
<p class="caption">(\#fig:network-plot)Network plot of correlations between model predictors</p>
</div>

We notice that most of the predictors are correlated with one another to some degree, which is unsurprising given that they all represent social demographic data. Collinearity can be diagnosed further by calculating the *variance inflation factor* (VIF) for the model, which takes into account not just pairwise correlations but the extent to which predictors are collinear with all other predictors. A VIF value of 1 indicates no collinearity; VIF values above 5 suggest a level of collinearity that has a problematic influence on model interpretation [@james2013c]. VIF is implemented by the `vif()` function in the **car** package [@fox2019].


```r
library(car)

vif(model1)
```

```
##         median_rooms        median_income          pct_college 
##             5.450436             6.210615             3.722434 
##     pct_foreign_born            pct_white           median_age 
##             2.013411             3.233142             1.833625 
## median_structure_age          percent_ooh          pop_density 
##             1.055760             3.953587             1.537508 
##     total_population 
##             1.174613
```

The most problematic variable is `median_income`, with a VIF value of over 6. A potential solution involves removing this variable and re-running the model; as it is highly correlated with other predictors in the model, the effect of median household income would in theory be captured by the remaining predictors.


```r
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model2 <- lm(formula = formula2, data = dfw_data_for_model)

summary(model2)
```

```
## 
## Call:
## lm(formula = formula2, data = dfw_data_for_model)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.91753 -0.15318 -0.00224  0.16192  1.58948 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           1.101e+01  6.203e-02 177.566  < 2e-16 ***
## median_rooms          7.326e-02  9.497e-03   7.713 2.18e-14 ***
## pct_college           1.775e-02  4.862e-04  36.506  < 2e-16 ***
## pct_foreign_born      4.170e-03  8.284e-04   5.034 5.38e-07 ***
## pct_white             4.996e-03  4.862e-04  10.274  < 2e-16 ***
## median_age            3.527e-03  1.429e-03   2.468   0.0137 *  
## median_structure_age  2.831e-05  2.696e-05   1.050   0.2939    
## percent_ooh          -3.888e-03  5.798e-04  -6.705 2.81e-11 ***
## pop_density          -5.474e-06  6.430e-06  -0.851   0.3947    
## total_population      9.711e-06  4.658e-06   2.085   0.0373 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2815 on 1549 degrees of freedom
## Multiple R-squared:  0.7618,	Adjusted R-squared:  0.7604 
## F-statistic: 550.4 on 9 and 1549 DF,  p-value: < 2.2e-16
```

The model R-squared drops slightly but not substantially to 0.76. Notably, the effect of `median_rooms` on median home value now comes through as strongly positive and statistically significant, suggesting that collinearity with median household income was suppressing this relationship in the first model. As a diagnostic, we can re-compute the VIF for the second model:


```r
vif(model2)
```

```
##         median_rooms          pct_college     pct_foreign_born 
##             4.025411             2.245227             1.976425 
##            pct_white           median_age median_structure_age 
##             3.124400             1.822825             1.052805 
##          percent_ooh          pop_density     total_population 
##             3.885779             1.535763             1.174378
```

The VIF values for all predictors in the model are now below 5.

### Dimension reduction with principal components analysis

In the example above, dropping median household income from the model had a fairly negligible impact on the overall model fit and significantly improved the model's problems with collinearity. However, this will not always be the best solution for analysts, especially when dropping variables has a more significant impact on the model fit. An alternative approach to resolving problems with collinearity is *dimension reduction*, which transforms the predictors into a series of dimensions that represent the variance in the predictors but are uncorrelated with one another. Dimension reduction is also a useful technique when an analyst is dealing with a massive number of predictors (hundreds or even thousands) and needs to reduce the predictors in the model to a more manageable number while still retaining the ability to explain the variance in the outcome variable.

One of the most popular methods for dimension reduction is *principal components analysis*. Principal components analysis (PCA) reduces a higher-dimensional dataset into a lower-dimensional representation based linear combinations of the variables used. The *first principal component* is the linear combination of variables that explains the most overall variance in the data; the *second principal component* explains the second-most overall variance but is also constrained to be uncorrelated with the first component; and so forth.

PCA can be computed with the `prcomp()` function. We will use the `dfw_estimates` object that we used to compute the correlation data frame here as it includes only the predictors in the regression model, and use the notation `formula = ~.` to compute PCA over all the predictors. By convention, `scale.` and `center` should be set to `TRUE` as this normalizes the variables in the dataset before computing PCA given that they are measured differently.


```r
pca <- prcomp(
  formula = ~., 
  data = dfw_estimates, 
  scale. = TRUE, 
  center = TRUE
)

summary(pca)
```

```
## Importance of components:
##                          PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     2.020 1.1832 1.1307 1.0093 0.89917 0.70312 0.67686
## Proportion of Variance 0.408 0.1400 0.1278 0.1019 0.08085 0.04944 0.04581
## Cumulative Proportion  0.408 0.5481 0.6759 0.7778 0.85860 0.90803 0.95385
##                            PC8     PC9    PC10
## Standard deviation     0.48099 0.36127 0.31567
## Proportion of Variance 0.02314 0.01305 0.00997
## Cumulative Proportion  0.97698 0.99003 1.00000
```

Printing the `summary()` of the PCA model shows 10 components that collectively explain 100% of the variance in the original predictors. The first principal component explains 40.8 percent of the overall variance; the second explains 14 percent; and so forth.

To understand what the different principal components now mean, it is helpful to plot the variable *loadings*. This represents the relationships between the original variables in the model and the derived components. This approach is derived from Julia Silge's blog post on the topic [@silge2020].

First, the variable loading matrix (stored in the `rotation` element of the `pca` object) is converted to a tibble so we can view it easier.


```r
pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:pca-tibble-show)PCA variable loadings</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> predictor </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC1 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC2 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC3 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC4 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC5 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC6 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC7 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC8 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC9 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> PC10 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> median_rooms </td>
   <td style="text-align:right;"> -0.4077852 </td>
   <td style="text-align:right;"> 0.1323021 </td>
   <td style="text-align:right;"> -0.3487253 </td>
   <td style="text-align:right;"> 0.0586440 </td>
   <td style="text-align:right;"> -0.2076430 </td>
   <td style="text-align:right;"> 0.2123836 </td>
   <td style="text-align:right;"> -0.1609126 </td>
   <td style="text-align:right;"> -0.2069992 </td>
   <td style="text-align:right;"> 0.4346406 </td>
   <td style="text-align:right;"> 0.5876075 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_population </td>
   <td style="text-align:right;"> -0.0032288 </td>
   <td style="text-align:right;"> 0.4604789 </td>
   <td style="text-align:right;"> -0.5213433 </td>
   <td style="text-align:right;"> -0.1272021 </td>
   <td style="text-align:right;"> 0.5458555 </td>
   <td style="text-align:right;"> -0.4347310 </td>
   <td style="text-align:right;"> 0.0936404 </td>
   <td style="text-align:right;"> -0.0506824 </td>
   <td style="text-align:right;"> -0.0371959 </td>
   <td style="text-align:right;"> -0.0173050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> median_age </td>
   <td style="text-align:right;"> -0.3474694 </td>
   <td style="text-align:right;"> -0.1380008 </td>
   <td style="text-align:right;"> 0.2393805 </td>
   <td style="text-align:right;"> -0.0781325 </td>
   <td style="text-align:right;"> -0.2960241 </td>
   <td style="text-align:right;"> -0.7849378 </td>
   <td style="text-align:right;"> -0.1486493 </td>
   <td style="text-align:right;"> -0.2533379 </td>
   <td style="text-align:right;"> 0.0605217 </td>
   <td style="text-align:right;"> -0.0558195 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> median_income </td>
   <td style="text-align:right;"> -0.4149988 </td>
   <td style="text-align:right;"> -0.2277427 </td>
   <td style="text-align:right;"> -0.3213461 </td>
   <td style="text-align:right;"> -0.0367450 </td>
   <td style="text-align:right;"> 0.0067868 </td>
   <td style="text-align:right;"> 0.2005786 </td>
   <td style="text-align:right;"> 0.1163622 </td>
   <td style="text-align:right;"> -0.0635095 </td>
   <td style="text-align:right;"> 0.3147651 </td>
   <td style="text-align:right;"> -0.7171939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pct_college </td>
   <td style="text-align:right;"> -0.3116212 </td>
   <td style="text-align:right;"> -0.5358812 </td>
   <td style="text-align:right;"> -0.1567328 </td>
   <td style="text-align:right;"> -0.1562603 </td>
   <td style="text-align:right;"> 0.2061776 </td>
   <td style="text-align:right;"> 0.0588802 </td>
   <td style="text-align:right;"> 0.3496196 </td>
   <td style="text-align:right;"> -0.1984163 </td>
   <td style="text-align:right;"> -0.5235463 </td>
   <td style="text-align:right;"> 0.2916531 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pct_foreign_born </td>
   <td style="text-align:right;"> 0.2812618 </td>
   <td style="text-align:right;"> -0.2409164 </td>
   <td style="text-align:right;"> -0.4667693 </td>
   <td style="text-align:right;"> 0.0769341 </td>
   <td style="text-align:right;"> -0.4514510 </td>
   <td style="text-align:right;"> -0.2779627 </td>
   <td style="text-align:right;"> 0.3495310 </td>
   <td style="text-align:right;"> 0.4664939 </td>
   <td style="text-align:right;"> 0.0962084 </td>
   <td style="text-align:right;"> 0.0945663 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pct_white </td>
   <td style="text-align:right;"> -0.3910590 </td>
   <td style="text-align:right;"> -0.0924262 </td>
   <td style="text-align:right;"> 0.2855863 </td>
   <td style="text-align:right;"> -0.1708498 </td>
   <td style="text-align:right;"> 0.3472563 </td>
   <td style="text-align:right;"> -0.0631469 </td>
   <td style="text-align:right;"> -0.0042362 </td>
   <td style="text-align:right;"> 0.7224557 </td>
   <td style="text-align:right;"> 0.2354127 </td>
   <td style="text-align:right;"> 0.1607084 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> percent_ooh </td>
   <td style="text-align:right;"> -0.3812989 </td>
   <td style="text-align:right;"> 0.3031944 </td>
   <td style="text-align:right;"> -0.1930025 </td>
   <td style="text-align:right;"> 0.1680217 </td>
   <td style="text-align:right;"> -0.3006266 </td>
   <td style="text-align:right;"> 0.0753161 </td>
   <td style="text-align:right;"> -0.3374642 </td>
   <td style="text-align:right;"> 0.3233115 </td>
   <td style="text-align:right;"> -0.6051618 </td>
   <td style="text-align:right;"> -0.1285341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pop_density </td>
   <td style="text-align:right;"> 0.2571344 </td>
   <td style="text-align:right;"> -0.4253855 </td>
   <td style="text-align:right;"> -0.2876627 </td>
   <td style="text-align:right;"> -0.3247835 </td>
   <td style="text-align:right;"> 0.1261611 </td>
   <td style="text-align:right;"> -0.0196740 </td>
   <td style="text-align:right;"> -0.7385667 </td>
   <td style="text-align:right;"> 0.0538260 </td>
   <td style="text-align:right;"> 0.0011908 </td>
   <td style="text-align:right;"> 0.0034392 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> median_structure_age </td>
   <td style="text-align:right;"> -0.0095808 </td>
   <td style="text-align:right;"> -0.2700238 </td>
   <td style="text-align:right;"> -0.0450257 </td>
   <td style="text-align:right;"> 0.8829973 </td>
   <td style="text-align:right;"> 0.3131336 </td>
   <td style="text-align:right;"> -0.1369883 </td>
   <td style="text-align:right;"> -0.1609821 </td>
   <td style="text-align:right;"> 0.0036929 </td>
   <td style="text-align:right;"> 0.0443381 </td>
   <td style="text-align:right;"> 0.0238885 </td>
  </tr>
</tbody>
</table>

Positive values for a given row mean that the original variable is *positively loaded* onto a given component, and negative values mean that the variable is *negatively loaded*. Larger values in each direction are of the most interest to us; values near 0 mean the variable is not meaningfully explained by a given component. To explore this further, we can visualize the first five components with **ggplot2**:


```r
pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) + 
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) + 
  facet_wrap(~component, nrow = 1) + 
  labs(y = NULL, x = "Value") + 
  theme_minimal()
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/pca-tibble-plot-1.png" alt="Loadings for first five principal components" width="100%" />
<p class="caption">(\#fig:pca-tibble-plot)Loadings for first five principal components</p>
</div>

With respect to PC1, which explains nearly 41 percent of the variance in the overall predictor set, the variables `percent_ooh`, `pct_white`, `pct_college`, `median_rooms`, `median_income`, and `median_age` load negatively, whereas `pop_density` and `pct_foreign_born` load positively. We can attach these principal components to our original data with `predict()` and `cbind()`, then make a map of PC1 for further exploration:


```r
components <- predict(pca, dfw_estimates)

dfw_pca <- dfw_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components) 

ggplot(dfw_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/pca-model-1.png" alt="Map of principal component 1" width="100%" />
<p class="caption">(\#fig:pca-model)Map of principal component 1</p>
</div>

The map, along with the bar chart, helps us understand how the multiple variables represent latent social processes at play in Dallas-Fort Worth. The brighter yellow areas, which have higher values for PC1, are located in communities like east Fort Worth, east Arlington, Grand Prairie, and south Dallas. Generally speaking, these are low-to-middle income areas with larger nonwhite populations. The locations with the lowest values for PC1 are Southlake (northeast of Fort Worth) and Highland Park (north of downtown Dallas); these communities are segregated, predominantly non-Hispanic white, and are among the wealthiest neighborhoods in the entire United States. In turn, PC1 captures the gradient that represents these social differences, with which multiple demographic characteristics will be associated.

These principal components can be used for *principal components regression*, in which the derived components themselves are used as model predictors. Generally, components should be chosen that account for at least 90 percent of the original variance in the predictors, though this will often be up to the discretion of the analyst. In the example below, we will fit a model using the first six principal components and the log of median home value as the outcome variable.


```r
pca_formula <- paste0("log(median_value) ~ ", 
                      paste0('PC', 1:6, collapse = ' + '))

pca_model <- lm(formula = pca_formula, data = dfw_pca)

summary(pca_model)
```

```
## 
## Call:
## lm(formula = pca_formula, data = dfw_pca)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.78888 -0.16854 -0.00726  0.16941  1.60089 
## 
## Coefficients:
##              Estimate Std. Error  t value Pr(>|t|)    
## (Intercept) 12.301439   0.007483 1643.902   <2e-16 ***
## PC1         -0.180706   0.003706  -48.765   <2e-16 ***
## PC2         -0.247181   0.006326  -39.072   <2e-16 ***
## PC3         -0.077097   0.006621  -11.645   <2e-16 ***
## PC4         -0.084417   0.007417  -11.382   <2e-16 ***
## PC5          0.111525   0.008325   13.397   <2e-16 ***
## PC6          0.003787   0.010646    0.356    0.722    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2955 on 1552 degrees of freedom
## Multiple R-squared:  0.737,	Adjusted R-squared:  0.736 
## F-statistic: 724.9 on 6 and 1552 DF,  p-value: < 2.2e-16
```

The model fit, as represented by the R-squared value, is similar to the models fit earlier in this chapter. One possible disadvantage of principal components regression, however, is the interpretation of the results as the different variables which are comprehensible on their own are now spread across the components. It can be helpful to think of the different components as *indices* in this sense.

As discussed above, PC1 represents a gradient from segregated, older, wealthy, white communities on the low end to more diverse, lower-income, and younger communities on the high end; this PC is negatively associated with median home values, with tracks with expectations. Reviewing the plot above, PC2 is associated with lower population densities and levels of educational attainment; in turn, it can be thought of as an urban (on the low end) to rural (on the high end) gradient. A negative association with median home value is then expected as home values are higher in the urban core than on the rural fringe of the metropolitan area.

## Spatial regression

A core assumption of the linear model is that the errors are independent of one another and normally distributed. Log-transforming the right-skewed outcome variable, median home values, was indented to resolve the latter; we can check this by adding the residuals for `model2` to our dataset and drawing a histogram to check its distribution.


```r
dfw_data_for_model$residuals <- residuals(model2)

ggplot(dfw_data_for_model, aes(x = residuals)) + 
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") + 
  theme_minimal()
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/residuals-1.png" alt="Distribution of model residuals with a ggplot2 histogram" width="100%" />
<p class="caption">(\#fig:residuals)Distribution of model residuals with a ggplot2 histogram</p>
</div>

The former assumption of independence of residuals is commonly violated in models that use spatial data, however. This is because models of spatial processes commonly are characterized by *spatial autocorrelation* in the error term, meaning that the model's performance itself depends on geographic location. We can assess this using techniques learned in the previous chapter such as Moran's $I$.


```r
library(spdep)

wts <- dfw_data_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(dfw_data_for_model$residuals, wts)
```

```
## 
## 	Moran I test under randomisation
## 
## data:  dfw_data_for_model$residuals  
## weights: wts    
## 
## Moran I statistic standard deviate = 14.023, p-value < 2.2e-16
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##      0.2101748515     -0.0006418485      0.0002259981
```

The Moran's $I$ test statistic is modest and positive (0.21) but is statistically significant. This can be visualized with a Moran scatterplot:


```r
dfw_data_for_model$lagged_residuals <- lag.listw(wts, dfw_data_for_model$residuals)

ggplot(dfw_data_for_model, aes(x = residuals, y = lagged_residuals)) + 
  theme_minimal() + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/residual-scatterplot-1.png" alt="Moran scatterplot of residual spatial autocorrelation" width="100%" />
<p class="caption">(\#fig:residual-scatterplot)Moran scatterplot of residual spatial autocorrelation</p>
</div>

The plot illustrates the positive spatial autocorrelation in the residuals, suggesting that the assumption of independence in the model error term is violated. To resolve this issue, we can turn to *spatial regression methods*.

### Methods for spatial regression

The field of *spatial econometrics* is broadly concerned with the estimation and specification of models that are appropriate for handling spatial dependence in statistical processes. In general, two families of models are used to address these concerns with respect to regression: *spatial lag models* and *spatial error models*.

#### Spatial lag models

Spatial lag models account for spatial dependence by including a spatial lag of the outcome variable in the model. In doing so, it accounts for *spatial spillover effects* -- the possibility that values in neighboring areas have an influence on values in a given location. A spatial lag model can be written as follows [@rey2020]:

$$
{Y_i} = \alpha + \rho{Y_{lag-i}} + \sum_k \beta_k X_{ki} + \epsilon_i, 
$$

where

$$
Y_{lag-i} = \sum\limits_{j}w_{ij}Y_j
$$

with $w_{ij}$ representing the spatial weights. In this notation, $\rho$ is the parameter measuring the effect of the spatial lag in the outcome variable, and $k$ is the number of predictors in the model. However, the inclusion of a spatially lagged outcome variable on the right-hand side of the equation violates the exogeneity assumption of the linear model. In turn, special methods are required for estimating the spatial lag model, which are implemented in R in the **spatialreg** package [@applied2013]. Below, we use the function `lagsarlm()` to estimate the relationship between logged median home value and its predictors as a spatial lag model:


```r
library(spatialreg)

lag_model <- lagsarlm(
  formula = formula2, 
  data = dfw_data_for_model, 
  listw = wts
)

summary(lag_model, Nagelkerke = TRUE)
```

```
## 
## Call:lagsarlm(formula = formula2, data = dfw_data_for_model, listw = wts)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -2.0647421 -0.1377312 -0.0032552  0.1386914  1.4820482 
## 
## Type: lag 
## Coefficients: (asymptotic standard errors) 
##                         Estimate  Std. Error z value  Pr(>|z|)
## (Intercept)           7.0184e+00  2.6898e-01 26.0927 < 2.2e-16
## median_rooms          6.2027e-02  8.8554e-03  7.0045 2.480e-12
## pct_college           1.2858e-02  5.4696e-04 23.5083 < 2.2e-16
## pct_foreign_born      2.0118e-03  7.7482e-04  2.5964  0.009420
## pct_white             2.7112e-03  4.7183e-04  5.7461 9.133e-09
## median_age            3.4421e-03  1.3163e-03  2.6150  0.008922
## median_structure_age  2.6093e-05  2.4827e-05  1.0510  0.293267
## percent_ooh          -3.0428e-03  5.4316e-04 -5.6021 2.118e-08
## pop_density          -1.3573e-05  5.9323e-06 -2.2879  0.022143
## total_population      8.3762e-06  4.2928e-06  1.9512  0.051031
## 
## Rho: 0.35319, LR test value: 210.86, p-value: < 2.22e-16
## Asymptotic standard error: 0.023376
##     z-value: 15.109, p-value: < 2.22e-16
## Wald statistic: 228.29, p-value: < 2.22e-16
## 
## Log likelihood: -125.2882 for lag model
## ML residual variance (sigma squared): 0.067179, (sigma: 0.25919)
## Nagelkerke pseudo-R-squared: 0.79193 
## Number of observations: 1559 
## Number of parameters estimated: 12 
## AIC: 274.58, (AIC for lm: 483.43)
## LM test for residual autocorrelation
## test value: 6.9225, p-value: 0.0085118
```

The general statistical relationships observed in the non-spatial model are preserved in the spatial lag model, though the effect sizes (the model parameters) are smaller, illustrating the importance of controlling for the spatial lag. Additionally, the $\rho$ parameter is positive and statistically significant, suggesting the presence of spatial spillover effects. This finding makes practical sense, as median home values may be influenced by the values of homes in neighboring Census tracts along with the characteristics of the neighborhood itself. The argument `Nagelkerke = TRUE` computes a pseudo-R-squared value, which is slightly higher than the corresponding value for the non-spatial model.

#### Spatial error models

In contrast with spatial lag models, spatial error models include a spatial lag in model's error term. This is designed to capture latent spatial processes that are not currently being accounted for in the model estimation and in turn show up in the model's residuals. The spatial error model can be written as follows:

$$
Y_i = \alpha + \sum\limits_k\beta_kX_{ki} + u_i,
$$

where

$$
u_i = \lambda u_{lag-i} + \epsilon_i 
$$

and

$$
u_{lag-i} = \sum\limits_jw_{ij}u_j
$$

Like the spatial lag model, estimating the spatial error model requires special methods, implemented with the `errorsarlm()` function in **spatialreg**.


```r
error_model <- errorsarlm(
  formula = formula2, 
  data = dfw_data_for_model, 
  listw = wts
)

summary(error_model, Nagelkerke = TRUE)
```

```
## 
## Call:errorsarlm(formula = formula2, data = dfw_data_for_model, listw = wts)
## 
## Residuals:
##         Min          1Q      Median          3Q         Max 
## -1.97990245 -0.13702534 -0.00030105  0.13933507  1.54937871 
## 
## Type: error 
## Coefficients: (asymptotic standard errors) 
##                         Estimate  Std. Error  z value  Pr(>|z|)
## (Intercept)           1.1098e+01  6.6705e-02 166.3753 < 2.2e-16
## median_rooms          8.2815e-02  9.7089e-03   8.5298 < 2.2e-16
## pct_college           1.5857e-02  5.7427e-04  27.6120 < 2.2e-16
## pct_foreign_born      3.6601e-03  9.6570e-04   3.7901 0.0001506
## pct_white             4.6754e-03  6.1175e-04   7.6426 2.132e-14
## median_age            3.9346e-03  1.4130e-03   2.7845 0.0053605
## median_structure_age  2.6093e-05  2.5448e-05   1.0254 0.3051925
## percent_ooh          -4.7538e-03  5.6726e-04  -8.3803 < 2.2e-16
## pop_density          -1.4999e-05  6.8731e-06  -2.1823 0.0290853
## total_population      1.0497e-05  4.4668e-06   2.3499 0.0187796
## 
## Lambda: 0.46765, LR test value: 164.17, p-value: < 2.22e-16
## Asymptotic standard error: 0.031997
##     z-value: 14.615, p-value: < 2.22e-16
## Wald statistic: 213.61, p-value: < 2.22e-16
## 
## Log likelihood: -148.6309 for error model
## ML residual variance (sigma squared): 0.067878, (sigma: 0.26053)
## Nagelkerke pseudo-R-squared: 0.7856 
## Number of observations: 1559 
## Number of parameters estimated: 12 
## AIC: 321.26, (AIC for lm: 483.43)
```

The $\lambda$ (lambda) value is large and statistically significant, again illustrating the importance of accounting for spatial autocorrelation in the model.

### Choosing between spatial lag and spatial error models

The spatial lag and spatial error models offer alternative approaches to accounting for processes of spatial autocorrelation when fitting models. This raises the question: which one of the two models should the analyst choose? On the one hand, this should be thought through in the context of the topic under study. For example, if spatial spillover effects are related to the hypotheses being evaluated by the analysts (e.g. the effect of neighboring home values on focal home values), a spatial lag model may be preferable; alternatively, if there are spatially autocorrelated factors that likely influence the outcome variable but are difficult to measure quantitatively (e.g. discrimination and racial bias in the housing market), a spatial error model might be preferred.

The two types of models can also be evaluated with respect to some quantitative metrics. For example, we can re-compute Moran's $I$ over the model residuals to see if the spatial model has resolved our problems with spatial dependence. First, we'll check the spatial lag model:


```r
moran.test(lag_model$residuals, wts)
```

```
## 
## 	Moran I test under randomisation
## 
## data:  lag_model$residuals  
## weights: wts    
## 
## Moran I statistic standard deviate = 2.0436, p-value = 0.0205
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##      0.0300648348     -0.0006418485      0.0002257748
```

Next, the spatial error model:


```r
moran.test(error_model$residuals, wts)
```

```
## 
## 	Moran I test under randomisation
## 
## data:  error_model$residuals  
## weights: wts    
## 
## Moran I statistic standard deviate = -1.6126, p-value = 0.9466
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##     -0.0248732656     -0.0006418485      0.0002257849
```

Both models reduce Moran's $I$; however, the error model does a better job of eliminating spatial autocorrelation in the residuals entirely. We can also use *Lagrange multiplier tests* to evaluate the appropriateness of these models together [@anselin1996]. These tests check for spatial error dependence, whether a spatially lagged dependent variable is missing, and the robustness of each in the presence of the other.

The `lm.LMtests()` function can be used with an input linear model to compute these tests. We'll use `model2`, the home value model with median household income omitted, to compute the tests.


```r
lm.LMtests(
  model2, 
  wts, 
  test = c("LMerr", "LMlag", "RLMerr", "RLMlag")
)
```

```
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = formula2, data = dfw_data_for_model)
## weights: wts
## 
## LMerr = 194.16, df = 1, p-value < 2.2e-16
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = formula2, data = dfw_data_for_model)
## weights: wts
## 
## LMlag = 223.37, df = 1, p-value < 2.2e-16
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = formula2, data = dfw_data_for_model)
## weights: wts
## 
## RLMerr = 33.063, df = 1, p-value = 8.921e-09
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = formula2, data = dfw_data_for_model)
## weights: wts
## 
## RLMlag = 62.276, df = 1, p-value = 2.998e-15
```

All test statistics are large and statistically significant; in this case, the robust versions of the statistics should be compared. While both the lag and error models would be appropriate for this data, the test statistic for the robust version of the lag model is larger, suggesting that the spatial lag model should be preferred over the spatial error model in this example.

## Geographically weighted regression

The models addressed in the previous sections -- both the regular linear model and its spatial adaptations -- estimate *global* relationships between the outcome variable, median home values, and its predictors. This lends itself to conclusions like "In the Dallas-Fort Worth metropolitan area, higher levels of educational attainment are associated with higher median home values." However, metropolitan regions like Dallas-Fort Worth are diverse and multifaceted. It is possible that a relationship between a predictor and the outcome variable that is observed for the entire region *on average* may vary significantly from neighborhood to neighborhood. This type of phenomenon is called *spatial non-stationarity*, and can be explored with *geographically weighted regression*, or GWR [@brunsdon1996].

GWR is a technique designed to evaluate local variations in the results of regression models given a kernel (distance-decay) weighting function. Following @lu2014, the basic form of GWR for a given location $i$ can be written as:

$$
Y_i = \alpha_i + \sum\limits_{k=1}^m\beta_{ik}X_{ik} + \epsilon_i
$$

where the model intercept, parameters, and error term are all location-specific. Notably, $\beta_{ik}$ represents a *local regression coefficient* for predictor $k$ (of the total number of predictors $m$) that is specific to location $i$.

GWR is implemented in the **GWmodel** R package [@gollini2015] as well as the spgwr package [@bivand_gwr]. These packages offer an interface to a wider family of geographically weighted methods, such as binomial, generalized linear, and robust geographically weighted regression; geographically weighted PCA; and geographically weighted summary statistics. The example below will adapt the regression model used in earlier examples to a locally-variation model with GWR.

### Choosing a bandwidth for GWR

GWR relies on the concept of a "kernel bandwidth" to compute the local regression model for each location. A kernel bandwidth is based on the kernel type (fixed or adaptive) and a distance-decay function. A fixed kernel uses a cutoff distance to determine which observations will be included in the local model for a given location $i$, whereas an adaptive kernel uses the nearest neighbors to a given location. In most circumstances with Census tract data where the size of tracts in a region will vary widely, an adaptive kernel will be preferred to a fixed kernel to ensure consistency of neighborhoods across the region. The distance-decay function then governs how observations will be weighted in the local model relative to their distance from location $i$. Closer tracts to $i$ will have a greater influence on the results for location $i$, with influence falling with distance.

Bandwidth sizes (either a distance cutoff or number of nearest neighbors) can be selected directly by the user; in the **GWmodel** R package, the `bw.gwr()` function also helps analysts choose an appropriate kernel bandwidth using cross-validation. The code below computes a bandwidth `bw` using this method. Note that we must first convert our data to a legacy `SpatialPolygonsDataFrame` object from the sp package as **GWmodel** does not yet support sf objects.


```r
library(GWmodel)
library(sf)

dfw_data_sp <- dfw_data_for_model %>%
  as_Spatial()

bw <- bw.gwr(
  formula = formula2, 
  data = dfw_data_sp, 
  kernel = "bisquare",
  adaptive = TRUE
)
```

`bw.gwr()` chose 187 as the number of nearest neighbors based on cross-validation. This means that for each Census tract, the nearest 187 of the total 1559 Census tracts in the Dallas-Fort Worth region will be used to estimate the local model, with weights calculated using the bisquare distance-decay function as follows:

$$
w_{ij} = 1-(\frac{d_{ij}^2}{h^2})^2
$$

where $d_{ij}$ is the distance between local observation $i$ and neighbor $j$, and $h$ is the kernel bandwidth. As we are using an adaptive kernel, $h$ will vary for each observation and take on the distance between location $i$ and its "neighbor" furthest from that location.

### Fitting and evaluating the GWR model

The basic form of GWR can be fit with the `gwr.basic()` function, which uses a similar argument structure to other models fit in this chapter. The formula can be passed to the `formula` parameter as a character string; we'll use the original formula with median household income omitted and include it below for a refresher. The derived bandwidth from `bw.gwr()` will be used with a bisquare, adaptive kernel.


```r
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

gw_model <- gwr.basic(
  formula = formula2, 
  data = dfw_data_sp, 
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)
```

Printing the object `gw_model` will show both the results of the global model and the ranges of the locally varying parameter estimates. The model object itself has the following elements:


```r
names(gw_model)
```

```
## [1] "GW.arguments"  "GW.diagnostic" "lm"            "SDF"          
## [5] "timings"       "this.call"     "Ftests"
```

Each element provides information about the model fit, but perhaps most interesting to the analyst is the `SDF` element, in this case a `SpatialPolygonsDataFrame` containing mappable model results. We will extract that element from the model object and convert it to simple features, then take a look at the columns in the object.


```r
gw_model_results <- gw_model$SDF %>%
  st_as_sf() 

names(gw_model_results)
```

```
##  [1] "Intercept"               "median_rooms"           
##  [3] "pct_college"             "pct_foreign_born"       
##  [5] "pct_white"               "median_age"             
##  [7] "median_structure_age"    "percent_ooh"            
##  [9] "pop_density"             "total_population"       
## [11] "y"                       "yhat"                   
## [13] "residual"                "CV_Score"               
## [15] "Stud_residual"           "Intercept_SE"           
## [17] "median_rooms_SE"         "pct_college_SE"         
## [19] "pct_foreign_born_SE"     "pct_white_SE"           
## [21] "median_age_SE"           "median_structure_age_SE"
## [23] "percent_ooh_SE"          "pop_density_SE"         
## [25] "total_population_SE"     "Intercept_TV"           
## [27] "median_rooms_TV"         "pct_college_TV"         
## [29] "pct_foreign_born_TV"     "pct_white_TV"           
## [31] "median_age_TV"           "median_structure_age_TV"
## [33] "percent_ooh_TV"          "pop_density_TV"         
## [35] "total_population_TV"     "Local_R2"               
## [37] "geometry"
```

The sf object includes columns with local parameter estimates, standard errors, and t-values for each predictor, along with local diagnostic elements such as the Local R-squared, giving information about how well a model performs in any particular location. Below, we use ggplot2 and `geom_sf()` to map the local R-squared, though it may be useful as well to use `mapview::mapview(gw_model_results, zcol = "Local_R2"` and explore these results interactively.


```r
ggplot(gw_model_results, aes(fill = Local_R2)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void()
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/map-local-r2-1.png" alt="Local R-squared values from the GWR model" width="100%" />
<p class="caption">(\#fig:map-local-r2)Local R-squared values from the GWR model</p>
</div>

The map suggests that the model performs very well in Fort Worth, Collin County, and the eastern edge of the metropolitan area, with local R-squared values exceeding 0.9. It performs worse in northwestern Denton County and in other more rural areas to the west of Fort Worth.

We can examine locally varying parameter estimates in much the same way. The first map below visualizes the local relationships between the percentage owner-occupied housing and median home values. Recall from the global model that this coefficient was negative and statistically significant.


```r
ggplot(gw_model_results, aes(fill = percent_ooh)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Local β for \npercent_ooh")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/map-ooh-betas-1.png" alt="Local parameter estimates for percent owner-occupied housing" width="100%" />
<p class="caption">(\#fig:map-ooh-betas)Local parameter estimates for percent owner-occupied housing</p>
</div>

The dark purple areas on the map are those areas where the global relationship in the model reflects the local relationship, as local parameter estimates are negative. The areas that stick out include the high-density area of uptown Dallas, where renter-occupied housing is common and median home values are very high. However, in rural areas on the fringe of the metropolitan area this relationship reverses, returning in some cases positive parameter estimates (the yellow parts of the map). This means that for those local areas, a greater percentage of owner-occupied housing is associated with higher home values.

We can explore this further by investigating the local parameter estimates for population density, which was not significant in the global model:


```r
ggplot(gw_model_results, aes(fill = pop_density)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Local β for \npopulation density")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/map-structure-age-betas-1.png" alt="Local parameter estimates for median structure age" width="100%" />
<p class="caption">(\#fig:map-structure-age-betas)Local parameter estimates for median structure age</p>
</div>

For large portions of the metropolitan area, the relationship between population density and median home values is negligible, which is what drives the global relationship. However, we do observe variations in different parts of the metropolitan area. Bright yellow locations are those where high population densities are associated with higher home values. Conversely, the darker blue and purple areas represent several affluent enclaves or suburbs of Dallas and Fort Worth where lower densities are associated with higher home values.

### Limitations of GWR

While GWR is an excellent method for exploring spatial non-stationarity in regression model results, it does have some limitations. When the **spgwr** package itself *is loaded into your R environment*, it prints the following warning:

> NOTE: This package does not constitute approval of GWR as a method of spatial analysis.

Why would an R package itself warn the user about its use? GWR is particularly susceptible to problems that plague other regression models using spatial data. Earlier sections in this chapter covered the topic of *collinearity*, where parameter estimates are biased due to high correlations between predictors. Given that predictor values tend to cluster spatially, GWR models often suffer from *local multicollinearity* where predictors are highly correlated in local areas.

Additionally, the impact of *edge effects* can be acute in GWR models. Edge effects - which are present in most spatial models and analysis techniques - refer to misleading results for observations on the edge of a dataset. In the example used in this chapter, the Dallas-Fort Worth metropolitan area represents the region under study. This artificially restricts the neighborhoods around Census tracts on the edge of the metropolitan area to only those tracts *that are also within the metropolitan area*, and omits the rural tracts that border them. For local models, this is a particular problem as results for observations on the edge of an area are based on incomplete information that may not reflect the true circumstances of that location.

With these concerns in mind, GWR is generally recommended as an *exploratory technique* that serves as a useful companion to the estimation of a global model. For example, a global parameter estimate may suggest that in Dallas-Fort Worth, median home values tend to be higher in areas with lower percentages of owner-occupied housing, controlling for other predictors in the model. GWR, used as a follow-up, helps the analyst understand that this global relationship is driven by higher home values near to urban cores such as in the uptown Dallas area, and may not necessarily characterize the dynamics of rural and exurban areas elsewhere in the metropolitan region.

## Classification and clustering of ACS data

The statistical models discussed earlier in this chapter were fit for the purpose of understanding relationships between an outcome variable and a series of predictors. In the social sciences, such models are generally used for *inference*, where a researcher tests those hypotheses with respect to those relationships to understand social processes. In industry, regression models are commonly used instead for *prediction,* where a model is trained on the relationship between an observed outcome and predictors then used to make predictions on out-of-sample data. In machine learning terminology, this is referred to as *supervised learning*, where the prediction target is known.

In other cases, the researcher is interested in discovering the structure of a dataset and generating meaningful labels for it rather than making predictions based on a known outcome. This type of approach is termed *unsupervised machine learning*. This section will explore two common applications of unsupervised machine learning with respect to demographic data: *geodemographic classification*, which identifies "clusters" of similar areas based on common demographic characteristics, and *regionalization*, which partitions an area into salient *regions* that are both spatially contiguous and share common demographic attributes.

### Geodemographic classification

*Geodemographic classification* refers to the grouping of geographic observations based on similar demographic (or other) characteristics [@singleton2013]. It is commonly used to generate neighborhood "typologies" that can help explain general similarities and differences among neighborhoods in a broader region. While the geodemographic approach has been criticized for essentializing neighborhoods [@goss1995], it is also widely used to understand dynamics of urban systems [@vicino2011] and has been proposed as a possible solution to problems with large margins of error for individual variables in the ACS [@spielman2015]. In industry, geodemographics are widely used for marketing and customer segmentation purposes. Popular frameworks include [Esri's Tapestry Segmentation](https://www.esri.com/en-us/arcgis/products/data/data-portfolio/tapestry-segmentation) and [Experian's Mosaic product](https://www.experian.com/marketing-services/consumer-segmentation).

While the exact methodology to produce a geodemographic classification system varies from implementation to implementation, the general process used involves *dimension reduction* applied to a high-dimensional input dataset of model features, followed by a *clustering algorithm* to partition observations into groups based on the derived dimensions. As we have already employed principal components analysis for dimension reduction on the Dallas-Fort Worth dataset, we can re-use those components for this purpose.

The *k*-means clustering algorithm is one of the most common unsupervised algorithms used to partition data in this way. *K-*means works by attempting to generate $K$ clusters that are internally similar but dissimilar from other clusters. Following @james2013c and @boehmke2019j, the goal of *K-*means clustering can be written as:

$$
\underset{C_1...C_k}{\text{minimize}}\left \{ \sum\limits_{k=1}^KW(C_k) \right \}
$$

where the within-cluster variation $W(C_k)$ is computed as

$$
W(C_k) = \sum_{x_i \in C_k}(x_{i} - \mu_k) ^ 2
$$

with $x_i$ representing an observation in the cluster $C_k$ and $\mu_k$ representing the mean value of all observations in cluster $C_k$.

To compute *k-*means, the analyst must first choose the number of desired clusters, represented with $k$. The analyst then specifies $k$ initial "centers" from the data (this is generally done at random) to seed the algorithm. The algorithm then iteratively assigns observations to clusters until the total within-cluster variation is minimized, returning a cluster solution.

In R, *k-*means can be computed with the `kmeans()` function. This example solution will generate 6 cluster groups. Given that the algorithm relies on random seeding of the cluster centers, `set.seed()` should be used to ensure stability of the solution.


```r
set.seed(1983)

dfw_kmeans <- dfw_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(dfw_kmeans$cluster)
```

```
## 
##   1   2   3   4   5   6 
## 456 193 172  83 228 427
```

The algorithm has partitioned the data into six clusters; the smallest (Cluster 4) has 83 Census tracts, whereas the largest (Cluster 3) has 456 Census tracts. At this stage, it is useful to explore the data in both *geographic space* and *variable space* to understand how the clusters differ from one another. We can assign the cluster ID to the original dataset as a new column and map it with `geom_sf()`.


```r
dfw_clusters <- dfw_pca %>%
  mutate(cluster = as.character(dfw_kmeans$cluster))

ggplot(dfw_clusters, aes(fill = cluster)) + 
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set1") + 
  theme_void() + 
  labs(fill = "Cluster ")
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/map-clusters-1.png" alt="Map of geodemographic clusters in Dallas-Fort Worth" width="100%" />
<p class="caption">(\#fig:map-clusters)Map of geodemographic clusters in Dallas-Fort Worth</p>
</div>

Some notable geographic patterns in the clusters are evident from the map, even if the viewer does not have local knowledge of the Dallas-Fort Worth region. Cluster 1 represents more rural communities on the edges of the metropolitan area, whereas Cluster 6 tends to be located in the core counties of Tarrant and Dallas as well as higher-density tracts in outer counties. Cluster 2 covers both big-city downtowns of Fort Worth and Dallas along with a scattering of suburban tracts.

A useful companion visualization to the map is a color-coded scatterplot using two of the principal components in the PCA dataset. We will use the two components discussed as "indices" in Section \@ref(dimension-reduction-with-principal-components-analysis): PC1, which is a gradient from affluent/older/white to lower-income/younger/nonwhite, and PC2, which represents areas with high population densities and educational attainment on the low end to lower-density, less educated areas on the high end. Given the data density, `ggplotly()` from the **plotly** package will convert the scatterplot to a graphic with an interactive legend, allowing the analyst to turn cluster groups on and off.


```r
library(plotly)

cluster_plot <- ggplot(dfw_clusters, 
                       aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15, 
                       x = 0.2, title = "Cluster"))
```

<div class="figure">

```{=html}
<div id="htmlwidget-22fc54e5bbce7453b398" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-22fc54e5bbce7453b398">{"x":{"data":[{"x":[-0.162850633877106,-0.90038658507515,-1.74932515641441,-0.946402059306467,0.0908499603479476,-1.06835815878384,-0.332402806608791,-1.22339775223047,0.400373573927795,-0.836503675854486,0.203434845998546,0.426890041411775,0.390194317081102,-0.505064278561518,0.417357084519761,-0.857099350161932,-0.636156652051581,0.380863573787639,-1.12592625716754,-0.951844794842175,-1.52237115916325,-1.17887415930226,-0.750667058787154,-0.689195126026117,-0.0520002351014292,-0.522267452422302,-1.09819689703306,-1.55676189327677,0.0886033787206449,-1.62017372971056,-0.70604345689481,-0.875907381794027,-0.438047786595484,0.296841016004362,-0.429982732692929,-0.47164357364041,-1.25235318318437,-1.39973413583051,-0.432087088737405,-0.965111632009025,-0.195624923858084,-0.936865429259843,-1.37892133604062,0.562520578948264,-0.867834090733668,-0.627273219686901,-0.400159920834009,-0.447955822111124,-0.271355496612726,0.165374712293679,-0.0434499180157989,-0.194244419384655,-0.593209273946913,-0.811720355496366,-1.6291325789047,-0.0163405741954424,-0.197225199956496,0.103336347309157,-0.588156095091285,-1.01606438481919,-0.7634794395398,0.0185821302633842,-0.551185494803973,0.421828450199824,-0.280951127469199,-0.102358467152813,0.257208684176262,0.166581851070222,-0.409092782207913,0.763068513648967,-0.286347816951744,-0.370281605026739,0.0856607348114652,-0.192704000241817,0.251037433607346,-1.00403737838727,0.413597113674434,0.457537764220837,-0.24088327686319,0.218821110075808,-0.0255727335075512,0.466876676211217,-0.214497966711692,0.363161335970137,0.449088671415713,0.350532606065826,-0.271227553371356,-0.546824293721689,0.781879437539291,0.397532802590846,-0.160118019281349,0.209265724758299,-0.503591381610189,-0.172186775137015,-0.162266017372101,0.273340908389407,-0.626181391157254,0.715704062189623,-0.625070862526459,-0.107061322366144,0.127811160177111,-0.538523761329278,-0.126656039283469,-0.0506518890635503,0.260655527806174,0.439538432390436,-0.0415431344893406,-0.57245503860326,-1.52952887127948,-0.203362183632711,0.449296603647769,-0.0315450291667669,-1.39901317346682,-0.123592983357618,-0.469941102985485,-0.646873626921529,-0.278477625132887,-0.346209786647322,-0.0178519231314837,0.0904177009715533,0.316455040844747,-0.636554137079339,-0.161035810226291,-1.33184945151947,-0.083757990056059,0.00137740903512999,0.211129624236329,0.382382572190512,-0.993416199828432,-1.92789397837706,-0.499311408995971,-0.0546384891651343,-1.11887399974873,-1.37433120167265,-0.851539276257216,-0.00205785015707946,-1.48972737349715,-0.988752135793743,-0.364553335909616,-1.50932941142996,-1.88753242993007,-1.28421307517366,-1.00094148709903,-2.0266286768521,-1.72882768518597,-1.7008718967679,-1.28011439701938,-1.57900325794631,-1.23096648885199,-0.183693620942186,0.691523575139508,-1.32071101497539,0.829453002201909,-1.74279719277639,0.687476996009033,0.593488000145159,0.691013623059066,-0.800725194808252,-0.83481817630574,-1.31385499383069,0.432457514270214,-0.269440498402602,-0.552200122006663,-1.37450340057156,-0.986196667843975,-1.05470507353288,-0.838145972181678,-0.965353207321844,-0.0192441503360207,-0.54256869805826,-0.203075020940789,-0.740497584779391,-0.798967723763021,-0.552718528634045,-0.19452364007085,-1.06114432647384,-1.08996098954088,-0.888835557638838,-0.28956875797534,-0.997019228073899,0.159046088473909,-0.93655647578283,0.505380315189717,-0.172215122290433,-0.593187952024571,0.418173581714838,-0.769010325890443,-1.73966634866885,-0.703732762804512,-1.07331706370515,-1.51348631492177,-0.813008198834602,-1.66180713362929,-1.08648580589682,-0.0994844573671932,0.0969135963752644,0.479115792690621,-0.631207503545237,0.262412205822456,-1.57491592425105,-0.618113986021109,-1.89885706506301,-0.281103759243992,-1.04101341532557,-0.351245915005376,-1.04324065005359,-0.760790471190709,-0.836829187665813,-1.26673682059329,0.194383310493979,-1.23167307103006,-1.22215501777228,-0.879705630197335,0.247260687031789,-1.23505077196676,0.196597803354739,-1.76810296119715,-0.4038768427117,-0.374750209933693,-0.740697490856908,-1.13799852922098,-1.50104790942502,-1.04644594682861,-0.782007339373876,-1.35834237856481,-1.41579949969904,-0.0543803589634838,-1.37146450261145,-1.51439063687654,-0.207346384955018,-0.287804967860767,-0.300452743231959,-1.09940207699929,0.164550381710077,-0.998771143538681,-0.350219730025201,-1.76923885760346,-1.66171120194546,0.135308199372496,-0.596827871952526,0.559168370647216,0.740811596986589,-0.0333998444974225,-1.82745796034335,-2.38577060222466,0.289150495116764,-0.21778413362978,-0.279865800315276,-0.47353732361009,-1.19448686648538,-0.417049604396607,0.0244702186535203,-0.746756267294025,-1.8059959486561,-0.789562597199439,-1.7941004162224,-0.85253907486025,-1.52204471578281,0.567316532261557,0.985982851544534,-1.63886080641423,0.171364482311507,0.760123305333957,-0.494166394904021,-0.739408267833747,-1.35087094259108,0.389653122437473,0.274104357754201,0.708323042752715,-0.709475744158211,1.27044083053469,-1.03061698329217,-1.04293571459168,-1.40115016672545,-0.535942053931865,-0.403372208907668,-0.90338178398101,-1.99642925210399,0.5095654205057,-1.02520407490296,-1.15442888616188,-0.635200303978491,-1.04025483480211,0.102533907691669,0.546622864414294,0.199542129870831,-0.477411544210163,-0.313291515322904,-1.12223092545488,-1.4396521859945,-0.600009824715641,-0.959759764185865,-1.65043648888798,-1.09043386670561,-1.21105892159336,-1.91547266830255,-2.18947730238457,-1.76236987618477,-1.21433160427806,-1.92277096508801,-1.10587055114347,-1.32427121391758,-2.00919269388493,-2.4729441236661,-1.00610073649581,-1.63716884958151,-0.487604754455064,-1.79143134658551,0.556817828606626,-0.829312037853186,-0.491466253720369,-1.68575677568627,-0.067347400899474,-1.15640821017146,0.99041590143957,0.187396493274476,0.107098708177542,0.845503113199094,0.701628765215298,-0.367030527732519,-0.597966550867977,0.217018593469766,0.816678231138099,0.0513294018938131,0.0739831205116849,0.660199975255742,0.0574344581946071,0.0925525053494696,-1.06652571834461,0.119517412276521,-0.754440295593765,0.337982007143256,0.345234981780344,0.820874860841546,0.775684422189043,-0.0508809698149535,-0.287663941968807,-1.52927853318114,-0.616689814700794,-1.59279503181444,-1.3025617941663,-0.518275150220851,-1.14339017027722,-0.200046621927924,-1.80179730036014,-0.135896330457473,0.020719036694737,-0.0984678551215101,0.0940767836577528,0.147011864828073,-0.214840294865541,-1.04853352113262,-0.184261947134174,-1.39152556712766,-1.24000726854247,-0.177831248188227,0.26042554432825,-1.7789549380062,-1.52423334599166,-0.928279636929032,0.352151266855984,-0.508225619057785,-1.55898806957936,-0.522882884514306,0.315708243829018,0.822986249037996,-1.16930483290589,-1.22174608957581,-1.52308671849504,-0.723800332723095,-0.079065121229202,0.427114876863146,-1.70777169026604,-1.14097089805794,-0.299182548686204,-0.475288422851985,-0.161355523136515,0.219544449900198,-0.17366224166662,0.207035570094674,-0.459087863350358,0.250379064483254,-0.29102118939729,-0.365614331934357,-1.08131534045707,0.794780988929444,-0.834536673505488,0.174456257465078,-0.168443422919024,0.877687061222433,-0.649506280506796,-0.0549730708898683,0.823233487817295,-1.61078365614213,-0.852451457550861,-1.05657505187035,0.248602479715537,0.270626015228127,-1.09923711916313,-0.319022507923858,-0.224357621640982,-0.325376564527388,0.173364273887827,0.798567731806778,0.676175989935073,0.459419483966593,0.208746496772374,-0.337519805404545,-1.07025232156465,-0.947041026591874,-1.09830873166751,0.064479975505888,-0.868833650955293,-0.426954205129948,0.444872519289292,-0.571895822325416,-0.453700479660707,-0.691736044797272,-0.186535287777358,-1.09886749908695,-1.84432433285086,-0.538952792264388,0.62424736708319,-0.306560124819919,0.775248319354074,-1.17795926391006,-1.26670943079685,-1.01001774473261,-0.986764520912892,-1.25318415540475,-1.3668542776572,-1.6052054112885,-1.69299692279657,-0.632389225535963,-1.04656999569446,-0.916231930080015,-0.612395751727577,0.168000077470783,0.0596165173793988,-0.252034910880084,-1.89674671029785,-1.29067031530687,-1.29941224431129,-0.135326242596736,0.355747783946358,-1.16614977753846,-1.59295627923627,-1.62386059423418,-1.50518696667874,-0.993276887845906,0.319469984204199,-0.555439363182229,-1.45550014925749,-0.94958626486266,-0.842530875036481,-0.991268679778774],"y":[1.04345756254961,0.446386631208022,0.66378932477351,0.742287795967083,0.936132066428538,-0.892812688576166,-1.06344386732154,0.568781327336952,1.16797516154776,0.0568214583696578,-0.350912367281045,-0.05099236156967,0.346540239657474,1.86174029313319,0.838259484624503,0.776743371323029,0.982263385961999,0.814472763550782,1.38985669005168,1.71044141662087,1.49656124505548,1.39502673056821,1.16107660254243,0.317140605906654,0.319449936653383,1.07065022357369,0.903529552558226,0.690154102713521,0.299065840591587,0.122702070665581,-0.127207862192103,-0.716454762723178,0.302511605040186,0.671656958563211,-0.345924543006234,-0.329791542820541,0.133814316633692,-0.153157413611359,-0.504003411183348,-0.0284223045324645,-0.514646103449723,-0.884100048876246,-0.512165524573597,0.525314329844862,-0.405722833121906,0.149936975974419,0.56355269097186,0.925022782936284,1.08772909490762,1.28678364361434,0.654816969085362,0.667447957093108,-0.870812135611471,-0.36935450199383,-0.0945710324144925,-0.586086415078499,-0.409393267432901,0.189816620242768,-0.233184452831948,-0.333499861728242,-0.375340786851498,0.496935607543821,-0.0177383371871052,-0.073735061118257,-0.113331214395437,0.743541820861071,0.804422829848597,1.06898349014613,0.183039368883883,0.988748281190662,1.19749382679596,0.777072705312203,0.38931982070039,-0.11661825090052,0.884365394514342,0.973183561278404,1.24595429088157,1.26137789916187,1.92399676644419,0.906275782752405,0.701948231991581,0.229377850431538,1.32330245961613,0.562441513483112,0.162302222661893,1.05366992555797,1.55511586586461,1.55847377300526,1.24022166171265,1.32479964559072,1.85771223359498,1.5652483940394,0.519876334722129,0.803160319995949,0.99990211893803,1.10139298848139,1.06944184943835,1.04528603186603,0.684375623769891,0.962403566763618,0.147144794637767,0.190477355993975,-0.0543926367776243,0.705266961632849,0.992929542901947,0.321589649283509,0.894161571696455,0.65454048207075,0.716760058577255,0.831321216782674,0.429322687709547,0.829781381643616,0.467561967078703,0.1318589107438,-0.130914401693586,0.638709624463657,0.428557533722212,1.02675225092581,0.504205069785681,0.345947621047631,0.253005948944626,-0.200410018078862,0.174197096246863,0.0336558827309783,0.856801299315837,0.206101251735665,0.266799128648222,1.68578437061951,1.39532577896665,1.016585538612,0.798067025982679,0.372465457440028,0.804483400694655,0.847364728134557,1.24098175351466,0.745952218497482,-0.133171388561258,-0.261099790054989,1.73424700041238,1.07115804520867,0.681887763702048,0.351499485341686,0.066949716260982,1.05885545792022,1.98910998134944,0.741242621938099,1.66456450468184,1.06209792109955,0.765080509241246,0.0024840582744573,0.604124344273979,1.13821733017494,0.728650402244526,0.331647694543505,0.235392375724088,0.333398979622651,0.770429716763337,-0.321210431205632,-0.444180711128897,0.418722975953121,1.61485822277722,0.815908827008925,0.457009688532501,-0.433239525535829,0.269379981142119,0.0303757355713123,1.32614325429984,1.5568973918447,0.458054465518344,0.672617712932132,0.368985273985184,-0.632436889925572,0.19752435917122,-0.262976448047471,-0.405548798679792,-0.201781015205139,-0.530541843708562,-0.0335898012012914,0.359600990511863,-0.354153229693887,0.450596785931865,1.29230555007921,1.70568956794497,1.17713740655642,1.21257278810323,1.64888631634443,1.92476810956952,0.749602886318424,1.51381413149072,1.55616302910164,1.80721408112999,0.656788967512227,0.973296375323204,0.920585784246395,0.877919959892003,0.538620904643822,0.687977187087106,1.21190412677116,1.21876228757449,1.85269545064404,0.58208007665106,1.95142916085305,1.53208604324531,1.58579402983042,0.618530021324854,1.02337918032804,0.224332560552156,0.962871667024685,1.59788398205455,0.882256910667323,0.81171155128239,1.2930414355633,1.36530546815686,0.597680133241339,1.32033620945044,1.32055198452723,0.636983326681462,1.18523921462035,1.52451391449892,1.64056802433556,1.29444770997614,1.45116874694855,1.45273263174454,1.73857141456282,0.885615855954535,1.52558318857566,0.455272266097421,1.40112252272733,1.56212675745211,2.2396839445538,1.83523741799312,1.9041268763503,1.21600556552877,0.443179931096054,0.311187613806097,0.633250344448274,0.954506979620415,0.642963574280072,0.623002363209305,0.356262155319836,1.49445482692668,1.14358014097707,1.27355826010787,1.5385965445945,1.24005481552009,1.78125517421834,1.75884784862034,1.69814063352658,0.645977141032791,0.771855817038427,1.27464889119117,1.39250065178328,-0.0271161020915138,0.876853251174032,1.34059216000503,0.467865008942513,1.77379357534268,0.261000850648393,0.970096751080866,1.55955845593262,0.871224037676305,1.1133452500844,1.86051028157569,1.63051635756283,1.64361972212585,1.83389960344908,1.8347162658432,1.22656814150073,0.831443680263618,1.50931224025764,1.57809831684739,1.10404922595432,1.29743775517015,1.48068685757526,1.165733166604,2.14703825046941,0.777870676394517,1.98838467910528,1.68421547891329,1.11343334758269,1.80814277886397,1.62414278094665,0.799934971988174,1.24245551798654,1.28392514051789,0.605037232537904,0.389960262023661,1.65897774939561,2.25307285180943,1.41115415268718,1.40483349818051,1.66147654193123,1.35288885616431,1.17885764456801,0.642669863714699,1.39861412781084,1.39173765490652,1.26110795459914,0.951729702694482,2.17424397766215,1.99859706718905,0.937546162660153,1.08961541315443,1.01571715699479,1.19995474801355,1.0719982780962,0.878923010126082,0.311279989683231,0.427992182003049,0.76013981989791,1.11236430103263,0.175173365573518,0.926812615072412,-0.241002427196398,1.07674237372623,0.823493252775516,-0.257287029351143,1.59390924290878,1.17004321990271,0.530903444469601,0.153910436636769,0.106065011479538,1.04920637846202,0.183285478818345,0.477438058060257,0.80218908449067,0.585676137494802,1.10551774899936,1.34479153080393,0.884628222370069,1.07394710138051,1.71801258901577,0.187907085683958,1.04526028350295,0.684566656608357,0.553224992487215,1.14551764985155,0.96123973931977,2.04876039605007,0.396786251838695,0.598743539737261,1.15200465143754,-0.0756730827624609,0.560923299203521,0.434304688445614,0.70528339474711,1.14680382730791,1.51313114499003,1.26671669797698,1.58863477911857,1.37294477922157,0.556374867557556,1.8347985793119,0.932699432494712,1.05314481656034,1.08935393768449,0.134775043127576,1.10440732957587,1.50422830637309,0.770207611209328,0.819366045112549,0.400640294458078,0.845857421608866,-0.0221620833920835,0.834135806735508,0.321205974342798,0.273740667547685,0.948244610884491,0.460886538753922,0.865495177863804,0.481762854598299,1.38530279226953,1.19132834903078,0.124086985801265,0.612556650044278,0.225815010550024,0.335095516051748,0.519809596564575,0.749016970984619,0.79389895323356,0.813316771617979,0.0882768633376521,0.233231178720674,-0.196405052435323,0.449658354928634,0.75446394699128,-0.0682063858643941,0.736071073155645,0.800453813005145,0.431281238491195,0.0359625076283315,-0.1882823717853,0.141966561219735,0.326818811756465,0.577934382085312,-0.197895103009346,-0.197998936405295,-0.32151386455277,-0.632937231251144,0.864252189754065,0.961375405086966,0.792170275062567,0.938894477541125,0.242708780565699,0.0714126531576057,0.821525152609193,0.441765128869165,0.791403282783825,-0.976346331133935,-0.617667131897967,0.142938525540266,0.846010472004677,0.352328476982698,-0.0607421124517614,0.691376363864495,-0.209456135405069,1.90924693607321,0.847390687563506,0.636706883677842,1.98190729438596,0.744144899068152,2.06364563276153,2.23656774852395,1.20500670587391,0.356522825054509,1.72085188534404,0.590261271219958,0.514558028129963,1.73909017414672,1.57083775913792,0.480073473113632,0.953483247278002,1.34457715231074,-0.0619867352933443,0.69117587887596,-0.562388159621839,0.482899206196624,-0.307553430207569,0.168902947550432,0.230450541829516,1.36450366926236,1.31253392143968,0.920059753806613,0.429352898493834,0.88209226315046,0.98065559373565,1.75652826448265,1.15275028753907,0.608545442706439,1.27958769795168,1.25103374731385,1.71296185547949,1.23363952511827,1.68947610990436,1.01589395974064,1.37103816795029],"text":["PC1: -0.162850634<br />PC2:  1.043458e+00<br />cluster: 1","PC1: -0.900386585<br />PC2:  4.463866e-01<br />cluster: 1","PC1: -1.749325156<br />PC2:  6.637893e-01<br />cluster: 1","PC1: -0.946402059<br />PC2:  7.422878e-01<br />cluster: 1","PC1:  0.090849960<br />PC2:  9.361321e-01<br />cluster: 1","PC1: -1.068358159<br />PC2: -8.928127e-01<br />cluster: 1","PC1: -0.332402807<br />PC2: -1.063444e+00<br />cluster: 1","PC1: -1.223397752<br />PC2:  5.687813e-01<br />cluster: 1","PC1:  0.400373574<br />PC2:  1.167975e+00<br />cluster: 1","PC1: -0.836503676<br />PC2:  5.682146e-02<br />cluster: 1","PC1:  0.203434846<br />PC2: -3.509124e-01<br />cluster: 1","PC1:  0.426890041<br />PC2: -5.099236e-02<br />cluster: 1","PC1:  0.390194317<br />PC2:  3.465402e-01<br />cluster: 1","PC1: -0.505064279<br />PC2:  1.861740e+00<br />cluster: 1","PC1:  0.417357085<br />PC2:  8.382595e-01<br />cluster: 1","PC1: -0.857099350<br />PC2:  7.767434e-01<br />cluster: 1","PC1: -0.636156652<br />PC2:  9.822634e-01<br />cluster: 1","PC1:  0.380863574<br />PC2:  8.144728e-01<br />cluster: 1","PC1: -1.125926257<br />PC2:  1.389857e+00<br />cluster: 1","PC1: -0.951844795<br />PC2:  1.710441e+00<br />cluster: 1","PC1: -1.522371159<br />PC2:  1.496561e+00<br />cluster: 1","PC1: -1.178874159<br />PC2:  1.395027e+00<br />cluster: 1","PC1: -0.750667059<br />PC2:  1.161077e+00<br />cluster: 1","PC1: -0.689195126<br />PC2:  3.171406e-01<br />cluster: 1","PC1: -0.052000235<br />PC2:  3.194499e-01<br />cluster: 1","PC1: -0.522267452<br />PC2:  1.070650e+00<br />cluster: 1","PC1: -1.098196897<br />PC2:  9.035296e-01<br />cluster: 1","PC1: -1.556761893<br />PC2:  6.901541e-01<br />cluster: 1","PC1:  0.088603379<br />PC2:  2.990658e-01<br />cluster: 1","PC1: -1.620173730<br />PC2:  1.227021e-01<br />cluster: 1","PC1: -0.706043457<br />PC2: -1.272079e-01<br />cluster: 1","PC1: -0.875907382<br />PC2: -7.164548e-01<br />cluster: 1","PC1: -0.438047787<br />PC2:  3.025116e-01<br />cluster: 1","PC1:  0.296841016<br />PC2:  6.716570e-01<br />cluster: 1","PC1: -0.429982733<br />PC2: -3.459245e-01<br />cluster: 1","PC1: -0.471643574<br />PC2: -3.297915e-01<br />cluster: 1","PC1: -1.252353183<br />PC2:  1.338143e-01<br />cluster: 1","PC1: -1.399734136<br />PC2: -1.531574e-01<br />cluster: 1","PC1: -0.432087089<br />PC2: -5.040034e-01<br />cluster: 1","PC1: -0.965111632<br />PC2: -2.842230e-02<br />cluster: 1","PC1: -0.195624924<br />PC2: -5.146461e-01<br />cluster: 1","PC1: -0.936865429<br />PC2: -8.841000e-01<br />cluster: 1","PC1: -1.378921336<br />PC2: -5.121655e-01<br />cluster: 1","PC1:  0.562520579<br />PC2:  5.253143e-01<br />cluster: 1","PC1: -0.867834091<br />PC2: -4.057228e-01<br />cluster: 1","PC1: -0.627273220<br />PC2:  1.499370e-01<br />cluster: 1","PC1: -0.400159921<br />PC2:  5.635527e-01<br />cluster: 1","PC1: -0.447955822<br />PC2:  9.250228e-01<br />cluster: 1","PC1: -0.271355497<br />PC2:  1.087729e+00<br />cluster: 1","PC1:  0.165374712<br />PC2:  1.286784e+00<br />cluster: 1","PC1: -0.043449918<br />PC2:  6.548170e-01<br />cluster: 1","PC1: -0.194244419<br />PC2:  6.674480e-01<br />cluster: 1","PC1: -0.593209274<br />PC2: -8.708121e-01<br />cluster: 1","PC1: -0.811720355<br />PC2: -3.693545e-01<br />cluster: 1","PC1: -1.629132579<br />PC2: -9.457103e-02<br />cluster: 1","PC1: -0.016340574<br />PC2: -5.860864e-01<br />cluster: 1","PC1: -0.197225200<br />PC2: -4.093933e-01<br />cluster: 1","PC1:  0.103336347<br />PC2:  1.898166e-01<br />cluster: 1","PC1: -0.588156095<br />PC2: -2.331845e-01<br />cluster: 1","PC1: -1.016064385<br />PC2: -3.334999e-01<br />cluster: 1","PC1: -0.763479440<br />PC2: -3.753408e-01<br />cluster: 1","PC1:  0.018582130<br />PC2:  4.969356e-01<br />cluster: 1","PC1: -0.551185495<br />PC2: -1.773834e-02<br />cluster: 1","PC1:  0.421828450<br />PC2: -7.373506e-02<br />cluster: 1","PC1: -0.280951127<br />PC2: -1.133312e-01<br />cluster: 1","PC1: -0.102358467<br />PC2:  7.435418e-01<br />cluster: 1","PC1:  0.257208684<br />PC2:  8.044228e-01<br />cluster: 1","PC1:  0.166581851<br />PC2:  1.068983e+00<br />cluster: 1","PC1: -0.409092782<br />PC2:  1.830394e-01<br />cluster: 1","PC1:  0.763068514<br />PC2:  9.887483e-01<br />cluster: 1","PC1: -0.286347817<br />PC2:  1.197494e+00<br />cluster: 1","PC1: -0.370281605<br />PC2:  7.770727e-01<br />cluster: 1","PC1:  0.085660735<br />PC2:  3.893198e-01<br />cluster: 1","PC1: -0.192704000<br />PC2: -1.166183e-01<br />cluster: 1","PC1:  0.251037434<br />PC2:  8.843654e-01<br />cluster: 1","PC1: -1.004037378<br />PC2:  9.731836e-01<br />cluster: 1","PC1:  0.413597114<br />PC2:  1.245954e+00<br />cluster: 1","PC1:  0.457537764<br />PC2:  1.261378e+00<br />cluster: 1","PC1: -0.240883277<br />PC2:  1.923997e+00<br />cluster: 1","PC1:  0.218821110<br />PC2:  9.062758e-01<br />cluster: 1","PC1: -0.025572734<br />PC2:  7.019482e-01<br />cluster: 1","PC1:  0.466876676<br />PC2:  2.293779e-01<br />cluster: 1","PC1: -0.214497967<br />PC2:  1.323302e+00<br />cluster: 1","PC1:  0.363161336<br />PC2:  5.624415e-01<br />cluster: 1","PC1:  0.449088671<br />PC2:  1.623022e-01<br />cluster: 1","PC1:  0.350532606<br />PC2:  1.053670e+00<br />cluster: 1","PC1: -0.271227553<br />PC2:  1.555116e+00<br />cluster: 1","PC1: -0.546824294<br />PC2:  1.558474e+00<br />cluster: 1","PC1:  0.781879438<br />PC2:  1.240222e+00<br />cluster: 1","PC1:  0.397532803<br />PC2:  1.324800e+00<br />cluster: 1","PC1: -0.160118019<br />PC2:  1.857712e+00<br />cluster: 1","PC1:  0.209265725<br />PC2:  1.565248e+00<br />cluster: 1","PC1: -0.503591382<br />PC2:  5.198763e-01<br />cluster: 1","PC1: -0.172186775<br />PC2:  8.031603e-01<br />cluster: 1","PC1: -0.162266017<br />PC2:  9.999021e-01<br />cluster: 1","PC1:  0.273340908<br />PC2:  1.101393e+00<br />cluster: 1","PC1: -0.626181391<br />PC2:  1.069442e+00<br />cluster: 1","PC1:  0.715704062<br />PC2:  1.045286e+00<br />cluster: 1","PC1: -0.625070863<br />PC2:  6.843756e-01<br />cluster: 1","PC1: -0.107061322<br />PC2:  9.624036e-01<br />cluster: 1","PC1:  0.127811160<br />PC2:  1.471448e-01<br />cluster: 1","PC1: -0.538523761<br />PC2:  1.904774e-01<br />cluster: 1","PC1: -0.126656039<br />PC2: -5.439264e-02<br />cluster: 1","PC1: -0.050651889<br />PC2:  7.052670e-01<br />cluster: 1","PC1:  0.260655528<br />PC2:  9.929295e-01<br />cluster: 1","PC1:  0.439538432<br />PC2:  3.215896e-01<br />cluster: 1","PC1: -0.041543134<br />PC2:  8.941616e-01<br />cluster: 1","PC1: -0.572455039<br />PC2:  6.545405e-01<br />cluster: 1","PC1: -1.529528871<br />PC2:  7.167601e-01<br />cluster: 1","PC1: -0.203362184<br />PC2:  8.313212e-01<br />cluster: 1","PC1:  0.449296604<br />PC2:  4.293227e-01<br />cluster: 1","PC1: -0.031545029<br />PC2:  8.297814e-01<br />cluster: 1","PC1: -1.399013173<br />PC2:  4.675620e-01<br />cluster: 1","PC1: -0.123592983<br />PC2:  1.318589e-01<br />cluster: 1","PC1: -0.469941103<br />PC2: -1.309144e-01<br />cluster: 1","PC1: -0.646873627<br />PC2:  6.387096e-01<br />cluster: 1","PC1: -0.278477625<br />PC2:  4.285575e-01<br />cluster: 1","PC1: -0.346209787<br />PC2:  1.026752e+00<br />cluster: 1","PC1: -0.017851923<br />PC2:  5.042051e-01<br />cluster: 1","PC1:  0.090417701<br />PC2:  3.459476e-01<br />cluster: 1","PC1:  0.316455041<br />PC2:  2.530059e-01<br />cluster: 1","PC1: -0.636554137<br />PC2: -2.004100e-01<br />cluster: 1","PC1: -0.161035810<br />PC2:  1.741971e-01<br />cluster: 1","PC1: -1.331849452<br />PC2:  3.365588e-02<br />cluster: 1","PC1: -0.083757990<br />PC2:  8.568013e-01<br />cluster: 1","PC1:  0.001377409<br />PC2:  2.061013e-01<br />cluster: 1","PC1:  0.211129624<br />PC2:  2.667991e-01<br />cluster: 1","PC1:  0.382382572<br />PC2:  1.685784e+00<br />cluster: 1","PC1: -0.993416200<br />PC2:  1.395326e+00<br />cluster: 1","PC1: -1.927893978<br />PC2:  1.016586e+00<br />cluster: 1","PC1: -0.499311409<br />PC2:  7.980670e-01<br />cluster: 1","PC1: -0.054638489<br />PC2:  3.724655e-01<br />cluster: 1","PC1: -1.118874000<br />PC2:  8.044834e-01<br />cluster: 1","PC1: -1.374331202<br />PC2:  8.473647e-01<br />cluster: 1","PC1: -0.851539276<br />PC2:  1.240982e+00<br />cluster: 1","PC1: -0.002057850<br />PC2:  7.459522e-01<br />cluster: 1","PC1: -1.489727373<br />PC2: -1.331714e-01<br />cluster: 1","PC1: -0.988752136<br />PC2: -2.610998e-01<br />cluster: 1","PC1: -0.364553336<br />PC2:  1.734247e+00<br />cluster: 1","PC1: -1.509329411<br />PC2:  1.071158e+00<br />cluster: 1","PC1: -1.887532430<br />PC2:  6.818878e-01<br />cluster: 1","PC1: -1.284213075<br />PC2:  3.514995e-01<br />cluster: 1","PC1: -1.000941487<br />PC2:  6.694972e-02<br />cluster: 1","PC1: -2.026628677<br />PC2:  1.058855e+00<br />cluster: 1","PC1: -1.728827685<br />PC2:  1.989110e+00<br />cluster: 1","PC1: -1.700871897<br />PC2:  7.412426e-01<br />cluster: 1","PC1: -1.280114397<br />PC2:  1.664565e+00<br />cluster: 1","PC1: -1.579003258<br />PC2:  1.062098e+00<br />cluster: 1","PC1: -1.230966489<br />PC2:  7.650805e-01<br />cluster: 1","PC1: -0.183693621<br />PC2:  2.484058e-03<br />cluster: 1","PC1:  0.691523575<br />PC2:  6.041243e-01<br />cluster: 1","PC1: -1.320711015<br />PC2:  1.138217e+00<br />cluster: 1","PC1:  0.829453002<br />PC2:  7.286504e-01<br />cluster: 1","PC1: -1.742797193<br />PC2:  3.316477e-01<br />cluster: 1","PC1:  0.687476996<br />PC2:  2.353924e-01<br />cluster: 1","PC1:  0.593488000<br />PC2:  3.333990e-01<br />cluster: 1","PC1:  0.691013623<br />PC2:  7.704297e-01<br />cluster: 1","PC1: -0.800725195<br />PC2: -3.212104e-01<br />cluster: 1","PC1: -0.834818176<br />PC2: -4.441807e-01<br />cluster: 1","PC1: -1.313854994<br />PC2:  4.187230e-01<br />cluster: 1","PC1:  0.432457514<br />PC2:  1.614858e+00<br />cluster: 1","PC1: -0.269440498<br />PC2:  8.159088e-01<br />cluster: 1","PC1: -0.552200122<br />PC2:  4.570097e-01<br />cluster: 1","PC1: -1.374503401<br />PC2: -4.332395e-01<br />cluster: 1","PC1: -0.986196668<br />PC2:  2.693800e-01<br />cluster: 1","PC1: -1.054705074<br />PC2:  3.037574e-02<br />cluster: 1","PC1: -0.838145972<br />PC2:  1.326143e+00<br />cluster: 1","PC1: -0.965353207<br />PC2:  1.556897e+00<br />cluster: 1","PC1: -0.019244150<br />PC2:  4.580545e-01<br />cluster: 1","PC1: -0.542568698<br />PC2:  6.726177e-01<br />cluster: 1","PC1: -0.203075021<br />PC2:  3.689853e-01<br />cluster: 1","PC1: -0.740497585<br />PC2: -6.324369e-01<br />cluster: 1","PC1: -0.798967724<br />PC2:  1.975244e-01<br />cluster: 1","PC1: -0.552718529<br />PC2: -2.629764e-01<br />cluster: 1","PC1: -0.194523640<br />PC2: -4.055488e-01<br />cluster: 1","PC1: -1.061144326<br />PC2: -2.017810e-01<br />cluster: 1","PC1: -1.089960990<br />PC2: -5.305418e-01<br />cluster: 1","PC1: -0.888835558<br />PC2: -3.358980e-02<br />cluster: 1","PC1: -0.289568758<br />PC2:  3.596010e-01<br />cluster: 1","PC1: -0.997019228<br />PC2: -3.541532e-01<br />cluster: 1","PC1:  0.159046088<br />PC2:  4.505968e-01<br />cluster: 1","PC1: -0.936556476<br />PC2:  1.292306e+00<br />cluster: 1","PC1:  0.505380315<br />PC2:  1.705690e+00<br />cluster: 1","PC1: -0.172215122<br />PC2:  1.177137e+00<br />cluster: 1","PC1: -0.593187952<br />PC2:  1.212573e+00<br />cluster: 1","PC1:  0.418173582<br />PC2:  1.648886e+00<br />cluster: 1","PC1: -0.769010326<br />PC2:  1.924768e+00<br />cluster: 1","PC1: -1.739666349<br />PC2:  7.496029e-01<br />cluster: 1","PC1: -0.703732763<br />PC2:  1.513814e+00<br />cluster: 1","PC1: -1.073317064<br />PC2:  1.556163e+00<br />cluster: 1","PC1: -1.513486315<br />PC2:  1.807214e+00<br />cluster: 1","PC1: -0.813008199<br />PC2:  6.567890e-01<br />cluster: 1","PC1: -1.661807134<br />PC2:  9.732964e-01<br />cluster: 1","PC1: -1.086485806<br />PC2:  9.205858e-01<br />cluster: 1","PC1: -0.099484457<br />PC2:  8.779200e-01<br />cluster: 1","PC1:  0.096913596<br />PC2:  5.386209e-01<br />cluster: 1","PC1:  0.479115793<br />PC2:  6.879772e-01<br />cluster: 1","PC1: -0.631207504<br />PC2:  1.211904e+00<br />cluster: 1","PC1:  0.262412206<br />PC2:  1.218762e+00<br />cluster: 1","PC1: -1.574915924<br />PC2:  1.852695e+00<br />cluster: 1","PC1: -0.618113986<br />PC2:  5.820801e-01<br />cluster: 1","PC1: -1.898857065<br />PC2:  1.951429e+00<br />cluster: 1","PC1: -0.281103759<br />PC2:  1.532086e+00<br />cluster: 1","PC1: -1.041013415<br />PC2:  1.585794e+00<br />cluster: 1","PC1: -0.351245915<br />PC2:  6.185300e-01<br />cluster: 1","PC1: -1.043240650<br />PC2:  1.023379e+00<br />cluster: 1","PC1: -0.760790471<br />PC2:  2.243326e-01<br />cluster: 1","PC1: -0.836829188<br />PC2:  9.628717e-01<br />cluster: 1","PC1: -1.266736821<br />PC2:  1.597884e+00<br />cluster: 1","PC1:  0.194383310<br />PC2:  8.822569e-01<br />cluster: 1","PC1: -1.231673071<br />PC2:  8.117116e-01<br />cluster: 1","PC1: -1.222155018<br />PC2:  1.293041e+00<br />cluster: 1","PC1: -0.879705630<br />PC2:  1.365305e+00<br />cluster: 1","PC1:  0.247260687<br />PC2:  5.976801e-01<br />cluster: 1","PC1: -1.235050772<br />PC2:  1.320336e+00<br />cluster: 1","PC1:  0.196597803<br />PC2:  1.320552e+00<br />cluster: 1","PC1: -1.768102961<br />PC2:  6.369833e-01<br />cluster: 1","PC1: -0.403876843<br />PC2:  1.185239e+00<br />cluster: 1","PC1: -0.374750210<br />PC2:  1.524514e+00<br />cluster: 1","PC1: -0.740697491<br />PC2:  1.640568e+00<br />cluster: 1","PC1: -1.137998529<br />PC2:  1.294448e+00<br />cluster: 1","PC1: -1.501047909<br />PC2:  1.451169e+00<br />cluster: 1","PC1: -1.046445947<br />PC2:  1.452733e+00<br />cluster: 1","PC1: -0.782007339<br />PC2:  1.738571e+00<br />cluster: 1","PC1: -1.358342379<br />PC2:  8.856159e-01<br />cluster: 1","PC1: -1.415799500<br />PC2:  1.525583e+00<br />cluster: 1","PC1: -0.054380359<br />PC2:  4.552723e-01<br />cluster: 1","PC1: -1.371464503<br />PC2:  1.401123e+00<br />cluster: 1","PC1: -1.514390637<br />PC2:  1.562127e+00<br />cluster: 1","PC1: -0.207346385<br />PC2:  2.239684e+00<br />cluster: 1","PC1: -0.287804968<br />PC2:  1.835237e+00<br />cluster: 1","PC1: -0.300452743<br />PC2:  1.904127e+00<br />cluster: 1","PC1: -1.099402077<br />PC2:  1.216006e+00<br />cluster: 1","PC1:  0.164550382<br />PC2:  4.431799e-01<br />cluster: 1","PC1: -0.998771144<br />PC2:  3.111876e-01<br />cluster: 1","PC1: -0.350219730<br />PC2:  6.332503e-01<br />cluster: 1","PC1: -1.769238858<br />PC2:  9.545070e-01<br />cluster: 1","PC1: -1.661711202<br />PC2:  6.429636e-01<br />cluster: 1","PC1:  0.135308199<br />PC2:  6.230024e-01<br />cluster: 1","PC1: -0.596827872<br />PC2:  3.562622e-01<br />cluster: 1","PC1:  0.559168371<br />PC2:  1.494455e+00<br />cluster: 1","PC1:  0.740811597<br />PC2:  1.143580e+00<br />cluster: 1","PC1: -0.033399844<br />PC2:  1.273558e+00<br />cluster: 1","PC1: -1.827457960<br />PC2:  1.538597e+00<br />cluster: 1","PC1: -2.385770602<br />PC2:  1.240055e+00<br />cluster: 1","PC1:  0.289150495<br />PC2:  1.781255e+00<br />cluster: 1","PC1: -0.217784134<br />PC2:  1.758848e+00<br />cluster: 1","PC1: -0.279865800<br />PC2:  1.698141e+00<br />cluster: 1","PC1: -0.473537324<br />PC2:  6.459771e-01<br />cluster: 1","PC1: -1.194486866<br />PC2:  7.718558e-01<br />cluster: 1","PC1: -0.417049604<br />PC2:  1.274649e+00<br />cluster: 1","PC1:  0.024470219<br />PC2:  1.392501e+00<br />cluster: 1","PC1: -0.746756267<br />PC2: -2.711610e-02<br />cluster: 1","PC1: -1.805995949<br />PC2:  8.768533e-01<br />cluster: 1","PC1: -0.789562597<br />PC2:  1.340592e+00<br />cluster: 1","PC1: -1.794100416<br />PC2:  4.678650e-01<br />cluster: 1","PC1: -0.852539075<br />PC2:  1.773794e+00<br />cluster: 1","PC1: -1.522044716<br />PC2:  2.610009e-01<br />cluster: 1","PC1:  0.567316532<br />PC2:  9.700968e-01<br />cluster: 1","PC1:  0.985982852<br />PC2:  1.559558e+00<br />cluster: 1","PC1: -1.638860806<br />PC2:  8.712240e-01<br />cluster: 1","PC1:  0.171364482<br />PC2:  1.113345e+00<br />cluster: 1","PC1:  0.760123305<br />PC2:  1.860510e+00<br />cluster: 1","PC1: -0.494166395<br />PC2:  1.630516e+00<br />cluster: 1","PC1: -0.739408268<br />PC2:  1.643620e+00<br />cluster: 1","PC1: -1.350870943<br />PC2:  1.833900e+00<br />cluster: 1","PC1:  0.389653122<br />PC2:  1.834716e+00<br />cluster: 1","PC1:  0.274104358<br />PC2:  1.226568e+00<br />cluster: 1","PC1:  0.708323043<br />PC2:  8.314437e-01<br />cluster: 1","PC1: -0.709475744<br />PC2:  1.509312e+00<br />cluster: 1","PC1:  1.270440831<br />PC2:  1.578098e+00<br />cluster: 1","PC1: -1.030616983<br />PC2:  1.104049e+00<br />cluster: 1","PC1: -1.042935715<br />PC2:  1.297438e+00<br />cluster: 1","PC1: -1.401150167<br />PC2:  1.480687e+00<br />cluster: 1","PC1: -0.535942054<br />PC2:  1.165733e+00<br />cluster: 1","PC1: -0.403372209<br />PC2:  2.147038e+00<br />cluster: 1","PC1: -0.903381784<br />PC2:  7.778707e-01<br />cluster: 1","PC1: -1.996429252<br />PC2:  1.988385e+00<br />cluster: 1","PC1:  0.509565421<br />PC2:  1.684215e+00<br />cluster: 1","PC1: -1.025204075<br />PC2:  1.113433e+00<br />cluster: 1","PC1: -1.154428886<br />PC2:  1.808143e+00<br />cluster: 1","PC1: -0.635200304<br />PC2:  1.624143e+00<br />cluster: 1","PC1: -1.040254835<br />PC2:  7.999350e-01<br />cluster: 1","PC1:  0.102533908<br />PC2:  1.242456e+00<br />cluster: 1","PC1:  0.546622864<br />PC2:  1.283925e+00<br />cluster: 1","PC1:  0.199542130<br />PC2:  6.050372e-01<br />cluster: 1","PC1: -0.477411544<br />PC2:  3.899603e-01<br />cluster: 1","PC1: -0.313291515<br />PC2:  1.658978e+00<br />cluster: 1","PC1: -1.122230925<br />PC2:  2.253073e+00<br />cluster: 1","PC1: -1.439652186<br />PC2:  1.411154e+00<br />cluster: 1","PC1: -0.600009825<br />PC2:  1.404833e+00<br />cluster: 1","PC1: -0.959759764<br />PC2:  1.661477e+00<br />cluster: 1","PC1: -1.650436489<br />PC2:  1.352889e+00<br />cluster: 1","PC1: -1.090433867<br />PC2:  1.178858e+00<br />cluster: 1","PC1: -1.211058922<br />PC2:  6.426699e-01<br />cluster: 1","PC1: -1.915472668<br />PC2:  1.398614e+00<br />cluster: 1","PC1: -2.189477302<br />PC2:  1.391738e+00<br />cluster: 1","PC1: -1.762369876<br />PC2:  1.261108e+00<br />cluster: 1","PC1: -1.214331604<br />PC2:  9.517297e-01<br />cluster: 1","PC1: -1.922770965<br />PC2:  2.174244e+00<br />cluster: 1","PC1: -1.105870551<br />PC2:  1.998597e+00<br />cluster: 1","PC1: -1.324271214<br />PC2:  9.375462e-01<br />cluster: 1","PC1: -2.009192694<br />PC2:  1.089615e+00<br />cluster: 1","PC1: -2.472944124<br />PC2:  1.015717e+00<br />cluster: 1","PC1: -1.006100736<br />PC2:  1.199955e+00<br />cluster: 1","PC1: -1.637168850<br />PC2:  1.071998e+00<br />cluster: 1","PC1: -0.487604754<br />PC2:  8.789230e-01<br />cluster: 1","PC1: -1.791431347<br />PC2:  3.112800e-01<br />cluster: 1","PC1:  0.556817829<br />PC2:  4.279922e-01<br />cluster: 1","PC1: -0.829312038<br />PC2:  7.601398e-01<br />cluster: 1","PC1: -0.491466254<br />PC2:  1.112364e+00<br />cluster: 1","PC1: -1.685756776<br />PC2:  1.751734e-01<br />cluster: 1","PC1: -0.067347401<br />PC2:  9.268126e-01<br />cluster: 1","PC1: -1.156408210<br />PC2: -2.410024e-01<br />cluster: 1","PC1:  0.990415901<br />PC2:  1.076742e+00<br />cluster: 1","PC1:  0.187396493<br />PC2:  8.234933e-01<br />cluster: 1","PC1:  0.107098708<br />PC2: -2.572870e-01<br />cluster: 1","PC1:  0.845503113<br />PC2:  1.593909e+00<br />cluster: 1","PC1:  0.701628765<br />PC2:  1.170043e+00<br />cluster: 1","PC1: -0.367030528<br />PC2:  5.309034e-01<br />cluster: 1","PC1: -0.597966551<br />PC2:  1.539104e-01<br />cluster: 1","PC1:  0.217018593<br />PC2:  1.060650e-01<br />cluster: 1","PC1:  0.816678231<br />PC2:  1.049206e+00<br />cluster: 1","PC1:  0.051329402<br />PC2:  1.832855e-01<br />cluster: 1","PC1:  0.073983121<br />PC2:  4.774381e-01<br />cluster: 1","PC1:  0.660199975<br />PC2:  8.021891e-01<br />cluster: 1","PC1:  0.057434458<br />PC2:  5.856761e-01<br />cluster: 1","PC1:  0.092552505<br />PC2:  1.105518e+00<br />cluster: 1","PC1: -1.066525718<br />PC2:  1.344792e+00<br />cluster: 1","PC1:  0.119517412<br />PC2:  8.846282e-01<br />cluster: 1","PC1: -0.754440296<br />PC2:  1.073947e+00<br />cluster: 1","PC1:  0.337982007<br />PC2:  1.718013e+00<br />cluster: 1","PC1:  0.345234982<br />PC2:  1.879071e-01<br />cluster: 1","PC1:  0.820874861<br />PC2:  1.045260e+00<br />cluster: 1","PC1:  0.775684422<br />PC2:  6.845667e-01<br />cluster: 1","PC1: -0.050880970<br />PC2:  5.532250e-01<br />cluster: 1","PC1: -0.287663942<br />PC2:  1.145518e+00<br />cluster: 1","PC1: -1.529278533<br />PC2:  9.612397e-01<br />cluster: 1","PC1: -0.616689815<br />PC2:  2.048760e+00<br />cluster: 1","PC1: -1.592795032<br />PC2:  3.967863e-01<br />cluster: 1","PC1: -1.302561794<br />PC2:  5.987435e-01<br />cluster: 1","PC1: -0.518275150<br />PC2:  1.152005e+00<br />cluster: 1","PC1: -1.143390170<br />PC2: -7.567308e-02<br />cluster: 1","PC1: -0.200046622<br />PC2:  5.609233e-01<br />cluster: 1","PC1: -1.801797300<br />PC2:  4.343047e-01<br />cluster: 1","PC1: -0.135896330<br />PC2:  7.052834e-01<br />cluster: 1","PC1:  0.020719037<br />PC2:  1.146804e+00<br />cluster: 1","PC1: -0.098467855<br />PC2:  1.513131e+00<br />cluster: 1","PC1:  0.094076784<br />PC2:  1.266717e+00<br />cluster: 1","PC1:  0.147011865<br />PC2:  1.588635e+00<br />cluster: 1","PC1: -0.214840295<br />PC2:  1.372945e+00<br />cluster: 1","PC1: -1.048533521<br />PC2:  5.563749e-01<br />cluster: 1","PC1: -0.184261947<br />PC2:  1.834799e+00<br />cluster: 1","PC1: -1.391525567<br />PC2:  9.326994e-01<br />cluster: 1","PC1: -1.240007269<br />PC2:  1.053145e+00<br />cluster: 1","PC1: -0.177831248<br />PC2:  1.089354e+00<br />cluster: 1","PC1:  0.260425544<br />PC2:  1.347750e-01<br />cluster: 1","PC1: -1.778954938<br />PC2:  1.104407e+00<br />cluster: 1","PC1: -1.524233346<br />PC2:  1.504228e+00<br />cluster: 1","PC1: -0.928279637<br />PC2:  7.702076e-01<br />cluster: 1","PC1:  0.352151267<br />PC2:  8.193660e-01<br />cluster: 1","PC1: -0.508225619<br />PC2:  4.006403e-01<br />cluster: 1","PC1: -1.558988070<br />PC2:  8.458574e-01<br />cluster: 1","PC1: -0.522882885<br />PC2: -2.216208e-02<br />cluster: 1","PC1:  0.315708244<br />PC2:  8.341358e-01<br />cluster: 1","PC1:  0.822986249<br />PC2:  3.212060e-01<br />cluster: 1","PC1: -1.169304833<br />PC2:  2.737407e-01<br />cluster: 1","PC1: -1.221746090<br />PC2:  9.482446e-01<br />cluster: 1","PC1: -1.523086718<br />PC2:  4.608865e-01<br />cluster: 1","PC1: -0.723800333<br />PC2:  8.654952e-01<br />cluster: 1","PC1: -0.079065121<br />PC2:  4.817629e-01<br />cluster: 1","PC1:  0.427114877<br />PC2:  1.385303e+00<br />cluster: 1","PC1: -1.707771690<br />PC2:  1.191328e+00<br />cluster: 1","PC1: -1.140970898<br />PC2:  1.240870e-01<br />cluster: 1","PC1: -0.299182549<br />PC2:  6.125567e-01<br />cluster: 1","PC1: -0.475288423<br />PC2:  2.258150e-01<br />cluster: 1","PC1: -0.161355523<br />PC2:  3.350955e-01<br />cluster: 1","PC1:  0.219544450<br />PC2:  5.198096e-01<br />cluster: 1","PC1: -0.173662242<br />PC2:  7.490170e-01<br />cluster: 1","PC1:  0.207035570<br />PC2:  7.938990e-01<br />cluster: 1","PC1: -0.459087863<br />PC2:  8.133168e-01<br />cluster: 1","PC1:  0.250379064<br />PC2:  8.827686e-02<br />cluster: 1","PC1: -0.291021189<br />PC2:  2.332312e-01<br />cluster: 1","PC1: -0.365614332<br />PC2: -1.964051e-01<br />cluster: 1","PC1: -1.081315340<br />PC2:  4.496584e-01<br />cluster: 1","PC1:  0.794780989<br />PC2:  7.544639e-01<br />cluster: 1","PC1: -0.834536674<br />PC2: -6.820639e-02<br />cluster: 1","PC1:  0.174456257<br />PC2:  7.360711e-01<br />cluster: 1","PC1: -0.168443423<br />PC2:  8.004538e-01<br />cluster: 1","PC1:  0.877687061<br />PC2:  4.312812e-01<br />cluster: 1","PC1: -0.649506281<br />PC2:  3.596251e-02<br />cluster: 1","PC1: -0.054973071<br />PC2: -1.882824e-01<br />cluster: 1","PC1:  0.823233488<br />PC2:  1.419666e-01<br />cluster: 1","PC1: -1.610783656<br />PC2:  3.268188e-01<br />cluster: 1","PC1: -0.852451458<br />PC2:  5.779344e-01<br />cluster: 1","PC1: -1.056575052<br />PC2: -1.978951e-01<br />cluster: 1","PC1:  0.248602480<br />PC2: -1.979989e-01<br />cluster: 1","PC1:  0.270626015<br />PC2: -3.215139e-01<br />cluster: 1","PC1: -1.099237119<br />PC2: -6.329372e-01<br />cluster: 1","PC1: -0.319022508<br />PC2:  8.642522e-01<br />cluster: 1","PC1: -0.224357622<br />PC2:  9.613754e-01<br />cluster: 1","PC1: -0.325376565<br />PC2:  7.921703e-01<br />cluster: 1","PC1:  0.173364274<br />PC2:  9.388945e-01<br />cluster: 1","PC1:  0.798567732<br />PC2:  2.427088e-01<br />cluster: 1","PC1:  0.676175990<br />PC2:  7.141265e-02<br />cluster: 1","PC1:  0.459419484<br />PC2:  8.215252e-01<br />cluster: 1","PC1:  0.208746497<br />PC2:  4.417651e-01<br />cluster: 1","PC1: -0.337519805<br />PC2:  7.914033e-01<br />cluster: 1","PC1: -1.070252322<br />PC2: -9.763463e-01<br />cluster: 1","PC1: -0.947041027<br />PC2: -6.176671e-01<br />cluster: 1","PC1: -1.098308732<br />PC2:  1.429385e-01<br />cluster: 1","PC1:  0.064479976<br />PC2:  8.460105e-01<br />cluster: 1","PC1: -0.868833651<br />PC2:  3.523285e-01<br />cluster: 1","PC1: -0.426954205<br />PC2: -6.074211e-02<br />cluster: 1","PC1:  0.444872519<br />PC2:  6.913764e-01<br />cluster: 1","PC1: -0.571895822<br />PC2: -2.094561e-01<br />cluster: 1","PC1: -0.453700480<br />PC2:  1.909247e+00<br />cluster: 1","PC1: -0.691736045<br />PC2:  8.473907e-01<br />cluster: 1","PC1: -0.186535288<br />PC2:  6.367069e-01<br />cluster: 1","PC1: -1.098867499<br />PC2:  1.981907e+00<br />cluster: 1","PC1: -1.844324333<br />PC2:  7.441449e-01<br />cluster: 1","PC1: -0.538952792<br />PC2:  2.063646e+00<br />cluster: 1","PC1:  0.624247367<br />PC2:  2.236568e+00<br />cluster: 1","PC1: -0.306560125<br />PC2:  1.205007e+00<br />cluster: 1","PC1:  0.775248319<br />PC2:  3.565228e-01<br />cluster: 1","PC1: -1.177959264<br />PC2:  1.720852e+00<br />cluster: 1","PC1: -1.266709431<br />PC2:  5.902613e-01<br />cluster: 1","PC1: -1.010017745<br />PC2:  5.145580e-01<br />cluster: 1","PC1: -0.986764521<br />PC2:  1.739090e+00<br />cluster: 1","PC1: -1.253184155<br />PC2:  1.570838e+00<br />cluster: 1","PC1: -1.366854278<br />PC2:  4.800735e-01<br />cluster: 1","PC1: -1.605205411<br />PC2:  9.534832e-01<br />cluster: 1","PC1: -1.692996923<br />PC2:  1.344577e+00<br />cluster: 1","PC1: -0.632389226<br />PC2: -6.198674e-02<br />cluster: 1","PC1: -1.046569996<br />PC2:  6.911759e-01<br />cluster: 1","PC1: -0.916231930<br />PC2: -5.623882e-01<br />cluster: 1","PC1: -0.612395752<br />PC2:  4.828992e-01<br />cluster: 1","PC1:  0.168000077<br />PC2: -3.075534e-01<br />cluster: 1","PC1:  0.059616517<br />PC2:  1.689029e-01<br />cluster: 1","PC1: -0.252034911<br />PC2:  2.304505e-01<br />cluster: 1","PC1: -1.896746710<br />PC2:  1.364504e+00<br />cluster: 1","PC1: -1.290670315<br />PC2:  1.312534e+00<br />cluster: 1","PC1: -1.299412244<br />PC2:  9.200598e-01<br />cluster: 1","PC1: -0.135326243<br />PC2:  4.293529e-01<br />cluster: 1","PC1:  0.355747784<br />PC2:  8.820923e-01<br />cluster: 1","PC1: -1.166149778<br />PC2:  9.806556e-01<br />cluster: 1","PC1: -1.592956279<br />PC2:  1.756528e+00<br />cluster: 1","PC1: -1.623860594<br />PC2:  1.152750e+00<br />cluster: 1","PC1: -1.505186967<br />PC2:  6.085454e-01<br />cluster: 1","PC1: -0.993276888<br />PC2:  1.279588e+00<br />cluster: 1","PC1:  0.319469984<br />PC2:  1.251034e+00<br />cluster: 1","PC1: -0.555439363<br />PC2:  1.712962e+00<br />cluster: 1","PC1: -1.455500149<br />PC2:  1.233640e+00<br />cluster: 1","PC1: -0.949586265<br />PC2:  1.689476e+00<br />cluster: 1","PC1: -0.842530875<br />PC2:  1.015894e+00<br />cluster: 1","PC1: -0.991268680<br />PC2:  1.371038e+00<br />cluster: 1"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.60585191698997,1.78679180084017,2.22810438248886,0.619682643575232,-0.138568627903596,1.74016818749273,-0.991165690881923,2.23240028535772,-0.0863468067983579,0.682383665476767,1.51458389413848,0.388372851083383,1.18940006922088,-0.290501587651513,1.75163487823772,0.401163656645823,0.561179616734637,0.0525653140775706,0.790487798045008,0.776940228700784,-0.151764332139224,-0.314839902061263,2.03838046709607,1.59282688715543,0.21896132359158,-0.166390996714452,0.578640544039812,0.117226426958032,-0.0829885199098182,-0.0991497635160669,0.844312160764636,0.531447678717537,-0.666421217518499,2.22317659097134,1.79557477352963,0.152559450879606,-1.05252363947637,-0.502138995617933,-0.895716480257475,0.305617507775807,0.127768877079468,-0.786895612972831,1.19433368989026,0.179963599224307,0.844085783400334,0.370389585520995,0.860376623974169,-0.269539892606233,-0.557212097385109,1.3282829124761,-0.715016583682178,0.637953011379378,1.4898968366469,2.15624699927995,0.129930896156561,0.189436314377849,1.96337625291884,1.26263449072077,1.27184415914564,0.153074318131984,0.252056284541668,0.190631926425463,1.81050010022459,1.02146500061894,1.26916707654433,-0.990122322468515,0.672106185740779,-1.19386131632452,0.200396594179834,0.612244357853752,2.6696625253009,1.49544202102504,0.510813670427428,-0.543876067778658,0.252378175148964,-0.299382228163255,-0.0986009481746707,-0.160998697951067,-0.675390000895858,1.48985853448278,0.938000427723402,0.114807468306014,1.03613642782903,0.207864240550994,1.71237975878894,1.87767201092916,2.21717042922571,0.302508961046826,0.556240457882746,1.62341782081863,1.16999953552412,1.00595513591761,1.04879411930922,1.82042127707857,0.0656785840272354,1.5928498561815,0.110670211408224,2.32674979972589,1.47506265980614,0.412302070244076,2.21424495583284,1.75295851903668,0.484532759357777,0.0871272731400832,1.84716161462259,1.94011516970175,0.656049822123359,-0.248437882580524,1.73955500757109,1.19851004436125,-1.02667187167896,0.716607312082477,1.00203675226545,1.60621014635598,2.56404047214011,2.13120525150009,0.243676739059794,1.65108029556201,-0.304430771740877,0.794348382361507,-0.318090066124771,-0.332256390876109,2.00760994763628,0.801736461499467,1.30006615672366,0.423483740419378,-1.08270315242074,0.723070987772801,0.692803443204288,1.22302201509301,1.86122545635851,1.64361803535311,1.0660975102734,2.22206359554626,1.04950099418505,0.905640085149774,2.0412469933574,0.717718898757732,-0.568237364731054,1.00108285984571,1.30105199867797,0.115459573479066,2.78561813024767,0.570004391280637,0.460097811732305,0.136003252270848,1.63111300128258,-0.757760888308523,-0.116066826630521,-0.395465890173117,1.62799039878463,-0.678325455810369,1.16017477531653,-0.547418726246591,0.286754657989112,1.50648318818125,-0.0499729689118665,-0.0475367302794121,0.0858756059268262,0.484857142819703,-1.02108312817855,1.71907436690199,2.18916899072821,1.69923208139749,0.15098878319745,0.374962354387014,2.38328725896372,0.57775772855349,1.77451190015135,0.950629684818348,1.45102606787169,1.23747433945711,1.07045354718969,1.95388538181561,0.826621197129957,1.1245753823609,1.30277601945351,0.0974108851182886,0.349571248470818,0.528776336751468,1.02467312517541,-0.514769732129434,0.412504803750673,2.15103116232401,1.33696587606706,1.74780125502465,0.523136365003934,0.664541989719776,-1.04463510571143,1.36811632712352,-0.891149924122317,1.12864675788182,0.0823699588774057],"y":[-0.402322250745955,-0.685261568501929,-0.981807428852663,-0.250400312456804,-0.96358426886849,-1.64651136262189,-1.07680453383082,-1.41681296161247,-2.44942616386999,-2.77662106975226,-0.168312342898294,-0.476611691404987,-0.876710328427124,-1.02665160840484,-1.3807308678814,-0.650988848858404,-0.226193580041523,-0.952951346003412,-0.728953950567148,-1.25663143551764,-1.57230740386324,-1.38171096776985,-1.88394904801822,-2.05341625693246,-0.993540557034257,-2.00956229617662,-1.54385001336442,-2.25378520086713,-1.17585547789188,-1.10060723547558,-0.877866001621245,-2.44521509332939,-2.09155575805087,-0.972480176259061,-0.525176737139086,-1.53775201346166,-1.63697789377237,-1.27687060904327,-2.74505647168402,-2.99943734699382,-2.80684783925539,-2.49923355911604,-2.46477965788087,-3.4771709105441,-3.18093856802631,-3.5431771835657,-2.97499234572933,-1.87088047227756,-1.65248787035531,-1.08276194116223,-3.35642950007939,-1.65402953930965,-2.0887927288106,-4.12537548513891,-2.48330659207881,-2.88596670377516,-1.37137182170963,-2.28455738501312,-1.8361693919614,-2.42032201312346,-0.303609971250936,-3.25738033147143,-2.41311011130062,-1.47642181010041,-1.87344475065645,-2.35304436163158,-0.24902048353218,-3.06914133816269,-0.803215755935966,-0.716478724069946,-3.6828270710929,-0.52194873338914,-2.69510627197777,-0.666407155080283,-0.69062050563068,-0.746277339801089,-0.916744315631185,-0.704943122255929,-3.14289679011239,-2.03477371606631,-1.01569686406261,-0.689018839625001,-0.970857864359976,-1.78770926772128,-1.46751054124689,-0.56083447631226,-0.855208878000192,-1.42763847543173,-0.822462281086769,-1.62968294889283,-4.55528439709084,-1.0517338379199,-1.60507888102241,-1.81423706652116,-1.62202048702824,-2.20575169262716,-2.01248483060807,-2.04444922747511,-1.50638377866778,-0.605194306483932,-2.08184728639933,-2.3224109339148,-1.68290060791295,-1.19481223159012,0.163447001210315,-0.952243973490799,0.0798280764053169,-0.630384729896709,0.116679055343565,-0.568995946529511,-1.97318868867005,-1.20161999046202,-0.183289944925731,-0.751035732587106,-0.176771614966998,-0.835179522868575,0.118778545411578,-0.524875813520396,-1.31875256787291,-1.00889092395875,-2.21868374259616,-0.903461843752746,-0.479078183607768,-1.34253204156857,-0.531020438987111,-1.07351955961102,-2.75311870137565,-3.94057670525018,-4.07536879239178,-0.44521800212395,-0.338667593581734,-1.37797502358509,-0.985364248765691,-1.78430847026305,-0.817677922940051,-1.00324848452187,-0.489949228156501,-0.671736918564528,-1.99746223611958,0.377515286528935,-0.74154687424709,-0.329159621769938,-2.14071466807796,-1.55552340661911,-0.637057716669178,-0.252992618584194,-0.410829859488354,-0.909892522132483,-2.85225367183863,-0.748757926162377,-0.727261141131703,-1.11048588100742,0.134527970495114,-0.835802305088145,-0.677343265581805,-0.521217907679289,-0.969999286153787,-0.669344701496921,-1.08235735069977,-0.639916441937919,-2.59738399904982,0.204897276719018,-0.734289300043032,-0.262212523614207,-1.18226889403498,-0.233949941465252,-0.636430856261557,-1.32849776973402,-1.08533741946127,-0.234763433174068,-0.957379708742291,-0.454429535265931,-1.91239891474762,-0.413526779896396,-0.565272262860003,-0.877120298411174,-1.0022249789718,-1.59425145561591,0.00916954218357632,-1.19576519711278,-0.227271459116416,-0.549991670563023,-0.409303270943521,-0.358713156575622,-0.5706680552592,-1.15101625684067,-0.352367292206593,-0.783832816264801,-1.29263573355682,-0.793143210864558,-2.30902210010905,-0.230669028600975,-1.05661838570201],"text":["PC1:  0.605851917<br />PC2: -4.023223e-01<br />cluster: 2","PC1:  1.786791801<br />PC2: -6.852616e-01<br />cluster: 2","PC1:  2.228104382<br />PC2: -9.818074e-01<br />cluster: 2","PC1:  0.619682644<br />PC2: -2.504003e-01<br />cluster: 2","PC1: -0.138568628<br />PC2: -9.635843e-01<br />cluster: 2","PC1:  1.740168187<br />PC2: -1.646511e+00<br />cluster: 2","PC1: -0.991165691<br />PC2: -1.076805e+00<br />cluster: 2","PC1:  2.232400285<br />PC2: -1.416813e+00<br />cluster: 2","PC1: -0.086346807<br />PC2: -2.449426e+00<br />cluster: 2","PC1:  0.682383665<br />PC2: -2.776621e+00<br />cluster: 2","PC1:  1.514583894<br />PC2: -1.683123e-01<br />cluster: 2","PC1:  0.388372851<br />PC2: -4.766117e-01<br />cluster: 2","PC1:  1.189400069<br />PC2: -8.767103e-01<br />cluster: 2","PC1: -0.290501588<br />PC2: -1.026652e+00<br />cluster: 2","PC1:  1.751634878<br />PC2: -1.380731e+00<br />cluster: 2","PC1:  0.401163657<br />PC2: -6.509888e-01<br />cluster: 2","PC1:  0.561179617<br />PC2: -2.261936e-01<br />cluster: 2","PC1:  0.052565314<br />PC2: -9.529513e-01<br />cluster: 2","PC1:  0.790487798<br />PC2: -7.289540e-01<br />cluster: 2","PC1:  0.776940229<br />PC2: -1.256631e+00<br />cluster: 2","PC1: -0.151764332<br />PC2: -1.572307e+00<br />cluster: 2","PC1: -0.314839902<br />PC2: -1.381711e+00<br />cluster: 2","PC1:  2.038380467<br />PC2: -1.883949e+00<br />cluster: 2","PC1:  1.592826887<br />PC2: -2.053416e+00<br />cluster: 2","PC1:  0.218961324<br />PC2: -9.935406e-01<br />cluster: 2","PC1: -0.166390997<br />PC2: -2.009562e+00<br />cluster: 2","PC1:  0.578640544<br />PC2: -1.543850e+00<br />cluster: 2","PC1:  0.117226427<br />PC2: -2.253785e+00<br />cluster: 2","PC1: -0.082988520<br />PC2: -1.175855e+00<br />cluster: 2","PC1: -0.099149764<br />PC2: -1.100607e+00<br />cluster: 2","PC1:  0.844312161<br />PC2: -8.778660e-01<br />cluster: 2","PC1:  0.531447679<br />PC2: -2.445215e+00<br />cluster: 2","PC1: -0.666421218<br />PC2: -2.091556e+00<br />cluster: 2","PC1:  2.223176591<br />PC2: -9.724802e-01<br />cluster: 2","PC1:  1.795574774<br />PC2: -5.251767e-01<br />cluster: 2","PC1:  0.152559451<br />PC2: -1.537752e+00<br />cluster: 2","PC1: -1.052523639<br />PC2: -1.636978e+00<br />cluster: 2","PC1: -0.502138996<br />PC2: -1.276871e+00<br />cluster: 2","PC1: -0.895716480<br />PC2: -2.745056e+00<br />cluster: 2","PC1:  0.305617508<br />PC2: -2.999437e+00<br />cluster: 2","PC1:  0.127768877<br />PC2: -2.806848e+00<br />cluster: 2","PC1: -0.786895613<br />PC2: -2.499234e+00<br />cluster: 2","PC1:  1.194333690<br />PC2: -2.464780e+00<br />cluster: 2","PC1:  0.179963599<br />PC2: -3.477171e+00<br />cluster: 2","PC1:  0.844085783<br />PC2: -3.180939e+00<br />cluster: 2","PC1:  0.370389586<br />PC2: -3.543177e+00<br />cluster: 2","PC1:  0.860376624<br />PC2: -2.974992e+00<br />cluster: 2","PC1: -0.269539893<br />PC2: -1.870880e+00<br />cluster: 2","PC1: -0.557212097<br />PC2: -1.652488e+00<br />cluster: 2","PC1:  1.328282912<br />PC2: -1.082762e+00<br />cluster: 2","PC1: -0.715016584<br />PC2: -3.356430e+00<br />cluster: 2","PC1:  0.637953011<br />PC2: -1.654030e+00<br />cluster: 2","PC1:  1.489896837<br />PC2: -2.088793e+00<br />cluster: 2","PC1:  2.156246999<br />PC2: -4.125375e+00<br />cluster: 2","PC1:  0.129930896<br />PC2: -2.483307e+00<br />cluster: 2","PC1:  0.189436314<br />PC2: -2.885967e+00<br />cluster: 2","PC1:  1.963376253<br />PC2: -1.371372e+00<br />cluster: 2","PC1:  1.262634491<br />PC2: -2.284557e+00<br />cluster: 2","PC1:  1.271844159<br />PC2: -1.836169e+00<br />cluster: 2","PC1:  0.153074318<br />PC2: -2.420322e+00<br />cluster: 2","PC1:  0.252056285<br />PC2: -3.036100e-01<br />cluster: 2","PC1:  0.190631926<br />PC2: -3.257380e+00<br />cluster: 2","PC1:  1.810500100<br />PC2: -2.413110e+00<br />cluster: 2","PC1:  1.021465001<br />PC2: -1.476422e+00<br />cluster: 2","PC1:  1.269167077<br />PC2: -1.873445e+00<br />cluster: 2","PC1: -0.990122322<br />PC2: -2.353044e+00<br />cluster: 2","PC1:  0.672106186<br />PC2: -2.490205e-01<br />cluster: 2","PC1: -1.193861316<br />PC2: -3.069141e+00<br />cluster: 2","PC1:  0.200396594<br />PC2: -8.032158e-01<br />cluster: 2","PC1:  0.612244358<br />PC2: -7.164787e-01<br />cluster: 2","PC1:  2.669662525<br />PC2: -3.682827e+00<br />cluster: 2","PC1:  1.495442021<br />PC2: -5.219487e-01<br />cluster: 2","PC1:  0.510813670<br />PC2: -2.695106e+00<br />cluster: 2","PC1: -0.543876068<br />PC2: -6.664072e-01<br />cluster: 2","PC1:  0.252378175<br />PC2: -6.906205e-01<br />cluster: 2","PC1: -0.299382228<br />PC2: -7.462773e-01<br />cluster: 2","PC1: -0.098600948<br />PC2: -9.167443e-01<br />cluster: 2","PC1: -0.160998698<br />PC2: -7.049431e-01<br />cluster: 2","PC1: -0.675390001<br />PC2: -3.142897e+00<br />cluster: 2","PC1:  1.489858534<br />PC2: -2.034774e+00<br />cluster: 2","PC1:  0.938000428<br />PC2: -1.015697e+00<br />cluster: 2","PC1:  0.114807468<br />PC2: -6.890188e-01<br />cluster: 2","PC1:  1.036136428<br />PC2: -9.708579e-01<br />cluster: 2","PC1:  0.207864241<br />PC2: -1.787709e+00<br />cluster: 2","PC1:  1.712379759<br />PC2: -1.467511e+00<br />cluster: 2","PC1:  1.877672011<br />PC2: -5.608345e-01<br />cluster: 2","PC1:  2.217170429<br />PC2: -8.552089e-01<br />cluster: 2","PC1:  0.302508961<br />PC2: -1.427638e+00<br />cluster: 2","PC1:  0.556240458<br />PC2: -8.224623e-01<br />cluster: 2","PC1:  1.623417821<br />PC2: -1.629683e+00<br />cluster: 2","PC1:  1.169999536<br />PC2: -4.555284e+00<br />cluster: 2","PC1:  1.005955136<br />PC2: -1.051734e+00<br />cluster: 2","PC1:  1.048794119<br />PC2: -1.605079e+00<br />cluster: 2","PC1:  1.820421277<br />PC2: -1.814237e+00<br />cluster: 2","PC1:  0.065678584<br />PC2: -1.622020e+00<br />cluster: 2","PC1:  1.592849856<br />PC2: -2.205752e+00<br />cluster: 2","PC1:  0.110670211<br />PC2: -2.012485e+00<br />cluster: 2","PC1:  2.326749800<br />PC2: -2.044449e+00<br />cluster: 2","PC1:  1.475062660<br />PC2: -1.506384e+00<br />cluster: 2","PC1:  0.412302070<br />PC2: -6.051943e-01<br />cluster: 2","PC1:  2.214244956<br />PC2: -2.081847e+00<br />cluster: 2","PC1:  1.752958519<br />PC2: -2.322411e+00<br />cluster: 2","PC1:  0.484532759<br />PC2: -1.682901e+00<br />cluster: 2","PC1:  0.087127273<br />PC2: -1.194812e+00<br />cluster: 2","PC1:  1.847161615<br />PC2:  1.634470e-01<br />cluster: 2","PC1:  1.940115170<br />PC2: -9.522440e-01<br />cluster: 2","PC1:  0.656049822<br />PC2:  7.982808e-02<br />cluster: 2","PC1: -0.248437883<br />PC2: -6.303847e-01<br />cluster: 2","PC1:  1.739555008<br />PC2:  1.166791e-01<br />cluster: 2","PC1:  1.198510044<br />PC2: -5.689959e-01<br />cluster: 2","PC1: -1.026671872<br />PC2: -1.973189e+00<br />cluster: 2","PC1:  0.716607312<br />PC2: -1.201620e+00<br />cluster: 2","PC1:  1.002036752<br />PC2: -1.832899e-01<br />cluster: 2","PC1:  1.606210146<br />PC2: -7.510357e-01<br />cluster: 2","PC1:  2.564040472<br />PC2: -1.767716e-01<br />cluster: 2","PC1:  2.131205252<br />PC2: -8.351795e-01<br />cluster: 2","PC1:  0.243676739<br />PC2:  1.187785e-01<br />cluster: 2","PC1:  1.651080296<br />PC2: -5.248758e-01<br />cluster: 2","PC1: -0.304430772<br />PC2: -1.318753e+00<br />cluster: 2","PC1:  0.794348382<br />PC2: -1.008891e+00<br />cluster: 2","PC1: -0.318090066<br />PC2: -2.218684e+00<br />cluster: 2","PC1: -0.332256391<br />PC2: -9.034618e-01<br />cluster: 2","PC1:  2.007609948<br />PC2: -4.790782e-01<br />cluster: 2","PC1:  0.801736461<br />PC2: -1.342532e+00<br />cluster: 2","PC1:  1.300066157<br />PC2: -5.310204e-01<br />cluster: 2","PC1:  0.423483740<br />PC2: -1.073520e+00<br />cluster: 2","PC1: -1.082703152<br />PC2: -2.753119e+00<br />cluster: 2","PC1:  0.723070988<br />PC2: -3.940577e+00<br />cluster: 2","PC1:  0.692803443<br />PC2: -4.075369e+00<br />cluster: 2","PC1:  1.223022015<br />PC2: -4.452180e-01<br />cluster: 2","PC1:  1.861225456<br />PC2: -3.386676e-01<br />cluster: 2","PC1:  1.643618035<br />PC2: -1.377975e+00<br />cluster: 2","PC1:  1.066097510<br />PC2: -9.853642e-01<br />cluster: 2","PC1:  2.222063596<br />PC2: -1.784308e+00<br />cluster: 2","PC1:  1.049500994<br />PC2: -8.176779e-01<br />cluster: 2","PC1:  0.905640085<br />PC2: -1.003248e+00<br />cluster: 2","PC1:  2.041246993<br />PC2: -4.899492e-01<br />cluster: 2","PC1:  0.717718899<br />PC2: -6.717369e-01<br />cluster: 2","PC1: -0.568237365<br />PC2: -1.997462e+00<br />cluster: 2","PC1:  1.001082860<br />PC2:  3.775153e-01<br />cluster: 2","PC1:  1.301051999<br />PC2: -7.415469e-01<br />cluster: 2","PC1:  0.115459573<br />PC2: -3.291596e-01<br />cluster: 2","PC1:  2.785618130<br />PC2: -2.140715e+00<br />cluster: 2","PC1:  0.570004391<br />PC2: -1.555523e+00<br />cluster: 2","PC1:  0.460097812<br />PC2: -6.370577e-01<br />cluster: 2","PC1:  0.136003252<br />PC2: -2.529926e-01<br />cluster: 2","PC1:  1.631113001<br />PC2: -4.108299e-01<br />cluster: 2","PC1: -0.757760888<br />PC2: -9.098925e-01<br />cluster: 2","PC1: -0.116066827<br />PC2: -2.852254e+00<br />cluster: 2","PC1: -0.395465890<br />PC2: -7.487579e-01<br />cluster: 2","PC1:  1.627990399<br />PC2: -7.272611e-01<br />cluster: 2","PC1: -0.678325456<br />PC2: -1.110486e+00<br />cluster: 2","PC1:  1.160174775<br />PC2:  1.345280e-01<br />cluster: 2","PC1: -0.547418726<br />PC2: -8.358023e-01<br />cluster: 2","PC1:  0.286754658<br />PC2: -6.773433e-01<br />cluster: 2","PC1:  1.506483188<br />PC2: -5.212179e-01<br />cluster: 2","PC1: -0.049972969<br />PC2: -9.699993e-01<br />cluster: 2","PC1: -0.047536730<br />PC2: -6.693447e-01<br />cluster: 2","PC1:  0.085875606<br />PC2: -1.082357e+00<br />cluster: 2","PC1:  0.484857143<br />PC2: -6.399164e-01<br />cluster: 2","PC1: -1.021083128<br />PC2: -2.597384e+00<br />cluster: 2","PC1:  1.719074367<br />PC2:  2.048973e-01<br />cluster: 2","PC1:  2.189168991<br />PC2: -7.342893e-01<br />cluster: 2","PC1:  1.699232081<br />PC2: -2.622125e-01<br />cluster: 2","PC1:  0.150988783<br />PC2: -1.182269e+00<br />cluster: 2","PC1:  0.374962354<br />PC2: -2.339499e-01<br />cluster: 2","PC1:  2.383287259<br />PC2: -6.364309e-01<br />cluster: 2","PC1:  0.577757729<br />PC2: -1.328498e+00<br />cluster: 2","PC1:  1.774511900<br />PC2: -1.085337e+00<br />cluster: 2","PC1:  0.950629685<br />PC2: -2.347634e-01<br />cluster: 2","PC1:  1.451026068<br />PC2: -9.573797e-01<br />cluster: 2","PC1:  1.237474339<br />PC2: -4.544295e-01<br />cluster: 2","PC1:  1.070453547<br />PC2: -1.912399e+00<br />cluster: 2","PC1:  1.953885382<br />PC2: -4.135268e-01<br />cluster: 2","PC1:  0.826621197<br />PC2: -5.652723e-01<br />cluster: 2","PC1:  1.124575382<br />PC2: -8.771203e-01<br />cluster: 2","PC1:  1.302776019<br />PC2: -1.002225e+00<br />cluster: 2","PC1:  0.097410885<br />PC2: -1.594251e+00<br />cluster: 2","PC1:  0.349571248<br />PC2:  9.169542e-03<br />cluster: 2","PC1:  0.528776337<br />PC2: -1.195765e+00<br />cluster: 2","PC1:  1.024673125<br />PC2: -2.272715e-01<br />cluster: 2","PC1: -0.514769732<br />PC2: -5.499917e-01<br />cluster: 2","PC1:  0.412504804<br />PC2: -4.093033e-01<br />cluster: 2","PC1:  2.151031162<br />PC2: -3.587132e-01<br />cluster: 2","PC1:  1.336965876<br />PC2: -5.706681e-01<br />cluster: 2","PC1:  1.747801255<br />PC2: -1.151016e+00<br />cluster: 2","PC1:  0.523136365<br />PC2: -3.523673e-01<br />cluster: 2","PC1:  0.664541990<br />PC2: -7.838328e-01<br />cluster: 2","PC1: -1.044635106<br />PC2: -1.292636e+00<br />cluster: 2","PC1:  1.368116327<br />PC2: -7.931432e-01<br />cluster: 2","PC1: -0.891149924<br />PC2: -2.309022e+00<br />cluster: 2","PC1:  1.128646758<br />PC2: -2.306690e-01<br />cluster: 2","PC1:  0.082369959<br />PC2: -1.056618e+00<br />cluster: 2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.5865314296849,-2.21919871848291,-4.12146358584266,-3.34339099837256,-3.36687304628142,-1.85117311698637,-2.58663121010511,-1.67407799300955,-1.54212090070218,-0.924819322627334,-0.910387254825178,-2.03868950651881,-1.42373358795727,-1.23502456695732,-0.80582016186064,-2.91190210985879,-1.77156629265127,-0.815332692712486,-1.0685856233302,-0.944894886807509,-2.66376672941178,-1.94314344508127,-1.08809318169219,-1.2129309112865,-1.51109867816427,-0.56506621343447,-1.54220568736177,-0.802021999845669,-3.03239798667451,-2.17445169223992,-1.7453176634549,-0.859818711763214,-1.89679423368285,-2.58934108568487,-3.92003383731553,-3.33970762469627,-2.04292531128089,-0.876988177366967,-1.18578910367488,-2.33182521175347,-2.1063482892836,-2.1303334853762,-0.649474697432285,-2.54629634803869,-1.67614617460114,-2.30313337551281,-2.34675253334618,-3.12796329507463,-1.59596461745531,-1.40789088348338,-1.71329191857072,-1.74973995089566,-2.0526886514657,-0.996050187946439,-1.60718793811404,-1.24644520599138,-0.313374616254057,-0.501076088509446,-1.93774701896384,-3.2807365888182,-2.91894137907293,-1.45069580450064,-1.24439570167203,-0.613283027926989,-2.55717688797986,-0.915135311196977,-1.18639116176698,-2.90597519663283,-1.12254496993578,-1.5233841514867,-3.81983109834203,-4.30637424817696,-2.53979571934102,-2.75177412668388,-2.44819914232325,-2.32788666323939,-1.23535254817133,-0.539838600058837,-0.365447472056001,-1.1305122684758,-1.17689536668475,-1.17199273838322,-2.20736108689445,-1.03099223797816,-0.999584708169941,-2.01264751424635,-0.777294901081354,-1.24402678410411,0.0970687776976365,-0.729081297225045,-1.79794154998527,-2.74134448543138,-5.11788912886444,-1.5846635847047,-3.46653940956054,-2.99917645100294,-3.46772084046373,-0.317927945583386,-1.68712773478591,-2.17383890912684,-1.69321501742623,-1.50439190874216,-1.18166509825957,-1.67652090514045,-3.81193761111665,-1.97661663843651,-0.875392522065925,-2.3599670112869,-3.69671445573594,-1.15067110703962,-1.61364876639842,-2.76676856158918,-2.46533678744243,-1.02167500246595,-1.86248510729815,-1.37349746420433,-0.40100089315245,-3.37745293935569,-4.12799835783342,-3.83028270236586,-3.66816199380257,-3.91455621537446,-3.75958578123101,-2.47152351009405,-3.01942368043813,-2.45819259588887,-3.03240701156122,-2.9558937781925,-3.80864896767328,-1.59427134282424,-1.04013337460865,-1.86230996958127,-1.51586559760069,-0.144612571268179,-0.787847757107364,-1.93417878525962,-2.41444858009638,-1.45686671384242,-0.138598001274864,-0.70550090155901,-1.66919851599191,-1.45957931538601,0.435013276269948,0.202658465149308,-2.99772728746474,-2.03082257900894,-2.91788413292649,-4.02379952813767,-4.84461817684004,-2.797126802836,-2.05016398825786,-4.63395924922648,-4.10836386231373,-4.65616370046259,-4.0436419872779,-1.45453252924611,-0.925151359725533,-0.725870048819882,-0.875543013570349,-1.543832336012,-1.91831947922346,-1.5620457705108,-1.38302378019112,-1.96032838851005,-2.10170134770889,-3.95395755052016,-3.35304794998114,-4.23276189181329,-4.86716909814925,-1.48765881238519,-2.09540892221861,-2.88756192647595],"y":[2.46485340999424,1.42750540693761,0.151079675170162,1.28332282682786,1.11462416009454,1.37218619502599,2.67927052926363,0.593867013082665,-0.0906921114056848,-1.30556846818812,-1.31527900384391,1.39749913839241,0.248777164934832,0.0243845972935561,0.636355390258497,0.493227683616956,0.100056248322054,-0.664876887780397,-0.719639278300077,-1.56400085722745,0.287836061567058,-1.63762323062952,-0.580609226748872,-2.201962648853,-0.34790322191162,0.443181967165157,-1.24616373735569,-0.198791005982113,-0.259669950441104,-0.663216511000424,0.427014483643739,1.33835933768921,0.922002472148488,-0.339626090600549,0.178985815973176,-0.676105471562967,-0.820109123128434,0.68522572045461,0.300870415607027,-0.270655410604776,0.202197822724962,0.55001001280426,0.378725693743984,0.710307524016738,-0.430692871566125,0.378836027367639,-0.964573125157921,-0.491917911772403,-0.769749948488683,-0.0685654150836581,0.594766298108513,-0.417193825647054,-0.666042901591822,-0.103582304953197,-0.698735437803691,-0.453002667248707,-0.731596689331377,-1.72153756212783,-1.09045685686561,-1.17669946495882,-0.805741902847554,-1.37290975498204,-1.2759859620159,-1.78879785547332,-1.97350056623805,-0.988773089121514,-2.21535994354301,-2.33990177244965,-0.972097814103425,-1.30638260454093,-1.48808402486473,-0.479464298039761,-0.819827400748875,-0.654709285950001,-1.00080980246368,0.206177299606864,-1.23612262305392,-1.11720171735018,-2.17097581973418,-3.81877775865988,0.628510255731652,0.517817748775364,0.534325379055311,2.06129240916386,1.03515349692346,0.446056209398361,0.415636584079471,-0.0833770695960345,1.20889915896481,-0.579293473419349,-1.35397408300154,-1.86150126594502,-0.933872506247233,-0.820404974440951,0.49330169237841,-0.258150740580542,-0.877692432025995,0.764465102605975,0.592150282592977,-1.22505855719429,1.48495705431582,0.133389642784968,1.09134695494492,-2.5470858774169,0.422370212452682,1.2996107920204,1.16668566964694,-0.88948947802742,-2.43258474692643,0.453110143928889,-0.915542555588421,-0.357108960832796,-0.672238619733125,0.0720693692962106,-0.201912572451408,-0.784951651499395,-1.58502598290451,-0.827760657365098,-0.330333516898872,-1.45702244230557,-0.544293538914806,0.524565867397237,-0.315055322270668,1.46541407803269,1.87198056936852,1.61195954213733,0.991021161668771,1.21348289309257,-2.18608987734522,0.0920187601473377,0.356466756712307,0.967143844662406,1.32741344162881,1.01021856125145,1.20312805563299,0.527618384444181,0.0687208780403898,0.697730802439104,1.2086067517638,0.69338602986917,0.203798883716227,0.0285235301151113,1.62211228342689,1.05907010230452,-2.9104688197898,1.12068429808517,0.376328266775653,0.642123919896906,0.12416291770241,-0.245258900934633,0.253540522065322,-0.100556537076367,0.175670632753371,-0.784248421326581,0.314500715035441,0.765354319193907,0.612689122497903,2.45243346399688,2.38048971974625,0.250481648454894,0.568397911373114,0.323057706319115,-0.839431576030806,-1.37310198707552,0.648025019030441,0.235743258003908,0.0306151343256272,-0.240947540198865,-1.26641293900299,2.39878776072809,1.81720317791839,0.733038510147169],"text":["PC1: -1.586531430<br />PC2:  2.464853e+00<br />cluster: 3","PC1: -2.219198718<br />PC2:  1.427505e+00<br />cluster: 3","PC1: -4.121463586<br />PC2:  1.510797e-01<br />cluster: 3","PC1: -3.343390998<br />PC2:  1.283323e+00<br />cluster: 3","PC1: -3.366873046<br />PC2:  1.114624e+00<br />cluster: 3","PC1: -1.851173117<br />PC2:  1.372186e+00<br />cluster: 3","PC1: -2.586631210<br />PC2:  2.679271e+00<br />cluster: 3","PC1: -1.674077993<br />PC2:  5.938670e-01<br />cluster: 3","PC1: -1.542120901<br />PC2: -9.069211e-02<br />cluster: 3","PC1: -0.924819323<br />PC2: -1.305568e+00<br />cluster: 3","PC1: -0.910387255<br />PC2: -1.315279e+00<br />cluster: 3","PC1: -2.038689507<br />PC2:  1.397499e+00<br />cluster: 3","PC1: -1.423733588<br />PC2:  2.487772e-01<br />cluster: 3","PC1: -1.235024567<br />PC2:  2.438460e-02<br />cluster: 3","PC1: -0.805820162<br />PC2:  6.363554e-01<br />cluster: 3","PC1: -2.911902110<br />PC2:  4.932277e-01<br />cluster: 3","PC1: -1.771566293<br />PC2:  1.000562e-01<br />cluster: 3","PC1: -0.815332693<br />PC2: -6.648769e-01<br />cluster: 3","PC1: -1.068585623<br />PC2: -7.196393e-01<br />cluster: 3","PC1: -0.944894887<br />PC2: -1.564001e+00<br />cluster: 3","PC1: -2.663766729<br />PC2:  2.878361e-01<br />cluster: 3","PC1: -1.943143445<br />PC2: -1.637623e+00<br />cluster: 3","PC1: -1.088093182<br />PC2: -5.806092e-01<br />cluster: 3","PC1: -1.212930911<br />PC2: -2.201963e+00<br />cluster: 3","PC1: -1.511098678<br />PC2: -3.479032e-01<br />cluster: 3","PC1: -0.565066213<br />PC2:  4.431820e-01<br />cluster: 3","PC1: -1.542205687<br />PC2: -1.246164e+00<br />cluster: 3","PC1: -0.802022000<br />PC2: -1.987910e-01<br />cluster: 3","PC1: -3.032397987<br />PC2: -2.596700e-01<br />cluster: 3","PC1: -2.174451692<br />PC2: -6.632165e-01<br />cluster: 3","PC1: -1.745317663<br />PC2:  4.270145e-01<br />cluster: 3","PC1: -0.859818712<br />PC2:  1.338359e+00<br />cluster: 3","PC1: -1.896794234<br />PC2:  9.220025e-01<br />cluster: 3","PC1: -2.589341086<br />PC2: -3.396261e-01<br />cluster: 3","PC1: -3.920033837<br />PC2:  1.789858e-01<br />cluster: 3","PC1: -3.339707625<br />PC2: -6.761055e-01<br />cluster: 3","PC1: -2.042925311<br />PC2: -8.201091e-01<br />cluster: 3","PC1: -0.876988177<br />PC2:  6.852257e-01<br />cluster: 3","PC1: -1.185789104<br />PC2:  3.008704e-01<br />cluster: 3","PC1: -2.331825212<br />PC2: -2.706554e-01<br />cluster: 3","PC1: -2.106348289<br />PC2:  2.021978e-01<br />cluster: 3","PC1: -2.130333485<br />PC2:  5.500100e-01<br />cluster: 3","PC1: -0.649474697<br />PC2:  3.787257e-01<br />cluster: 3","PC1: -2.546296348<br />PC2:  7.103075e-01<br />cluster: 3","PC1: -1.676146175<br />PC2: -4.306929e-01<br />cluster: 3","PC1: -2.303133376<br />PC2:  3.788360e-01<br />cluster: 3","PC1: -2.346752533<br />PC2: -9.645731e-01<br />cluster: 3","PC1: -3.127963295<br />PC2: -4.919179e-01<br />cluster: 3","PC1: -1.595964617<br />PC2: -7.697499e-01<br />cluster: 3","PC1: -1.407890883<br />PC2: -6.856542e-02<br />cluster: 3","PC1: -1.713291919<br />PC2:  5.947663e-01<br />cluster: 3","PC1: -1.749739951<br />PC2: -4.171938e-01<br />cluster: 3","PC1: -2.052688651<br />PC2: -6.660429e-01<br />cluster: 3","PC1: -0.996050188<br />PC2: -1.035823e-01<br />cluster: 3","PC1: -1.607187938<br />PC2: -6.987354e-01<br />cluster: 3","PC1: -1.246445206<br />PC2: -4.530027e-01<br />cluster: 3","PC1: -0.313374616<br />PC2: -7.315967e-01<br />cluster: 3","PC1: -0.501076089<br />PC2: -1.721538e+00<br />cluster: 3","PC1: -1.937747019<br />PC2: -1.090457e+00<br />cluster: 3","PC1: -3.280736589<br />PC2: -1.176699e+00<br />cluster: 3","PC1: -2.918941379<br />PC2: -8.057419e-01<br />cluster: 3","PC1: -1.450695805<br />PC2: -1.372910e+00<br />cluster: 3","PC1: -1.244395702<br />PC2: -1.275986e+00<br />cluster: 3","PC1: -0.613283028<br />PC2: -1.788798e+00<br />cluster: 3","PC1: -2.557176888<br />PC2: -1.973501e+00<br />cluster: 3","PC1: -0.915135311<br />PC2: -9.887731e-01<br />cluster: 3","PC1: -1.186391162<br />PC2: -2.215360e+00<br />cluster: 3","PC1: -2.905975197<br />PC2: -2.339902e+00<br />cluster: 3","PC1: -1.122544970<br />PC2: -9.720978e-01<br />cluster: 3","PC1: -1.523384151<br />PC2: -1.306383e+00<br />cluster: 3","PC1: -3.819831098<br />PC2: -1.488084e+00<br />cluster: 3","PC1: -4.306374248<br />PC2: -4.794643e-01<br />cluster: 3","PC1: -2.539795719<br />PC2: -8.198274e-01<br />cluster: 3","PC1: -2.751774127<br />PC2: -6.547093e-01<br />cluster: 3","PC1: -2.448199142<br />PC2: -1.000810e+00<br />cluster: 3","PC1: -2.327886663<br />PC2:  2.061773e-01<br />cluster: 3","PC1: -1.235352548<br />PC2: -1.236123e+00<br />cluster: 3","PC1: -0.539838600<br />PC2: -1.117202e+00<br />cluster: 3","PC1: -0.365447472<br />PC2: -2.170976e+00<br />cluster: 3","PC1: -1.130512268<br />PC2: -3.818778e+00<br />cluster: 3","PC1: -1.176895367<br />PC2:  6.285103e-01<br />cluster: 3","PC1: -1.171992738<br />PC2:  5.178177e-01<br />cluster: 3","PC1: -2.207361087<br />PC2:  5.343254e-01<br />cluster: 3","PC1: -1.030992238<br />PC2:  2.061292e+00<br />cluster: 3","PC1: -0.999584708<br />PC2:  1.035153e+00<br />cluster: 3","PC1: -2.012647514<br />PC2:  4.460562e-01<br />cluster: 3","PC1: -0.777294901<br />PC2:  4.156366e-01<br />cluster: 3","PC1: -1.244026784<br />PC2: -8.337707e-02<br />cluster: 3","PC1:  0.097068778<br />PC2:  1.208899e+00<br />cluster: 3","PC1: -0.729081297<br />PC2: -5.792935e-01<br />cluster: 3","PC1: -1.797941550<br />PC2: -1.353974e+00<br />cluster: 3","PC1: -2.741344485<br />PC2: -1.861501e+00<br />cluster: 3","PC1: -5.117889129<br />PC2: -9.338725e-01<br />cluster: 3","PC1: -1.584663585<br />PC2: -8.204050e-01<br />cluster: 3","PC1: -3.466539410<br />PC2:  4.933017e-01<br />cluster: 3","PC1: -2.999176451<br />PC2: -2.581507e-01<br />cluster: 3","PC1: -3.467720840<br />PC2: -8.776924e-01<br />cluster: 3","PC1: -0.317927946<br />PC2:  7.644651e-01<br />cluster: 3","PC1: -1.687127735<br />PC2:  5.921503e-01<br />cluster: 3","PC1: -2.173838909<br />PC2: -1.225059e+00<br />cluster: 3","PC1: -1.693215017<br />PC2:  1.484957e+00<br />cluster: 3","PC1: -1.504391909<br />PC2:  1.333896e-01<br />cluster: 3","PC1: -1.181665098<br />PC2:  1.091347e+00<br />cluster: 3","PC1: -1.676520905<br />PC2: -2.547086e+00<br />cluster: 3","PC1: -3.811937611<br />PC2:  4.223702e-01<br />cluster: 3","PC1: -1.976616638<br />PC2:  1.299611e+00<br />cluster: 3","PC1: -0.875392522<br />PC2:  1.166686e+00<br />cluster: 3","PC1: -2.359967011<br />PC2: -8.894895e-01<br />cluster: 3","PC1: -3.696714456<br />PC2: -2.432585e+00<br />cluster: 3","PC1: -1.150671107<br />PC2:  4.531101e-01<br />cluster: 3","PC1: -1.613648766<br />PC2: -9.155426e-01<br />cluster: 3","PC1: -2.766768562<br />PC2: -3.571090e-01<br />cluster: 3","PC1: -2.465336787<br />PC2: -6.722386e-01<br />cluster: 3","PC1: -1.021675002<br />PC2:  7.206937e-02<br />cluster: 3","PC1: -1.862485107<br />PC2: -2.019126e-01<br />cluster: 3","PC1: -1.373497464<br />PC2: -7.849517e-01<br />cluster: 3","PC1: -0.401000893<br />PC2: -1.585026e+00<br />cluster: 3","PC1: -3.377452939<br />PC2: -8.277607e-01<br />cluster: 3","PC1: -4.127998358<br />PC2: -3.303335e-01<br />cluster: 3","PC1: -3.830282702<br />PC2: -1.457022e+00<br />cluster: 3","PC1: -3.668161994<br />PC2: -5.442935e-01<br />cluster: 3","PC1: -3.914556215<br />PC2:  5.245659e-01<br />cluster: 3","PC1: -3.759585781<br />PC2: -3.150553e-01<br />cluster: 3","PC1: -2.471523510<br />PC2:  1.465414e+00<br />cluster: 3","PC1: -3.019423680<br />PC2:  1.871981e+00<br />cluster: 3","PC1: -2.458192596<br />PC2:  1.611960e+00<br />cluster: 3","PC1: -3.032407012<br />PC2:  9.910212e-01<br />cluster: 3","PC1: -2.955893778<br />PC2:  1.213483e+00<br />cluster: 3","PC1: -3.808648968<br />PC2: -2.186090e+00<br />cluster: 3","PC1: -1.594271343<br />PC2:  9.201876e-02<br />cluster: 3","PC1: -1.040133375<br />PC2:  3.564668e-01<br />cluster: 3","PC1: -1.862309970<br />PC2:  9.671438e-01<br />cluster: 3","PC1: -1.515865598<br />PC2:  1.327413e+00<br />cluster: 3","PC1: -0.144612571<br />PC2:  1.010219e+00<br />cluster: 3","PC1: -0.787847757<br />PC2:  1.203128e+00<br />cluster: 3","PC1: -1.934178785<br />PC2:  5.276184e-01<br />cluster: 3","PC1: -2.414448580<br />PC2:  6.872088e-02<br />cluster: 3","PC1: -1.456866714<br />PC2:  6.977308e-01<br />cluster: 3","PC1: -0.138598001<br />PC2:  1.208607e+00<br />cluster: 3","PC1: -0.705500902<br />PC2:  6.933860e-01<br />cluster: 3","PC1: -1.669198516<br />PC2:  2.037989e-01<br />cluster: 3","PC1: -1.459579315<br />PC2:  2.852353e-02<br />cluster: 3","PC1:  0.435013276<br />PC2:  1.622112e+00<br />cluster: 3","PC1:  0.202658465<br />PC2:  1.059070e+00<br />cluster: 3","PC1: -2.997727287<br />PC2: -2.910469e+00<br />cluster: 3","PC1: -2.030822579<br />PC2:  1.120684e+00<br />cluster: 3","PC1: -2.917884133<br />PC2:  3.763283e-01<br />cluster: 3","PC1: -4.023799528<br />PC2:  6.421239e-01<br />cluster: 3","PC1: -4.844618177<br />PC2:  1.241629e-01<br />cluster: 3","PC1: -2.797126803<br />PC2: -2.452589e-01<br />cluster: 3","PC1: -2.050163988<br />PC2:  2.535405e-01<br />cluster: 3","PC1: -4.633959249<br />PC2: -1.005565e-01<br />cluster: 3","PC1: -4.108363862<br />PC2:  1.756706e-01<br />cluster: 3","PC1: -4.656163700<br />PC2: -7.842484e-01<br />cluster: 3","PC1: -4.043641987<br />PC2:  3.145007e-01<br />cluster: 3","PC1: -1.454532529<br />PC2:  7.653543e-01<br />cluster: 3","PC1: -0.925151360<br />PC2:  6.126891e-01<br />cluster: 3","PC1: -0.725870049<br />PC2:  2.452433e+00<br />cluster: 3","PC1: -0.875543014<br />PC2:  2.380490e+00<br />cluster: 3","PC1: -1.543832336<br />PC2:  2.504816e-01<br />cluster: 3","PC1: -1.918319479<br />PC2:  5.683979e-01<br />cluster: 3","PC1: -1.562045771<br />PC2:  3.230577e-01<br />cluster: 3","PC1: -1.383023780<br />PC2: -8.394316e-01<br />cluster: 3","PC1: -1.960328389<br />PC2: -1.373102e+00<br />cluster: 3","PC1: -2.101701348<br />PC2:  6.480250e-01<br />cluster: 3","PC1: -3.953957551<br />PC2:  2.357433e-01<br />cluster: 3","PC1: -3.353047950<br />PC2:  3.061513e-02<br />cluster: 3","PC1: -4.232761892<br />PC2: -2.409475e-01<br />cluster: 3","PC1: -4.867169098<br />PC2: -1.266413e+00<br />cluster: 3","PC1: -1.487658812<br />PC2:  2.398788e+00<br />cluster: 3","PC1: -2.095408922<br />PC2:  1.817203e+00<br />cluster: 3","PC1: -2.887561926<br />PC2:  7.330385e-01<br />cluster: 3"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2.4126505426315,2.37289964457661,2.87649075646543,1.79941103270071,2.36473230626234,3.73113825498611,2.75652114790998,1.47870670882462,2.75866306756438,2.1281878053462,2.66880944724775,3.2940019971935,2.75990096142708,3.23722600310603,2.31675601164295,3.41003837082881,3.23921932057441,2.80307621623925,1.15562665264146,1.11975512488442,1.78768820973315,5.20044388216701,6.90939384886755,4.64422520519589,3.3340818399877,3.4282940673402,6.34307142513501,2.64867910883317,2.03887522030325,3.13183276915892,3.59152831082777,3.62050650296205,4.3109678809554,3.68338624019852,3.65304016034542,2.91245777818459,3.02166061020328,5.19155244859147,3.98217954215686,3.92035531755418,1.28211454779174,3.1969944624691,3.82689127252526,2.04490670526105,2.45931289065935,5.0911450302814,3.23272849140702,2.36022140355715,2.56070347762622,3.64837769525642,5.21998095487498,2.27427257009218,3.58887156504066,5.05629471441073,2.49754256428011,2.34877585378725,4.03399452704072,3.33196617141353,4.48568141788225,3.54460311457616,4.01519590270714,3.28394484423689,2.87210816763785,2.35269385875853,2.70211874344027,2.47846341797178,2.96171595218055,2.30786825291481,3.32628297832971,2.25315455535727,2.75696109072648,4.08823615491815,2.52423168584702,2.52696628436454,3.52927947620778,3.32081717794149,3.46863592601391,2.83145916499326,2.81117263697325,4.7436955835243,4.23781714545715,3.23736105381513,4.05910181756789],"y":[-2.87593047398229,-3.05094639072247,-3.59206769657938,-2.63973327422586,-2.48344709041287,-2.4408804053149,-2.11778630785066,-2.46270317523015,-4.64475973605588,-2.67783349361897,-1.81044097107888,-2.3748324912877,-2.550350921528,-1.07333232552533,-1.2976321548887,-1.07839347915746,-1.97407554483367,-1.3496200663514,-3.34657819808891,-3.70287591684646,-3.30168483290321,-1.55940156254684,-4.14536040202906,-1.01480294706728,-1.38780493627922,-2.26366665118223,-4.73589090779907,-1.70320898858656,-2.14551574836373,-0.527476016561684,-0.279886471298076,-0.910294848997277,-1.13071225308499,-3.4920219821694,-0.86774225487819,-2.02002643386152,-3.01693129957609,-3.48639352670334,-4.13499667979432,-0.949340501721259,-2.59608996415504,-2.67145755495979,-3.16113231437344,-2.91212839895742,-2.04448013668082,-4.25627728049844,-3.7172889510406,-2.98203890945184,-3.08401609169538,-1.07041906119075,-2.125664068894,-2.64747162004457,-4.07269652258613,-2.36269771132344,-2.55882450672778,-1.49442446719074,-0.607171507399177,-0.833966332118117,-1.33720813167355,-0.530495634724885,-1.47213636845184,-0.462986852171997,-0.220942921968893,-0.682197013332606,-1.23923939315535,-1.03470470745413,-2.4195803067219,-1.37963953037613,-0.460888919223269,-1.47226944986346,-0.783627193316978,-1.46252983904511,0.00578427004954771,-0.922795168244968,-1.39906054148217,-1.55376774635542,-0.864018897417995,-0.475136592716425,-2.10553694266261,-0.777452716220543,-0.910787144045511,-0.733603667314255,-1.07136889678816],"text":["PC1:  2.412650543<br />PC2: -2.875930e+00<br />cluster: 4","PC1:  2.372899645<br />PC2: -3.050946e+00<br />cluster: 4","PC1:  2.876490756<br />PC2: -3.592068e+00<br />cluster: 4","PC1:  1.799411033<br />PC2: -2.639733e+00<br />cluster: 4","PC1:  2.364732306<br />PC2: -2.483447e+00<br />cluster: 4","PC1:  3.731138255<br />PC2: -2.440880e+00<br />cluster: 4","PC1:  2.756521148<br />PC2: -2.117786e+00<br />cluster: 4","PC1:  1.478706709<br />PC2: -2.462703e+00<br />cluster: 4","PC1:  2.758663068<br />PC2: -4.644760e+00<br />cluster: 4","PC1:  2.128187805<br />PC2: -2.677833e+00<br />cluster: 4","PC1:  2.668809447<br />PC2: -1.810441e+00<br />cluster: 4","PC1:  3.294001997<br />PC2: -2.374832e+00<br />cluster: 4","PC1:  2.759900961<br />PC2: -2.550351e+00<br />cluster: 4","PC1:  3.237226003<br />PC2: -1.073332e+00<br />cluster: 4","PC1:  2.316756012<br />PC2: -1.297632e+00<br />cluster: 4","PC1:  3.410038371<br />PC2: -1.078393e+00<br />cluster: 4","PC1:  3.239219321<br />PC2: -1.974076e+00<br />cluster: 4","PC1:  2.803076216<br />PC2: -1.349620e+00<br />cluster: 4","PC1:  1.155626653<br />PC2: -3.346578e+00<br />cluster: 4","PC1:  1.119755125<br />PC2: -3.702876e+00<br />cluster: 4","PC1:  1.787688210<br />PC2: -3.301685e+00<br />cluster: 4","PC1:  5.200443882<br />PC2: -1.559402e+00<br />cluster: 4","PC1:  6.909393849<br />PC2: -4.145360e+00<br />cluster: 4","PC1:  4.644225205<br />PC2: -1.014803e+00<br />cluster: 4","PC1:  3.334081840<br />PC2: -1.387805e+00<br />cluster: 4","PC1:  3.428294067<br />PC2: -2.263667e+00<br />cluster: 4","PC1:  6.343071425<br />PC2: -4.735891e+00<br />cluster: 4","PC1:  2.648679109<br />PC2: -1.703209e+00<br />cluster: 4","PC1:  2.038875220<br />PC2: -2.145516e+00<br />cluster: 4","PC1:  3.131832769<br />PC2: -5.274760e-01<br />cluster: 4","PC1:  3.591528311<br />PC2: -2.798865e-01<br />cluster: 4","PC1:  3.620506503<br />PC2: -9.102948e-01<br />cluster: 4","PC1:  4.310967881<br />PC2: -1.130712e+00<br />cluster: 4","PC1:  3.683386240<br />PC2: -3.492022e+00<br />cluster: 4","PC1:  3.653040160<br />PC2: -8.677423e-01<br />cluster: 4","PC1:  2.912457778<br />PC2: -2.020026e+00<br />cluster: 4","PC1:  3.021660610<br />PC2: -3.016931e+00<br />cluster: 4","PC1:  5.191552449<br />PC2: -3.486394e+00<br />cluster: 4","PC1:  3.982179542<br />PC2: -4.134997e+00<br />cluster: 4","PC1:  3.920355318<br />PC2: -9.493405e-01<br />cluster: 4","PC1:  1.282114548<br />PC2: -2.596090e+00<br />cluster: 4","PC1:  3.196994462<br />PC2: -2.671458e+00<br />cluster: 4","PC1:  3.826891273<br />PC2: -3.161132e+00<br />cluster: 4","PC1:  2.044906705<br />PC2: -2.912128e+00<br />cluster: 4","PC1:  2.459312891<br />PC2: -2.044480e+00<br />cluster: 4","PC1:  5.091145030<br />PC2: -4.256277e+00<br />cluster: 4","PC1:  3.232728491<br />PC2: -3.717289e+00<br />cluster: 4","PC1:  2.360221404<br />PC2: -2.982039e+00<br />cluster: 4","PC1:  2.560703478<br />PC2: -3.084016e+00<br />cluster: 4","PC1:  3.648377695<br />PC2: -1.070419e+00<br />cluster: 4","PC1:  5.219980955<br />PC2: -2.125664e+00<br />cluster: 4","PC1:  2.274272570<br />PC2: -2.647472e+00<br />cluster: 4","PC1:  3.588871565<br />PC2: -4.072697e+00<br />cluster: 4","PC1:  5.056294714<br />PC2: -2.362698e+00<br />cluster: 4","PC1:  2.497542564<br />PC2: -2.558825e+00<br />cluster: 4","PC1:  2.348775854<br />PC2: -1.494424e+00<br />cluster: 4","PC1:  4.033994527<br />PC2: -6.071715e-01<br />cluster: 4","PC1:  3.331966171<br />PC2: -8.339663e-01<br />cluster: 4","PC1:  4.485681418<br />PC2: -1.337208e+00<br />cluster: 4","PC1:  3.544603115<br />PC2: -5.304956e-01<br />cluster: 4","PC1:  4.015195903<br />PC2: -1.472136e+00<br />cluster: 4","PC1:  3.283944844<br />PC2: -4.629869e-01<br />cluster: 4","PC1:  2.872108168<br />PC2: -2.209429e-01<br />cluster: 4","PC1:  2.352693859<br />PC2: -6.821970e-01<br />cluster: 4","PC1:  2.702118743<br />PC2: -1.239239e+00<br />cluster: 4","PC1:  2.478463418<br />PC2: -1.034705e+00<br />cluster: 4","PC1:  2.961715952<br />PC2: -2.419580e+00<br />cluster: 4","PC1:  2.307868253<br />PC2: -1.379640e+00<br />cluster: 4","PC1:  3.326282978<br />PC2: -4.608889e-01<br />cluster: 4","PC1:  2.253154555<br />PC2: -1.472269e+00<br />cluster: 4","PC1:  2.756961091<br />PC2: -7.836272e-01<br />cluster: 4","PC1:  4.088236155<br />PC2: -1.462530e+00<br />cluster: 4","PC1:  2.524231686<br />PC2:  5.784270e-03<br />cluster: 4","PC1:  2.526966284<br />PC2: -9.227952e-01<br />cluster: 4","PC1:  3.529279476<br />PC2: -1.399061e+00<br />cluster: 4","PC1:  3.320817178<br />PC2: -1.553768e+00<br />cluster: 4","PC1:  3.468635926<br />PC2: -8.640189e-01<br />cluster: 4","PC1:  2.831459165<br />PC2: -4.751366e-01<br />cluster: 4","PC1:  2.811172637<br />PC2: -2.105537e+00<br />cluster: 4","PC1:  4.743695584<br />PC2: -7.774527e-01<br />cluster: 4","PC1:  4.237817145<br />PC2: -9.107871e-01<br />cluster: 4","PC1:  3.237361054<br />PC2: -7.336037e-01<br />cluster: 4","PC1:  4.059101818<br />PC2: -1.071369e+00<br />cluster: 4"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.57036146677793,-2.38000981703091,-3.54996283060225,-2.97748136072212,-2.78253336058582,-1.44835678299925,-1.33476390280871,-3.01783624694526,-4.43103217426482,-1.94162230849625,-4.11012348635211,-2.58698652932263,-2.26794226518585,-2.18713553653026,-1.73247473363374,-3.54093628560508,-4.13852769071395,-1.51760457630862,-2.76970834239382,-2.2853665244782,-1.24305651030907,-4.08406494567206,-1.85526115499272,-2.19515582577583,-4.64958999298141,-3.36386074371938,-3.35928430576453,-2.20418096560037,-5.18451133428559,-0.98547682665889,-2.10690542917345,-1.87168401038426,-1.46490837876262,-2.14007516247956,-2.07248587307042,-2.63175104859746,-2.01775793618833,-1.47341710248499,-2.45515283536331,-1.85087775987724,-1.7128143354284,-2.91594935292826,-1.81803396637521,-1.65271793710583,-1.61540169455998,-2.18524116790452,-4.25270041152559,-1.9961327408887,-2.97129960500219,-5.90529534005334,-4.14995641211363,-2.86981792445569,-2.41471103487441,-4.17674275265746,-3.50172065797235,-1.64066184952811,-3.81569066975645,-2.57015314652522,-3.15504768650268,-3.88346285971393,-2.48187230368824,-2.42728491364828,-3.4930295664641,-1.90940402757524,-1.38488778514245,-1.81675576161816,-1.03000758672208,-1.52898535407247,-3.6388529469752,-3.46331332136059,-3.33112404863579,-3.09085215393753,-5.305020206854,-4.58034372836516,-5.55448700656991,-3.05589470206445,-1.62491257727737,-4.30454767988228,-2.84815275473188,-1.29881304113886,-2.97784323774407,-2.09240863855395,-1.88117201701908,-3.49436890099728,-1.5519113146182,-2.17017357789215,-2.04886036370259,-2.08892605453478,-1.93699569718202,-1.82925487778177,-2.03484515312814,-2.67779772415468,-2.27016618883504,-1.7343683282814,-3.10464446430252,-3.12852603524886,-1.16495877286695,-1.4171422212432,-3.24441728987586,-2.58981032320313,-2.2450832325483,-2.69148292551657,-3.11864918635133,-1.50755890759686,-5.16190004873613,-4.34145577965251,-3.42557798352601,-5.55740707066464,-4.06666346379578,-5.7409643129484,-1.42206974186309,-2.0477064011629,-2.41684978121822,-2.4623485177267,-2.07551878632442,-2.38920548726579,-2.31830393378733,-4.52850812872177,-3.87009458791973,-4.12202998320199,-3.6593097996014,-2.36138135204268,-1.54982180584666,-3.86258422204824,-2.68985058083157,-2.11774266148571,-2.83529231865026,-2.49809130173515,-1.74361654061365,-4.39131049955236,-3.24848452484992,-3.0156727088357,-2.49213956583892,-2.90061153771722,-3.33222094142313,-2.80720536612451,-1.91045816913083,-2.5769161709102,-3.71751369876559,-2.99856630990871,-0.756439228302262,-2.47549009844661,-2.4306057717304,-2.38559582466467,-1.86843231440661,-4.05992670611579,-3.03625374847139,-4.82623511483064,-4.15146188048068,-2.32810639344481,-3.17819738619624,-2.33628939723981,-2.67486553969775,-2.70939073384245,-2.03174120694243,-2.18221253707571,-2.44894796600699,-2.38191888298036,-2.83098359344734,-2.45029287206763,-2.75967032143054,-1.80178474801289,-2.79948233672881,-3.72902124158471,-2.92409511082874,-2.24215570472158,-3.28630173433395,-3.06826030671926,-3.42760506994977,-2.22420799029287,-4.67581694748426,-2.91412023333093,-1.84740210491644,-1.42082638402032,-2.18464710205475,-4.82733665386296,-4.5629189338406,-2.69568400681206,-2.75779606236858,-3.21029055455065,-2.824838868615,-2.87096504273185,-1.54650431260032,-2.14078596308881,-2.62908782805127,-1.79915189890186,-2.89115683623007,-3.76432885021372,-1.43051366921894,-4.84065238987341,-3.4956472566897,-2.40869217044801,-1.59813500583662,-1.66583370743644,-4.08014109946506,-2.25272935582991,-2.35264515316213,-1.59715656456712,-2.33527797648425,-2.96193469875469,-2.55701715954026,-2.44188281552726,-2.9631233568749,-2.32695713939008,-1.90987325418619,-2.04151017486313,-2.04710088453651,-2.708635245911,-2.47785164731763,-2.34185614109456,-2.50537298375299,-2.21559969985979,-4.34251128741001,-3.14958735169838,-4.60532840604151,-3.31947568575681,-3.52284276455075,-4.02064222834355,-4.04254053155575,-1.95839245334191,-4.40652070243218,-5.30183821219352,-3.98881216346445,-2.70031148141398,-2.82170561280439,-2.34659551251913,-2.74582069779098,-2.56419384953285],"y":[0.282600820311505,0.384206012587796,-0.616615195426022,-1.06740581204851,-0.142071777532422,-0.971807197920967,-1.70332753835318,-0.777721962624607,-0.931360207736392,-0.176312507687801,0.0126758488157614,0.463694380705497,-0.5596587685242,-0.260605454543238,-0.209368506784734,0.383723009117572,-0.548531397518041,-0.536811524155441,0.103872545760497,-0.531458696369635,-0.609331471894223,-1.49711370692197,-1.15898286899811,-1.58831734892512,-1.87416525188207,-1.42817525089411,-1.11398264272975,-1.32360613955814,-1.70130840248359,-1.37607439111134,-1.66052892257135,-1.49117593002424,-1.69960271564471,-0.350091266328256,-0.0929094516660351,-3.0382384175383,-1.00776674657389,-0.952522602651146,-1.34393080230022,-1.31083751024271,-1.18525940116039,-1.57457800315288,-2.22620229710674,-2.10330974188446,-2.05638732281815,-1.14482509524973,-2.15958425382306,-0.916702064471734,-1.46942380253319,-1.90702826285453,-1.89548718938147,-1.40831395421447,-1.32750670813682,-1.49693913428359,-1.66327794126779,-1.62141317325464,-2.63508052617562,-0.737397250500219,-1.59218987688472,-1.53819346215196,-0.891016019280537,-0.909048875847913,-0.92616086712143,-0.663223411176209,-0.428977412945731,-0.948079279045343,-1.03998092726273,-0.962069879847245,-0.929251241494186,-1.2796126986723,-1.787000704921,-1.51832722142101,-2.27975197749815,-2.04769401350358,-1.79429699467239,-0.551987335003657,-0.859751732050587,-1.3494672537974,-1.43220940036078,-1.18369181170553,-0.993346701512473,0.0881302042161518,-0.508441966716394,-1.44368837950182,0.47943495547357,-0.290987791782605,-0.212407483510902,-0.525530076454187,0.670439616327403,1.11752809808045,0.00897020571056266,0.628306857392989,-0.647140881987466,0.195659150998454,0.288186379628345,-0.92892760093387,-0.544978775345982,-0.896433221444112,-0.961563010710933,0.3184145718728,-0.770995719728189,-0.92031759053123,-0.510441138502968,-0.446105396547737,-2.07850792238355,-2.18877431357904,-2.11680280555454,-2.16807299604833,-1.51853547613345,-1.92673791430971,-0.212117509660586,-0.200910835333878,-2.3452781196033,-0.139202090581332,-2.40300036574071,0.00888687080493503,0.419922454567014,0.591803444249971,-0.210993153709407,-0.538606997995629,-2.05103045809064,0.613732995952817,-0.282001916123858,0.157628424464135,0.745952911319728,0.187025666205116,-0.545451708204339,0.37783306702413,0.055374208137057,0.123273865926787,-0.14389897672472,-0.203042737757422,0.0747773225533784,-1.16876510018223,-0.387745275834408,-0.405957429424794,-0.682593578636144,-1.36680071885966,-1.0485070470179,0.126833224733925,-1.35944549476236,-0.726172611822181,-0.774117776307704,-0.964366803232834,-0.322988785659089,0.0919247851268711,-0.366788705849143,-0.757348282373503,-0.312192231382346,-0.99577193267269,-0.379619186148219,-0.666568685251926,-1.21412229285204,0.81263761244549,0.767013557615748,0.699808245148258,0.613208861845392,0.358982314417168,0.893208381386302,1.00411044723741,0.452965677629027,0.384877541908762,0.351670013197054,0.55704652531743,0.0273295305811289,0.536477171900483,0.0960214247570971,0.217391964554035,-0.613841735204185,0.186537627908817,-3.23637725373506,0.000185958185115603,-0.327411930075817,0.141327588402977,0.355945159302768,0.0687614670724935,-0.0702517044716276,-0.212002568392418,0.0429757191090768,-0.232135583113519,0.66801830946389,0.659255663639215,-1.856605035233,-0.303861406983654,-1.64099615907731,-0.554851511323508,-1.38417040952094,-1.11071565375326,-1.1886989192857,-1.357875695874,-0.237145048879963,-0.462342293477461,-1.392120364052,0.0200213982040011,-0.331978790818904,0.418249279629067,0.106355506097773,-0.205678246855687,0.308265714049622,-0.643405847214954,-0.525265331817556,-0.888978592007792,-0.268695506854192,0.140231643614691,0.220113745768276,-0.33223295194652,-0.0514187688423821,0.187355529764712,-0.0719080498426356,-0.401981389627793,-0.513097746368385,-0.465703011504505,-0.223231773979628,-0.802927208704053,-0.456117925024976,-0.402248334608308,0.0930662083524544,-0.127166603515054,-0.228274889477113,0.53085661028385,-0.0126561386056404,-1.18721129302572,-0.0927106782549826,0.392101560571228,0.0143875157493011,-0.234468360846849,0.126008811672109,-0.57148390811116],"text":["PC1: -2.570361467<br />PC2:  2.826008e-01<br />cluster: 5","PC1: -2.380009817<br />PC2:  3.842060e-01<br />cluster: 5","PC1: -3.549962831<br />PC2: -6.166152e-01<br />cluster: 5","PC1: -2.977481361<br />PC2: -1.067406e+00<br />cluster: 5","PC1: -2.782533361<br />PC2: -1.420718e-01<br />cluster: 5","PC1: -1.448356783<br />PC2: -9.718072e-01<br />cluster: 5","PC1: -1.334763903<br />PC2: -1.703328e+00<br />cluster: 5","PC1: -3.017836247<br />PC2: -7.777220e-01<br />cluster: 5","PC1: -4.431032174<br />PC2: -9.313602e-01<br />cluster: 5","PC1: -1.941622308<br />PC2: -1.763125e-01<br />cluster: 5","PC1: -4.110123486<br />PC2:  1.267585e-02<br />cluster: 5","PC1: -2.586986529<br />PC2:  4.636944e-01<br />cluster: 5","PC1: -2.267942265<br />PC2: -5.596588e-01<br />cluster: 5","PC1: -2.187135537<br />PC2: -2.606055e-01<br />cluster: 5","PC1: -1.732474734<br />PC2: -2.093685e-01<br />cluster: 5","PC1: -3.540936286<br />PC2:  3.837230e-01<br />cluster: 5","PC1: -4.138527691<br />PC2: -5.485314e-01<br />cluster: 5","PC1: -1.517604576<br />PC2: -5.368115e-01<br />cluster: 5","PC1: -2.769708342<br />PC2:  1.038725e-01<br />cluster: 5","PC1: -2.285366524<br />PC2: -5.314587e-01<br />cluster: 5","PC1: -1.243056510<br />PC2: -6.093315e-01<br />cluster: 5","PC1: -4.084064946<br />PC2: -1.497114e+00<br />cluster: 5","PC1: -1.855261155<br />PC2: -1.158983e+00<br />cluster: 5","PC1: -2.195155826<br />PC2: -1.588317e+00<br />cluster: 5","PC1: -4.649589993<br />PC2: -1.874165e+00<br />cluster: 5","PC1: -3.363860744<br />PC2: -1.428175e+00<br />cluster: 5","PC1: -3.359284306<br />PC2: -1.113983e+00<br />cluster: 5","PC1: -2.204180966<br />PC2: -1.323606e+00<br />cluster: 5","PC1: -5.184511334<br />PC2: -1.701308e+00<br />cluster: 5","PC1: -0.985476827<br />PC2: -1.376074e+00<br />cluster: 5","PC1: -2.106905429<br />PC2: -1.660529e+00<br />cluster: 5","PC1: -1.871684010<br />PC2: -1.491176e+00<br />cluster: 5","PC1: -1.464908379<br />PC2: -1.699603e+00<br />cluster: 5","PC1: -2.140075162<br />PC2: -3.500913e-01<br />cluster: 5","PC1: -2.072485873<br />PC2: -9.290945e-02<br />cluster: 5","PC1: -2.631751049<br />PC2: -3.038238e+00<br />cluster: 5","PC1: -2.017757936<br />PC2: -1.007767e+00<br />cluster: 5","PC1: -1.473417102<br />PC2: -9.525226e-01<br />cluster: 5","PC1: -2.455152835<br />PC2: -1.343931e+00<br />cluster: 5","PC1: -1.850877760<br />PC2: -1.310838e+00<br />cluster: 5","PC1: -1.712814335<br />PC2: -1.185259e+00<br />cluster: 5","PC1: -2.915949353<br />PC2: -1.574578e+00<br />cluster: 5","PC1: -1.818033966<br />PC2: -2.226202e+00<br />cluster: 5","PC1: -1.652717937<br />PC2: -2.103310e+00<br />cluster: 5","PC1: -1.615401695<br />PC2: -2.056387e+00<br />cluster: 5","PC1: -2.185241168<br />PC2: -1.144825e+00<br />cluster: 5","PC1: -4.252700412<br />PC2: -2.159584e+00<br />cluster: 5","PC1: -1.996132741<br />PC2: -9.167021e-01<br />cluster: 5","PC1: -2.971299605<br />PC2: -1.469424e+00<br />cluster: 5","PC1: -5.905295340<br />PC2: -1.907028e+00<br />cluster: 5","PC1: -4.149956412<br />PC2: -1.895487e+00<br />cluster: 5","PC1: -2.869817924<br />PC2: -1.408314e+00<br />cluster: 5","PC1: -2.414711035<br />PC2: -1.327507e+00<br />cluster: 5","PC1: -4.176742753<br />PC2: -1.496939e+00<br />cluster: 5","PC1: -3.501720658<br />PC2: -1.663278e+00<br />cluster: 5","PC1: -1.640661850<br />PC2: -1.621413e+00<br />cluster: 5","PC1: -3.815690670<br />PC2: -2.635081e+00<br />cluster: 5","PC1: -2.570153147<br />PC2: -7.373973e-01<br />cluster: 5","PC1: -3.155047687<br />PC2: -1.592190e+00<br />cluster: 5","PC1: -3.883462860<br />PC2: -1.538193e+00<br />cluster: 5","PC1: -2.481872304<br />PC2: -8.910160e-01<br />cluster: 5","PC1: -2.427284914<br />PC2: -9.090489e-01<br />cluster: 5","PC1: -3.493029566<br />PC2: -9.261609e-01<br />cluster: 5","PC1: -1.909404028<br />PC2: -6.632234e-01<br />cluster: 5","PC1: -1.384887785<br />PC2: -4.289774e-01<br />cluster: 5","PC1: -1.816755762<br />PC2: -9.480793e-01<br />cluster: 5","PC1: -1.030007587<br />PC2: -1.039981e+00<br />cluster: 5","PC1: -1.528985354<br />PC2: -9.620699e-01<br />cluster: 5","PC1: -3.638852947<br />PC2: -9.292512e-01<br />cluster: 5","PC1: -3.463313321<br />PC2: -1.279613e+00<br />cluster: 5","PC1: -3.331124049<br />PC2: -1.787001e+00<br />cluster: 5","PC1: -3.090852154<br />PC2: -1.518327e+00<br />cluster: 5","PC1: -5.305020207<br />PC2: -2.279752e+00<br />cluster: 5","PC1: -4.580343728<br />PC2: -2.047694e+00<br />cluster: 5","PC1: -5.554487007<br />PC2: -1.794297e+00<br />cluster: 5","PC1: -3.055894702<br />PC2: -5.519873e-01<br />cluster: 5","PC1: -1.624912577<br />PC2: -8.597517e-01<br />cluster: 5","PC1: -4.304547680<br />PC2: -1.349467e+00<br />cluster: 5","PC1: -2.848152755<br />PC2: -1.432209e+00<br />cluster: 5","PC1: -1.298813041<br />PC2: -1.183692e+00<br />cluster: 5","PC1: -2.977843238<br />PC2: -9.933467e-01<br />cluster: 5","PC1: -2.092408639<br />PC2:  8.813020e-02<br />cluster: 5","PC1: -1.881172017<br />PC2: -5.084420e-01<br />cluster: 5","PC1: -3.494368901<br />PC2: -1.443688e+00<br />cluster: 5","PC1: -1.551911315<br />PC2:  4.794350e-01<br />cluster: 5","PC1: -2.170173578<br />PC2: -2.909878e-01<br />cluster: 5","PC1: -2.048860364<br />PC2: -2.124075e-01<br />cluster: 5","PC1: -2.088926055<br />PC2: -5.255301e-01<br />cluster: 5","PC1: -1.936995697<br />PC2:  6.704396e-01<br />cluster: 5","PC1: -1.829254878<br />PC2:  1.117528e+00<br />cluster: 5","PC1: -2.034845153<br />PC2:  8.970206e-03<br />cluster: 5","PC1: -2.677797724<br />PC2:  6.283069e-01<br />cluster: 5","PC1: -2.270166189<br />PC2: -6.471409e-01<br />cluster: 5","PC1: -1.734368328<br />PC2:  1.956592e-01<br />cluster: 5","PC1: -3.104644464<br />PC2:  2.881864e-01<br />cluster: 5","PC1: -3.128526035<br />PC2: -9.289276e-01<br />cluster: 5","PC1: -1.164958773<br />PC2: -5.449788e-01<br />cluster: 5","PC1: -1.417142221<br />PC2: -8.964332e-01<br />cluster: 5","PC1: -3.244417290<br />PC2: -9.615630e-01<br />cluster: 5","PC1: -2.589810323<br />PC2:  3.184146e-01<br />cluster: 5","PC1: -2.245083233<br />PC2: -7.709957e-01<br />cluster: 5","PC1: -2.691482926<br />PC2: -9.203176e-01<br />cluster: 5","PC1: -3.118649186<br />PC2: -5.104411e-01<br />cluster: 5","PC1: -1.507558908<br />PC2: -4.461054e-01<br />cluster: 5","PC1: -5.161900049<br />PC2: -2.078508e+00<br />cluster: 5","PC1: -4.341455780<br />PC2: -2.188774e+00<br />cluster: 5","PC1: -3.425577984<br />PC2: -2.116803e+00<br />cluster: 5","PC1: -5.557407071<br />PC2: -2.168073e+00<br />cluster: 5","PC1: -4.066663464<br />PC2: -1.518535e+00<br />cluster: 5","PC1: -5.740964313<br />PC2: -1.926738e+00<br />cluster: 5","PC1: -1.422069742<br />PC2: -2.121175e-01<br />cluster: 5","PC1: -2.047706401<br />PC2: -2.009108e-01<br />cluster: 5","PC1: -2.416849781<br />PC2: -2.345278e+00<br />cluster: 5","PC1: -2.462348518<br />PC2: -1.392021e-01<br />cluster: 5","PC1: -2.075518786<br />PC2: -2.403000e+00<br />cluster: 5","PC1: -2.389205487<br />PC2:  8.886871e-03<br />cluster: 5","PC1: -2.318303934<br />PC2:  4.199225e-01<br />cluster: 5","PC1: -4.528508129<br />PC2:  5.918034e-01<br />cluster: 5","PC1: -3.870094588<br />PC2: -2.109932e-01<br />cluster: 5","PC1: -4.122029983<br />PC2: -5.386070e-01<br />cluster: 5","PC1: -3.659309800<br />PC2: -2.051030e+00<br />cluster: 5","PC1: -2.361381352<br />PC2:  6.137330e-01<br />cluster: 5","PC1: -1.549821806<br />PC2: -2.820019e-01<br />cluster: 5","PC1: -3.862584222<br />PC2:  1.576284e-01<br />cluster: 5","PC1: -2.689850581<br />PC2:  7.459529e-01<br />cluster: 5","PC1: -2.117742661<br />PC2:  1.870257e-01<br />cluster: 5","PC1: -2.835292319<br />PC2: -5.454517e-01<br />cluster: 5","PC1: -2.498091302<br />PC2:  3.778331e-01<br />cluster: 5","PC1: -1.743616541<br />PC2:  5.537421e-02<br />cluster: 5","PC1: -4.391310500<br />PC2:  1.232739e-01<br />cluster: 5","PC1: -3.248484525<br />PC2: -1.438990e-01<br />cluster: 5","PC1: -3.015672709<br />PC2: -2.030427e-01<br />cluster: 5","PC1: -2.492139566<br />PC2:  7.477732e-02<br />cluster: 5","PC1: -2.900611538<br />PC2: -1.168765e+00<br />cluster: 5","PC1: -3.332220941<br />PC2: -3.877453e-01<br />cluster: 5","PC1: -2.807205366<br />PC2: -4.059574e-01<br />cluster: 5","PC1: -1.910458169<br />PC2: -6.825936e-01<br />cluster: 5","PC1: -2.576916171<br />PC2: -1.366801e+00<br />cluster: 5","PC1: -3.717513699<br />PC2: -1.048507e+00<br />cluster: 5","PC1: -2.998566310<br />PC2:  1.268332e-01<br />cluster: 5","PC1: -0.756439228<br />PC2: -1.359445e+00<br />cluster: 5","PC1: -2.475490098<br />PC2: -7.261726e-01<br />cluster: 5","PC1: -2.430605772<br />PC2: -7.741178e-01<br />cluster: 5","PC1: -2.385595825<br />PC2: -9.643668e-01<br />cluster: 5","PC1: -1.868432314<br />PC2: -3.229888e-01<br />cluster: 5","PC1: -4.059926706<br />PC2:  9.192479e-02<br />cluster: 5","PC1: -3.036253748<br />PC2: -3.667887e-01<br />cluster: 5","PC1: -4.826235115<br />PC2: -7.573483e-01<br />cluster: 5","PC1: -4.151461880<br />PC2: -3.121922e-01<br />cluster: 5","PC1: -2.328106393<br />PC2: -9.957719e-01<br />cluster: 5","PC1: -3.178197386<br />PC2: -3.796192e-01<br />cluster: 5","PC1: -2.336289397<br />PC2: -6.665687e-01<br />cluster: 5","PC1: -2.674865540<br />PC2: -1.214122e+00<br />cluster: 5","PC1: -2.709390734<br />PC2:  8.126376e-01<br />cluster: 5","PC1: -2.031741207<br />PC2:  7.670136e-01<br />cluster: 5","PC1: -2.182212537<br />PC2:  6.998082e-01<br />cluster: 5","PC1: -2.448947966<br />PC2:  6.132089e-01<br />cluster: 5","PC1: -2.381918883<br />PC2:  3.589823e-01<br />cluster: 5","PC1: -2.830983593<br />PC2:  8.932084e-01<br />cluster: 5","PC1: -2.450292872<br />PC2:  1.004110e+00<br />cluster: 5","PC1: -2.759670321<br />PC2:  4.529657e-01<br />cluster: 5","PC1: -1.801784748<br />PC2:  3.848775e-01<br />cluster: 5","PC1: -2.799482337<br />PC2:  3.516700e-01<br />cluster: 5","PC1: -3.729021242<br />PC2:  5.570465e-01<br />cluster: 5","PC1: -2.924095111<br />PC2:  2.732953e-02<br />cluster: 5","PC1: -2.242155705<br />PC2:  5.364772e-01<br />cluster: 5","PC1: -3.286301734<br />PC2:  9.602142e-02<br />cluster: 5","PC1: -3.068260307<br />PC2:  2.173920e-01<br />cluster: 5","PC1: -3.427605070<br />PC2: -6.138417e-01<br />cluster: 5","PC1: -2.224207990<br />PC2:  1.865376e-01<br />cluster: 5","PC1: -4.675816947<br />PC2: -3.236377e+00<br />cluster: 5","PC1: -2.914120233<br />PC2:  1.859582e-04<br />cluster: 5","PC1: -1.847402105<br />PC2: -3.274119e-01<br />cluster: 5","PC1: -1.420826384<br />PC2:  1.413276e-01<br />cluster: 5","PC1: -2.184647102<br />PC2:  3.559452e-01<br />cluster: 5","PC1: -4.827336654<br />PC2:  6.876147e-02<br />cluster: 5","PC1: -4.562918934<br />PC2: -7.025170e-02<br />cluster: 5","PC1: -2.695684007<br />PC2: -2.120026e-01<br />cluster: 5","PC1: -2.757796062<br />PC2:  4.297572e-02<br />cluster: 5","PC1: -3.210290555<br />PC2: -2.321356e-01<br />cluster: 5","PC1: -2.824838869<br />PC2:  6.680183e-01<br />cluster: 5","PC1: -2.870965043<br />PC2:  6.592557e-01<br />cluster: 5","PC1: -1.546504313<br />PC2: -1.856605e+00<br />cluster: 5","PC1: -2.140785963<br />PC2: -3.038614e-01<br />cluster: 5","PC1: -2.629087828<br />PC2: -1.640996e+00<br />cluster: 5","PC1: -1.799151899<br />PC2: -5.548515e-01<br />cluster: 5","PC1: -2.891156836<br />PC2: -1.384170e+00<br />cluster: 5","PC1: -3.764328850<br />PC2: -1.110716e+00<br />cluster: 5","PC1: -1.430513669<br />PC2: -1.188699e+00<br />cluster: 5","PC1: -4.840652390<br />PC2: -1.357876e+00<br />cluster: 5","PC1: -3.495647257<br />PC2: -2.371450e-01<br />cluster: 5","PC1: -2.408692170<br />PC2: -4.623423e-01<br />cluster: 5","PC1: -1.598135006<br />PC2: -1.392120e+00<br />cluster: 5","PC1: -1.665833707<br />PC2:  2.002140e-02<br />cluster: 5","PC1: -4.080141099<br />PC2: -3.319788e-01<br />cluster: 5","PC1: -2.252729356<br />PC2:  4.182493e-01<br />cluster: 5","PC1: -2.352645153<br />PC2:  1.063555e-01<br />cluster: 5","PC1: -1.597156565<br />PC2: -2.056782e-01<br />cluster: 5","PC1: -2.335277976<br />PC2:  3.082657e-01<br />cluster: 5","PC1: -2.961934699<br />PC2: -6.434058e-01<br />cluster: 5","PC1: -2.557017160<br />PC2: -5.252653e-01<br />cluster: 5","PC1: -2.441882816<br />PC2: -8.889786e-01<br />cluster: 5","PC1: -2.963123357<br />PC2: -2.686955e-01<br />cluster: 5","PC1: -2.326957139<br />PC2:  1.402316e-01<br />cluster: 5","PC1: -1.909873254<br />PC2:  2.201137e-01<br />cluster: 5","PC1: -2.041510175<br />PC2: -3.322330e-01<br />cluster: 5","PC1: -2.047100885<br />PC2: -5.141877e-02<br />cluster: 5","PC1: -2.708635246<br />PC2:  1.873555e-01<br />cluster: 5","PC1: -2.477851647<br />PC2: -7.190805e-02<br />cluster: 5","PC1: -2.341856141<br />PC2: -4.019814e-01<br />cluster: 5","PC1: -2.505372984<br />PC2: -5.130977e-01<br />cluster: 5","PC1: -2.215599700<br />PC2: -4.657030e-01<br />cluster: 5","PC1: -4.342511287<br />PC2: -2.232318e-01<br />cluster: 5","PC1: -3.149587352<br />PC2: -8.029272e-01<br />cluster: 5","PC1: -4.605328406<br />PC2: -4.561179e-01<br />cluster: 5","PC1: -3.319475686<br />PC2: -4.022483e-01<br />cluster: 5","PC1: -3.522842765<br />PC2:  9.306621e-02<br />cluster: 5","PC1: -4.020642228<br />PC2: -1.271666e-01<br />cluster: 5","PC1: -4.042540532<br />PC2: -2.282749e-01<br />cluster: 5","PC1: -1.958392453<br />PC2:  5.308566e-01<br />cluster: 5","PC1: -4.406520702<br />PC2: -1.265614e-02<br />cluster: 5","PC1: -5.301838212<br />PC2: -1.187211e+00<br />cluster: 5","PC1: -3.988812163<br />PC2: -9.271068e-02<br />cluster: 5","PC1: -2.700311481<br />PC2:  3.921016e-01<br />cluster: 5","PC1: -2.821705613<br />PC2:  1.438752e-02<br />cluster: 5","PC1: -2.346595513<br />PC2: -2.344684e-01<br />cluster: 5","PC1: -2.745820698<br />PC2:  1.260088e-01<br />cluster: 5","PC1: -2.564193850<br />PC2: -5.714839e-01<br />cluster: 5"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.84538542541711,1.68194637985946,0.213047804663634,0.18519413611005,2.19978930271364,1.62939689170438,1.3587280770293,1.65762735724783,2.6095754038546,1.8469342242205,2.1459400315146,2.49738562841509,2.43859358861097,1.72660557002697,1.82897611354436,3.04323356083976,3.35193206283351,2.05120552921874,2.21173208138242,2.24373632968155,2.06555584696161,0.892854258044662,2.83266888117123,2.34191205956455,1.68152899249744,2.49981076108115,2.86936390988903,3.18470832416458,1.85357304319356,2.41356194830151,1.71451213893665,1.48348423202584,2.01011182031006,3.48141477394981,2.51808037871003,1.97601731497195,2.97731759190687,3.04106537897884,2.39225944807619,3.14745234456575,1.65577913239901,2.00085418063669,1.70224723062381,1.77164672847129,3.22716487084035,2.4791890487186,1.74521274967163,2.41484461541032,1.13422385160967,2.11915294519276,3.05225279914806,2.25291632108033,1.4428027317145,2.71873024098886,1.71750147407644,2.38487764425153,2.82023373511355,2.7951610515346,2.26811757409535,2.51373809967667,2.07991977104754,3.76642282259944,1.89294194159435,0.650379109091962,1.15616345205094,3.14959334491583,1.07091766898382,2.17915859367276,2.25387937079913,1.82725167933474,1.91659301184722,1.62510620013733,2.53114204230947,2.97180608214797,2.79818220946642,3.80381776655204,2.37687578806747,2.26293560864214,3.25140024084922,2.41970115770762,3.17878933257125,1.37956983015038,1.97141513294387,1.81433291603864,1.32216808771837,2.78357862562386,1.58195139605951,1.10504475412286,2.49288060721662,3.49174819330607,2.15805943858604,2.29418887113518,1.5121907971283,2.16208754351732,1.83206706029119,2.58664248945369,2.40761599226597,1.75285524770931,1.85446499479357,3.93603758345237,3.3744576533906,1.95262277427515,2.47036049029238,2.34263217891949,2.07705810706812,3.33874570782157,1.27351388258453,2.20999449911836,2.7879238979502,0.457348662069318,3.37171831306905,1.83320550852466,1.34026710503037,1.19370250373755,3.00424691913354,1.30164173429777,1.89704822698382,2.94720041594029,2.18198735644361,2.10711734616945,2.6421805118327,1.41721186409289,1.82256826176135,2.51747951089026,1.72192931825615,1.88607902299579,2.23064916871599,1.16428624360338,2.1994090866435,2.14566635512913,3.39633152516859,1.80671299148412,0.561337478577453,2.98767564456934,1.8905554467367,2.91175507402164,2.4020449839626,3.34349688786546,3.00192641260697,2.63625836731486,2.37036017116791,1.06545161559809,1.35857315008859,2.89728591033203,3.08090706956378,3.16332869079787,2.35516537268162,0.454522969744901,1.95913062728134,1.92921990205533,1.77151642164246,1.58663745811518,0.89875665119033,3.10199042944211,2.84061674785459,1.99985381757438,2.44621425829361,1.99446982478118,2.4038318978221,2.67798254556424,3.05785812121141,1.83203386056755,1.31715367215791,1.98521317424591,1.24980111574782,2.71916649121331,2.18400183226389,2.43076569053148,0.738744998266065,0.946816786438953,0.600177277329127,0.838765321455693,0.97781246495735,1.83394625216832,1.55621649067237,1.59368335972964,1.37580294490329,1.84583541354791,1.10508587140892,1.12550664326255,2.30247174033965,2.57005496907401,1.32624533092155,1.19075614777545,0.782024297670606,0.420369894846026,1.72099345273296,1.33035700516717,1.31822189141891,1.79320020104123,0.63060536973126,1.71365836438205,1.30480170950233,1.73276531420717,0.925323442514235,1.55351296389757,1.55623050235313,1.49668123989383,1.19015116209597,1.57034603584998,2.22216690095033,3.13646855725281,1.86384121911592,1.62336327589544,2.47461127798849,1.91245885648376,2.1137798972249,0.738758652530021,0.731788947012428,0.278065291779038,0.822035627729812,1.04687310501525,1.79785531687095,2.09072188322699,2.14105232875522,2.78266879754978,2.07296743169915,2.31390529586544,1.36407909336894,2.16643608202368,0.574430042836294,2.19030866440812,2.34666157432883,2.19798673147116,0.404958215199602,1.9765305185469,1.2385440715975,2.01016317410114,2.10211501224652,1.20845872500537,1.16671754902564,0.816556544567724,2.07153919797631,2.41075802773918,2.47989589908464,1.56955208580176,0.657931055637425,1.41231938566808,2.89208434070827,2.61044805748422,2.83476382976163,1.92019376585455,1.5772560995472,1.93050935135141,2.46593662512352,1.47456022451846,1.64021763164117,2.52885685285117,1.85806162859284,1.66208847077459,1.19605812065773,1.29288059845434,2.88473230480341,2.15110882699553,2.38481020731128,1.057655368394,1.55967852190448,1.97722782907261,2.22612789847955,1.1841062726012,2.2667855617055,1.25076760648407,2.0937300179839,1.5765959674629,3.58121082841385,2.14818136765978,1.68981153009719,2.02420752003676,1.96857018484376,0.540502062599019,1.51223540562228,1.48756436621528,1.09039137162725,0.445101669875914,0.295627552022929,1.53478859705232,0.967919725069272,0.557778495458206,1.54240188994001,2.07577661958642,1.60784293944937,1.37319496939922,1.36165479330218,1.23075989741141,2.20197265565344,1.47350085380389,2.21911417317686,1.71007914503134,1.30295313115923,1.50144126089105,1.7109733895891,0.906433666993005,3.13066213395541,2.29270505348901,2.37585700018801,2.48223190626702,3.03295581678097,2.25687589649969,1.94093723784889,3.06807483012647,2.9048799714956,2.07361142119363,1.97837203861837,0.460879208969594,1.66726389556545,1.69251376882006,0.771119810821387,2.29956298694864,2.46562041649398,1.2870216571356,2.44531656717318,1.65731496222259,1.07090465428437,2.02946380067098,2.56801092712242,3.05564674240663,1.53678430717205,1.96087358376572,2.43520782399725,2.44948296301078,1.16174027605169,1.84123556995245,2.29639194197268,2.23893998362472,1.85669235291712,2.68982715607074,2.38942697680414,1.97821813897503,2.43615585100325,2.01259257085829,2.93557047555146,3.10046105665228,2.56878960687288,2.83964352823663,2.5492789032776,1.5176029826692,2.08556400142844,2.77083784273946,1.59892102615694,0.974397566948121,0.858353066462026,1.37924588832452,2.17267201721364,1.816732403356,2.4880305310064,2.54255411077574,3.48472775525625,1.27382099340277,1.6329712037786,1.53888277143234,2.01098271705743,2.83853254470086,2.59440462704654,1.94505558860718,1.66014079904932,1.26982346897993,0.9372112828781,1.15374012632927,1.21105313301339,1.71138509853337,1.99883757740383,1.46003084066512,2.30996367067301,2.48734397107529,1.39492916347015,1.25912008445864,1.12304302075037,1.18445835394601,1.06003351493795,2.13143816255134,1.53117410219274,1.32234734703294,1.21009178962377,0.891538974805834,0.725638313367351,1.12295336536417,1.00377257115104,1.53937636107971,1.98516521187063,1.02294176505345,1.30603655276407,1.06252570942324,0.337112468120278,0.460578308983742,0.770952057693491,0.849894217157871,1.5246843179147,1.38145572361906,2.4319047212038,1.28059110625349,0.794280895737063,0.95441240292568,1.7783573730526,1.9927272015519,1.53175688468178,2.00074293827831,1.19102329762372,1.31887797271521,2.09009897425291,3.07168176181901,1.23029617539796,1.44852669611077,2.6475259105854,1.73473193313887,1.96888577800017,2.10807588183317,2.2627624803422,2.46483097280743,1.05515067475269,1.11094510303973,0.858751196490414,1.0129763702697,2.02460128396247,1.10326939528495,3.18985367035985,2.73344582876635,3.09937233508043,3.32104686941232,2.19233963668731,3.12601565265036,2.37245687388422,2.9054169263977,2.45993932304426,2.45002529905711,2.70997049267161,1.79957615833771,2.29539271979163],"y":[0.568835391938178,0.71056577847587,-0.0108794647492937,-1.01382030152985,0.196803070175391,0.361201722227618,-0.0805068004665474,-0.055978728213611,0.466929503413675,0.513767733881627,-0.650391680988032,0.94700543447408,1.23840981535667,0.434072320146338,0.247514704710243,-0.874994927099741,-0.912029003503852,0.184851368943085,-0.081758121786413,0.806269322110102,0.810047485933992,1.01949198912718,1.22010470376614,1.10068149840169,1.34818067317025,0.750299571064451,0.0948778110535372,0.352735883932642,0.724884877842268,0.965349417225016,1.14116096954791,0.186781137485632,-0.466311641778838,-0.0255140464429574,0.333133701785776,0.521985519043234,0.229914660721539,-0.247849492141864,-0.327400013622764,-0.234377457039279,-0.0501656010128836,0.937037526436634,0.943717935783773,0.610458856785322,0.200029844573769,1.68527880035579,0.589456979101871,1.03888861916599,0.617916936487791,1.21194242905496,1.09022740021374,0.366742675521669,1.171168679826,0.209540536416483,0.173028217819434,0.544451197640439,1.39375360386173,0.225181176154425,1.84088539793622e-05,0.930043323337685,1.19512421630639,1.84549692600028,-0.788772220243325,-0.523058044868619,-0.0563875702020971,-0.0180317049466818,-0.511407143659193,0.0942529888837878,0.748798959311939,0.682163856167388,1.19722833974129,1.23103059299417,0.794705326654439,0.669061297780538,1.56251158986514,0.501355933047352,0.911645155241524,0.53429530001836,0.0211134736490342,0.35601383132403,0.596452216383856,0.733016107907034,-0.594166451526238,0.76497842051135,0.876613299978083,0.553106600450913,0.90678424028285,0.858460274440239,1.70492503800858,1.02582809282408,1.22803063738841,0.797271689433733,0.865282461408999,1.98278222090237,1.51924736582047,1.34943919038404,1.28133445482547,1.09095754381027,1.17570792558657,1.42678947112569,-0.0716749435154078,0.678286371777577,1.19985913102448,-0.0181275017236317,0.371200682824833,1.38696361275905,0.197908642065476,0.14473368292278,0.933971594284222,-0.132042334957798,0.399030859588017,0.564649008466771,-0.178290605536028,-0.481589079676628,-0.452476836736762,-0.429449430007828,0.331198313189536,0.0346354790015446,0.613049862338657,0.261492406136784,-1.50457422478406,-0.0790846977750934,-0.771173332269315,-0.658500247364957,0.765859748269387,-0.8381527309342,-0.913098517487046,-0.359087564142544,-0.410763719267838,0.130116575184441,-0.360089687904221,-0.467826836040655,0.233768114196673,-0.0296426897906906,-0.54393515088559,-0.134382997756019,-0.543513399922255,-0.61251043021415,0.382872233266685,-0.127172785465632,-0.0455859473104281,0.285917108774729,0.348248562439864,-0.568820734216851,-0.540268184750569,-0.571340266176765,-0.162267718718738,0.159501316156384,0.254815874806811,0.341262413393333,1.42509986347972,-0.0884145023386042,0.990701552837166,-0.431824392315315,0.39596103049133,0.307433924865856,1.21037360553827,0.713028640620281,0.577553369993344,0.60874945820259,0.391714214690117,0.798634231015769,0.723601102932931,0.962825252489477,1.68500062715857,-0.443137520087695,-0.305763543936302,1.11630408103623,0.99623302815024,1.15661443726933,-0.025185379778959,0.884170856430591,0.724684613759576,0.864480101366207,0.785938689167891,0.925813971292671,1.59909414000728,0.0488770752183958,1.1313984058043,0.256759084759186,0.741383066897795,0.412788398796575,1.35614075094177,0.432990676908926,0.722515377627201,2.14233293567182,0.280356938816637,0.87764786705307,0.739996088682976,1.28498541115721,0.243360042967899,0.712061181482361,1.16092227553007,0.52469309142543,1.27541954666706,0.807897984379588,0.0498180084955103,2.15774572132355,1.71152826939481,1.42192195621838,1.74881045686247,1.05063757231847,1.79262408853149,1.75019245583195,1.07728144341087,0.891967356703235,1.14464774614502,0.475288725571789,0.200614164318968,2.14718751933214,1.65568730800691,1.27483795179186,0.68741330596872,0.417269430436365,1.16466016898687,0.657400138610325,1.26567683669873,-0.581593096003089,1.30973975781007,0.346096421734105,0.0234250497144233,0.179969306718294,0.0592971905233306,-0.257492885819935,-0.00605272011928451,1.13821955609604,0.940446977972948,0.319823837325981,1.04131666784864,0.961166245407649,0.694250894970672,0.0674182877489419,0.34889089359177,0.360705607319893,0.143208902590349,0.30828385146031,0.489288483670674,1.22478109671615,0.72703940776905,-0.239083513978742,0.524568117956247,1.27597937376969,0.225924083589961,1.34958714099909,1.07864449105724,0.805104954303544,0.90513928213224,-0.721312439328289,0.192821684581773,0.135856216823677,-0.264559047982651,0.250086497732718,-0.1725996468657,-0.39444422856224,0.0331150292526078,-0.271874382929627,-0.706675657793751,-0.0564214651613146,-0.30756285933115,-0.616519067277913,0.164666862061394,0.13171057946738,0.219118643894347,0.185830522885394,1.41151832311004,1.53944387603496,0.403178351782181,1.40498531947947,1.1873539028029,1.21174457642585,0.0306203385120984,1.03955505800358,0.854993064611361,0.064578054194249,-0.0272488819146339,-0.857379598987048,0.505112556147508,-0.974338591852521,-0.742338202454775,-0.359893635958095,0.622147010994126,1.44318403277019,1.7103795238617,1.71705877344131,0.378726367271406,0.998233307620496,1.01226561503434,1.15886962573464,0.617688398071419,0.39119380234582,0.704194239201813,0.119747881761945,0.834107584833221,1.06357240149017,1.71699973865347,-0.352820762483816,0.208922786352295,-0.319779816517222,0.796338297846039,0.655507456283253,0.45389806410144,0.853054637627281,-1.013065629311,0.106696497514801,1.35363170052875,0.854524068642448,1.13937247826364,1.03100809532422,1.06324254927925,0.190744687966126,0.589364928574577,0.425229327410343,0.989496890974639,0.690679439291245,1.29668880868115,0.58601258971724,0.559797489566888,0.759786167394962,0.781237126876065,0.489917431623294,0.26020963604742,0.138095980598873,0.632262121014435,0.330699202887236,1.26405069482922,1.25724541713193,0.890082116866181,0.751673126533674,0.866869091856521,1.20235169264081,0.094349634902638,0.426692119059089,0.921099455186822,0.0526032165034649,0.470215373127189,0.561339977470686,1.91618405742038,0.506148347933636,0.232014069217552,0.951379273061732,0.545503553738927,-0.0231627599153419,-0.109040241356877,0.0889830388203528,0.330645771659493,0.464401864621167,0.667986774591654,1.22473346398603,2.4084096330859,1.07877462054743,0.672940548758884,0.270300081831043,0.892532563877408,1.08284461831518,1.16414442821137,0.801209482691399,0.782119819676181,0.0990148608292195,0.319960319074141,0.134820231069936,1.11615876164995,0.6048686634575,-0.0920175707449656,-0.356714916336872,0.619076115925197,1.80728040269961,0.138028704827751,0.911056157246439,0.587952829708891,0.657285937106395,1.24017561172573,0.178762378152105,1.13907131986204,1.33784186271525,0.268258892963865,-0.298613862097108,1.10077757652038,1.75104094980112,0.760263751503459,0.664822356516037,1.35775006265733,0.898334324951666,0.742182230713479,0.773100014976282,1.07124889808551,1.07943050101483,1.01799790558452,1.07849622477145,-0.168277267435995,0.286054035449054,1.08368958810751,0.0643664971408247,0.700235125281171,-0.0350147461125495,0.25805834918407,0.604994492632794,0.541019730911249,0.704126579516626,0.269631321553245,0.263368612560589,-0.159637148160577,-0.579147045097413,-1.07434279504359,0.416101944763993,-0.439883761140666,-0.59513555247841,0.267345746183078,-0.500572035301106,0.300492671423647,-0.198982889227059,0.294029360627,-0.0527134749430767,-0.310353885610246,0.253460756457881,0.0951366923417227,0.0881283259271124,0.311773894933085,-0.210184865942572,0.194426836943264,0.141303157918698,1.73351958283745,-0.526671239613801,0.325211566413351,0.408669206253886,-0.722440157790322,0.717814000783377,0.863492160294784],"text":["PC1:  1.845385425<br />PC2:  5.688354e-01<br />cluster: 6","PC1:  1.681946380<br />PC2:  7.105658e-01<br />cluster: 6","PC1:  0.213047805<br />PC2: -1.087946e-02<br />cluster: 6","PC1:  0.185194136<br />PC2: -1.013820e+00<br />cluster: 6","PC1:  2.199789303<br />PC2:  1.968031e-01<br />cluster: 6","PC1:  1.629396892<br />PC2:  3.612017e-01<br />cluster: 6","PC1:  1.358728077<br />PC2: -8.050680e-02<br />cluster: 6","PC1:  1.657627357<br />PC2: -5.597873e-02<br />cluster: 6","PC1:  2.609575404<br />PC2:  4.669295e-01<br />cluster: 6","PC1:  1.846934224<br />PC2:  5.137677e-01<br />cluster: 6","PC1:  2.145940032<br />PC2: -6.503917e-01<br />cluster: 6","PC1:  2.497385628<br />PC2:  9.470054e-01<br />cluster: 6","PC1:  2.438593589<br />PC2:  1.238410e+00<br />cluster: 6","PC1:  1.726605570<br />PC2:  4.340723e-01<br />cluster: 6","PC1:  1.828976114<br />PC2:  2.475147e-01<br />cluster: 6","PC1:  3.043233561<br />PC2: -8.749949e-01<br />cluster: 6","PC1:  3.351932063<br />PC2: -9.120290e-01<br />cluster: 6","PC1:  2.051205529<br />PC2:  1.848514e-01<br />cluster: 6","PC1:  2.211732081<br />PC2: -8.175812e-02<br />cluster: 6","PC1:  2.243736330<br />PC2:  8.062693e-01<br />cluster: 6","PC1:  2.065555847<br />PC2:  8.100475e-01<br />cluster: 6","PC1:  0.892854258<br />PC2:  1.019492e+00<br />cluster: 6","PC1:  2.832668881<br />PC2:  1.220105e+00<br />cluster: 6","PC1:  2.341912060<br />PC2:  1.100681e+00<br />cluster: 6","PC1:  1.681528992<br />PC2:  1.348181e+00<br />cluster: 6","PC1:  2.499810761<br />PC2:  7.502996e-01<br />cluster: 6","PC1:  2.869363910<br />PC2:  9.487781e-02<br />cluster: 6","PC1:  3.184708324<br />PC2:  3.527359e-01<br />cluster: 6","PC1:  1.853573043<br />PC2:  7.248849e-01<br />cluster: 6","PC1:  2.413561948<br />PC2:  9.653494e-01<br />cluster: 6","PC1:  1.714512139<br />PC2:  1.141161e+00<br />cluster: 6","PC1:  1.483484232<br />PC2:  1.867811e-01<br />cluster: 6","PC1:  2.010111820<br />PC2: -4.663116e-01<br />cluster: 6","PC1:  3.481414774<br />PC2: -2.551405e-02<br />cluster: 6","PC1:  2.518080379<br />PC2:  3.331337e-01<br />cluster: 6","PC1:  1.976017315<br />PC2:  5.219855e-01<br />cluster: 6","PC1:  2.977317592<br />PC2:  2.299147e-01<br />cluster: 6","PC1:  3.041065379<br />PC2: -2.478495e-01<br />cluster: 6","PC1:  2.392259448<br />PC2: -3.274000e-01<br />cluster: 6","PC1:  3.147452345<br />PC2: -2.343775e-01<br />cluster: 6","PC1:  1.655779132<br />PC2: -5.016560e-02<br />cluster: 6","PC1:  2.000854181<br />PC2:  9.370375e-01<br />cluster: 6","PC1:  1.702247231<br />PC2:  9.437179e-01<br />cluster: 6","PC1:  1.771646728<br />PC2:  6.104589e-01<br />cluster: 6","PC1:  3.227164871<br />PC2:  2.000298e-01<br />cluster: 6","PC1:  2.479189049<br />PC2:  1.685279e+00<br />cluster: 6","PC1:  1.745212750<br />PC2:  5.894570e-01<br />cluster: 6","PC1:  2.414844615<br />PC2:  1.038889e+00<br />cluster: 6","PC1:  1.134223852<br />PC2:  6.179169e-01<br />cluster: 6","PC1:  2.119152945<br />PC2:  1.211942e+00<br />cluster: 6","PC1:  3.052252799<br />PC2:  1.090227e+00<br />cluster: 6","PC1:  2.252916321<br />PC2:  3.667427e-01<br />cluster: 6","PC1:  1.442802732<br />PC2:  1.171169e+00<br />cluster: 6","PC1:  2.718730241<br />PC2:  2.095405e-01<br />cluster: 6","PC1:  1.717501474<br />PC2:  1.730282e-01<br />cluster: 6","PC1:  2.384877644<br />PC2:  5.444512e-01<br />cluster: 6","PC1:  2.820233735<br />PC2:  1.393754e+00<br />cluster: 6","PC1:  2.795161052<br />PC2:  2.251812e-01<br />cluster: 6","PC1:  2.268117574<br />PC2:  1.840885e-05<br />cluster: 6","PC1:  2.513738100<br />PC2:  9.300433e-01<br />cluster: 6","PC1:  2.079919771<br />PC2:  1.195124e+00<br />cluster: 6","PC1:  3.766422823<br />PC2:  1.845497e+00<br />cluster: 6","PC1:  1.892941942<br />PC2: -7.887722e-01<br />cluster: 6","PC1:  0.650379109<br />PC2: -5.230580e-01<br />cluster: 6","PC1:  1.156163452<br />PC2: -5.638757e-02<br />cluster: 6","PC1:  3.149593345<br />PC2: -1.803170e-02<br />cluster: 6","PC1:  1.070917669<br />PC2: -5.114071e-01<br />cluster: 6","PC1:  2.179158594<br />PC2:  9.425299e-02<br />cluster: 6","PC1:  2.253879371<br />PC2:  7.487990e-01<br />cluster: 6","PC1:  1.827251679<br />PC2:  6.821639e-01<br />cluster: 6","PC1:  1.916593012<br />PC2:  1.197228e+00<br />cluster: 6","PC1:  1.625106200<br />PC2:  1.231031e+00<br />cluster: 6","PC1:  2.531142042<br />PC2:  7.947053e-01<br />cluster: 6","PC1:  2.971806082<br />PC2:  6.690613e-01<br />cluster: 6","PC1:  2.798182209<br />PC2:  1.562512e+00<br />cluster: 6","PC1:  3.803817767<br />PC2:  5.013559e-01<br />cluster: 6","PC1:  2.376875788<br />PC2:  9.116452e-01<br />cluster: 6","PC1:  2.262935609<br />PC2:  5.342953e-01<br />cluster: 6","PC1:  3.251400241<br />PC2:  2.111347e-02<br />cluster: 6","PC1:  2.419701158<br />PC2:  3.560138e-01<br />cluster: 6","PC1:  3.178789333<br />PC2:  5.964522e-01<br />cluster: 6","PC1:  1.379569830<br />PC2:  7.330161e-01<br />cluster: 6","PC1:  1.971415133<br />PC2: -5.941665e-01<br />cluster: 6","PC1:  1.814332916<br />PC2:  7.649784e-01<br />cluster: 6","PC1:  1.322168088<br />PC2:  8.766133e-01<br />cluster: 6","PC1:  2.783578626<br />PC2:  5.531066e-01<br />cluster: 6","PC1:  1.581951396<br />PC2:  9.067842e-01<br />cluster: 6","PC1:  1.105044754<br />PC2:  8.584603e-01<br />cluster: 6","PC1:  2.492880607<br />PC2:  1.704925e+00<br />cluster: 6","PC1:  3.491748193<br />PC2:  1.025828e+00<br />cluster: 6","PC1:  2.158059439<br />PC2:  1.228031e+00<br />cluster: 6","PC1:  2.294188871<br />PC2:  7.972717e-01<br />cluster: 6","PC1:  1.512190797<br />PC2:  8.652825e-01<br />cluster: 6","PC1:  2.162087544<br />PC2:  1.982782e+00<br />cluster: 6","PC1:  1.832067060<br />PC2:  1.519247e+00<br />cluster: 6","PC1:  2.586642489<br />PC2:  1.349439e+00<br />cluster: 6","PC1:  2.407615992<br />PC2:  1.281334e+00<br />cluster: 6","PC1:  1.752855248<br />PC2:  1.090958e+00<br />cluster: 6","PC1:  1.854464995<br />PC2:  1.175708e+00<br />cluster: 6","PC1:  3.936037583<br />PC2:  1.426789e+00<br />cluster: 6","PC1:  3.374457653<br />PC2: -7.167494e-02<br />cluster: 6","PC1:  1.952622774<br />PC2:  6.782864e-01<br />cluster: 6","PC1:  2.470360490<br />PC2:  1.199859e+00<br />cluster: 6","PC1:  2.342632179<br />PC2: -1.812750e-02<br />cluster: 6","PC1:  2.077058107<br />PC2:  3.712007e-01<br />cluster: 6","PC1:  3.338745708<br />PC2:  1.386964e+00<br />cluster: 6","PC1:  1.273513883<br />PC2:  1.979086e-01<br />cluster: 6","PC1:  2.209994499<br />PC2:  1.447337e-01<br />cluster: 6","PC1:  2.787923898<br />PC2:  9.339716e-01<br />cluster: 6","PC1:  0.457348662<br />PC2: -1.320423e-01<br />cluster: 6","PC1:  3.371718313<br />PC2:  3.990309e-01<br />cluster: 6","PC1:  1.833205509<br />PC2:  5.646490e-01<br />cluster: 6","PC1:  1.340267105<br />PC2: -1.782906e-01<br />cluster: 6","PC1:  1.193702504<br />PC2: -4.815891e-01<br />cluster: 6","PC1:  3.004246919<br />PC2: -4.524768e-01<br />cluster: 6","PC1:  1.301641734<br />PC2: -4.294494e-01<br />cluster: 6","PC1:  1.897048227<br />PC2:  3.311983e-01<br />cluster: 6","PC1:  2.947200416<br />PC2:  3.463548e-02<br />cluster: 6","PC1:  2.181987356<br />PC2:  6.130499e-01<br />cluster: 6","PC1:  2.107117346<br />PC2:  2.614924e-01<br />cluster: 6","PC1:  2.642180512<br />PC2: -1.504574e+00<br />cluster: 6","PC1:  1.417211864<br />PC2: -7.908470e-02<br />cluster: 6","PC1:  1.822568262<br />PC2: -7.711733e-01<br />cluster: 6","PC1:  2.517479511<br />PC2: -6.585002e-01<br />cluster: 6","PC1:  1.721929318<br />PC2:  7.658597e-01<br />cluster: 6","PC1:  1.886079023<br />PC2: -8.381527e-01<br />cluster: 6","PC1:  2.230649169<br />PC2: -9.130985e-01<br />cluster: 6","PC1:  1.164286244<br />PC2: -3.590876e-01<br />cluster: 6","PC1:  2.199409087<br />PC2: -4.107637e-01<br />cluster: 6","PC1:  2.145666355<br />PC2:  1.301166e-01<br />cluster: 6","PC1:  3.396331525<br />PC2: -3.600897e-01<br />cluster: 6","PC1:  1.806712991<br />PC2: -4.678268e-01<br />cluster: 6","PC1:  0.561337479<br />PC2:  2.337681e-01<br />cluster: 6","PC1:  2.987675645<br />PC2: -2.964269e-02<br />cluster: 6","PC1:  1.890555447<br />PC2: -5.439352e-01<br />cluster: 6","PC1:  2.911755074<br />PC2: -1.343830e-01<br />cluster: 6","PC1:  2.402044984<br />PC2: -5.435134e-01<br />cluster: 6","PC1:  3.343496888<br />PC2: -6.125104e-01<br />cluster: 6","PC1:  3.001926413<br />PC2:  3.828722e-01<br />cluster: 6","PC1:  2.636258367<br />PC2: -1.271728e-01<br />cluster: 6","PC1:  2.370360171<br />PC2: -4.558595e-02<br />cluster: 6","PC1:  1.065451616<br />PC2:  2.859171e-01<br />cluster: 6","PC1:  1.358573150<br />PC2:  3.482486e-01<br />cluster: 6","PC1:  2.897285910<br />PC2: -5.688207e-01<br />cluster: 6","PC1:  3.080907070<br />PC2: -5.402682e-01<br />cluster: 6","PC1:  3.163328691<br />PC2: -5.713403e-01<br />cluster: 6","PC1:  2.355165373<br />PC2: -1.622677e-01<br />cluster: 6","PC1:  0.454522970<br />PC2:  1.595013e-01<br />cluster: 6","PC1:  1.959130627<br />PC2:  2.548159e-01<br />cluster: 6","PC1:  1.929219902<br />PC2:  3.412624e-01<br />cluster: 6","PC1:  1.771516422<br />PC2:  1.425100e+00<br />cluster: 6","PC1:  1.586637458<br />PC2: -8.841450e-02<br />cluster: 6","PC1:  0.898756651<br />PC2:  9.907016e-01<br />cluster: 6","PC1:  3.101990429<br />PC2: -4.318244e-01<br />cluster: 6","PC1:  2.840616748<br />PC2:  3.959610e-01<br />cluster: 6","PC1:  1.999853818<br />PC2:  3.074339e-01<br />cluster: 6","PC1:  2.446214258<br />PC2:  1.210374e+00<br />cluster: 6","PC1:  1.994469825<br />PC2:  7.130286e-01<br />cluster: 6","PC1:  2.403831898<br />PC2:  5.775534e-01<br />cluster: 6","PC1:  2.677982546<br />PC2:  6.087495e-01<br />cluster: 6","PC1:  3.057858121<br />PC2:  3.917142e-01<br />cluster: 6","PC1:  1.832033861<br />PC2:  7.986342e-01<br />cluster: 6","PC1:  1.317153672<br />PC2:  7.236011e-01<br />cluster: 6","PC1:  1.985213174<br />PC2:  9.628253e-01<br />cluster: 6","PC1:  1.249801116<br />PC2:  1.685001e+00<br />cluster: 6","PC1:  2.719166491<br />PC2: -4.431375e-01<br />cluster: 6","PC1:  2.184001832<br />PC2: -3.057635e-01<br />cluster: 6","PC1:  2.430765691<br />PC2:  1.116304e+00<br />cluster: 6","PC1:  0.738744998<br />PC2:  9.962330e-01<br />cluster: 6","PC1:  0.946816786<br />PC2:  1.156614e+00<br />cluster: 6","PC1:  0.600177277<br />PC2: -2.518538e-02<br />cluster: 6","PC1:  0.838765321<br />PC2:  8.841709e-01<br />cluster: 6","PC1:  0.977812465<br />PC2:  7.246846e-01<br />cluster: 6","PC1:  1.833946252<br />PC2:  8.644801e-01<br />cluster: 6","PC1:  1.556216491<br />PC2:  7.859387e-01<br />cluster: 6","PC1:  1.593683360<br />PC2:  9.258140e-01<br />cluster: 6","PC1:  1.375802945<br />PC2:  1.599094e+00<br />cluster: 6","PC1:  1.845835414<br />PC2:  4.887708e-02<br />cluster: 6","PC1:  1.105085871<br />PC2:  1.131398e+00<br />cluster: 6","PC1:  1.125506643<br />PC2:  2.567591e-01<br />cluster: 6","PC1:  2.302471740<br />PC2:  7.413831e-01<br />cluster: 6","PC1:  2.570054969<br />PC2:  4.127884e-01<br />cluster: 6","PC1:  1.326245331<br />PC2:  1.356141e+00<br />cluster: 6","PC1:  1.190756148<br />PC2:  4.329907e-01<br />cluster: 6","PC1:  0.782024298<br />PC2:  7.225154e-01<br />cluster: 6","PC1:  0.420369895<br />PC2:  2.142333e+00<br />cluster: 6","PC1:  1.720993453<br />PC2:  2.803569e-01<br />cluster: 6","PC1:  1.330357005<br />PC2:  8.776479e-01<br />cluster: 6","PC1:  1.318221891<br />PC2:  7.399961e-01<br />cluster: 6","PC1:  1.793200201<br />PC2:  1.284985e+00<br />cluster: 6","PC1:  0.630605370<br />PC2:  2.433600e-01<br />cluster: 6","PC1:  1.713658364<br />PC2:  7.120612e-01<br />cluster: 6","PC1:  1.304801710<br />PC2:  1.160922e+00<br />cluster: 6","PC1:  1.732765314<br />PC2:  5.246931e-01<br />cluster: 6","PC1:  0.925323443<br />PC2:  1.275420e+00<br />cluster: 6","PC1:  1.553512964<br />PC2:  8.078980e-01<br />cluster: 6","PC1:  1.556230502<br />PC2:  4.981801e-02<br />cluster: 6","PC1:  1.496681240<br />PC2:  2.157746e+00<br />cluster: 6","PC1:  1.190151162<br />PC2:  1.711528e+00<br />cluster: 6","PC1:  1.570346036<br />PC2:  1.421922e+00<br />cluster: 6","PC1:  2.222166901<br />PC2:  1.748810e+00<br />cluster: 6","PC1:  3.136468557<br />PC2:  1.050638e+00<br />cluster: 6","PC1:  1.863841219<br />PC2:  1.792624e+00<br />cluster: 6","PC1:  1.623363276<br />PC2:  1.750192e+00<br />cluster: 6","PC1:  2.474611278<br />PC2:  1.077281e+00<br />cluster: 6","PC1:  1.912458856<br />PC2:  8.919674e-01<br />cluster: 6","PC1:  2.113779897<br />PC2:  1.144648e+00<br />cluster: 6","PC1:  0.738758653<br />PC2:  4.752887e-01<br />cluster: 6","PC1:  0.731788947<br />PC2:  2.006142e-01<br />cluster: 6","PC1:  0.278065292<br />PC2:  2.147188e+00<br />cluster: 6","PC1:  0.822035628<br />PC2:  1.655687e+00<br />cluster: 6","PC1:  1.046873105<br />PC2:  1.274838e+00<br />cluster: 6","PC1:  1.797855317<br />PC2:  6.874133e-01<br />cluster: 6","PC1:  2.090721883<br />PC2:  4.172694e-01<br />cluster: 6","PC1:  2.141052329<br />PC2:  1.164660e+00<br />cluster: 6","PC1:  2.782668798<br />PC2:  6.574001e-01<br />cluster: 6","PC1:  2.072967432<br />PC2:  1.265677e+00<br />cluster: 6","PC1:  2.313905296<br />PC2: -5.815931e-01<br />cluster: 6","PC1:  1.364079093<br />PC2:  1.309740e+00<br />cluster: 6","PC1:  2.166436082<br />PC2:  3.460964e-01<br />cluster: 6","PC1:  0.574430043<br />PC2:  2.342505e-02<br />cluster: 6","PC1:  2.190308664<br />PC2:  1.799693e-01<br />cluster: 6","PC1:  2.346661574<br />PC2:  5.929719e-02<br />cluster: 6","PC1:  2.197986731<br />PC2: -2.574929e-01<br />cluster: 6","PC1:  0.404958215<br />PC2: -6.052720e-03<br />cluster: 6","PC1:  1.976530519<br />PC2:  1.138220e+00<br />cluster: 6","PC1:  1.238544072<br />PC2:  9.404470e-01<br />cluster: 6","PC1:  2.010163174<br />PC2:  3.198238e-01<br />cluster: 6","PC1:  2.102115012<br />PC2:  1.041317e+00<br />cluster: 6","PC1:  1.208458725<br />PC2:  9.611662e-01<br />cluster: 6","PC1:  1.166717549<br />PC2:  6.942509e-01<br />cluster: 6","PC1:  0.816556545<br />PC2:  6.741829e-02<br />cluster: 6","PC1:  2.071539198<br />PC2:  3.488909e-01<br />cluster: 6","PC1:  2.410758028<br />PC2:  3.607056e-01<br />cluster: 6","PC1:  2.479895899<br />PC2:  1.432089e-01<br />cluster: 6","PC1:  1.569552086<br />PC2:  3.082839e-01<br />cluster: 6","PC1:  0.657931056<br />PC2:  4.892885e-01<br />cluster: 6","PC1:  1.412319386<br />PC2:  1.224781e+00<br />cluster: 6","PC1:  2.892084341<br />PC2:  7.270394e-01<br />cluster: 6","PC1:  2.610448057<br />PC2: -2.390835e-01<br />cluster: 6","PC1:  2.834763830<br />PC2:  5.245681e-01<br />cluster: 6","PC1:  1.920193766<br />PC2:  1.275979e+00<br />cluster: 6","PC1:  1.577256100<br />PC2:  2.259241e-01<br />cluster: 6","PC1:  1.930509351<br />PC2:  1.349587e+00<br />cluster: 6","PC1:  2.465936625<br />PC2:  1.078644e+00<br />cluster: 6","PC1:  1.474560225<br />PC2:  8.051050e-01<br />cluster: 6","PC1:  1.640217632<br />PC2:  9.051393e-01<br />cluster: 6","PC1:  2.528856853<br />PC2: -7.213124e-01<br />cluster: 6","PC1:  1.858061629<br />PC2:  1.928217e-01<br />cluster: 6","PC1:  1.662088471<br />PC2:  1.358562e-01<br />cluster: 6","PC1:  1.196058121<br />PC2: -2.645590e-01<br />cluster: 6","PC1:  1.292880598<br />PC2:  2.500865e-01<br />cluster: 6","PC1:  2.884732305<br />PC2: -1.725996e-01<br />cluster: 6","PC1:  2.151108827<br />PC2: -3.944442e-01<br />cluster: 6","PC1:  2.384810207<br />PC2:  3.311503e-02<br />cluster: 6","PC1:  1.057655368<br />PC2: -2.718744e-01<br />cluster: 6","PC1:  1.559678522<br />PC2: -7.066757e-01<br />cluster: 6","PC1:  1.977227829<br />PC2: -5.642147e-02<br />cluster: 6","PC1:  2.226127898<br />PC2: -3.075629e-01<br />cluster: 6","PC1:  1.184106273<br />PC2: -6.165191e-01<br />cluster: 6","PC1:  2.266785562<br />PC2:  1.646669e-01<br />cluster: 6","PC1:  1.250767606<br />PC2:  1.317106e-01<br />cluster: 6","PC1:  2.093730018<br />PC2:  2.191186e-01<br />cluster: 6","PC1:  1.576595967<br />PC2:  1.858305e-01<br />cluster: 6","PC1:  3.581210828<br />PC2:  1.411518e+00<br />cluster: 6","PC1:  2.148181368<br />PC2:  1.539444e+00<br />cluster: 6","PC1:  1.689811530<br />PC2:  4.031784e-01<br />cluster: 6","PC1:  2.024207520<br />PC2:  1.404985e+00<br />cluster: 6","PC1:  1.968570185<br />PC2:  1.187354e+00<br />cluster: 6","PC1:  0.540502063<br />PC2:  1.211745e+00<br />cluster: 6","PC1:  1.512235406<br />PC2:  3.062034e-02<br />cluster: 6","PC1:  1.487564366<br />PC2:  1.039555e+00<br />cluster: 6","PC1:  1.090391372<br />PC2:  8.549931e-01<br />cluster: 6","PC1:  0.445101670<br />PC2:  6.457805e-02<br />cluster: 6","PC1:  0.295627552<br />PC2: -2.724888e-02<br />cluster: 6","PC1:  1.534788597<br />PC2: -8.573796e-01<br />cluster: 6","PC1:  0.967919725<br />PC2:  5.051126e-01<br />cluster: 6","PC1:  0.557778495<br />PC2: -9.743386e-01<br />cluster: 6","PC1:  1.542401890<br />PC2: -7.423382e-01<br />cluster: 6","PC1:  2.075776620<br />PC2: -3.598936e-01<br />cluster: 6","PC1:  1.607842939<br />PC2:  6.221470e-01<br />cluster: 6","PC1:  1.373194969<br />PC2:  1.443184e+00<br />cluster: 6","PC1:  1.361654793<br />PC2:  1.710380e+00<br />cluster: 6","PC1:  1.230759897<br />PC2:  1.717059e+00<br />cluster: 6","PC1:  2.201972656<br />PC2:  3.787264e-01<br />cluster: 6","PC1:  1.473500854<br />PC2:  9.982333e-01<br />cluster: 6","PC1:  2.219114173<br />PC2:  1.012266e+00<br />cluster: 6","PC1:  1.710079145<br />PC2:  1.158870e+00<br />cluster: 6","PC1:  1.302953131<br />PC2:  6.176884e-01<br />cluster: 6","PC1:  1.501441261<br />PC2:  3.911938e-01<br />cluster: 6","PC1:  1.710973390<br />PC2:  7.041942e-01<br />cluster: 6","PC1:  0.906433667<br />PC2:  1.197479e-01<br />cluster: 6","PC1:  3.130662134<br />PC2:  8.341076e-01<br />cluster: 6","PC1:  2.292705053<br />PC2:  1.063572e+00<br />cluster: 6","PC1:  2.375857000<br />PC2:  1.717000e+00<br />cluster: 6","PC1:  2.482231906<br />PC2: -3.528208e-01<br />cluster: 6","PC1:  3.032955817<br />PC2:  2.089228e-01<br />cluster: 6","PC1:  2.256875896<br />PC2: -3.197798e-01<br />cluster: 6","PC1:  1.940937238<br />PC2:  7.963383e-01<br />cluster: 6","PC1:  3.068074830<br />PC2:  6.555075e-01<br />cluster: 6","PC1:  2.904879971<br />PC2:  4.538981e-01<br />cluster: 6","PC1:  2.073611421<br />PC2:  8.530546e-01<br />cluster: 6","PC1:  1.978372039<br />PC2: -1.013066e+00<br />cluster: 6","PC1:  0.460879209<br />PC2:  1.066965e-01<br />cluster: 6","PC1:  1.667263896<br />PC2:  1.353632e+00<br />cluster: 6","PC1:  1.692513769<br />PC2:  8.545241e-01<br />cluster: 6","PC1:  0.771119811<br />PC2:  1.139372e+00<br />cluster: 6","PC1:  2.299562987<br />PC2:  1.031008e+00<br />cluster: 6","PC1:  2.465620416<br />PC2:  1.063243e+00<br />cluster: 6","PC1:  1.287021657<br />PC2:  1.907447e-01<br />cluster: 6","PC1:  2.445316567<br />PC2:  5.893649e-01<br />cluster: 6","PC1:  1.657314962<br />PC2:  4.252293e-01<br />cluster: 6","PC1:  1.070904654<br />PC2:  9.894969e-01<br />cluster: 6","PC1:  2.029463801<br />PC2:  6.906794e-01<br />cluster: 6","PC1:  2.568010927<br />PC2:  1.296689e+00<br />cluster: 6","PC1:  3.055646742<br />PC2:  5.860126e-01<br />cluster: 6","PC1:  1.536784307<br />PC2:  5.597975e-01<br />cluster: 6","PC1:  1.960873584<br />PC2:  7.597862e-01<br />cluster: 6","PC1:  2.435207824<br />PC2:  7.812371e-01<br />cluster: 6","PC1:  2.449482963<br />PC2:  4.899174e-01<br />cluster: 6","PC1:  1.161740276<br />PC2:  2.602096e-01<br />cluster: 6","PC1:  1.841235570<br />PC2:  1.380960e-01<br />cluster: 6","PC1:  2.296391942<br />PC2:  6.322621e-01<br />cluster: 6","PC1:  2.238939984<br />PC2:  3.306992e-01<br />cluster: 6","PC1:  1.856692353<br />PC2:  1.264051e+00<br />cluster: 6","PC1:  2.689827156<br />PC2:  1.257245e+00<br />cluster: 6","PC1:  2.389426977<br />PC2:  8.900821e-01<br />cluster: 6","PC1:  1.978218139<br />PC2:  7.516731e-01<br />cluster: 6","PC1:  2.436155851<br />PC2:  8.668691e-01<br />cluster: 6","PC1:  2.012592571<br />PC2:  1.202352e+00<br />cluster: 6","PC1:  2.935570476<br />PC2:  9.434963e-02<br />cluster: 6","PC1:  3.100461057<br />PC2:  4.266921e-01<br />cluster: 6","PC1:  2.568789607<br />PC2:  9.210995e-01<br />cluster: 6","PC1:  2.839643528<br />PC2:  5.260322e-02<br />cluster: 6","PC1:  2.549278903<br />PC2:  4.702154e-01<br />cluster: 6","PC1:  1.517602983<br />PC2:  5.613400e-01<br />cluster: 6","PC1:  2.085564001<br />PC2:  1.916184e+00<br />cluster: 6","PC1:  2.770837843<br />PC2:  5.061483e-01<br />cluster: 6","PC1:  1.598921026<br />PC2:  2.320141e-01<br />cluster: 6","PC1:  0.974397567<br />PC2:  9.513793e-01<br />cluster: 6","PC1:  0.858353066<br />PC2:  5.455036e-01<br />cluster: 6","PC1:  1.379245888<br />PC2: -2.316276e-02<br />cluster: 6","PC1:  2.172672017<br />PC2: -1.090402e-01<br />cluster: 6","PC1:  1.816732403<br />PC2:  8.898304e-02<br />cluster: 6","PC1:  2.488030531<br />PC2:  3.306458e-01<br />cluster: 6","PC1:  2.542554111<br />PC2:  4.644019e-01<br />cluster: 6","PC1:  3.484727755<br />PC2:  6.679868e-01<br />cluster: 6","PC1:  1.273820993<br />PC2:  1.224733e+00<br />cluster: 6","PC1:  1.632971204<br />PC2:  2.408410e+00<br />cluster: 6","PC1:  1.538882771<br />PC2:  1.078775e+00<br />cluster: 6","PC1:  2.010982717<br />PC2:  6.729405e-01<br />cluster: 6","PC1:  2.838532545<br />PC2:  2.703001e-01<br />cluster: 6","PC1:  2.594404627<br />PC2:  8.925326e-01<br />cluster: 6","PC1:  1.945055589<br />PC2:  1.082845e+00<br />cluster: 6","PC1:  1.660140799<br />PC2:  1.164144e+00<br />cluster: 6","PC1:  1.269823469<br />PC2:  8.012095e-01<br />cluster: 6","PC1:  0.937211283<br />PC2:  7.821198e-01<br />cluster: 6","PC1:  1.153740126<br />PC2:  9.901486e-02<br />cluster: 6","PC1:  1.211053133<br />PC2:  3.199603e-01<br />cluster: 6","PC1:  1.711385099<br />PC2:  1.348202e-01<br />cluster: 6","PC1:  1.998837577<br />PC2:  1.116159e+00<br />cluster: 6","PC1:  1.460030841<br />PC2:  6.048687e-01<br />cluster: 6","PC1:  2.309963671<br />PC2: -9.201757e-02<br />cluster: 6","PC1:  2.487343971<br />PC2: -3.567149e-01<br />cluster: 6","PC1:  1.394929163<br />PC2:  6.190761e-01<br />cluster: 6","PC1:  1.259120084<br />PC2:  1.807280e+00<br />cluster: 6","PC1:  1.123043021<br />PC2:  1.380287e-01<br />cluster: 6","PC1:  1.184458354<br />PC2:  9.110562e-01<br />cluster: 6","PC1:  1.060033515<br />PC2:  5.879528e-01<br />cluster: 6","PC1:  2.131438163<br />PC2:  6.572859e-01<br />cluster: 6","PC1:  1.531174102<br />PC2:  1.240176e+00<br />cluster: 6","PC1:  1.322347347<br />PC2:  1.787624e-01<br />cluster: 6","PC1:  1.210091790<br />PC2:  1.139071e+00<br />cluster: 6","PC1:  0.891538975<br />PC2:  1.337842e+00<br />cluster: 6","PC1:  0.725638313<br />PC2:  2.682589e-01<br />cluster: 6","PC1:  1.122953365<br />PC2: -2.986139e-01<br />cluster: 6","PC1:  1.003772571<br />PC2:  1.100778e+00<br />cluster: 6","PC1:  1.539376361<br />PC2:  1.751041e+00<br />cluster: 6","PC1:  1.985165212<br />PC2:  7.602638e-01<br />cluster: 6","PC1:  1.022941765<br />PC2:  6.648224e-01<br />cluster: 6","PC1:  1.306036553<br />PC2:  1.357750e+00<br />cluster: 6","PC1:  1.062525709<br />PC2:  8.983343e-01<br />cluster: 6","PC1:  0.337112468<br />PC2:  7.421822e-01<br />cluster: 6","PC1:  0.460578309<br />PC2:  7.731000e-01<br />cluster: 6","PC1:  0.770952058<br />PC2:  1.071249e+00<br />cluster: 6","PC1:  0.849894217<br />PC2:  1.079431e+00<br />cluster: 6","PC1:  1.524684318<br />PC2:  1.017998e+00<br />cluster: 6","PC1:  1.381455724<br />PC2:  1.078496e+00<br />cluster: 6","PC1:  2.431904721<br />PC2: -1.682773e-01<br />cluster: 6","PC1:  1.280591106<br />PC2:  2.860540e-01<br />cluster: 6","PC1:  0.794280896<br />PC2:  1.083690e+00<br />cluster: 6","PC1:  0.954412403<br />PC2:  6.436650e-02<br />cluster: 6","PC1:  1.778357373<br />PC2:  7.002351e-01<br />cluster: 6","PC1:  1.992727202<br />PC2: -3.501475e-02<br />cluster: 6","PC1:  1.531756885<br />PC2:  2.580583e-01<br />cluster: 6","PC1:  2.000742938<br />PC2:  6.049945e-01<br />cluster: 6","PC1:  1.191023298<br />PC2:  5.410197e-01<br />cluster: 6","PC1:  1.318877973<br />PC2:  7.041266e-01<br />cluster: 6","PC1:  2.090098974<br />PC2:  2.696313e-01<br />cluster: 6","PC1:  3.071681762<br />PC2:  2.633686e-01<br />cluster: 6","PC1:  1.230296175<br />PC2: -1.596371e-01<br />cluster: 6","PC1:  1.448526696<br />PC2: -5.791470e-01<br />cluster: 6","PC1:  2.647525911<br />PC2: -1.074343e+00<br />cluster: 6","PC1:  1.734731933<br />PC2:  4.161019e-01<br />cluster: 6","PC1:  1.968885778<br />PC2: -4.398838e-01<br />cluster: 6","PC1:  2.108075882<br />PC2: -5.951356e-01<br />cluster: 6","PC1:  2.262762480<br />PC2:  2.673457e-01<br />cluster: 6","PC1:  2.464830973<br />PC2: -5.005720e-01<br />cluster: 6","PC1:  1.055150675<br />PC2:  3.004927e-01<br />cluster: 6","PC1:  1.110945103<br />PC2: -1.989829e-01<br />cluster: 6","PC1:  0.858751196<br />PC2:  2.940294e-01<br />cluster: 6","PC1:  1.012976370<br />PC2: -5.271347e-02<br />cluster: 6","PC1:  2.024601284<br />PC2: -3.103539e-01<br />cluster: 6","PC1:  1.103269395<br />PC2:  2.534608e-01<br />cluster: 6","PC1:  3.189853670<br />PC2:  9.513669e-02<br />cluster: 6","PC1:  2.733445829<br />PC2:  8.812833e-02<br />cluster: 6","PC1:  3.099372335<br />PC2:  3.117739e-01<br />cluster: 6","PC1:  3.321046869<br />PC2: -2.101849e-01<br />cluster: 6","PC1:  2.192339637<br />PC2:  1.944268e-01<br />cluster: 6","PC1:  3.126015653<br />PC2:  1.413032e-01<br />cluster: 6","PC1:  2.372456874<br />PC2:  1.733520e+00<br />cluster: 6","PC1:  2.905416926<br />PC2: -5.266712e-01<br />cluster: 6","PC1:  2.459939323<br />PC2:  3.252116e-01<br />cluster: 6","PC1:  2.450025299<br />PC2:  4.086692e-01<br />cluster: 6","PC1:  2.709970493<br />PC2: -7.224402e-01<br />cluster: 6","PC1:  1.799576158<br />PC2:  7.178140e-01<br />cluster: 6","PC1:  2.295392720<br />PC2:  8.634922e-01<br />cluster: 6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,255,51,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,255,51,1)"}},"hoveron":"points","name":"6","legendgroup":"6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-6.54602979949938,7.55012830831359],"tickmode":"array","ticktext":["-4","0","4"],"tickvals":[-4,0,4],"categoryorder":"array","categoryarray":["-4","0","4"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"PC1","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-5.10664897965221,3.05002860111676],"tickmode":"array","ticktext":["-5.0","-2.5","0.0","2.5"],"tickvals":[-5,-2.5,0,2.5],"categoryorder":"array","categoryarray":["-5.0","-2.5","0.0","2.5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"PC2","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":"Cluster","orientation":"h","y":-0.15,"x":0.2},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"214a19372a":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"214a19372a","visdat":{"214a19372a":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

<p class="caption">(\#fig:cluster-scatterplot)Interactive scatterplot of PC1 and PC3 colored by cluster</p>
</div>

While all clusters overlap one another to some degree, each occupies distinct feature space. Double-click any cluster in the legend to isolate it. Cluster 1, which covers much of the rural fringe of Dallas-Fort Worth, scores very high on the "rurality" index PC2 (low density / educational attainment) and modestly negative on the "diversity" index PC1. Cluster 2, which includes the two downtowns, scores lower on the "rurality" index PC2 but scores higher on the "diversity" index PC1. A geodemographic analyst may adopt these visualization approaches to explore the proposed typology in greater depth, and aim to produce informative "labels" for each cluster.

### Spatial clustering & regionalization

The geodemographic classification outlined in the previous section offers a useful methodology for identifying similar types of Census tracts in varying parts of a metropolitan region. However, this approach was *aspatial* in that it did not take the geographic properties of the Census tracts into account. In other applications, an analyst may want to generate meaningful clusters that are constrained to be neighboring or contiguous areas. An application of this workflow might be sales territory generation, where sales representatives will be assigned to communities in which they have local market knowledge but also want to minimize overall travel time.

A suite of *regionalization* algorithms are available that adapt the clustering approach by introducing spatial constraints. A spatial constraint might be an additional requirement that the algorithm minimize overall geographic distance between observations, or even that the derived clusters must be geographically contiguous. The latter scenario is explored in this subsection.

The workflow below illustrates the SKATER algorithm [@assunção2006], an acronym that stands for "Spatial 'K'luster Analysis by Tree Edge Removal." This algorithm is implemented in R with the `skater()` function in the **spdep** package, and is also available in PySAL, GeoDa, and ArcGIS as the "[Spatially Constrained Multivariate Clustering](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/how-spatially-constrained-multivariate-clustering-works.htm)" tool.

SKATER relies on the concept of *minimum spanning trees*, where a connectivity graph is drawn between all observations in the dataset with graph edges weighted by the attribute similarity between observations. The graph is then "pruned" by removing edges that connect observations that are not similar to one another.

The setup for SKATER involves similar steps to clustering algorithms used in Chapter \@ref(spatial-analysis-with-us-census-data), as a queens-case contiguity weights matrix is generated. The key differences include the use of *costs* - which represent the differences between neighbors based on an input set of variables, which in this example will be principal components 1 through 8 - and the use of a binary weights matrix with `style = "B"`.


```r
library(spdep)

input_vars <- dfw_pca %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame() 

skater_nbrs <- poly2nb(dfw_pca, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")
```

Once the weights have been generated, a minimum spanning tree is created with `mstree()`, and used in the call to `skater()`.


```r
mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2], 
  input_vars, 
  ncuts = 7,
  crit = 10
)
```

The `ncuts` parameter dictates how many times the algorithm should prune the minimum spanning tree; a value of `7` will create 8 groups. The `crit` parameter is used to determine the minimum number of observations per group; above, we have set this value to `10`, requiring that each region will have at least 10 Census tracts.

The solution can be extracted and assigned to the spatial dataset, then visualized with `geom_sf()`:


```r
dfw_clusters$region <- as.character(regions$group)

ggplot(dfw_clusters, aes(fill = region)) + 
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set1") + 
  theme_void()
```

<div class="figure">
<img src="08-modeling-census-data_files/figure-html/map-skater-1.png" alt="Map of contiguous regions derived with the SKATER algorithm" width="100%" />
<p class="caption">(\#fig:map-skater)Map of contiguous regions derived with the SKATER algorithm</p>
</div>

The algorithm has partitioned the data into eight contiguous regions. These regions are largely geographic in nature (region 5 covers the northwestern portion of the metropolitan area, whereas region 2 covers the east and south), but they also incorporate demographic variations in the data. For example, region 6 covers downtown and uptown Dallas along with the Bishop Arts neighborhood; these represent the highest-density and most traditionally "urban" parts of the metropolitan area. Additionally, region 4 represents the "northeast Tarrant County" suburban community along with similar suburbs in Denton County, which is a socially meaningful sub-region in north Texas.

## Exercises

Identify a different region of the United States of interest to you. Complete the following tasks:

1.  Acquire race/ethnicity data from **tidycensus** for your chosen region and compute the dissimilarity index. How does segregation in your chosen region compare with urban areas in California?
2.  Reproduce the regression modeling workflow outlined in this chapter for your chosen region. Is residual spatial autocorrelation more, or less, of an issue for your region than in Dallas-Fort Worth?
3.  Create a geodemographic classification for your region using the sample code in this chapter. Does the typology you've generated resemble that of Dallas-Fort Worth, or does it differ?
