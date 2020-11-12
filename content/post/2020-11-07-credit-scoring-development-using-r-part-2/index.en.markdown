---
title: Credit Scoring Development Using R - Part 2
author: Ng Yong Kad
date: '2020-11-07'
slug: []
categories:
  - model-development
tags:
  - R
  - credit-scoring
lastmod: '2020-11-07T15:38:40+08:00'
keywords: []
description: ''
comment: no
toc: TRUE
autoCollapseToc: TRUE
postMetaInFooter: no
hiddenFromHomePage: no
contentCopyright: no
reward: no
mathjax: no
mathjaxEnableSingleDollar: no
mathjaxEnableAutoNumber: no
hideHeaderAndFooter: no
flowchartDiagrams:
  enable: no
  options: ''
sequenceDiagrams:
  enable: no
  options: ''
---


This is the second part of a 3-parts series.


## 4.0 Univariate Analysis


### 4.1 Fine Classing


Fine classing is a technique that groups a variable's values into a number of fine bins.  Using these bins, a measure of the variable's predictive power, known as information value (IV), can be computed. Also from these fine bins, further grouping can be carried out to result in coarse classing.  As will be shown in the section below, bins from coarse classing are the bins that will be used in a credit scorecard.  

Specifically, the fine classing technique used in this post is called the frequency method in the package `scorecard`. What this technique entails is grouping a variable's values by having equal percentage of counts in each bin. This is a commonly used technique (see the note below). Following industry practice, the maximum number of fine bins is set at 20.

{{% admonition note "Note" true %}}
+ There are other techniques available in the package `scorecard`.  Interested readers are referred to its  
 [manual](https://cran.r-project.org/web/packages/scorecard/index.html).

+ The number of fine bins depends on the distribution of the variable.  Some variables will have concentration in some values.  In such cases, the number of fine bins will turn out to be less than 20.

+ The frequency method in the package `scorecard` works with **numeric** variables only.  It is therefore necessary to convert or recode character variables (e.g. sex, education etc.) into numeric variables before hand (as the collectors of the data set used here had done).

{{% /admonition %}}

From this point on, the package `scorecard` will be used.




```r
library(scorecard)

# select all the variables for IV computation
var_list <- dev %>%
  select(-id, -gb_flag) %>%
  names()

# invoke the woebin function in package scorecard
fine_class <- woebin(dev, 
                    y = "gb_flag", 
                    x = var_list, 
                    positive = 1, # the value in gb_flag that indicates default
                    method = "freq", # frequency method
                    bin_num_limit = 20)  # the max number of fine bins
```

```
## [INFO] creating woe binning ...
```

```r
# collect the IV of the variables
iv <- map_df(fine_class, ~pluck(.x, 10, 1)) %>%
  pivot_longer(everything(), names_to = "var", values_to = "iv")
```


|var            |        iv|
|:--------------|---------:|
|limit_bal      | 0.1785871|
|sex            | 0.0079849|
|education      | 0.0164957|
|marriage       | 0.0056892|
|age            | 0.0193498|
|bill_amt1      | 0.0128997|
|bill_amt2      | 0.0146458|
|bill_amt3      | 0.0128501|
|bill_amt4      | 0.0148755|
|bill_amt5      | 0.0153958|
|bill_amt6      | 0.0247551|
|pay_amt1       | 0.1066663|
|pay_amt2       | 0.0798784|
|pay_amt3       | 0.0744712|
|pay_amt4       | 0.0490983|
|pay_amt5       | 0.0421035|
|pay_amt6       | 0.0199621|
|pay_1_recode   | 0.8648336|
|pay_2_recode   | 0.5459762|
|pay_3_recode   | 0.4117610|
|pay_4_recode   | 0.3616306|
|pay_5_recode   | 0.3354879|
|pay_6_recode   | 0.2923127|
|avg_deq_l3m    | 0.8797259|
|avg_deq_l6m    | 0.3011544|
|max_deq_l3m    | 0.7979072|
|max_deq_l6m    | 0.7441613|
|min_deq_l3m    | 0.4711887|
|min_deq_l6m    | 0.0000000|
|bill_amt1_util | 0.0156583|
|bill_amt2_util | 0.0133279|
|bill_amt3_util | 0.0128110|
|bill_amt4_util | 0.0177728|
|bill_amt5_util | 0.0182308|
|bill_amt6_util | 0.0209823|
|avg_util_l3m   | 0.1136387|
|avg_util_l6m   | 0.1346854|
|max_util_l3m   | 0.0899689|
|max_util_l6m   | 0.1039890|
|min_util_l3m   | 0.0146800|
|min_util_l6m   | 0.0072010|

The reports of the fine classing are contained in `fine_class` and can be viewed by typing `fine_class` and running it. Due to the massive amount of reports, only the first one is shown below. 


```
## $limit_bal
##      variable             bin count count_distr good bad   badprob        woe
##  1: limit_bal    [-Inf,30000)  1990  0.08291667 1272 718 0.3608040  0.6802441
##  2: limit_bal   [30000,50000)  1477  0.06154167  936 541 0.3662830  0.7039240
##  3: limit_bal   [50000,70000)  3358  0.13991667 2466 892 0.2656343  0.2352337
##  4: limit_bal  [70000,100000)  2369  0.09870833 1774 595 0.2511608  0.1596895
##  5: limit_bal [100000,140000)  2452  0.10216667 1853 599 0.2442904  0.1228206
##  6: limit_bal [140000,160000)  1512  0.06300000 1228 284 0.1878307 -0.2120476
##  7: limit_bal [160000,200000)  1973  0.08220833 1623 350 0.1773948 -0.2819782
##  8: limit_bal [200000,210000)  1202  0.05008333  987 215 0.1788686 -0.2719118
##  9: limit_bal [210000,270000)  2725  0.11354167 2259 466 0.1710092 -0.3263717
## 10: limit_bal [270000,360000)  2258  0.09408333 1941 317 0.1403897 -0.5599366
## 11: limit_bal [360000,430000)  1448  0.06033333 1235 213 0.1470994 -0.5054139
## 12: limit_bal   [430000, Inf)  1236  0.05150000 1090 146 0.1181230 -0.7582061
##          bin_iv  total_iv breaks is_special_values
##  1: 0.045171693 0.1785871  30000             FALSE
##  2: 0.036066804 0.1785871  50000             FALSE
##  3: 0.008242682 0.1785871  70000             FALSE
##  4: 0.002628082 0.1785871 100000             FALSE
##  5: 0.001593515 0.1785871 140000             FALSE
##  6: 0.002665805 0.1785871 160000             FALSE
##  7: 0.006024928 0.1785871 200000             FALSE
##  8: 0.003423424 0.1785871 210000             FALSE
##  9: 0.010999969 0.1785871 270000             FALSE
## 10: 0.024967126 0.1785871 360000             FALSE
## 11: 0.013268441 0.1785871 430000             FALSE
## 12: 0.023534628 0.1785871    Inf             FALSE
```

{{% admonition note "Note" true %}}
All of the generated fine classing reports can be saved in csv file using the package `erer` `write.list` function. Interested reader is encouraged to explore. 
{{% /admonition %}}


### 4.2 Initial Variables Removal


As discussed in the section above, new variables are derived because not all raw variables should be used directly. Numeric variables in amount such as balance, limit, payment are not advisable to be used because the variances in their values are large, rendering a credit scorecard unstable. Instead, these variables are converted into ratios (e.g. utilization rates) which are bounded below by zero and the maximum value should not be too far away from 100%. 

Also discussed above is variables with a lot of missing values should be discarded. But this does not apply in this post because all variables have no missing values. 

Lastly, variables with low predictive power as measured by IV are also removed (IV<0.02). However, since *age*'s IV is at the boarder line and it is preferable to have some demographic variables in a credit scorecard, *age* is kept.    

With the above, the following variables are removed:

1. Low IV 
 + sex
 + education
 + marriage
 + bill_amt1 to bill_amt5
 + pay_amt6
 + min_deq_l6m
 + bill_amt1_util to bill_amt5_util
 + min_util_l3m
 + min_util_l6m

2. Variables in amount
 + limit_bal
 + pay_amt1 to pay_amt5
 + bill_amt6
 

```r
# keep the relevant variables
dev <- dev %>%
  select(id, age, gb_flag:max_util_l6m, avg_deq_l3m:min_deq_l3m, bill_amt6_util)
```
 

### 4.2 Variables Clustering


An analyst frequently faces thousands of variables. Even after filtering out variables with high missing values or with low IV values, hundreds of variables will still remain. 

In addition to the IV and missing values criteria, *variables clustering* can be added to the variables reduction tool kit. The idea of variables clustering is to group similar variables into one cluster. Then, a few variables can be selected from each of the different clusters. An additional benefit that stems from variables clustering is that it could help reduce multi-collinearity among variables.

While it is not really applicable in this post as the number of variables is small, variables clustering is conducted for completeness.  From the stability plot shown below, 4 clusters appear appropriate for the development sample.  Hence, 4 cluster is set using `cuttree`.  In the final output table below, the "x" column is cluster name.  The application of this output is provided in the section below.  

{{% admonition note "Note" true %}}
Variables clustering is computation intensive.  It could take up to hours to complete the task if the number of variables is huge.
{{% /admonition %}}


```r
library(ClustOfVar)

# perform variables clustering
tree <- dev %>%
  select(-id, -gb_flag) %>%
  hclustvar() 

# stability plot
set.seed(345)
stab <- stability(tree, B = 30) # B is bootstrapping sample. set B at 30 to 50 to reduce computing time 
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
boxplot(stab$matCR, main = "Dispersion of the adjusted Rand index")
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-6-2.png" width="672" />

```r
# generate the final cluster output
clus <- cutree(tree, 4) 
```



|               |  x|
|:--------------|--:|
|age            |  1|
|pay_1_recode   |  2|
|pay_2_recode   |  2|
|pay_3_recode   |  2|
|pay_4_recode   |  3|
|pay_5_recode   |  3|
|pay_6_recode   |  3|
|avg_deq_l3m    |  2|
|avg_deq_l6m    |  2|
|max_deq_l3m    |  2|
|max_deq_l6m    |  2|
|min_deq_l3m    |  2|
|min_deq_l6m    |  3|
|bill_amt1_util |  4|
|bill_amt2_util |  4|
|bill_amt3_util |  4|
|bill_amt4_util |  4|
|bill_amt5_util |  4|
|bill_amt6_util |  4|
|avg_util_l3m   |  4|
|avg_util_l6m   |  4|
|max_util_l3m   |  4|
|max_util_l6m   |  4|
 
 
### 4.3 Variables Reduction


One way to utilize the output from variables clustering is combining it with IV as follows:  


|var            | clus|        iv|
|:--------------|----:|---------:|
|age            |    1| 0.0193498|
|avg_deq_l3m    |    2| 0.8797259|
|pay_1_recode   |    2| 0.8648336|
|max_deq_l3m    |    2| 0.7979072|
|max_deq_l6m    |    2| 0.7441613|
|pay_2_recode   |    2| 0.5459762|
|min_deq_l3m    |    2| 0.4711887|
|pay_3_recode   |    2| 0.4117610|
|avg_deq_l6m    |    2| 0.3011544|
|pay_4_recode   |    3| 0.3616306|
|pay_5_recode   |    3| 0.3354879|
|pay_6_recode   |    3| 0.2923127|
|min_deq_l6m    |    3| 0.0000000|
|avg_util_l6m   |    4| 0.1346854|
|avg_util_l3m   |    4| 0.1136387|
|max_util_l6m   |    4| 0.1039890|
|max_util_l3m   |    4| 0.0899689|
|bill_amt6_util |    4| 0.0209823|
|bill_amt5_util |    4| 0.0182308|
|bill_amt4_util |    4| 0.0177728|
|bill_amt1_util |    4| 0.0156583|
|bill_amt2_util |    4| 0.0133279|
|bill_amt3_util |    4| 0.0128110|

In cluster 1, there is only one variable. Thus, *age* is short-listed. In cluster 2, the first 2 variables *avg_deq_l3m* and *pay_1_recode* are selected because they have the highest IVs in the cluster. Likewise for cluster 3, *pay_4_recode* and *pay_5_recode* are chosen. Finally, in cluster 4, the first 2 variables' IV are quite close and these 2 variables are of average utilization. Therefore, instead of choosing the first 2 variables, the first (*avg_util_l6m*) and the third (*max_util_l6m*) are short-listed.   

{{% admonition note "Note" true %}}
The process presented above is ***one of many ways*** of using variables clustering.  Different analysts use variables clustering differently.     
{{% /admonition %}}


## 5.0 Coarse Classing


To do coarse classing, it is necessary to go back to the fine classing reports generated previously, focusing solely on the short-listed variables. 



One can use the fine classing reports directly for coarse classing. In this post, plots are used because they are easier for presentation. The plots are shown below one by one for the short-listed variables.   

The first plot is for *age*.  Referring to the plot, the bad rates of this variable is of U-shape.  A plausible explanation that can be put forward is younger credit card holders are risky because of their lower income. On the other hand, old credit card holders are also risky because of having multiple loans, in retirement, or incurring heavy medical costs etc. For *age*, the coarse classing could be: a) <25, b)25 to <45, c) 45 and above. Note that the third bin is not starting from 46 as suggested by the plot. 45 is used just to make the bound nicer.  


```r
plot <- woebin_plot(fine_class_final)
plot[[1]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-10-1.png" width="672" />

*ave_deq_l3m* is straight-forward. The bins with 44.1% and 45.1% bad rates are grouped together while others remain unchanged.


```r
plot[[2]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-11-1.png" width="672" />

No further classing is necessary for *pay_1_recode*.


```r
plot[[3]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-12-1.png" width="672" />

No further classing is necessary for *pay_4_recode*.


```r
plot[[4]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-13-1.png" width="672" />

No further classing is necessary for *pay_5_recode*.


```r
plot[[5]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Utilization rates are expected to be monotonic in bad rates. That is, higher utilization rates associate with higher bad rates, or vice versa.  Yet, this expectation is not consistent with what shown in the plot of *avg_util_l6m*.  In the plot, it is those with lower utilization rates (i.e. first few bins) have higher bad rates.  

For this variable, the coarse classing could be: a) <45%, b) 45% to <83%, and c) 83% and above. Note that minor tweaks are applied to the bounds of the bins. (Refer to the fine class report for the details of the bins.)  


```r
plot[[6]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-15-1.png" width="672" />

The situation of *max_util_l6m* is similar to *avg_util_l6m*. The coarse classing could be: a) <43%, b) 43% to <100%, and c) 100% and above.  Minor tweaks are applied to the bounds of the bins. 


```r
plot[[7]]
```

<img src="/post/2020-11-07-credit-scoring-development-using-r-part-2/index.en_files/figure-html/unnamed-chunk-16-1.png" width="672" />

The bounds resulted from the coarse classing of all short-listed variables can be saved into a list for future use. 


```r
breaks_list <- list(age = c("25", "45"), 
                    avg_deq_l3m = c("0.67",  "2"), 
                    pay_1_recode = c("1", "2"), 
                    pay_4_recode = c("1"), 
                    pay_5_recode = c("2"),
                    avg_util_l6m = c("0.45", "0.83"),
                    max_util_l6m = c("0.43", "1.00")
                    )
```

{{% admonition danger Danger %}}
What is to be done next is important.

As opposed to other applications, credit scorecards in financial institutions are usually developed to predict "good" instead of "bad" (i.e. default).  

The reason of predicting "good" is because credit scores are used as a risk ranking tool. Credit worthy customers (i.e. good customers) are given high scores.  And this is easy to communicate to non-technical persons.

Given the afore-mentioned reason, the codes need to be changed slightly.  That is, `positive = 1` needs to be set as `positive = 0`.  This will not change the IV values and the WOE values of the bins.  However, the sign of the WOE values will change.  The WOE values with `positive = 0` are the values that will be used to develop credit scorecards.


```r
# set positive = 0
coarse_class <- woebin(dev, 
                       y = "gb_flag", 
                       x = short_var_list, 
                       positive = 0, 
                       method = "freq", 
                       breaks_list = breaks_list) # from coarse classing results
```

```
## [INFO] creating woe binning ...
```

```r
# transform variables' values into WOE values
dev_woe <- woebin_ply(dev, coarse_class)  
```

```
## [INFO] converting into woe values ...
```
{{% /admonition %}}



