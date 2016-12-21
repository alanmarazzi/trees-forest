# Don't get lost in a forest

Reference for "Don't get lost in a forest" post on [rdisorder.eu](http://rdisorder.eu)

This will be an intro to tree-based models in R.

## Work in progress

The project will be ready by the end of 2016
&nbsp;
&nbsp;

### In the meantime you can get a look at the brief intro below

&nbsp;

Fast & brief primer on dplyr + intubate
=======================================

This is a short intro to `dplyr` and `intubate` packages. I didn't want to load too much the **Don't get lost in a forest post** this intro is referring to.

Why dplyr and not base R?
-------------------------

With `dplyr` we can avoid the creation of temporary datasets saving computation time and memory. It might look trivial, but you'll realize this approach not only will save a ton of time in the long-run, but it will also improve code clarity and simplicity.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

Now let's say I want to see the mean of every species, how would I do?

``` r
tapply(iris$Petal.Width, iris[,5], mean)
```

    ##     setosa versicolor  virginica 
    ##      0.246      1.326      2.026

This is a very nice *one-liner* and for basic stuff like this is actually the best way to deal with grouping. But what if I want the mean of every variable at the same time?

``` r
iris %>% # Take iris data set
    group_by(Species) %>% # group everything by Species
    summarize_each(funs(mean)) # summarize each group with the funs I want
```

    ## # A tibble: 3 × 5
    ##      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
    ##       <fctr>        <dbl>       <dbl>        <dbl>       <dbl>
    ## 1     setosa        5.006       3.428        1.462       0.246
    ## 2 versicolor        5.936       2.770        4.260       1.326
    ## 3  virginica        6.588       2.974        5.552       2.026

Et voilà! And the result is a convenient *tibble* which is a dataframe 2.0 and you can use the same as a base dataframe.

Moreover, piping makes reading code more natural, just look at the comments in the code chunk above.

Why intubate?
-------------

Don't think this is enough, with the `intubate` package we can go even further and pipe models as well.

``` r
library(intubate)

summary(lm(Petal.Width ~ ., data = iris[sample(1:nrow(iris), nrow(iris) * .7), - 5]))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Width ~ ., data = iris[sample(1:nrow(iris), 
    ##     nrow(iris) * 0.7), -5])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.37386 -0.12411 -0.01595  0.08865  0.54763 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.05771    0.19494  -0.296    0.768    
    ## Sepal.Length -0.25555    0.05048  -5.063 1.86e-06 ***
    ## Sepal.Width   0.23180    0.05419   4.277 4.30e-05 ***
    ## Petal.Length  0.54363    0.02651  20.503  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1818 on 101 degrees of freedom
    ## Multiple R-squared:  0.9462, Adjusted R-squared:  0.9446 
    ## F-statistic: 591.7 on 3 and 101 DF,  p-value: < 2.2e-16

The *one-liner* above is a mouthful and not so clear. One way to make the same thing more clearly would be to create copies and lookup variables, but we don't like that much right?

``` r
iris %>% # Take iris data set
    select(-Species) %>% # keep all columns except Species
    sample_n(nrow(iris) * .7) %>% # take a random sample of rows
    ntbt_lm(Petal.Width ~ .) %>% # run a linear regression
    summary # show me a summary
```

    ## 
    ## Call:
    ## lm(formula = Petal.Width ~ ., data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.61250 -0.09824 -0.02436  0.10692  0.60443 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.25337    0.22534  -1.124 0.263506    
    ## Sepal.Length -0.21542    0.05656  -3.809 0.000240 ***
    ## Sepal.Width   0.23909    0.06015   3.975 0.000132 ***
    ## Petal.Length  0.52827    0.02883  18.320  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1959 on 101 degrees of freedom
    ## Multiple R-squared:  0.935,  Adjusted R-squared:  0.9331 
    ## F-statistic: 484.2 on 3 and 101 DF,  p-value: < 2.2e-16

Much clearer and makes doing transformations before running a model feel like a breeze. If you want to use a function not included in the `intubate` package, or if you're not sure if it's implemented or not you can use it with the simple `ntbt` framework.

``` r
iris %>% # Take iris data set
    select(-Species) %>% # keep all columns except Species
    sample_n(nrow(iris) * .7) %>% # take a random sample of rows
    ntbt(lm, Petal.Width ~ .) %>% # run a linear regression
    summary # show me a summary
```

    ## 
    ## Call:
    ## lm(formula = Petal.Width ~ ., data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.40603 -0.10177 -0.00889  0.08524  0.60071 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.21301    0.20300  -1.049    0.297    
    ## Sepal.Length -0.22614    0.05502  -4.110 8.06e-05 ***
    ## Sepal.Width   0.23660    0.05713   4.142 7.17e-05 ***
    ## Petal.Length  0.53710    0.02758  19.472  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1811 on 101 degrees of freedom
    ## Multiple R-squared:  0.9454, Adjusted R-squared:  0.9437 
    ## F-statistic: 582.4 on 3 and 101 DF,  p-value: < 2.2e-16