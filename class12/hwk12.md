# Hwk class 12
Niam Shasha

> Q13 Read this file into R and determine the sample size for each
> genotype and their corresponding median expression levels for each of
> these genotypes.

Hint: The read.table(), summary() and boxplot() functions will likely be
useful here. There is an example R script online to be used ONLY if you
are struggling in vein. Note that you can find the medium value from
saving the output of the boxplot() function to an R object and examining
this object. There is also the medium() and summary() function that you
can use to check your understanding.

``` r
url <- "https://bioboot.github.io/bggn213_W19/class-material/rs8067378_ENSG00000172057.6.txt"

data <- read.table(url, header = TRUE)
head(data)
```

       sample geno      exp
    1 HG00367  A/G 28.96038
    2 NA20768  A/G 20.24449
    3 HG00361  A/A 31.32628
    4 HG00135  A/A 34.11169
    5 NA18870  G/G 18.25141
    6 NA11993  A/A 32.89721

``` r
table(data$geno)
```


    A/A A/G G/G 
    108 233 121 

``` r
summary(data$exp)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      6.675  20.004  25.116  25.640  30.779  51.518 

``` r
tapply(data$exp, data$geno, median)
```

         A/A      A/G      G/G 
    31.24847 25.06486 20.07363 

The file was read into R the sample size for each genotype is 108 for
A/A, 233 for A/G, 121 for G/G. The median expression levels was 31.24847
for A/A, 25.06486 for A/G, and 20.07363 for G/G.

> Q14: Generate a boxplot with a box per genotype, what could you infer
> from the relative expression value between A/A and G/G displayed in
> this plot? Does the SNP effect the expression of ORMDL3? Hint: An
> example boxplot is provided overleaf – yours does not need to be as
> polished as this one.

``` r
boxplot(data$exp ~ data$geno, main= "Expression by Genotype", xlab= "Genotype", ylab="Expression" )
```

![](hwk12_files/figure-commonmark/unnamed-chunk-5-1.png)

The boxplot demonstrates that the A/A genotype has the highest
expression value than A/G and G/G. This shows that the SNP does affect
the expression of ORMDL3 as there are varying expression levels for each
genotype. The G allele may be associated with lower expression based on
the plot, more analysis would need to be conducted to fully conclude
this statement.
