Statistical assignment 3
================
\[Krystof Burysek\] \[680021055\]
\[18.02.2020\]

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
   pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value)
```

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

?case\_when

class(Long$vote6)

table(Long$wave)

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = recode(sex_dv, '1' = 'male', '2' = 'female')) %>%
        mutate(vote6 = recode(vote6, '1' =  1,
                                      '2' = 2,
                                      '3' = 3,
                                      '4' = 4,
                                      .default = NA_real_))
```

    ## Warning: Unreplaced values treated as NA as .x is not compatible. Please specify
    ## replacements exhaustively or supply .default

``` r
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

``` r
table(Long$sex_dv)
```

    ## 
    ## female   male 
    ## 117665 100342

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
  group_by(sex_dv, wave) %>%
  summarise(mean.sex = mean(vote6, na.rm = TRUE))
```

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6.wide <- meanVote6 %>%
  pivot_wider(names_from = wave, values_from = mean.sex) 


## It can be observed that that both sexes exhibit slightly different level of political interest 
## with men slightly more intersted to begin with. There is a slight and almost equal 
## increase in political interest when first and last wave is compared. This tendecy challenges the
## the common asuumption that people are becoming ignorant towards politics and democracy, often
## derived from the decreasing percentages in turnout across western liberal democracies. However,
## the computed numbers show that interest in politics still exists. Other things being equal, 
## People tend to participate in politics differently than via casting polls, with the social media 
## expression being the prime example. But the term politics can mean various things to 
## to different people and hence may consider themselves as intrested in politics whereas others do ## perceive them as such. Therefore this measure may not accuarately reflect the tangible increase 
## in politics.


## Code for question below. Interpretation in the next chunk.

## 1.


# Filtering data set to the needed values 

Delta.set <- all7 %>%
  select(pidp, a_sex_dv, a_age_dv, a_vote6, b_vote6, c_vote6, d_vote6, e_vote6, f_vote6, g_vote6)
  

Delta.set <- Delta.set %>%
  filter(!is.na (a_vote6)) %>%
  filter(a_vote6 >= 1)

Delta.set <- Delta.set %>%
  filter(!is.na (b_vote6)) %>%
  filter(b_vote6 >= 1)

Delta.set <- Delta.set %>%
  filter(!is.na (c_vote6)) %>%
  filter(c_vote6 >= 1)
  

Delta.set <- Delta.set %>%
  filter(!is.na (d_vote6)) %>%
  filter(d_vote6 >= 1)
  

Delta.set <- Delta.set %>%
  filter(!is.na (e_vote6)) %>%
  filter(e_vote6 >= 1)
  

Delta.set <- Delta.set %>%
  filter(!is.na (f_vote6)) %>%
  filter(f_vote6 >= 1)

Delta.set <- Delta.set %>%
  filter(!is.na (g_vote6)) %>%
  filter(g_vote6 >= 1)
  




## 2.

#

Delta.set <- Delta.set %>%
  mutate(delta = abs(a_vote6 - b_vote6) + abs(b_vote6 - c_vote6) + abs(c_vote6 - d_vote6) + abs(d_vote6 - e_vote6) + abs(e_vote6 - f_vote6) + abs(f_vote6 - g_vote6))

max(Delta.set$delta)
```

    ## [1] 16

``` r
## 3.
## By men and women


Table.delta.sex <- Delta.set %>%
  group_by(a_sex_dv) %>%
  summarise(mean.delta.sex = mean(delta))

Table.delta.sex <- Table.delta.sex %>%
  rename('sex' = 'a_sex_dv')


Table.delta.sex <- Table.delta.sex %>%
  mutate (sex = recode(sex, '1' = 'male' , '2' = 'female'))

## 4.

# By age 

Table.delta.age <- Delta.set %>%
  group_by(a_age_dv) %>%
  summarise(mean.delta = mean(delta)) 

scatter.smooth(Table.delta.age)
```

![](assignment3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
mean(Delta.set$delta)
```

    ## [1] 2.596297

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

## 5\.

# Interpretation

## For Sex: Overall, women exhibit slightly greater stability in their political interest than men.

## Why that is the case is difficult to answer, as the difference computed does not seem to be

## big. Perhaps men still experience greater social and economic mobility, be it in upward or

## downward direction that leads them to increase or decrease their interest in politics. Women

## on the other hand may be more bound to their economic or social position which they were born \#\#into

## leading them interests to noct hange their level of the interest accordingly. It could be also \#\#argued that political interest

## may be influenced by health as the sick or elderly people tend to care less about politics.

## Since men are more likely to suffer from bad heath than women, they may take varying interest

## in politics over the course of the lifetime. It should, however, be noted that the computation

## does not tell us any generational issues as men and women in different generations may have a \#\#different

## evolution of the political intrerest considering the recent trends in electoral praticipation by

## youngsters.

## For age: It can observed that evolution of political interest follows a statistical pattern

## At the younger age, delta measure is greater and decreases up until the age of 50 to 60 where it

## starts rising again. The explanation could be that younger people are ttring to figure out their

## political perefernces which can also entail whether to take interest in politics at all. As they

## become members of the society, they start forming their own idea of citizneship and how to \#\#express

## it, if at all. Hence, the greatest stability of the interest comes at the age where people

## tend to have more solid economic and social position and adhere to the preferences they formed

## when they were younger. As they grow older, however, the delta increases at higher rate than it

## decreased from the younger cohort. Health, including mental and physical, does probably play role

## in this as does the associated shift ins social position, like entering retirement. However, one

## should be catious as to conclude that this computation proves that political interest follows \#\#this

## pattern. This is only the dataset recorded in Britain which operates in two-party systems. It

## possible that people may take differnt levels of political interest PR systems as they do not

## have to vote strategically making their choice easier. Moreover, Britain has always been a \#\#democracy

## and its citizens may have been ‘conditioned’ to take democracy fo granted unlike

## in countries which experienced regime change. Morever, socioeconomic standing of age cohorts

## and ditribution of inequality is also country-specific so the question of stability of political

## interest should be further investigated, ideally in cross-national fashion.

\`\`\`r
