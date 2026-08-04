# Progress Report

Last updated: August 4, 2026

-   Other important plots can be found in the shared Word document

-   The most updated R script is Normalized_DPA.Rmd

## Updates

-   Calculated the ordinary logit of normalized fixation proportions before running the DPA.

    -   logit(p) = log(p/(1-p))

    -   p = fixation proportion

    -   1-p = non-fixation proportion

-   The DPA is run by first grouping the data by determiner type, then comparing between ASI and HSS.

## Current results

For target vs. competitor, after calculating the ordinary logit, with a consecutive bin threshold of 5 (instead of the previous 20):

|                             | ASI | HSS |
|-----------------------------|-----|-----|
| Definite determiner group   | 1   | 192 |
| Possessive determiner group | NA  | NA  |

(divergence points are in pseudo-bins)

Corresponding plot for reference:

![](./images/four_facets.png)

## Issues

-   329 out of 3044 rows in trackloss_summary are 1.000
    -   Each row represents the determiner or noun portion of a trial done by a participant
    -   This means that there is no data at all in that portion - should probably be removed
-   Would it be possible to double check the definitions of the bootstrap functions? It is uncertain whether the changes discussed in May about the t-tests were implemented
-   When I ran the DPA on the concatenated data set of 1.normalized determiner + 2.normalized noun + 3.non-normalized remainder, the model crashed - possibly due to a discontinuity that occurs when you join the normalized and non-normalized portions
    -   However, I do not understand why the discontinuity between 1.normalized determiner and 2.normalized noun was fine
-   I have not yet run the DPA after restricting the data to alternating (minimal pair) trials only
-   I have not yet run the DPA on target vs. distractor, since target vs. competitor...
    -   looks weird for the definite determiner group (why is the divergence point at bin 1 for ASI and bin 193 for HSS?)
    -   yielded NAs for the possessive determiner group
-   I have not yet tried to first group the data by speaker type, then run the DPA on definite vs. possessive determiners yet
