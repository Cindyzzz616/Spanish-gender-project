---
editor_options: 
  markdown: 
    wrap: 72
---

Notes: things to fix

-   [ ] find out which participants did which trials, and only fill in
    rows that are actually missing - figure out a more elegant way of
    handling trackloss (right now, for a given AOI, we can't distinguish
    between looks to another AOI vs trackloss (vs looks to blank space))

-   [x] normalize determiners and nouns separately - stitch windows
    together: normalized portion from Det onset to N offset + absolute
    time afterwards - separate out all the definite determiner trials

-   [x] add light horizontal lines to indicate 0.33 and 0.67

-   [ ] the lines don't intersect they way they do in the OG data - bc
    non-target group right now includes a lot of things

    -   the only way you know there's a trackloss/look to white space is
        when all three columns are 0 in tobii

    -   take a look at original data before crystal cleaned it

    -   [ ] check what's the proportion of data vs no data (tracklosss
        AND look to white space - use tidyverse filter for rows that are
        all zero)

        | Window             | Average trackloss proportion |
        |--------------------|------------------------------|
        | Determiner portion | 0.2394                       |
        | Noun portion       | 0.3226                       |

-   [x] tack the remainder on to the concatenanted plot

-   [ ] missing participant-trial combinations - crystal will check with
    danielle

    -   we are supposed to have 48*32 = 1536 trial-participant pairs;
        we only have 1532 `observed_combos`
    -   run pt_matrix code on dataframe AFTER filling in missing time
        points

-   [ ] check whether target and nontarget are 0 or 1 at the same time
    in fix.twovar (SHOULD NOT HAPPEN - they should be complementary)

    -   check this for AUCs too - should add up to 1 for each pseudo-bin

-   [ ] first sanity plot for one participant and trial doesn't work

-   [ ] discontinuities that we forcefully filled in when integrating
    the step functions

    -   check at what time points when NAs pop up - should be the same
        time points for targets and nontargets
    -   also check the percentage of NAs - less than 1% should be fine
    -   probably jump discontinuities in the step functions - how does R
        define the step functions

-   [ ] more plots

    -   exclude trackloss from nontarget
    -   only have competitor or distractor as nontarget
