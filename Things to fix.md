---
editor_options: 
  markdown: 
    wrap: 72
---

Meeting notes - May 19, 2026

- look for UPDATED in the code

- weird that crossover doesn't happen in the noun duration...
--- DPA assumes that the crossover occurs in a certain interval

- strategy 1: run 1 DPA across noun (normalized) + remainder (non-normalized)
  - pro: visually, we see the divergence happening in the remainder, so include remainder in order to not break assumption that interval contains divergence point
  - con: can we mix normalized and non-normalized data?
         how do we interpret the point as a relative percentage?
  -> or just run the DPA on the normalized noun portion only?
  -> go back to the t-tests from IPC (IPC_DPA.html, in Ito & Knoeferle)
  --- watch the empirical logit part - might have to convert fixation portion to empirical logit fixation
      ^ it might work now that our data is normalized
         
- strategy 2: run 1 DPA across noun (normalized) + remainder (normalized)
  ex.
  trial 1: noun = 50 ms, remainder = 300 ms
  trial 2: noun = 200 ms, remainder = 200 ms
  - if you normalize across the entire interval, then 20% into the two trials would mean different things
  
what if we define the non-targets differently?
- have the non-target be only the competitor or the distractor
so we have:
- target vs. non-target
- target vs. distractor
- target vs. competitor
do we include trackloss? - exclude it in all three cases
- also plot these before running the DPA

where did they define the lmer model?
e.g. something like lmer(y ~ x1 + x2, data = df)
^ it's already a very simple model - they did a t-test


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
