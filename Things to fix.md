---
editor_options: 
  markdown: 
    wrap: 72
---

======= new DPAs ====== 1. ASI, alt AND nonalt - target fixations for
def vs target fixations for pos

2.  HSS, alt ONLY - target fixations for def vs target fixations for pos

-\> getting a lot of "boundary (singular) fit: see help('isSingular')"

3.  ASI, alt AND nonalt - def only, target vs competitor
4.  HSS, alt ONLY - def only, target vs competitor
5.  ASI, alt AND nonalt - pos only, target vs competitor
6.  HSS, alt ONLY - pos only, target vs competitor

``` r
# original bootstrap function
boot_L1L2_lmer = function(original_data, resample_indices){
  dat = original_data[resample_indices, ]

  # a statistical test at each timepoint for each group
#  test_g1 = dat %>% # t-test for L1 group
#    subset(SpeakerType == 1) %>% group_by(TimeMS_adjusted) %>%
#    dplyr::summarise(t = summary(lmer(FixP ~ AOI + (1|Participant)))$coefficients["AOITarget", "t value"])

  test_g1 <- tryCatch(

    dat %>%
        subset(Determiner=="Definite") %>%
        group_by(TimeMS_adjusted) %>%
        group_modify(~{

          fit <- lmer(
              FixP~AOI+
              (1|Participant),
              data=.x
          )

          tibble(

              t=
                  summary(fit)$coefficients[
                      "AOITarget",
                      "t value"
                  ]

          )

      }),

    error=function(e){

        cat("\nMODEL FAILED\n")

        print(
            dat %>%
            subset(SpeakerType==1) %>%
            filter(TimeMS_adjusted==201) %>%
            count(AOI)
        )

        print(
            dat %>%
            subset(SpeakerType==1) %>%
            filter(TimeMS_adjusted==201) %>%
            count(Participant)
        )

        stop(e)

    }

)

  test_g2 = dat %>% # t-test for L2 group
    subset(Determiner == "Possessive") %>% group_by(TimeMS_adjusted) %>%
    dplyr::summarise(t = summary(lmer(FixP ~ AOI + (1|Participant)))$coefficients["AOITarget", "t value"])

  # return a TRUE/FALSE vector of significant positive t-scores
  # (positive means more looks to the target than unrelated)
  t_g1 = test_g1$t > 1.96
  t_g2 = test_g2$t > 1.96

  # create empty vectors to store onsets
  onset_g1 = onset_g2 = c()

  # find the index of the earliest run of sequential TRUEs; threshold is Nbins
  for (i in 1:(length(t_g2)-Nbins)) {
    onset_g1[i] = sum(t_g1[i:(i+Nbins-1)]) == Nbins
    onset_g2[i] = sum(t_g2[i:(i+Nbins-1)]) == Nbins
  }

  # find the difference between onsets
  delta_g1g2 = which(onset_g2)[1] - which(onset_g1)[1]

  # print
  # note: the bootstrap returns the indices of the respective timepoints, not absolute times.
  # The annotations to the right of each index (e.g. t[,1]) indicate where in the boot object the bootstrapped onset distributions can be found.
  c(delta_g1g2,         # onset difference L1 vs. L2 t[,1]
    which(onset_g1)[1], # onset bin for looks to target L1 t[,2]
    which(onset_g2)[1])  # onset bin for looks to target L2 t[,3]
}
```

======= July meeting ======= - need to do window analysis first to get
yes/no answer - what did previous analyses do that favoured a positive
divergence? - do it on just alternating (minimal pair) - do just target
vs competitor or target vs distractor - try to first group by speaker
type, then do dpa on determiner type - try empirical logit
transformation on the fixation proportions before running DPA -
empirical logit doesn't make sense because we only have fixation
proportions after normalization - we don't know N and Y - we can take
ordinary logit - - logit(p) = log(p/(1-p)) - p = fixation proportion -
1-p = non-fixation proportion - but we don't know what the non-fixation
proportion is because we don't know how many trials were trackloss -
examples at: <https://osf.io/tzn8u/overview>

======= New notes - July 2026 ======= - why is the trackloss proportion
1 for the det/noun portions of some trials? should we just exclude those
trials? - there are already NAs in AUC - must have occurred when
integrating the step functions

======= Notes for Crystal - June 2026 ======= - 329 out of 3044 rows in
trackloss_summary are 1.000 - which means that there is no data at all
in that det/noun portion and the trial should be excluded

Idea - trackloss threshold - exclude a trial from plotting if trackloss
in the det/noun portion is greater than a certain threshold

======= Meeting notes - May 19, 2026 =======

-   look for UPDATED in the code

-   weird that crossover doesn't happen in the noun duration... --- DPA
    assumes that the crossover occurs in a certain interval

-   strategy 1: run 1 DPA across noun (normalized) + remainder
    (non-normalized)

    -   pro: visually, we see the divergence happening in the remainder,
        so include remainder in order to not break assumption that
        interval contains divergence point
    -   con: can we mix normalized and non-normalized data? how do we
        interpret the point as a relative percentage? -\> or just run
        the DPA on the normalized noun portion only? -\> go back to the
        t-tests from IPC (IPC_DPA.html, in Ito & Knoeferle) --- watch
        the empirical logit part - might have to convert fixation
        portion to empirical logit fixation \^ it might work now that
        our data is normalized

-   strategy 2: run 1 DPA across noun (normalized) + remainder
    (normalized) ex. trial 1: noun = 50 ms, remainder = 300 ms trial 2:
    noun = 200 ms, remainder = 200 ms

    -   if you normalize across the entire interval, then 20% into the
        two trials would mean different things

what if we define the non-targets differently? - have the non-target be
only the competitor or the distractor so we have: - target vs.
non-target - target vs. distractor - target vs. competitor do we include
trackloss? - exclude it in all three cases - also plot these before
running the DPA

where did they define the lmer model? e.g. something like lmer(y \~ x1 +
x2, data = df) \^ it's already a very simple model - they did a t-test

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

    -   we are supposed to have 48\*32 = 1536 trial-participant pairs;
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
