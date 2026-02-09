# Divergence Point Analysis (DPA) for Eye-Tracking Data
<div style="display: flex; gap: 100px; justify-content: center;">
  <img src="images/bilinguals_in_toronto.png" width="300" style="margin-right: 200px;">
  <img src="images/uoft_logo.png" width="300">
</div>
<br>
<br>

This repository contains an R pipeline for performing **Divergence Point Analysis (DPA)** on visual-world eye-tracking data. The script generates time-course plots of fixation proportions and estimates the time at which looks to the target diverge reliably from looks to non-target objects using bootstrap-based statistical testing.

The analysis is adapted from the methods described in:

> Ito, A., & Knoeferle, P. (2023). *Analysing data from the psycholinguistic visual-world paradigm: Comparison of different analysis methods.*

The implementation is customized for **high-resolution (10 ms) binomial fixation data** produced by modern eye trackers and for experiments with substantial trial-to-trial variability in stimulus duration.


## Project and team context

This analysis is part of [Bilinguals in Toronto](https://bilingualsintoronto.ca), a lab in the Department of Spanish and Portuguese at the University of Toronto. It investigates how bilingual and multilingual speakers process language in real time. The project brings together researchers and students interested in psycholinguistics, bilingualism, and experimental methods, with a focus on spoken language comprehension across diverse speaker populations.

The broader goal of the project is to better understand how linguistic experience, input variability, and speaker background shape online language processing. The eye-tracking experiments analyzed in this repository contribute to that goal by examining fine-grained temporal dynamics of spoken-word recognition in bilingual listeners.

### Authors and contributors

Cindy Zhang — Data analysis, statistical modeling, visualization, and implementation of divergence point analysis

Ana Pérez-Leroux — Principal Investigator

Laura Colantoni — Principal Investigator

Danielle Thomas — Lab Manager

Crystal Chen — Statistical consulting and methodological support

## Overview

The pipeline supports the following analyses:

* Preprocessing and reshaping of raw eye-tracking time-course data
* Computation of mean fixation proportions with standard error and confidence intervals
* Generation of publication-quality time-course plots
* Divergence Point Analysis comparing Target vs. NonTarget fixations
* Bootstrap estimation of divergence points and confidence intervals
* Group comparisons of divergence timing using permutation bootstrapping

The workflow is designed for visual-world experiments with multiple Areas of Interest (AOIs) and linguistic manipulations, but can be adapted to other eye-tracking datasets with similar structure.

## Experimental context

The analysis assumes a visual-world paradigm with:

* Three AOIs: **Target**, **Competitor**, and **Distractor**
* Linguistic manipulations such as:

  * Determiner type (Definite vs. Possessive)
  * Phonological overlap (Minimal Pair vs. Non-Minimal Pair)
  * Gender match relationships between nouns
* Participant group comparisons (e.g. Adult Spanish Immigrants vs. Heritage Spanish Speakers)

Although originally developed for Spanish gender-processing experiments, the pipeline is not language-specific.

## Data format

The input dataset is a long-format data frame containing one row per **10 ms time bin per trial per participant**. Key columns include:

* Participant, TOI (trial ID), SpeakerType
* Bin, Bin_duration, TimeMS
* Target, Competitor, Distractor (binomial fixation indicators)
* Linguistic timing variables (ArticleOnset, NounOnset, EndingOnset, EndingOffset)
* Adjusted timing variables (zeroed at article onset)
* Experimental condition variables (Determiner, Phonological Overlap, Gender Match)

All bins with trackloss are removed prior to analysis.

## Normalization of trial durations

Trials in the experiment vary substantially in the duration of the determiner–noun sequence, and key linguistic landmarks (e.g. determiner onset, noun onset, word-final vowel onset/offset) are misaligned across trials. Because DPA requires aggregating fixation data across trials, divergence points cannot be estimated in absolute trial time, but only relative to a shared time window.

To address this, the analysis applies a **temporal normalization (time-warping) procedure**:

1. Fixation data from the eye tracker are sampled in 10 ms bins and encoded binomially (0/1) for each AOI. For each AOI, the fixation time course is treated as a step function over time, where the area under the curve corresponds to fixation proportion.

2. For each trial, the determiner–noun window is stretched or compressed to fill a fixed number of **pseudo-bins** (e.g. 100). Fixation proportions within each pseudo-bin are computed based on the area of the original step function that falls within that bin.

3. This transformation yields continuous fixation proportions between 0 and 1 while preserving the **relative temporal structure within each trial**, allowing early vs. late fixation dynamics to be compared across trials of different absolute durations.

4. The resulting normalized time series is compatible with proportion-based DPA methods and enables aggregation across all trials.

This normalization is purely temporal and structural. No baseline subtraction, z-scoring, or scaling of fixation magnitudes is applied.

## Time-course plotting

The pipeline computes mean fixation proportions and standard errors across participants for each time bin and condition. Plots can be faceted by:

* Speaker group
* Determiner type
* Phonological overlap
* Gender match condition

Plots are aligned relative to article onset and include reference lines marking critical linguistic events.

## Divergence Point Analysis

For divergence analysis:

* Competitor and Distractor fixations are collapsed into a single **NonTarget** category.
* The analysis compares **Target vs. NonTarget** fixations at each time bin.
* Divergence is defined as the earliest time point at which a specified number of **consecutive bins** (e.g. 20 bins) show a statistically significant advantage for the Target.

Two modeling approaches are implemented:

* Linear mixed-effects models (`lmer`) with random intercepts for participants
* Generalized linear models (`glm`, binomial)

Divergence points are estimated using **stratified bootstrap resampling**, preserving the within-participant temporal structure of the data.

Bootstrap outputs include:

* Mean divergence point (in milliseconds)
* Confidence intervals
* Separate estimates for each speaker group and determiner condition

## Group comparison

To test whether divergence timing differs between groups, the pipeline implements a **permutation bootstrap under the null hypothesis**:

* Group labels are randomly reassigned within participants and time bins.
* Divergence points are re-estimated on each resample.
* A p-value is computed as the proportion of null samples exceeding the observed group difference.

## Dependencies

Key R packages used:

* tidyverse
* ggplot2
* Rmisc
* boot
* lme4
* dplyr

Package versions are documented in the script for reproducibility.

## Outputs

The pipeline produces:

* High-resolution fixation time-course plots
* Annotated plots with divergence points and confidence intervals
* Bootstrap result objects saved as `.rds` files

## Notes and limitations

* Divergence estimates depend on the chosen number of consecutive significant bins.
* Trial-level DPA is included but may yield unstable estimates due to limited per-trial data.
* Divergence points are interpretable as **relative positions within the normalized time window**, not absolute acoustic times.
* Due to the large file size, df_cleaned.csv is NOT on the github repo and must be accessed through a local copy.
