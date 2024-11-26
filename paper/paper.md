---
title: '**gg1d**: An R Package for Graphically Summarizing Datasets using Stacked 1-Dimensional Plots'
tags:
- R
- cancer
- genomics
- visualisation
date: "17 June 2024"
output: word_document
authors:
- name: "Sam El-Kamand"
  orcid: "0000-0003-2270-8088"
  affiliation: 1
- name: Julian M.W. Quinn
  orcid: "0000-0001-9674-9646"
  affiliation: 1
- name: Mark J. Cowley
  affiliation: 1, 2
  orcid: "0000-0002-9519-5714"
  corresponding: true
bibliography: paper.bib
affiliations:
- name: Children’s Cancer Institute, Australia
  index: 1
- name: School of Clinical Medicine, UNSW Medicine & Health, Australia
  index: 2
---

# Summary

Exploratory data analysis (EDA) involves examining the relationships between both categorical and quantitative features. The gg1d R package streamlines EDA by providing a turnkey approach to visualising n-dimensional data which graphically reveals correlative or associative relationships between 2 or more features (\autoref{fig:figure1}). gg1d represents all dataset features as distinct, vertically aligned bar or tile plots, with plot types auto-selected based on whether variables are categorical or numeric. It reduces both the code and time required to detect complex multi-feature relationships that would otherwise only be found through statistical modelling or thorough manual review (\autoref{fig:figure2}, \autoref{fig:figure3}).

![**gg1d** visualizations of common datasets revealing: A) Petals of the *setosa* species of iris are drastically smaller than other iris species; B) The majority of individuals who perished during the Titanic disaster were adult males; C) *Gentoo* penguins from Biscoe Island (dark green) have shallower bill depths than *Chinstrap* or *Adelie* penguins, despite their increased body mass. Exclamation marks indicate missing values. \label{fig:figure1}](figure1.pdf)

\newpage

# Statement of Need

The R ecosystem already includes popular EDA packages such as skimr, which textually summarizes completeness and descriptive statistics for individual features (1-dimensional), and GGally, which graphically describes pairwise feature correlations (2-dimensional). gg1d is an n-dimensional generalization with key advantages over other EDA packages, most notably its ability to reveal more complex multidimensional patterns (\autoref{fig:figure2}, \autoref{fig:figure3}). 

![Comparison of R packages that create visualisations commonly used for exploratory data analysis. \label{fig:figure2}](figure2.pdf){width="85%"}

The benefits of **gg1d** are exemplified when visualizing the artificial Lazy Birdwatcher dataset, which records magpie observations by two birdwatchers (\autoref{fig:figure3}). One birdwatcher does not work on weekends, creating a missing data pattern dependent on both birdwatcher and day of the week. This multidimensional pattern becomes immediately apparent from **gg1d** output, whereas it is difficult to detect using only one-dimensional EDA tools like **skimr** or two-dimensional tools like **ggpairs** from the **GGally** package.  

![Visualisation of the Lazy Birdwatcher dataset using the **gg1d** package reveals a pattern of missingness dependent on multiple variables, Birdwatcher and Day (A). This pattern is difficult to detect using one-dimensional EDA tools like **skimr** (B) or two-dimensional tools like **ggpairs** from the **GGally** package (C). \label{fig:figure3}](figure3.png)

We developed gg1d for the visualisation of clinical and multiomics data and anticipate it will prove valuable for any exploratory EDA activities. Further examples of gg1d visualisations are available in the [gg1d gallery](https://selkamand.github.io/gg1d/articles/gallery.html).

# Acknowledgements
We thank the developers of the packages integral to gg1d, especially David Gohel for ggiraph [@gohel:2024], which enables its interactivity, and Thomas Lin Pedersen for patchwork [@pedersen:2024] and ggplot2 maintenance. We also acknowledge Hadley Wickham and all contributors to ggplot2 [@wickham:2016]. 


# References