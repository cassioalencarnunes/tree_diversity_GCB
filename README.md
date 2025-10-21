[![DOI](https://zenodo.org/badge/1080410947.svg)](https://doi.org/10.5281/zenodo.17407988)
# **Multi-faceted assessment of Amazonian tree diversity reveals pervasive impacts of human modification**

Erika Berenguer\* & Cássio Alencar Nunes\*, Jesus Aguirre-Gutiérrez, Joice Ferreira, Yadvinder Malhi, Luiz E. O. C. Aragão, Adriane Esquivel-Muelbert, Axa E. S. Figueiredo, Joseph E. Hawes, Carlos A. Joly, Carlos A. Quesada, Marina M. M. de Seixas, Ima Vieira, Jos Barlow

October 2025

\* Shared first authorship

**e-mail**: [cassioalencarnunes\@gmail.com](mailto:cassioalencarnunes@gmail.com) and [erikaberenguer\@gmail.com](mailto:erikaberenguer@gmail.com)

**Code repository for the paper:** Berenguer and Nunes et al. 2025. Multi-faceted assessment of Amazonian tree diversity reveals pervasive impacts of human modification. *Global Change Biology*.

In this repository we included codes and data that we used to run the all the analyses presented in the paper.

1.  You will find the code to run the analysis of alpha-diversity of Amazonian trees in response to the human-modification gradient (forest class) in the script “Script_diversity.R”. We used Linear Mixed-Effect Models (LMMs) to assess the significance of difference between the alpha-diversity in different forest classes. In this script we also calculated the effect sizes of all forest class comparisons to be used in the sensitivity analysis (see **#3**). In this script you will also find the codes to create the figures of alpha-diversity analyses.

2.  You will find the code to run the analysis of community composition in different forest classes in the human-modification gradient in the script “Script_composition.R”. We used PERMANOVAs to assess the significance of difference between the community composition in different forest classes. In this script we also calculated the effect sizes of all forest class comparisons to be used in the sensitivity analysis (see **#3**). The script “Script_heatmaps_composition.R” was used to create the figures of community composition analyses.

3.  The script “Script_sensitivity_analysis.R” contains all the code used to run the analysis of the sensitivity of different metrics used to assess Amazonian tree diversity, such as diversity facet, dominance weighting and tree size, in addition to the forest class comparison. This script contains analyses for both alpha-diversity and community composition.

4.  The script "Script_figure_4_and_5.R" contains the codes used to construct the figures 4 and 5 after the sensitivity analyses.

5.  You will find all the data needed for these analyses in the folder “Data” and “Results”.
