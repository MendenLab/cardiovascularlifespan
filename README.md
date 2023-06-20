This is the repository belonging to the paper "The impact of the cardiovascular component and somatic mutation on ageing" by Garger et al. 

# Data
The "Supplementary Table 1.xlsx" - "1j - Data for analysis" sheet contains the data which is used for the analysis.

# Code
- The CreateFigure.R file contains the code for executing the analysis of the paper. Furthermore, figures and supplementary tables can be reproduced.
- Create_Figure_1_and_Supplementary_Figs.Rmd file creates the phylogenetic tree used for phylogenetic analysis, also creates a basic version of Figure 1 (final version created with Adobe Illustrator), and also all Supplementary Figures along with required analysis (phylogenetic regression, bootstrapping analysis).


# Libraries
We used R 4.1.2 and the following libraries for the analysis:
- readxl
- ggplot2
- tidyverse
- ggrepel
- reshape2
- broom
- car
- ppcor
- rockchalk
- scatterplot3d
- cowplot
- writexl
