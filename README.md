# Children_eco_knowledge

This repository contains data and code to reproduce results from [Pretelli, I., Borgerhoff Mulder, M., & McElreath, R. (2022). _Rates of ecological knowledge learning in Pemba, Tanzania: Implications for childhood evolution._ Evolutionary Human Sciences, 4, E34](https://doi.org/10.1017/ehs.2022.31) 

## Abstract
Humans live in diverse, complex niches where survival and reproduction are conditional on the acquisition of locally relevant knowledge. Humans also have long childhoods, spending more than a decade before they begin to produce more than they consume. Whether the time it takes to learn locally relevant knowledge has been a selective force in the evolution of long human childhood is unclear, because there is little comparative data on the growth of ecological knowledge throughout childhood. 
We measured ecological knowledge at different ages in the Island of Pemba, Zanzibar (TZ), interviewing 93 children and teenagers between 4 and 26 years old.
We then developed a series of Bayesian latent-knowledge models to estimate individual knowledge acquisition in one or different dimensions, and to infer the influence of age, as well as of activities and social environment on such knowledge.
In the studied population, children keep learning during the whole pre-reproductive period, but at varying rate, the increase with age being fastest in young children and slower in teenager and young adults. Sex differences in knowledge appear during middle childhood, driven by increases in boy-specific knowledge, as it appears when parsing knowledge in multiple dimensions. Individual activity patterns contribute to knowledge acquisition and mediate the emergence of differences between the sexes. Co-residence with mothers and, for boys only, with fathers is associated with greater knowledge, whereas we detected almost no effect of school attendance. 

## The repo contains:
- makefile.R: r script that defines set of tasks to be executed. Runs in order all the necessary scripts to produce the results and figures (folders 1-4), which are then used by the .tex files (in folder 5). 
- 0_DAG.Rmd: a Directed Acyclic Graph that illustrates the causal relations between factors influencing knowledge. 
- models: contains stan codes for the models.
- 1\_Simulation: contains all the scripts for running the mock analysis  
	- 1\_simulation\_knowledge.r: simulates data for mock analysis.
	- 2\_mock\_analysis.r: tests main statistical models on the simulated data.
	- 3\_checks\_model\_knowledge.r: describes more in detail the process of developing the statistical models.
- 2\_Data\_preparation: scripts for translating data into the dataset used in the analysis
	- anonymized_data: contains anonymized data to be processed, plus complementary data for processing.
	- supp_materials: questionnaires and other supplementary information on data collection. 
	- 0\_anonimize\_data.r: translates raw data into anonymized data. It is not executable as it requires raw data, which contain identifying information and hence cannot be shared.
	- 1\_process\_data.r: translates anonymized data into processed data. 
	- functions.R: set of functions used to anonymize data and other data wrangling
	- processed_data.RData: data ready for analysis.
- 3\_Analysis
	- fit\_models: fit models from analysis and supplementary analyses
	- 1\_analysis.R: script to compile models for descriptive and causal analyses
	- 2\_results\_exploration.R: random code 
	- 3\_supplementary\_analyses.R: script to compile models for estimation of sampling bias and for each data type
- 4\_Outputs
	- plots: contains png figures
	- 0\_set\_the\_plot\_stage: prepares the data for plotting and creates plotting functions
	- 1\_figures: makes the figures for main text
	- 2\_generated\_values: calculates some interesting values  
	- 3\_supplementary\_figures: makes the figures for supplementary materials
- 5\_Text
	- 1\_main.Rtex: main part of the text
	- 2\_supplementary.Rtex: supplementary materials
	- notes.tex: additional space for work
	- references.bib: references

## Required packages outside of CRAN
- `rethinking` package, see installation instructions at https://github.com/rmcelreath/rethinking.
- `cmdstanr` package, install with `devtools::install_github("stan-dev/cmdstanr")` and, for the first time you install it, `cmdstanr::install_cmdstan()`.
- `ggfree` package to make ridgeplots. Install with: `devtools::install_github("ArtPoon/ggfree")`.
