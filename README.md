# Children_eco_knowledge

## The study
Humans live in diverse, complex niches where survival and reproduction are conditional on the acquisition of knowledge. And yet, individuals are born naive, remaining dependent on adult provisioning for more than a decade as they learn. The need to learn locally relevant knowledge has been connected to the evolution of childhood and other stages of early life history. The rate at which learning happens during childhood can shed some light on the role of phase in our evolution. We collected data to measure environmental knowledge at different ages in the Island of Pemba, Zanzibar (TZ), interviewing 93 children and teenagers between 4 and 26 years old. The population on the island relies heavily on the natural environment, and children routinely forage in the forest and seashore. We then developed a series of Bayesian latent-knowledge models to estimate individual knowledge acquisition, and to infer the influence of age, as well as of activities and social environment on such knowledge. Compared to previous studies, thanks to the use of latent models, we can describe with more details the variation of knowledge with age as well as other factors, and infer the presence of different dimensions of knowledge, which appear to have age and sex-specific relevance. The results suggest that in our sample children acquire the majority of the shared knowledge between sexes. Sex differences in total knowledge start to appear during middle childhood, right when labor division emerge. According to the dimension analysis, during adolescence most of the knowledge increases are sex specific. Activities practiced play a mayor role in determining the differences in knowledge between the sexes.
     These data and the inferred developmental pattern contribute to a growing comparative literature on children's foraging and the life history of cultural knowledge.

## Thought process
The whole development of the project is described here with commented scripts. To summarize, we started by parsing all the factors influencing the emergence of knowledge's age dependence. To describe this, we drew a Directed Acyclic Graph that summarizes the causal structure of our question (0\_DAG.Rmd). Based on the relations predicted according to our DAG, we simulated mock data to generatively develop the analytical models and test their effectiveness (in the 1\_Simulation folder). We collected the data according to the requirements of the models and organized them for analysis (in 2\_Data\_preparation). Only after developing the analysis plan, we run the model on real data (in 3\_Analysis). Finally, we structure the figure and results (in 4\_Outputs). The script for compiling the text is also included in the repository (5\_Text).


## The repo contains:
- 0_DAG.Rmd: a Directed Acyclic Graph that illustrates the causal relations between factors influencing knowledge. 
- models: contains stan code for the models.
- 1\_Simulation: contains all the scripts for running the mock analysis  
	- 1\_simulation\_knowledge.r: simulates data for mock analysis.
	- 2\_mock\_analysis.r: tests main statistical models on the simulated data.
	- 3\_checks\_model\_knowledge.r: describes more in detail the process of developing the statistical models.
- 2\_Data\_preparation: scripts for translating data into the dataset used in the analysis
	- anonymized_data: contains anonymized data to be processed, plus complementary data for processing.
	- supp_materials: questionnaires and other supplementary information on data collection. 
	- 0\_anonimize\_data.r: translates raw data into anonymized data. It is not executable as it requires raw data, which can not be made available because contain identifying information.
	- 1\_process\_data.r: translates anonymized data into processed data. Executable.
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
	- 2_supplementary.Rtex: supplementary materials
	- notes.tex: additional space for work
	- references.bib: references

## What you'll need
- `rethinking` package, see installation instructions at https://github.com/rmcelreath/rethinking.
- `cmdstanr` package, install with `devtools::install_github("stan-dev/cmdstanr")` and, for the first time you install it, `cmdstanr::install_cmdstan()`.
- `ggfree` package to make ridgeplots. Install with: `devtools::install_github("ArtPoon/ggfree")`.
