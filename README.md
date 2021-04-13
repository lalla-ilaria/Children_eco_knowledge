# Children_eco_knowledge

## The study
Humans live in complex niches where survival and reproduction are conditional on the acquisition of large amounts of knowledge. As individuals are born naive, a great deal of learning has to happen during development in order to ensure fitness when they become adults. The rate at which knowledge is acquired depends on several factors, both intrinsic, such as the physiology of the brain, and extrinsic, such as the frequency of learinig opportunities. With this study we aim at inferring the pattern of knowledge increase with age from a set of interviews to children of a village in the Island of Pemba, Zanzibar. We also consider the effect of other factors on age specific knowledge, including perfomed activities, school commitment and familiar contingencies. We focus on knowledge of the environment, as this represents a subsettable area of information with ripercussions on life quality of the individuals who posses it, as well as being widely discussed in studies of traditional ecological knowledge. To do so, we developed a series of Bayesian IRT models to estimate knowledge as a latent dimension emerging from the answers to the questionnaire. 

## Thought process
The whole development of the project is described here with commented scripts. To summarize, we started by parsing all the factors influencing the emergence of knowledge's age dependence. We hence drew a Directed Acyclic Graph that summarizes the causal structure of our question (0\_DAG.Rmd). Based on the relations predicted according to our DAG, we simulated mock data to generatively develop the analytical models and test their effectiveness (in the 1\_Simulation folder). We collected the data according to the requirements of the models and organized them for analysis (in 2\_Data\_preparation). Only after developing the analysis plan, we run the model on real data (in 3\_Analysis). Finally, we structure the figure and results (in 4\_Outputs). The script for compiling the text is also included in the repository (5\_Text).


## The repo contains:
- 0_DAG.Rmd: a Directed Acyclic Graph that illustrates the causal relations between factors influencing knowledge. 
- models: folder containing all the statistical models used in the project, both for mock and real analysis.
- 1\_Simulation: contains all the scripts for running the mock analysis  
	- 1\_simulation\_knowledge.r: a script for simulating data esplicitly developed for testing the statistical model.
	- 2\_mock\_analysis.r: a script to test the main statistical models on the simulated data
	- 3\_checks\_model\_knowledge: a (chaotic) script describing some more models and exploration of directions of analysis
- 2\_Data\_preparation: contains the scripts used to organize the data and the anonymized data used to run the models
	- 1\_answers.r: script producing a set of matrices with the answers (each row is an individual, each column an item, zeroes and ones mark correct vs wrong answers).
	- 2\_individuals.r: script organizing the individual level data from the children's interviews.
	- 3\_households.r: script including the data about individuals and household from the household interviews.
	- d.csv: the anonymized data ready for analysis.
- 3\_Analysis
- 4\_Outputs
- 5\_Text
	- main.Rtex: main part of the text
	- notes.tex: additional space for work

