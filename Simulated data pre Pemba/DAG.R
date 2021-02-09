library(dagitty)


## ---- DAGs

#############################
######causal models##########
#############################

#general causal model with all relevant variables (almost, friendship ties to be treated separately)
#we might want to pay attention to some of the causal relations


DAG1 <- dagitty("dag{
                Age -> Know   
                Act -> Know   
                Fam -> Know  
                Sch -> Know     
                Sex -> Act    
                Fam -> Act
                Age -> Act
                Sch -> Act   
                Age -> Sch  
                Fam -> Sch
                Sex -> Sch
                }")

coordinates(DAG1) <- list ( x= c(Age=1, Know=1, Sex=0, Act=0, Fam=2, Sch=2), y= c(Age=0, Sex=0, Fam=0, Act=1, Sch=1, Know=2))

plot(DAG1)
