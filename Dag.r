library(dagitty)
DAG1 <- dagitty("dag {
Time -> Returns 
Body -> Returns  
Knowledge -> Returns 
Age -> HeightWeight 
Age -> Strength 
Strength -> Body 
HeightWeight -> Body 
Age -> Knowledge
Other -> Returns
Age -> Returns
}
")

coordinates(DAG1) <- list ( x= c(Returns=0, Body = -1, HeightWeight = -1, Strength = -0.5, Knowledge=0, Time=1, Age=-1, Other=0), 
                            y= c(Returns=0, Body = 0, HeightWeight = -0.5, Strength = -0.5,Knowledge=-1, Time=0, Age=-1, Other=1))

plot(DAG1)

dag {
Returns [pos="0,1"]
Body [pos="-1.4,0"]
Knowledge [outcome,pos="0,-1.4"]
Time [exposure,pos="1.4,0"]
Age [pos="-1.4,-1.4"]
Time -> Returns 
Body -> Returns  #[pos="-0.791,-1.045"]
Knowledge -> Returns 
Age -> Body #[pos="0.680,-0.496"]
Age -> Knowledge
}
