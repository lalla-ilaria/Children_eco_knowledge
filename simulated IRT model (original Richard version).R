# simulated IRT model
library(rethinking)

# N students with random knowledge
N <- 100
K <- rnorm(N)


# M items! each has unique difficulty (b) and discrimination (g)
M <- 10
g <- abs(rnorm(M))
b <- rnorm(M)

plot(NULL,xlim=c(-4,4),ylim=c(0,1))
for ( i in 1:M ) curve( inv_logit(g[i]*(x-b[i])) , add=TRUE )

# simulate answers
Y <- matrix(NA,nrow=N,ncol=M)
for ( i in 1:N ) for( j in 1:M ) {
	p <- inv_logit( g[j]*( K[i] - b[j] ) )
	Y[i,j] <- rbern(1,p)
}

# now IRT model

model_code <- '
data{
	int N;
	int M;
	int Y[N,M];
}
parameters{
	vector[N] K;
	vector[M] b;
	vector<lower=0>[M] g;
}
model{
	K ~ normal(0,1);
	g ~ normal(0,1);
	b ~ normal(0,1);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(K[i]-b[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

dat <- list( N=N, M=M , Y=Y )

m <- stan( model_code=model_code , data=dat , chains=1 )

post <- extract.samples(m)

K_est <- apply( post$K , 2 , mean )
K_CI <- apply( post$K , 2 , PI )
plot( K , K_est )
for ( i in 1:N ) lines( c(K[i],K[i]) , K_CI[,i] )

b_est <- apply( post$b , 2 , mean )
plot( b , b_est )




##############################
# now again with age as predictor of knowledge


# knowledge as logistically growing
N <- 100
A <- runif(N, 5, 20)
# Knowledge
a_eff <- A + rnorm (N, 0, 3)
K <- inv_logit(0.3*(a_eff- mean(A))) #uses rethinking: invert_logit, which gives you logistic function without having to write it
plot(A,K)



# M items! each has unique difficulty (b) and discrimination (g)
M <- 90
g <- abs(rnorm(M))
b <- rnorm(M)

# simulate answers
Y <- matrix(NA,nrow=N,ncol=M)
for ( i in 1:N ) for( j in 1:M ) {
	p <- inv_logit( g[j]*( K[i] - b[j] ) )
	Y[i,j] <- rbern(1,p)
}

#check correlation between knowledge and number of right answers
y <- rowSums(Y)
plot(K, y)



# new model code

model_code2 <- '
data{
	int N;
	int M;
	int Y[N,M];
	real A[N];
}
parameters{
	vector[N] K0; // individual intercepts on knowledge
	real bA; // coefficient relating age to knowledge
	vector[M] b;
	vector<lower=0>[M] g;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] = inv_logit( K0[i] + bA*A[i] );
}
model{
	K0 ~ normal(0,1);
	bA ~ normal(0,0.5);
	g ~ normal(0,1);
	b ~ normal(0,1);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(Ki[i]-b[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

dat <- list( N=N, M=M , Y=Y , A=standardize(A) )

m2 <- stan( model_code=model_code2 , data=dat , chains=1 )

prec <- precis(m2,2)

post <- extract.samples(m2)

K0_est <- apply( post$K0 , 2 , mean )
K0_CI <- apply( post$K0 , 2 , PI )
plot( K , K0_est )
for ( i in 1:N ) lines( c(K[i],K[i]) , K0_CI[,i] )

Ki_est <- apply( post$Ki , 2 , mean )
Ki_CI <- apply( post$Ki , 2 , PI )
plot( K , Ki_est )
for ( i in 1:N ) lines( c(K[i],K[i]) , Ki_CI[,i] )


b_est <- apply( post$b , 2 , mean )
plot( b , b_est )

plot (A, K0_est)







########
##check for robustness of the model by messing up the values
# change values in knowmod


knowmod <- function (N = 100, M = 90, gr = 0.3, mean_g = 0, sd_g = 1, mean_b = 0, sd_b = 1) { #gr changes the slope of the logistic regression for knowledge if individuals

  
##tried gr = 0.1, 0.3, 1
  ##mean_g = 0, 1, 4
  ##  sd_g = 1, 0.1, 4
  ##mean_b = 0, 1, 4
  
#age
A <- abs(rnorm(N, 12, 3)) #age distribution more similar to real data, model doesn't seem to care
a_eff <- A + rnorm (N, 0, 3)


# Knowledge

K <- inv_logit( gr *(a_eff- mean(A))) #uses rethinking: invert_logit, which gives you logistic function without having to write it
#plot(A,K)



# M items! each has unique difficulty (b) and discrimination (g)

g <- abs(rnorm(M, mean_g, sd_g))
b <- rnorm(M, mean_b, sd_b)

# simulate answers
Y <- matrix(NA,nrow=N,ncol=M)
for ( i in 1:N ) for( j in 1:M ) {
  p <- inv_logit( g[j]*( K[i] - b[j] ) )
  Y[i,j] <- rbern(1,p)
}

#check correlation between knowledge and number of right answers
y <- rowSums(Y)
#plot(K, y)



# new model code

model_code2 <- '
data{
	int N;
	int M;
	int Y[N,M];
	real A[N];
}
parameters{
	vector[N] K0; // individual intercepts on knowledge
	real bA; // coefficient relating age to knowledge
	vector[M] b;
	vector<lower=0>[M] g;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] = inv_logit( K0[i] + bA*A[i] );
}
model{
	K0 ~ normal(0,1);
	bA ~ normal(0,0.5);
	g ~ normal(0,1);
	b ~ normal(0,1);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(Ki[i]-b[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

dat <- list( N=N, M=M , Y=Y , A=standardize(A) )

m2 <- stan( model_code=model_code2 , data=dat , chains=1 )

prec <- precis(m2,2)

post <- extract.samples(m2)


return(list(A = A, a_eff = a_eff, K = K, gr = gr, 
            g = g, b = b, Y = Y, y = y, 
            prec = prec, post = post
            ))
}

K0_est <- apply( post$K0 , 2 , mean )
K0_CI <- apply( post$K0 , 2 , PI )
plot( K , K0_est )
for ( i in 1:N ) lines( c(K[i],K[i]) , K0_CI[,i] )

Ki_est <- apply( post$Ki , 2 , mean )
Ki_CI <- apply( post$Ki , 2 , PI )
plot( K , Ki_est )
for ( i in 1:N ) lines( c(K[i],K[i]) , Ki_CI[,i] )


b_est <- apply( post$b , 2 , mean )
plot( b , b_est )

plot (A, Ki_est)


