data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	// int Q; //n items questionnaire
	// int R; //n items image recognition
	// int C; //n activities
	// int H; //n households
	int Y_l[N,L]; //answers freelist
  // int Y_q[N,Q]; //answers questionnaire
  // int Y_r[N,R]; //answers image recognition
	// real A[N]; //age of individuals
	// real SY[N];//school years of individuals
	// real OS[N];//n of older sibs of individuals
	// real YS[N];//n of younger sibs of individuals
	// real AD[N];//n of adults in the household of individuals
	// int  HH[N]; //household of individuals
	// row_vector[C]am[N] ; //activities performed by individuals
}

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
	// vector<lower=0>[D] bA; // coefficient relating age to knowledge
	// vector[D] bSY;     //coefficient for school years
	// matrix[H,D] aHH;//intercept for household
	// vector[D] bOS;     //coefficient for older sibs
	// vector[D] bYS;     //coefficient for younger sibs
	// vector[D] bAD;     //coefficient for adults in the household
	// matrix[C,D] AE; //a vector of coefficients for activities
	
	//item parameters
	//discrimination
	matrix<lower=0>[L,D] a_l;
	// matrix<lower=0>[Q,D] a_q;
	// matrix<lower=0>[R,D] a_r;
	//difficulty
	matrix[L,D] b_l;
	// matrix[Q,D] b_q;
	// matrix[R,D] b_r;
	// pseudoguessing
	// matrix<lower=0,upper=1>[Q,D] c_q;
}

transformed parameters{
  matrix[N,D] K;
  for ( i in 1:N ) for ( j in 1:D ) K[i,j] =  aK[i,j] // individual effects
                                    // + bA[j] * A[i] 
                                    // + bSY[j] * SY[i] 
                                    // + aHH[HH[i],j] // household and family effects
                                    // + bOS[j] * OS[i] 
                                    // + bYS[j] * YS[i] 
                                    // + bAD[j] * AD[i] 
                                    // + dot_product( AE[,j], am[i]) //activity effects
                                    ; 
}

model{
  //priors for individual parameters
	to_vector  (aK)  ~ normal(0,1);
	// for(i in 1:D) bA[i]  ~ normal(0,0.5) T[0,]; //only positive relations possible
	// for(i in 1:D) bSY[i] ~ normal(0,0.3);
	// to_vector (aHH) ~ normal(0,0.5);
	// for(i in 1:D) bOS[i] ~ normal(0,0.3);
	// for(i in 1:D) bYS[i] ~ normal(0,0.3);
	// for(i in 1:D) bAD[i] ~ normal(0,0.3);
	// to_vector (AE)  ~ normal(0,0.3);
	
	//priors for item parameters
	to_vector (a_l) ~ exponential(1.5); //value constrained above zero
	// to_vector (a_q) ~ lognormal(0, 0.5); //value constrained above zero
	// to_vector (a_r) ~ lognormal(0, 0.5); //value constrained above zero
	to_vector (b_l) ~ normal(0,1);
// 	to_vector (b_q) ~ normal(0,1);
// 	to_vector (b_r) ~ normal(0,1);
//   to_vector (c_q) ~ beta(5,5);

  //model
	//freelist
	for ( i in 1:N ) {
		for (j in 1:L ) {
			real p = 0;
      for ( d in 1:D ) p = p + a_l[j,d] * (K[i,d] - b_l[j,d]);
      if ( inv_logit(p) >= 1 ) print(inv_logit(p), K[i,], aK[i,], a_l[j,], b_l[j,]) ;
      Y_l[i,j] ~ bernoulli( inv_logit( p ));
		}
	}
// 	//questions
// 	for ( i in 1:N ) {
// 		for (j in 1:Q ) {
// 			real p = 0;
//       for ( d in 1:D ) p = p + a_q[j,d] * (K[i,d] - b_q[j,d]);
//       Y_q[i,j] ~ bernoulli(c_q[j] + (1 - c_q[j]) * inv_logit( p ));
// 		}
// 	}
// 	//image recognition
// 	for ( i in 1:N ) {
// 		for (j in 1:R ) {
// 			real p = 0;
//       for ( d in 1:D ) p = p + a_r[j,d] * (K[i,d] - b_r[j,d]);
//       Y_r[i,j] ~ bernoulli( inv_logit( p ));
// 		}
// 	}
}
//  generated quantities {
//    vector [N * L 
//           // + N * Q  
//           // + N * R
//           ] log_lik;
// {
//    int k = 1;
//     for ( i in 1:N ) {
//   		for (j in 1:L ) {
//   			real p = 0;
//         for ( d in 1:D ) p = p + a_l[j,d] * (K[i,d] - b_l[j,d]);
//         log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | inv_logit( p ));
//    	  	k = k + 1;
//    	  	} // L
//       } // N
//   //   for ( i in 1:N ) {
//   // 		for (j in 1:Q ) {
//   // 			real p = 0;
//   //       for ( d in 1:D ) p = p + a_q[j,d] * (K[i,d] - b_q[j,d]);
//   //       log_lik[k] = bernoulli_lpmf( Y_q[ i, j] | c_q[j] + (1 - c_q[j]) * inv_logit( p ));
//   //  	  	k = k + 1;
//   //  	  	} // Q
//   //     } // N
//   //   for ( i in 1:N ) {
//   // 		for (j in 1:R ) {
//   // 			real p = 0;
//   //       for ( d in 1:D ) p = p + a_r[j,d] * (K[i,d] - b_r[j,d]);
//   //       log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | inv_logit( p ));
//   //  	  	k = k + 1;
//   //  	  	} // R
//   //     } // N
//   } 
//}

