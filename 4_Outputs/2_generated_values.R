#Set the stage for plotting figures
if( !exists( "plotagesandknow", mode = "function")) source( "4_Outputs/0_set_the_plot_stage.R" )


##############################
#SAMPLE SIZES TABLE
generate_s_size_table <- function (qn_table, age = d$A){
  s_s <- data.frame( "All ages" = c(length (age)-1, #one individual over 50 year was excluded from analysis
                                    max(apply(qn_table, 1, sum)), 
                                    min(apply(qn_table, 1, sum)) ,
                                    mean(apply(qn_table, 1, sum)), 
                                    sd(apply(qn_table, 1, sum))), 
                     "Aged <10" = c(sum(age <= 10),
                                    max(apply(qn_table[which(age <= 10),], 1, sum)),
                                    min(apply(qn_table[which(age <= 10),], 1, sum)),
                                    mean(apply(qn_table[which(age <= 10),], 1, sum)),
                                    sd(apply(qn_table[which(age <= 10),], 1, sum))),
                     "Aged 10-20" = c(sum(age >= 10 & age <= 20 ),
                                      max(apply(qn_table[age >= 10 & age <= 20,], 1, sum)),
                                      min(apply(qn_table[age >= 10 & age <= 20,], 1, sum)),
                                      mean(apply(qn_table[age >= 10 & age <= 20,], 1, sum)),
                                      sd(apply(qn_table[age >= 10 & age <= 20,], 1, sum))),
                     "Aged >20" = c(sum (age >= 20)-1,
                                    max(apply(qn_table[which(age >= 20),], 1, sum)),
                                    min(apply(qn_table[which(age >= 20),], 1, sum)),
                                    mean(apply(qn_table[which(age >= 20),], 1, sum)),
                                    sd(apply(qn_table[which(age >= 20),], 1, sum)))
  )
  if (ncol(qn_table) <= 200) s_s[2:5,] <- s_s[2:5,]/ncol(qn_table)  
  
  s_s <- round(s_s, digits = 2)
  
  if (ncol(qn_table) >= 200) {
    rownames(s_s) <- c("n", "max n items listed",  "min n items listed" , "mean n items listed", "sd n items listed"  )} else {
      if (ncol(qn_table) <= 55) {
        rownames(s_s) <- c("n", "max % questions correct",  "min % questions correct" , "mean % questions correct", "sd % questions correct"  )} else {
          if (ncol(qn_table) <= 200 & ncol(qn_table) >= 55) {
            rownames(s_s) <- c("n", "max % images recognized",  "min % images recognized" , "mean % images recognized", "sd % images recognized"  )} else{
              print("No data to function. Choose between d$Y_l, d$Y_q, d$Y_r ")}}}
  return(s_s)
}

write.csv( generate_s_size_table(qn_table = d$Y_l), "4_Outputs/generated_values/sample_sizes_freelist.csv")
write.csv( generate_s_size_table(qn_table = d$Y_q), "4_Outputs/generated_values/sample_sizes_questionnaire.csv")
write.csv( generate_s_size_table(qn_table = d$Y_r), "4_Outputs/generated_values/sample_sizes_images.csv")


#contrasts between sexes
#effect of age in boys minus effect of age in girls
mean_diff_sexes <- function (post, dimn = 1){ mean(post$bA[,1,dimn] - post$bA[,2,dimn])}
PI_diff_sexes <- function (post, dimn = 1){ PI(post$bA[,1,dimn] - post$bA[,2,dimn])}

mean_diff_sexes(post = post_age_1)
PI_diff_sexes(post = post_age_1)

mean_diff_sexes(post = post_act_1)
PI_diff_sexes(post = post_act_1)

posts_for_contrasts <- c("post_age_1",#fig 4
                            "post_age_3","post_age_3","post_age_3",  #fig 5
                            "post_age_l_1", "post_age_q_1", "post_age_r_1", #fig S3
                            "post_age_l_3", "post_age_l_3", "post_age_l_3", #fig S4
                            "post_age_2", "post_age_2", #fig S12
                            "post_age_4","post_age_4","post_age_4","post_age_4",  #fig s13
                            "post_age_5","post_age_5","post_age_5","post_age_5","post_age_5",  #fig s14
                            "post_act_1"#figS16
                         )
dimn_for_contrasts <- c(1,#fig 4
                        1,3,2,#fig 5
                        1,1,1,#fig S3
                        1,2,3,#fig S4
                        1,2,#fig S12
                        4,2,1,3,#fig S13
                        1,5,2,3,4,#fig S14
                        1)#fig S16
contrasts <- data.frame( "mean difference" = rep(NA, length(dimn_for_contrasts)),
                         "5% difference" =  rep(NA, length(dimn_for_contrasts)),
                         "95% difference" =  rep(NA, length(dimn_for_contrasts)) )
rownames(contrasts) <- c("Model 1 unidimensional, fig 4", #fig 4
                         "Model 1, 3 dim, general knowledge, fig 5a", "Model 1, 3 dim, male knowledge, fig 5b", "Model 1, 3 dim, other knowledge, fig 5c", #fig 5
                         "Model 1, freelist only, fig S3a", "Model 1, questionnaire only, fig S3b", "Model 1, image only, fig S3c", #fig S3
                         "Model 1, freelist only, 3 dim, general knowledge, fig S4a", "Model 1, freelist only, 3 dim, male knowledge, fig S4b", "Model 1, freelist only, 3 dim, other knowledge, fig S4c", #fig S4
                         "Model 1, 2 dim, general knowledge, fig S12a", "Model 1, 2 dim, male knowledge, fig S12b",
                         "Model 1, 4 dim, general knowledge, fig S13a", "Model 1, 4 dim, male knowledge, fig S13b", "Model 1, 4 dim, other knowledge, fig S13c", "Model 1, 4 dim, other knowledge2, fig S13d", #fig 5
                         "Model 1, 5 dim, general knowledge, fig S14a", "Model 1, 5 dim, male knowledge, fig S14b", "Model 1, 5 dim, male knowledge2, fig S14c", "Model 1, 5 dim, other knowledge, fig S14d", "Model 1, 5 dim, other knowledge2, fig S14e", #fig 5
                          "Model 2 unidimensional, fig S16")
for (i in 1:length(dimn_for_contrasts)) {
  contrasts[i,1] <- mean_diff_sexes(post = get(posts_for_contrasts[i]), dimn = dimn_for_contrasts[i] )
  contrasts[i,2] <- PI_diff_sexes(post = get(posts_for_contrasts[i]), dimn = dimn_for_contrasts[i] )[1]
  contrasts[i,3] <- PI_diff_sexes(post = get(posts_for_contrasts[i]), dimn = dimn_for_contrasts[i] )[2]
}

write.csv(contrasts, "4_Outputs/generated_values/contrasts_sexes_by_image.csv")

#activities
agediff_by_act <- data.frame(t(as.data.frame(lapply(actdiff, PI)))) 
agediff_by_act <- cbind (  as.vector(unlist(lapply(actdiff, mean))), agediff_by_act)
colnames(agediff_by_act) <- c("mean", "5%PI", "95%PI")

write.csv(agediff_by_act, "4_Outputs/generated_values/age_difference_by_activity.csv")


#Presence of same sex parent
mean(post_ssp_1$bSP[,1,])
mean(post_ssp_1$bSP[,2,])
PI(post_ssp_1$bSP[,1,])
PI(post_ssp_1$bSP[,2,])

##########################
#Differences between question types

mean(post_age_1$a_l)
PI(post_age_1$a_l)
mean(post_age_1$a_q)
PI(post_age_1$a_q)
mean(post_age_1$a_r)
PI(post_age_1$a_r)

###########################
#Sampling bias#############
###########################

#effect of distance from research station
#mean years gained by living one standard deviation further than the mean from the research station, compared to one sd closer to the station
mean(diffage(post = post_dis, counterfactual_1 = -1 * post_dis$bHD[,1] , counterfactual_2 = 1 * post_dis$bHD[,1]  ))
PI(diffage(post = post_dis, counterfactual_1 = -1 * post_dis$bHD[,1] , counterfactual_2 = 1 * post_dis$bHD[,1]  ))
mean(d$HH_dist [ !is.na(d$HH_dist) & d$A <= 50]) - sd(d$HH_dist [ !is.na(d$HH_dist) & d$A <= 50])
mean(d$HH_dist [ !is.na(d$HH_dist) & d$A <= 50]) + sd(d$HH_dist [ !is.na(d$HH_dist) & d$A <= 50])


##########################################
#Value of stuff and content of dimensions#
##########################################
b_ls <- apply(post_age_1$b_l, 2, mean)
b_ls_1 <- apply(post_age_3$b_l[,,1], 2, mean)
b_ls_2 <- apply(post_age_3$b_l[,,2], 2, mean)
b_ls_3 <- apply(post_age_3$b_l[,,3], 2, mean)

b_ls_1 <- apply(post_age_2$b_l[,,2], 2, mean)
b_ls_2 <- apply(post_age_2$b_l[,,1], 2, mean)
b_ls_3 <- apply(post_age_4$b_l[,,2], 2, mean)


colnames(d$Y_l)[which(b_ls <=max(sort(b_ls)[1:10]))]
colnames(d$Y_l)[which(b_ls_1 <=max(sort(b_ls_1)[1:10]))]
colnames(d$Y_l)[which(b_ls_2 <=max(sort(b_ls_2)[1:10]))]
colnames(d$Y_l)[which(b_ls_3 <=max(sort(b_ls_3)[1:10]))]

colnames(d$Y_l)[which(b_ls >=min(sort(b_ls, decreasing = T)[1:10]))]
colnames(d$Y_l)[which(b_ls_1 >=min(sort(b_ls_1, decreasing = T)[1:30]))]
colnames(d$Y_l)[which(b_ls_2 >=min(sort(b_ls_2, decreasing = T)[1:30]))]
colnames(d$Y_l)[which(b_ls_3 >=min(sort(b_ls_3, decreasing = T)[1:30]))]

b_qs <- apply(post_age_1$b_q, 2, mean)
b_qs_1 <- apply(post_age_3$b_q[,,1], 2, mean)
b_qs_2 <- apply(post_age_3$b_q[,,2], 2, mean)
b_qs_3 <- apply(post_age_3$b_q[,,3], 2, mean)
print("one dimension model")
colnames(d$Y_q)[which(b_qs <=max(sort(b_qs)[1:10]))]
colnames(d$Y_q)[which(b_qs >=min(sort(b_qs, decreasing = T)[1:10]))]
print("three dimensions model_2")
easy_q2 <- colnames(d$Y_q)[which(b_qs_2 <=max(sort(b_qs_2)[1:10]))]
colnames(d$Y_q)[which(b_qs_2 >=min(sort(b_qs_2, decreasing = T)[1:30]))]
print("three dimensions model_1")
easy_q1 <- colnames(d$Y_q)[which(b_qs_1 <=max(sort(b_qs_1)[1:10]))]
colnames(d$Y_q)[which(b_qs_1 >=min(sort(b_qs_1, decreasing = T)[1:30]))]
print("three dimensions model_3")
easy_q3 <- colnames(d$Y_q)[which(b_qs_3 <=max(sort(b_qs_3)[1:10]))]
colnames(d$Y_q)[which(b_qs_3 >=min(sort(b_qs_3, decreasing = T)[1:30]))]

easy_q2 [! easy_q2 %in% easy_q1 & ! easy_q2 %in% easy_q3]
easy_q1 [! easy_q1 %in% easy_q3 & ! easy_q1 %in% easy_q2]
easy_q3 [! easy_q3 %in% easy_q1 & ! easy_q3 %in% easy_q2]

b_rs <- apply(post_age_1$b_r, 2, mean)
b_rs_1 <- apply(post_age_3$b_r[,,1], 2, mean)
b_rs_2 <- apply(post_age_3$b_r[,,2], 2, mean)
b_rs_3 <- apply(post_age_3$b_r[,,3], 2, mean)
print("one dimension model")
colnames(d$Y_r)[which(b_rs <=max(sort(b_rs)[1:10]))]
colnames(d$Y_r)[which(b_rs >=min(sort(b_rs, decreasing = T)[1:10]))]
print("three dimensions model_2")
easy_r2 <- colnames(d$Y_r)[which(b_rs_2 <=max(sort(b_rs_2)[1:10])) & ! which(b_rs_2 <=max(sort(b_rs_2)[1:10])) & !which(b_rs_3 <=max(sort(b_rs_3)[1:10]))]
colnames(d$Y_r)[which(b_rs_2 >=min(sort(b_rs_2, decreasing = T)[1:30]))]
print("three dimensions model_1")
easy_r1 <- colnames(d$Y_r)[which(b_rs_1 <=max(sort(b_rs_1)[1:10]))]
colnames(d$Y_r)[which(b_rs_1 >=min(sort(b_rs_1, decreasing = T)[1:30]))]
print("three dimensions model_3")
easy_r3 <- colnames(d$Y_r)[which(b_rs_3 <=max(sort(b_rs_3)[1:10]))]
colnames(d$Y_r)[which(b_rs_3 >=min(sort(b_rs_3, decreasing = T)[1:30]))]

easy_r2 [! easy_r2 %in% easy_r3 & ! easy_r2 %in% easy_r1]
easy_r1 [! easy_r1 %in% easy_r3 & ! easy_r1 %in% easy_r2]
easy_r3 [! easy_r3 %in% easy_r1 & ! easy_r3 %in% easy_r2]

Ks <- apply(post_age_1$K, 2, mean)
Ks_1 <- apply(post_age_3$K[,,1], 2, mean)
Ks_2 <- apply(post_age_3$K[,,2], 2, mean)
Ks_3 <- apply(post_age_3$K[,,3], 2, mean)
best_all <- rownames(d$Y_l)[which(Ks >=min(sort(Ks, decreasing = T)[1:10]))]
best_1 <- rownames(d$Y_l)[which(Ks_2 >=min(sort(Ks_2, decreasing = T)[1:10]))]
best_2 <- rownames(d$Y_l)[which(Ks_1 >=min(sort(Ks_1, decreasing = T)[1:10]))]
best_3 <- rownames(d$Y_l)[which(Ks_3 >=min(sort(Ks_3, decreasing = T)[1:10]))]

rare_w <- which(apply(d$Y_l, 2, sum) <=1)
rare_w_all <- NA
for (i in 1:10){
  rare_w_all <- append( rare_w_all, names(  rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_all[i]),]>=1)))]))}
rw_all <- data.frame(rare_w_all, type = rep(NA, length(rare_w_all)), dim = rep("d_all", length(rare_w_all)))
rw_all <- rw_all[-is.na(rw_all$rare_w_all),]
for ( i in 1: nrow(rw_all)) {
  rw_all$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_all$rare_w_all[i])]}

rare_w_1 <- NA
for (i in 1:10){
  rare_w_1 <- append( rare_w_1, names(  rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_1[i]),]>=1)))]))}
rw_1 <- data.frame(rare_w_1, type = rep(NA, length(rare_w_1)), dim = rep("d1", length(rare_w_1)))
rw_1 <- rw_1[-is.na(rw_1$rare_w_1),]
for ( i in 1: nrow(rw_1)) {
  rw_1$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_1$rare_w_1[i])]}

rare_w_2 <- NA
for (i in 1:10){
  rare_w_2 <- append( rare_w_2, names(rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_2[i]),]>=1)))]))}
rw_2 <- data.frame(rare_w_2, type = rep(NA, length(rare_w_2)), dim = rep("d2", length(rare_w_2)))
rw_2 <- rw_2[-is.na(rw_2$rare_w_2),]
for ( i in 1: nrow(rw_2)) {
  rw_2$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_2$rare_w_2[i])]}

rare_w_3 <- NA
for (i in 1:10){
  rare_w_3 <- append( rare_w_3, names(rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_3[i]),]>=1)))]))}
rw_3 <- data.frame(rare_w_3, type = rep(NA, length(rare_w_3)), dim = rep("d3", length(rare_w_3)))
rw_3 <- rw_3[-is.na(rw_3$rare_w_3),]
for ( i in 1: nrow(rw_3)) {
  rw_3$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_3$rare_w_3[i])]}


rw <- as.data.frame(mapply(c,rw_all, rw_1, rw_2, rw_3))
write.csv(rw, "rare_words.csv", row.names = FALSE)

rw_counts <- as.data.frame(rw %>% group_by(type, dim) %>% count())
ggplot(rw_counts, aes(fill=type, y=n, x=dim)) + 
  geom_bar(position="stack", stat="identity")
rm(rare_w, rare_w_1, rare_w_2, rare_w_3, rare_w_all, 
   rw_1, rw_2, rw_3, rw_all, 
   best_1, best_2, best_3, best_all,
   Ks, Ks_1, Ks_2, Ks_3)

