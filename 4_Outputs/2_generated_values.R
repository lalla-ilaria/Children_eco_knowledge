#Set the stage for plotting figures
if( !exists( "plotagesandknow", mode = "function")) source( "4_Outputs/0_set_the_plot_stage.R" )

waics_age <- compare(age_1, age_2, age_3, age_4, age_5)
waics_age_l <- compare(age_1, age_2, age_3, age_4, age_5, log_lik = "log_lik_l")
waics_age_q <- compare(age_1, age_2, age_3, age_4, age_5, log_lik = "log_lik_q")
waics_age_r <- compare(age_1, age_2, age_3, age_4, age_5, log_lik = "log_lik_r")


#contrasts between sexes
#effect of age in boys minus effect of age in girls
mean_diff_sexes <- function (post, dimn = 1){ mean(post$bA[,1,dimn] - post$bA[,2,dimn])}
PI_diff_sexes <- function (post, dimn = 1){ PI(post$bA[,1,dimn] - post$bA[,2,dimn])}

mean_diff_sexes(post = post_age_1)
PI_diff_sexes(post = post_age_1)

mean_diff_sexes(post = post_act_1)
PI_diff_sexes(post = post_act_1)


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

colnames(d$Y_l)[which(b_ls <=max(sort(b_ls)[1:10]))]
colnames(d$Y_l)[which(b_ls_2 <=max(sort(b_ls_2)[1:10]))]
colnames(d$Y_l)[which(b_ls_3 <=max(sort(b_ls_3)[1:10]))]
colnames(d$Y_l)[which(b_ls_1 <=max(sort(b_ls_1)[1:10]))]

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

