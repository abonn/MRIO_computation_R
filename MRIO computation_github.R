library(plyr)
ptm <- proc.time()
######### Y (consumption)
Y1 = read.table("Y matrix path", sep="\t", header = FALSE)

Y <- Y1[, -c(1:3)] ## range of columns to remove
Y <- Y[-c(1:2), ] ## range of rows to remove

Y_summary <- data.frame(nrow(Y),ncol(Y))
Y_summary <- plyr::rename(Y_summary, c("nrow.Y."="nrow.Y","ncol.Y."="ncol.Y"))
Y_summary <- as.data.frame(t(Y_summary))
Y_summary <- plyr::rename(Y_summary, c("V1"="value"))

Y_matrix <- data.matrix(Y)


######### L (Leontief Inverse)
L = read.csv("L matrix path", sep="\t", header = FALSE)

L_summary <- data.frame(nrow(L),ncol(L))
L_summary <- plyr::rename(L_summary, c("nrow.L."="nrow.L","ncol.L."="ncol.L"))
L_summary <- as.data.frame(t(L_summary))
L_summary <- plyr::rename(L_summary, c("V1"="value"))

L_matrix <- data.matrix(L)


######## S (emissions)
S1 = read.csv("S matrix path", sep="\t", header = FALSE)

S <- S1[, -c(1:3)] ## range of columns to remove
S <- S[-c(1:2), ] ## range of rows to remove

S_matrix <- data.matrix(S)

S_summary <- data.frame(nrow(S),ncol(S))
S_summary <- plyr::rename(S_summary, c("nrow.S."="nrow.S","ncol.S."="ncol.S"))
S_summary <- as.data.frame(t(S_summary))
S_summary <- plyr::rename(S_summary, c("V1"="value"))

######## computation ########

LxY <- L_matrix %*% Y_matrix

B <- S_matrix %*% LxY

B_summary <- data.frame(nrow(B),ncol(B))
B_summary <- plyr::rename(B_summary, c("nrow.B."="nrow.B","ncol.B."="ncol.B"))
B_summary <- as.data.frame(t(B_summary))
B_summary <- plyr::rename(B_summary, c("V1"="value"))

write.table(B,file="B_rawdata.txt",row.names=FALSE, sep = "\t")

######## re-integrate emissions + country data #######

re_int_r <- data.frame(subset(Y1[c(1:2), ])) ##text
re_int_c <- data.frame(subset(S1[, c(1:3)])) ##text
B_df <- data.frame(B)
xx <- rep(NA, ncol(B_df)) ##create NA columns for replacement
xy <- rep(NA, ncol(B_df))
B_df <- rbind(xx, B_df)
B_df <- rbind(xy, B_df)
B_df <- cbind(re_int_c, B_df) ##insert text into NA columns
B_df_test <- B_df[-c(1:2),]

##due to rbinds issue merging factors with numerical data
##it needed to be merged in two steps, with the extra rows
##deleted after
B_df_test <- rbind(B_df_test, re_int_r)
B_df_test <- rbind(re_int_r, B_df_test)
B_df_test <- B_df_test[-c(173:174),]
##column integration dependent on country selection and MRIO database

write.table(B_df_test,file="B_textintegrated.txt",row.names=FALSE, col.names=FALSE, sep = "\t")

##reintegrate: DE only //example
B_df_test_DE <- B_df_test[,-c(4:38)]
B_df_test_DE <- B_df_test_DE[,-c(11:304)]
write.table(B_df_test_DE,file="B_textintegrated_DE.txt",row.names=FALSE, col.names=FALSE, sep = "\t")


######## summary_table ########

summary_precomp <- rbind(Y_summary, L_summary, S_summary)
summary_full <- rbind(Y_summary, L_summary, S_summary, B_summary)


write.table(summary_precomp,file="summary_precomp.csv",na = "NA", row.names=TRUE, 
            col.names = TRUE, sep = ",") ## export summary table

write.table(summary_full,file="summary_full.csv",na = "NA", row.names=TRUE, 
            col.names = TRUE, sep = ",") ## export summary table

proc.time() - ptm



