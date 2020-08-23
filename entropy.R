
rm(list=ls())

setwd("/Volumes/Samsung_T3/yura/20170615_Preprocessed_Data")
# setwd("h:/yura/20170615_Preprocessed_Data")
load("20170904_preprocessed_all_final_ready_to_analyze2.RData")


# 
# ## 1. T.Test
# p_values = matrix(1,ncol(datax),10)  
# for (i in 1:10){
#   
#   bad_idx = which(datay[,i] == 1)
#   good_idx = which(datay[,i] == 0)
#   
#   bad_x = datax[bad_idx,]
#   good_x = datax[good_idx,]
#   
#   for (j in seq(1,ncol(bad_x))){
#     
#     tmp_bad = bad_x[,j]
#     tmp_good = good_x[,j]
#     
#     if(length(which(tmp_bad == -9999)) > 0){
#       tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
#     }
#     if(length(which(tmp_good == -9999)) > 0){
#       tmp_good = tmp_good[-which(tmp_good == -9999)]
#     }
#     
#     if (length(tmp_bad) > 0){
#       if (length(tmp_good) > 0){
#         if (sd(tmp_bad != 0)){
#           if (sd(tmp_good != 0)){
#             aa = t.test(tmp_good, tmp_bad)
#             p_values[j,i] = aa$p.value
#           }
#         }
#       }
#     }
#   }
# }
# 
# write.csv(p_values, "p_values.csv")
# 
# ## P value가 좋??? ?????? ?????????
# ## ?????? 민감??? 것도 ???지???
# ## 불량 ???????????? ??????가 ????????? ????????? 값만 ?????? 경우
# ## 불량 ????????? 쪽의 sd=0 ???므??? 무조??? p value = 0??? ??????


# ss = sort(p_values, index.return=TRUE)
# 
# 
# 
# j = ss$ix[9]
# 
# j= 11302

# 
# colnames(bad_x)[j]
# tmp_bad = bad_x[,j]
# tmp_good = good_x[,j]
# if(length(which(tmp_bad == -9999)) > 0){
#   tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
# }
# if(length(which(tmp_good == -9999)) > 0){
#   tmp_good = tmp_good[-which(tmp_good == -9999)]
# }
# mean(tmp_bad)
# mean(tmp_good)
# 
# plot(tmp_good)
# points(tmp_bad, col="RED")
# 
# plot(density(tmp_good), ylim=c(0,200),col="BLUE")
# lines(density(tmp_bad), col="RED")
# 
# # hist(tmp_bad)









## 2. Interval Analysis

set_interval = 10
result_list = list()

for (i in 2){
  
  print(i)
  
  idx = 0
  result_matrix = matrix(-1,ncol(datax)*set_interval,14)
  
  
  bad_idx = which(datay[,i] == 1)
  good_idx = which(datay[,i] == 0)
  
  bad_x = datax[bad_idx,]
  good_x = datax[good_idx,]
  
  
  for (j in seq(1,ncol(bad_x))){
    
    tmp_bad = bad_x[,j]
    tmp_good = good_x[,j]
    
    if(length(which(tmp_bad == -9999)) > 0){
      tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
    }
    if(length(which(tmp_good == -9999)) > 0){
      tmp_good = tmp_good[-which(tmp_good == -9999)]
    }
    
    tmp_range = range(tmp_bad, tmp_good)
    tmp_interval = (max(tmp_range) - min(tmp_range))/set_interval
    
    if (tmp_interval > 0){
      
      for (k in 1:set_interval){
        
        idx = idx + 1
        
        tmp_low = min(tmp_range) + (k-1)*tmp_interval
        tmp_high = min(tmp_range) + k*tmp_interval
        
        tmp_idx_bad = intersect(which(tmp_bad >= tmp_low), which(tmp_bad < tmp_high))
        tmp_idx_good = intersect(which(tmp_good >= tmp_low), which(tmp_good < tmp_high))
        
        result_matrix[idx, 1] = colnames(bad_x)[j]
        result_matrix[idx, 2] = length(tmp_idx_bad)
        result_matrix[idx, 3] = length(tmp_idx_good)
        result_matrix[idx, 4] = k
        result_matrix[idx, 5] = tmp_low
        result_matrix[idx, 6] = tmp_high
        result_matrix[idx, 7] = length(tmp_idx_bad)/length(tmp_bad)
        result_matrix[idx, 8] = length(tmp_idx_good)/length(tmp_good)
        
        if (length(tmp_idx_good) > 0){
          result_matrix[idx, 9] = (length(tmp_idx_bad)/length(tmp_bad))/(length(tmp_idx_good)/length(tmp_good))
        }else{
          result_matrix[idx, 9] = 100
        }
        
        if (length(tmp_idx_bad) + length(tmp_idx_good) > 0){
          result_matrix[idx, 10] = length(tmp_idx_bad)/(length(tmp_idx_bad) + length(tmp_idx_good))
          result_matrix[idx, 11] = length(tmp_idx_good)/(length(tmp_idx_bad) + length(tmp_idx_good))
          result_matrix[idx, 12] = (length(tmp_idx_bad)/(length(tmp_idx_bad) + length(tmp_idx_good)))/(nrow(bad_x)/nrow(datax))
          result_matrix[idx, 13] = length(tmp_idx_bad)/nrow(datax)
        }
        
        if (length(tmp_idx_bad) + length(tmp_idx_good) == 0){
          result_matrix[idx, 14] = -1
        }else{
          tmp_ratio = length(tmp_idx_bad) + length(tmp_idx_good)
          tmp_ratio_bad = length(tmp_idx_bad)/tmp_ratio
          tmp_ratio_good = length(tmp_idx_good)/tmp_ratio
          result_matrix[idx, 14] = -tmp_ratio_bad*log(tmp_ratio_bad) + -tmp_ratio_good*log(tmp_ratio_good)
          
          if (result_matrix[idx, 14] == 'NaN'){
            result_matrix[idx, 14] = -1
          }
          
        }
      }
    }
    
    
    # result_matrix[idx, 8] = mean(tmp_bad)
    # result_matrix[idx, 9] = mean(tmp_good)
    # 
    # if (length(tmp_bad) > 0){
    #   if (length(tmp_good) > 0){
    #     if (sd(tmp_bad != 0)){
    #       if (sd(tmp_good != 0)){
    #         aa = t.test(tmp_good, tmp_bad)
    #         result_matrix[idx,10] = aa$p.value
    #       }
    #     }
    #   }
    # }
    # 
  }
  
  colnames(result_matrix) = c("X", "N_BAD", "N_GOOD", "N INTERVAL", "RANGE_LOW", "RANGE_HIGH", "BAD_P", "GOOD_P", "P_LIFT", "BAD_CONF", "GOOD_CONF", "LIFT", "BAD_RATIO", "ENTROPY")
  
  result_list[[i]] = result_matrix
  
}




write.csv(result_matrix, "aa.csv")








colnames(datay)

ii=2
i=552
headers_y_range[i,]

r_bad_idx = which(datay[,ii] == 1)
r_bad_values = datay_value[r_bad_idx,ii-1]

r_bad_products = as.data.frame(matrix(0,length(r_bad_idx),4))
r_bad_products[,1] = r_bad_idx
r_bad_products[,2] = headers$IFT_DATE[r_bad_idx]
r_bad_products[,3] = headers$IFT_TIME[r_bad_idx]
r_bad_products[,4] = r_bad_values

aa = as.data.frame(result_list[[ii]])
r_rare_idx = which(as.numeric(as.matrix(aa$N_BAD)) >= 5)
aa2 = aa[r_rare_idx,]
r_lift_sum = as.numeric(as.matrix(aa2$P_LIFT)) + as.numeric(as.matrix(aa2$LIFT))

sort_lift_sum = sort(r_lift_sum, decreasing=TRUE, index.return=TRUE)
aa2_sort = aa2[sort_lift_sum$ix,]

write.csv(aa2_sort, "result_y487.csv")






bad_x = datax[r_bad_idx,]
good_x = datax[setdiff(seq(1:nrow(datax)),r_bad_idx),]


target_name = aa2_sort$X[1]
j = which(colnames(datax) == target_name)
colnames(datax)[j]

tmp_bad = bad_x[,j]
tmp_good = good_x[,j]

if(length(which(tmp_bad == -9999)) > 0){
  tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
}
if(length(which(tmp_good == -9999)) > 0){
  tmp_good = tmp_good[-which(tmp_good == -9999)]
}
mean(tmp_bad)
mean(tmp_good)

kk = density(tmp_good)
kkk = density(tmp_bad)
kkkk = max(kk$y, kkk$y)
plot(density(tmp_good), ylim=c(0,kkkk),col="BLUE")
lines(density(tmp_bad), col="RED")


par(mfrow=c(2,1))
hist(tmp_good, xlim=c(-0.15,0.15))
hist(tmp_bad, xlim=c(-0.15,0.15))

plot(tmp_good)
points(tmp_bad, col="RED", pch=19)








cor_matrix = matrix(0,100,1)
for (i in 1:100){
  
  cor_matrix[i,1] = cor(as.numeric(datax[,i]), as.numeric(datay_value[,1]))
  
}

sort_result = sort(cor_matrix, decreasing=TRUE, index.return=TRUE)
cor_sort = as.data.frame(cor_matrix[sort_result$ix,1])
colnames(datax)[sort_result$ix[1:10]]
plot(datax[,sort_result$ix[1]], datay_value[,1])
points(datax[which(datay[,2] == 1),sort_result$ix[1]], datay_value[which(datay[,2] == 1),1], pch=19, col="RED")


cor_matrix = matrix(0,100,100)
for (i in 1:100){
  for (j in 1:100){
    
    cor_matrix[i,j] = cor(as.numeric(datax[,i]), as.numeric(datax[,j]))
  }
  
}

write.csv(cor_matrix, "cor_matrix.csv")












#
j= 11302


colnames(datax)[j]
tmp_bad = bad_x[,j]
tmp_good = good_x[,j]
if(length(which(tmp_bad == -9999)) > 0){
  tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
}
if(length(which(tmp_good == -9999)) > 0){
  tmp_good = tmp_good[-which(tmp_good == -9999)]
}
mean(tmp_bad)
mean(tmp_good)

plot(tmp_good)
points(tmp_bad, col="RED")

plot(density(tmp_good), ylim=c(0,0.0007),col="BLUE")
lines(density(tmp_bad), col="RED")

# hist(tmp_bad)




for (i in 1:10){
  
  bad_idx = which(datay[,i] == 1)
  good_idx = which(datay[,i] == 0)
  
  
  bad_x = datax[bad_idx,]
  good_x = datax[good_idx,]
  
  
  p_values = matrix(1,ncol(bad_x),1)  
  for (j in seq(1,ncol(bad_x))){
    
    tmp_bad = bad_x[,j]
    tmp_good = good_x[,j]
    
    if(length(which(tmp_bad == -9999)) > 0){
      tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
    }
    if(length(which(tmp_good == -9999)) > 0){
      tmp_good = tmp_good[-which(tmp_good == -9999)]
    }
    
    if (length(tmp_bad) > 0){
      if (length(tmp_good) > 0){
        if (sd(tmp_bad != 0)){
          if (sd(tmp_good != 0)){
            aa = t.test(tmp_good, tmp_bad)
            p_values[j,1] = aa$p.value
          }
        }
      }
    }
  }
}

ss = sort(p_values, index.return=TRUE)



j = ss$ix[9]

j= 7818

colnames(bad_x)[j]
tmp_bad = bad_x[,j]
tmp_good = good_x[,j]
if(length(which(tmp_bad == -9999)) > 0){
  tmp_bad = tmp_bad[-which(tmp_bad == -9999)]
}
if(length(which(tmp_good == -9999)) > 0){
  tmp_good = tmp_good[-which(tmp_good == -9999)]
}
mean(tmp_bad)
mean(tmp_good)

plot(tmp_good)
points(tmp_bad, col="RED")

plot(density(tmp_good), ylim=c(0,200),col="BLUE")
lines(density(tmp_bad), col="RED")

# hist(tmp_bad)
# hist(tmp_good)

# tmp_bad2 = tmp_bad/length(tmp_bad)
# tmp_good2 = tmp_good/length(tmp_good)

# xlim = range(tmp_bad, tmp_good)
# ylim = range(0,1)


# hist(tmp_good,col=scales::alpha('red',.5),border=F)
# hist(tmp_bad,add=T,col='skyblue',border=F)





# dens_bad = density(tmp_bad)
# dens_good = density(tmp_good)
# 
# xlim = range(dens_bad$x, dens_good$x)
# ylim = range(0,dens_bad$y, dens_good$y)
# 
# bad_col <- rgb(1,0,0,0.2)
# good_col <- rgb(0,0,1,0.2)
# 
# plot(dens_bad, xlim = xlim, ylim = ylim, xlab = 'Value',
#      main = 'Distribution of bad and good', 
#      panel.first = grid())
# polygon(dens_bad, density = -1, col = bad_col)
# polygon(dens_good, density = -1, col = good_col)
# legend('topleft',c('Bad','Good'),
#        fill = c(bad_col, good_col), bty = 'n',
#        border = NA)
# 






