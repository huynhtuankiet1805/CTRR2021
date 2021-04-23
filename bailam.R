#Bài i
  #Số sinh viên tham gia kỳ thi giữa kỳ:
    nStuGK = nrow(file_GK)
  #Số sinh viên tham gia kỳ thi cuối kỳ:
    nStuCK = nrow(file_CK)
#Bài ii
    #1 Tổng các câu đúng của mỗi sinh viên trong tập mẫu
      nRightGK = rowSums(file_GK[4:28])
      nRightCK = rowSums(file_CK[4:41],na.rm = TRUE)
    #2 Tổng các câu sai:
      nWrongGK = 25 - nRightGK
      nWrongCK = 38 - nRightCK
    #3 Số câu đúng nhiều nhất:
      #Tổng các câu đúng GK:
        sumofNumGK <- data.frame(t(colSums(file_GK[4:28])))
        
      #Tên câu đúng nhiều nhất GK:
        maxofNumGK = colnames(sumofNumGK)[max.col(sumofNumGK,ties.method = "first")]
        
      #Tổng các câu đúng CK:
        sumofNumCK <- data.frame(t(colSums(file_CK[4:41], na.rm = TRUE)))
        
      #Tên câu đúng nhiều nhất CK:
        maxofNumCK = colnames(sumofNumCK)[max.col(sumofNumCK,ties.method = "first")]
        
      ###Tên câu đúng nhiều nhất: maxofNum
        if(max(sumofNumCK) < max(sumofNumGK)) maxofNum = maxofNumGK
    #3 Số câu đúng thấp nhất:
        #Tên câu đúng ít nhất GK:
          minofNumGK = colnames(sumofNumGK)[apply(sumofNumGK, 1, which.min)]
        
        #Tên câu đúng ít nhất CK:
          minofNumCK = colnames(sumofNumCK)[apply(sumofNumCK, 1, which.min)]
        
        #Tên câu đúng ít nhất: minofNum
          if(min(sumofNumCK) < min(sumofNumGK)){minofNum = minofNumCK}else{minofNum = minofNumGK}
    #4 Vẽ biểu đồ
          #Tổng các câu đúng của mỗi sinh viên theo mã đề:
            #Giữa Kỳ
              nRightGK_2011 <- data.frame(rowSums(file_GK[1:42,4:28]))
                colnames(nRightGK_2011) <- "socaudung"
              nRightGK_2012 <- data.frame(rowSums(file_GK[43:82,4:28]))
                colnames(nRightGK_2012) <- "socaudung"
              nRightGK_2013 <- data.frame(rowSums(file_GK[83:117,4:28]))
                colnames(nRightGK_2013) <- "socaudung"
              nRightGK_2014 <- data.frame(rowSums(file_GK[118:157,4:28]))
                colnames(nRightGK_2014) <- "socaudung"
              nWrongGK_2011 <- data.frame(25 - rowSums(file_GK[1:42,4:28]))
                colnames(nWrongGK_2011) <- "socausai"
              nWrongGK_2012 <- data.frame(25 - rowSums(file_GK[43:82,4:28]))
                colnames(nWrongGK_2012) <- "socausai"
              nWrongGK_2013 <- data.frame(25 - rowSums(file_GK[83:117,4:28]))
                colnames(nWrongGK_2013) <- "socausai"
              nWrongGK_2014 <- data.frame(25 - rowSums(file_GK[118:157,4:28]))
                colnames(nWrongGK_2014) <- "socausai"
              
            #Cuối kỳ
              #Sắp xếp theo mã đề: sxMD_CK
                file_CKsx = file_CK
                colnames(file_CKsx)[42] <- "Made"
                sxMD_CK <- file_CKsx %>%arrange(desc(Made))
              #Cho vào dataframe :
                nRightCK_2011 <- data.frame(rowSums(sxMD_CK[113:163,4:41],na.rm = TRUE))
                colnames(nRightCK_2011) <- "socaudung"
                nRightCK_2012 <- data.frame(rowSums(sxMD_CK[76:112,4:41],na.rm = TRUE))
                colnames(nRightCK_2012) <- "socaudung"
                nRightCK_2013 <- data.frame(rowSums(sxMD_CK[36:75,4:41],na.rm = TRUE))
                colnames(nRightCK_2013) <- "socaudung"
                nRightCK_2014 <- data.frame(rowSums(sxMD_CK[1:35,4:41],na.rm = TRUE))
                colnames(nRightCK_2014) <- "socaudung"
                
                nWrongCK_2011 <- data.frame(38 - rowSums(sxMD_CK[113:163,4:41],na.rm = TRUE))
                colnames(nWrongCK_2011) <- "socausai"
                
                nWrongCK_2012 <- data.frame(38 - rowSums(sxMD_CK[76:112,4:41],na.rm = TRUE))
                colnames(nWrongCK_2012) <- "socausai"
                
                nWrongCK_2013 <- data.frame(38 - rowSums(sxMD_CK[36:75,4:41],na.rm = TRUE))
                colnames(nWrongCK_2013) <- "socausai"
                
                nWrongCK_2013 <- data.frame(38 - rowSums(sxMD_CK[36:75,4:41],na.rm = TRUE))
                colnames(nWrongCK_2013) <- "socausai"
                
                nWrongCK_2014 <- data.frame(38 - rowSums(sxMD_CK[1:35,4:41],na.rm = TRUE))
                colnames(nWrongCK_2014) <- "socausai"
                
                
                
              
              
  
          #Vẽ
            #Giữa kỳ
              #MD2011
                #-----Câu đúng-----
              ggplot(nRightGK_2011, aes(x=socaudung)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu đúng GK MD2011")+
                scale_fill_gradient("Số câu", low="red", high="green")
                #-----Câu sai-------
              ggplot(nWrongGK_2011, aes(x=socausai)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu sai GK MD2011")+
                scale_fill_gradient("Số câu", low="green", high="red")
              
              #MD2012
                #-----Câu đúng-----
              ggplot(nRightGK_2012, aes(x=socaudung)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu đúng GK MD2012")+
                scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
              ggplot(nWrongGK_2012, aes(x=socausai)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu sai GK MD2012")+
                scale_fill_gradient("Số câu", low="green", high="red")
              
              
              #MD2013
                #-----Câu đúng-----
              ggplot(nRightGK_2013, aes(x=socaudung)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu đúng GK MD2013")+
                scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
              ggplot(nWrongGK_2013, aes(x=socausai)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu sai GK MD2013")+
                scale_fill_gradient("Số câu", low="green", high="red")
              #MD2014
                
                #-----Câu đúng-----
              ggplot(nRightGK_2014, aes(x=socaudung)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu đúng GK MD2014")+
                scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
              ggplot(nWrongGK_2014, aes(x=socausai)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                ggtitle("Biểu đồ phổ số câu sai GK MD2014")+
                scale_fill_gradient("Số câu", low="green", high="red")
            #Cuối kỳ
                #MD2011
                #-----Câu đúng-----
                ggplot(nRightCK_2011, aes(x=socaudung)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu đúng CK MD2011")+
                  scale_fill_gradient("Số câu", low="red", high="green")
                #-----Câu sai-------
                ggplot(nWrongCK_2011, aes(x=socausai)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu sai CK MD2011")+
                  scale_fill_gradient("Số câu", low="green", high="red")
                
                #MD2012
                #-----Câu đúng-----
                ggplot(nRightCK_2012, aes(x=socaudung)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu đúng CK MD2012")+
                  scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
                ggplot(nWrongCK_2012, aes(x=socausai)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu sai CK MD2012")+
                  scale_fill_gradient("Số câu", low="green", high="red")
                
                
                #MD2013
                #-----Câu đúng-----
                ggplot(nRightCK_2013, aes(x=socaudung)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu đúng CK MD2013")+
                  scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
                ggplot(nWrongCK_2013, aes(x=socausai)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu sai")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu sai CK MD2013")+
                  scale_fill_gradient("Số câu", low="green", high="red")
                #MD2014
                
                #-----Câu đúng-----
                ggplot(nRightCK_2014, aes(x=socaudung)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu đúng")+ ylab("Số lượng sinh viên") +
                  ggtitle("Biểu đồ phổ số câu đúng CK MD2014")+
                  scale_fill_gradient("Số câu", low="red", high="green")
                
                #-----Câu sai-----
                ggplot(nWrongCK_2014, aes(x=socausai)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 1) +
                  xlab("Số câu sai")+ ylab("Số lượng CK sinh viên") +
                  ggtitle("Biểu đồ phổ số câu sai MD2014")+
                  scale_fill_gradient("Số câu", low="green", high="red")
#Bài v
                chuongGK <- read_xlsx("201_CO1007.xlsx",sheet = "GK_0", range = "C12:AA16")
                chuongCK <- read_xlsx("201_CO1007.xlsx",sheet = "CK_0", range = "C12:AN16")
                #Tổng hợp số lượng chương liên quan
                      THchuong <- cbind(chuongGK, chuongCK)
                      mattrixchuong <- as.character(THchuong)
                      charChuong <- data.frame(mattrixchuong)
                      #-----------------------------------
                      charChuong$C1 <- str_count(charChuong$mattrixchuong,"1") - 2*str_count(charChuong$mattrixchuong,"11")-str_count(charChuong$mattrixchuong,"10")
                      charChuong$C2 <- str_count(charChuong$mattrixchuong,"2")
                      charChuong$C3 <- str_count(charChuong$mattrixchuong,"3")
                      charChuong$C4 <- str_count(charChuong$mattrixchuong,"4")
                      charChuong$C5 <- str_count(charChuong$mattrixchuong,"5")
                      charChuong$C6 <- str_count(charChuong$mattrixchuong,"6")
                      charChuong$C7 <- str_count(charChuong$mattrixchuong,"7")
                      charChuong$C8 <- str_count(charChuong$mattrixchuong,"8")
                      charChuong$C9 <- str_count(charChuong$mattrixchuong,"9")
                      charChuong$C10 <- str_count(charChuong$mattrixchuong,"10")
                      charChuong$C11 <- str_count(charChuong$mattrixchuong,"11")
                      #--------------------------------------------------
                      #1
                      
                      nChapter = nrow(data.frame(colSums(charChuong[2:12])))
                      #2
                      
                      Chapter = colSums(charChuong[2:12])
                      
                      #-----------------------------------------------------
                        
                      
                  
                
        
                
      
             
            
          
          
        
              
      