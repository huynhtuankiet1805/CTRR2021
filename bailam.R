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
# bai III
              
              #a va b
              data$col <- as.numeric(as.character(data$col))
              fileGKfix <- file_GK[order(file_GK$No),]
              nRightGKfix = rowSums(fileGKfix[4:28])
              DiemGK <- formattable(round(nRightGKfix/25*10,2),digits = 2, format ="f")
              DiemGKcheck <- data.frame(DiemGK)
              DiemCK <- formattable(round(nRightCK/38*10,2),digits = 2, format ="f")
              DiemCKcheck <- data.frame(DiemCK)
              DiemTK <- formattable(round(DiemGK*0.4 + DiemCK*0.6,2),digits = 2, format ="f")
              #3.1
              TVmauGK = median(DiemGK)
              TVmauCK = median(DiemCK)
              CDmauGK = max(DiemGK)
              CDmauCK = max(DiemCK)
              CTmauGK = min(DiemGK)
              CTmauCK = min(DiemCK)
              #3.2
              SVhon9GK = 0
              SVhon9CK = 0
              for (i in 1:163)
              {
                if (DiemGK[i]>=9 && i<=157){SVhon9GK=SVhon9GK + 1}
                if (DiemCK[i]>=9){SVhon9CK=SVhon9CK + 1}
              }
              #3.3
              SVhon7GK=0
              SVhon7CK=0
              for (i in 1:163)
              {
                if (DiemGK[i]>=7 && i<=157){SVhon7GK=SVhon7GK+1}
                if (DiemCK[i]>=7){SVhon7CK=SVhon7CK+1}
              }
              #3.4
              SVhon5GK=0
              SVhon5CK=0
              for (i in 1:163)
              {
                if (DiemGK[i]>=5 && i<=157){SVhon5GK=SVhon5GK+1}
                if (DiemCK[i]>=5){SVhon5CK=SVhon5CK+1}
              }
              #3.5
              SVbehon5=0
              for (i in 1:163)
              {
                if (DiemTK[i]<=5){SVbehon5=SVbehon5+1}
              }
             
              #3.6 ve bieu do pho diem GK va CK
              DiemGK1<-data.frame(DiemGK)
              DiemCK1<-data.frame(DiemCK)
              colnames(DiemGK1)<- "DiemGK"
              colnames(DiemCK1)<- "DiemCK"
                #bieudoGK
                ggplot(DiemGK1, aes(x=DiemGK)) + 
                geom_histogram(aes(fill=..x..), col = "white",binwidth = 0.4) +
                xlab("Di???m")+ ylab("S??? Sinh Viên") +
                ggtitle("Bi???u d??? ph??? di???m Gi???a k??? c???a Sinh Viên")+
                scale_fill_gradient("Di???m", low="red", high="green")
                #bieudoCK
                ggplot(DiemCK1,aes(x=DiemCK))+
                geom_histogram(aes(fill=..x..),col= "white",binwidth=0.4 )+
                xlab("Di???m")+ylab("S??? Sinh Viên")+
                ggtitle("Bi???u d??? ph??? di???m Cu???i k??? c???a Sinh Viên")+
                scale_fill_gradient("Di???m",low="red",high="green")
              #3.7
                #Nhom cao nhat, thap nhat GK vs CK
                
                diemL01=0
                diemL02=0
                fileGKfixnhom <- file_GK[order(file_GK$MANH,file_GK$No),]
                nRightGKfixnhom = rowSums(fileGKfixnhom[4:28])
                diemGKfixnhom <- formattable(round((nRightGKfixnhom/25*10),2),digits = 2, format ="f")
                for (i in 1:78)
                {
                  diemL01=diemL01+diemGKfixnhom[i]
                }
                for (i in 79:157)
                {
                  diemL02=diemL02+diemGKfixnhom[i]
                }
                if (diemL02>=diemL01){NhomMAXGK<-fileGKfixnhom[79:157,1:29]
                                      NhomMINGK<-fileGKfixnhom[1:78,1:29]} else 
                {
                  NhomMAXGK<-fileGKfixnhom[1:78,1:29]
                  NhomMINGK<-fileGKfixnhom[79:157,1:29]
                }
                diemL01=0
                diemL02=0
                for (i in 1:82)
                {
                  diemL01=diemL01+DiemCK[i]
                }
                for (i in 83:163)
                {
                  diemL02=diemL02+DiemCK[i]
                }
                if (diemL02>=diemL01){NhomMAXCK<-file_CK[83:163,1:42]
                                      NhomMINCK<-file_CK[1:82,1:42]} else
                {
                  NhomMAXCK<-file_CK[1:82,1:42]
                  NhomMINCK<-file_CK[83:163,1:42]
                }
                # To cao nhat, thap nhat GK va CK
                
                diemA=0
                diemB=0
                fileGKfixto<- file_GK[order(file_GK$TO,file_GK$No),]
                fileCKfixto<- file_CK[order(file_CK$TO),]
                nRightGKfixto = rowSums(fileGKfixto[4:28])
                diemGKfixto <- formattable(round((nRightGKfixto/25*10),2),digits = 2, format ="f")
                nRightCKfixto = rowSums(fileCKfixto[4:28],na.rm=TRUE)
                diemCKfixto <- formattable(round((nRightCKfixto/38*10),2),digits = 2, format ="f")
                for (i in 1:78)
                {
                  diemA=diemA+diemGKfixto[i]
                }
                for (i in 79:157)
                {
                  diemB=diemB+diemGKfixto[i]
                }
                if (diemA>=diemB){ToMAXGK<-fileGKfixto[1:78,1:29]
                                  ToMINGK<-fileGKfixto[79:157,1:29]} else
                {
                  ToMAXGK<-fileGKfixto[79:157,1:29]
                  ToMINGK<-fileGKfixto[1:78,1:29]
                }
                diemA=0
                diemB=0
                for (i in 1:79)
                {
                  diemA=diemA+diemCKfixto[i]
                }
                for (i in 80:163)
                {
                  diemB=diemB+diemCKfixto[i]
                }
                if (diemA>=diemB){ToMAXCK<-fileCKfixto[1:79,1:42]
                                  ToMINCK<-fileCKfixto[80:163,1:42]} else
                {
                  ToMAXCK <- fileCKfixto[80:163,1:42]
                  ToMINCK <- fileCKfixto[1:79,1:42]
                }
              #3.9
                DtbGK= round(mean(DiemGK),2)
                DtbCK= round(mean(DiemCK),2)
              #3.10  
                soSVtbGK=0
                soSVtbCK=0
                for (i in 1:157)
                {
                  if (round(DiemGK[i],2)==DtbGK){soSVtbGK=soSVtbGK+1}
                }
                for (i in 1:163)
                {
                  if (round(DiemCK[i],2)==DtbCK){soSVtbCK=soSVtbCK+1}
                }
              #3.11
                mdptanGK=sd(DiemGK)
                mdptanCK=sd(DiemCK)
              #3.12
                snessGK=skewness(DiemGK)
                snessCK=skewness(DiemCK)
                ktsisGK=kurtosis(DiemGK)
                ktsisCK=kurtosis(DiemCK)
              #3.13
                DiemGKmintomax <- DiemGKcheck[order(DiemGKcheck$DiemGK),]
                DiemCKmintomax <- DiemCKcheck[order(DiemCKcheck$DiemCK),]
                pvi1GK=median(DiemGKmintomax[1:78])
                pvi1CK=median(DiemCKmintomax[1:81])
                pvi3GK=median(DiemGKmintomax[80:157])
                pvi3CK=median(DiemCKmintomax[83:163])
              #3.14
                SV2mucGK=0
                SV2mucCK=0
                for (i in 1:157)
                {
                  if (DiemGK[i]>=DiemGKmintomax[156] && DiemGK[i]<=DiemGKmintomax[157]){SV2mucGK=SV2mucGK+1}
                }
                for (i in 1:163)
                {
                  if (DiemCK[i]>=DiemCKmintomax[162] && DiemCK[i]<=DiemCKmintomax[163]){SV2mucCK=SV2mucCK+1}
                }
              #3.15
                diemdau=158-SV2mucGK
                phloaiSV2mucGK = data.frame(DiemGKmintomax[diemdau:157])
                diemdau=164-SV2mucCK
                phloaiSV2mucCK = data.frame(DiemCKmintomax[diemdau:163])
                colnames(phloaiSV2mucGK)="Diemso"
                colnames(phloaiSV2mucCK)="Diemso"
                   #BieudoGK
                  ggplot(phloaiSV2mucGK, aes(x=Diemso)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 0.05) +
                  xlab("Di???m")+ ylab("S??? Sinh Viên") +
                  ggtitle("Bi???u d??? ph??? di???m c???a các sinh viên có di???m s??? ??? 2 m???c di???m cao nh???t GK")+
                  scale_fill_gradient("Di???m", low="red", high="green")
                   #BieudoCK
                  ggplot(phloaiSV2mucCK, aes(x=Diemso)) + 
                  geom_histogram(aes(fill=..x..), col = "white",binwidth = 0.05) +
                  xlab("Di???m")+ ylab("S??? Sinh Viên") +
                  ggtitle("Bi???u d??? ph??? di???m c???a các sinh viên có di???m s??? ??? 2 m???c di???m cao nh???t CK")+
                  scale_fill_gradient("Di???m", low="red", high="green")
              #3.16
                  k=scan()
                  diemdau=158-k
                  SVkmucGK=0
                  SVkmucCK=0
                  for (i in 1:157)
                  {
                    if (DiemGK[i]>=DiemGKmintomax[diemdau] && DiemGK[i]<=DiemGKmintomax[157]){SVkmucGK=SVkmucGK+1}
                  }
                  diemdau=164-k
                  for (i in 1:163)
                  {
                    if (DiemCK[i]>=DiemCKmintomax[diemdau] && DiemCK[i]<=DiemCKmintomax[163]) {SVkmucCK=SVkmucCK+1}
                  }
              #3.17
                diemdau=158-SVkmucGK
                phloaiSVkmucGK = data.frame(DiemGKmintomax[diemdau:157])
                diemdau=164-SVkmucCK
                phloaiSVkmucCK = data.frame(DiemCKmintomax[diemdau:163])
                colnames(phloaiSVkmucGK)="Diemso"
                colnames(phloaiSVkmucCK)="Diemso"
                  #BieudoGK
                    ggplot(phloaiSVkmucGK, aes(x=Diemso)) + 
                    geom_histogram(aes(fill=..x..), col = "white",binwidth = 0.05) +
                    xlab("Di???m")+ ylab("S??? Sinh Viên") +
                    ggtitle("Bi???u d??? ph??? di???m c???a các sinh viên có di???m s??? v???i k m???c di???m cao nh???t GK")+
                    scale_fill_gradient("Di???m", low="red", high="green")
                  #BieudoCK
                    ggplot(phloaiSVkmucCK, aes(x=Diemso)) + 
                    geom_histogram(aes(fill=..x..), col = "white",binwidth = 0.05) +
                    xlab("Di???m")+ ylab("S??? Sinh Viên") +
                    ggtitle("Bi???u d??? ph??? di???m c???a các sinh viên có di???m s??? v???i k m???c di???m cao nh???t CK")+
                    scale_fill_gradient("Di???m", low="red", high="green")  
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
                        
                      
                  
                
        
                
      
             
            
          
          
        
              
      
