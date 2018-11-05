
#train<-function()
{# training and testing
  #input
  table<-read.csv(file.choose())
  gen<-data.frame(table)
  na.omit(gen)
  set.seed(123)
  ind<-sample(2,nrow(gen),replace = TRUE,prob = c(0.7,0.3))
  train<-gen[ind==1,]
  test<-gen[ind==2,]
  set.seed(123)# set seed for reproducible model
  rd<-randomForest(gen$Gender~.,data=gen[, !(colnames(gen) %in% c("Emotion"))],mtry=4)#gender model training using random forest(does not contain emotion as a factor)
  i=0
  male<-NULL
  female<-NULL
  for(i in c(1:1117)){#seperates male and female values for later classification based on previous trained randomforest model
    
    test1<-gen[i,]#[, !(colnames(train) %in% c("Emotion"))]
    set.seed(123)
    pd1<-predict(rd,test1)
    
    if(pd1=="Male")
    {
      male<- rbind(male,select(test1,-Gender))#male values separeted here
    }
    
    else
    {
      female<-rbind(female,select(test1,-Gender))#female values in here
    }
    
    i=i+1
  }
  set.seed(123)
  indm<-sample(2,nrow(male),replace = TRUE,prob = c(0.7,0.3))#70,30 train test split
  trainm<-male[indm==1,]
  testm<-male[indm==2,]
  trainm$Emotion<-as.factor(trainm$Emotion)
  #gbm model for emotion classification for males
  set.seed(123)
  gb<-gbm(formula = trainm$Emotion~.,data = trainm,distribution = "multinomial",shrinkage = 0.01,interaction.depth = 1,n.trees = 100,keep.data = TRUE)
  tp<-NULL
  i=0
  #colnames(tp)<-c("anger","fear","happy","neutral","sad")
  for (i in c(1:159)){
    tp<-rbind(tp,predict.gbm( gb,testm[i,],n.trees = 100) )
  }
  colnames(tp)<-c("anger","fear","happy","neutral","sad")
  i=0
  tp1<-NULL
  tp1<-data.frame(colnames(tp)[apply(tp,1,which.max)])#segregation of male emotion
  confusionMatrix(colnames(x = tp1)[apply(tp1, 1, which.max)],data = testm$Emotion,reference = testm$Emotion)
  #colnames(tp)[apply(tp,1,which.max)]
  
  set.seed(123)
  indf<-sample(2,nrow(female),replace=TRUE,prob=c(0.7,0.3))
  trainf<-female[indf==1,]
  testf<-female[indf==2,]
  trainf$Emotion<-as.factor(trainf$Emotion)
  # model for female emotions
  set.seed(123)
  gbf<-gbm(formula = trainf$Emotion~.,data = trainf,distribution = "multinomial",shrinkage = 0.01,interaction.depth = 1,n.trees = 100,keep.data = TRUE)
  i=0
  tpf<-NULL
  for (i in c(1:159)){
    tpf<-rbind(tpf,predict.gbm( gbf,testf[i,],n.trees = 100) )
  }
  colnames(tpf)<-c("anger","fear","happy","neutral","sad")
  i=0
  tpf1<-NULL
  #for(i in c(1:159)){
   # tpf1<-rbind(tpf[i,],colnames(x=tpf[i,])[apply(tpf, 1, which.max)])}
  tpf1<-data.frame(colnames(tpf)[apply(tpf,1,which.max)])#segregation of female emotion
  #confusionMatrix(colnames(x = tpf1)[apply(tpf1, 1, which.max)],data = testf$Emotion,reference = testf$Emotion)
  #colnames(tpf1)[apply(tpf1,1,which.max)]
}


{# actual works!!
  testboii<-read.csv(file.choose())
  i=0
  male1<-NULL
  female1<-NULL
  for(i in c(1:46)){
    
    test1<-testboii[i,]#[, !(colnames(train) %in% c("Emotion"))]
    set.seed(123)
    pd1<-predict(rd,test1)
    
    if(pd1=="Male")
    {
      male1<- rbind(male1,test1)#male values segregated
    }
    
    else
    {
      female1<-rbind(female1,test1)#female values segregated
    }
    
    i=i+1
  }
  tppm<-NULL
  i=0
  #colnames(tp)<-c("anger","fear","happy","neutral","sad")
  for (i in c(1:46)){
    tppm<-rbind(tppm,predict.gbm( gb,male1[i,],n.trees = 100) )#male emotion model
  }
  colnames(tppm)<-c("anger","fear","happy","neutral","sad")
  i=0
  tppm1<-NULL
  #for(i in c(1:46)){
   # tppm1<-rbind(tppm[i,], colnames(tppm[i,])[apply(tppm,1,which.max)])}
  tppm1<-data.frame(colnames(tppm)[apply(tppm,1,which.max)])#segregating male emotion prediction
  #tppm 
  
  tppf<-NULL
  i=0
  #colnames(tp)<-c("anger","fear","happy","neutral","sad")
  for (i in c(1:46)){
    tppf<-rbind(tppf,predict.gbm( gbf,female1[i,],n.trees = 100) )
  }
  colnames(tppf)<-c("anger","fear","happy","neutral","sad")
  i=0
  tppf1<-NULL
  tppf1<-data.frame(colnames(tppf)[apply(tppf,1,which.max)])#segregation female emotion prediction
  tppf1
}

