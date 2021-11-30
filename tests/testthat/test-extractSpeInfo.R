data(LappetMoths)
ref.seq<-LappetMoths$ref.seq
seqID.full<-rownames(ref.seq)

pattern.ref<-"^[-\\.[:alnum:][:space:]_]+,[-\\.[:alnum:][:digit:][:space:]_]+,[-\\.[:digit:][:space:]]+$"
pattern.que<-"^[-\\.[:alnum:][:space:]_]+,[-\\.[:digit:][:space:]]+$"
seqID.full.as.list<-as.list(seqID.full)
seqID.ok.ref<-sapply(seqID.full.as.list,grepl,pattern=pattern.ref)
seqID.ok.que<-sapply(seqID.full.as.list,grepl,pattern=pattern.que)
if(!(all(seqID.ok.ref)==TRUE)&&!(all(seqID.ok.que)==TRUE)){
  cat("bad format:",seqID.full[which(seqID.ok.ref==FALSE)],"\n")
  #stop ("The input data must BE like \n (ref):>ABLRP543-09,Agamidae_Phrynocephalus_helioscopus,61.367 46.228 !\n or (que):>ABLRP543-09,61.367 46.228 !\n")
}else{### format is ok!
  if(all(seqID.ok.ref)==TRUE){
    SppDistrRef<-gsub("^[-\\.[:alnum:]]+,","",seqID.full)
    mpattern<-",[-\\.[:alnum:][:space:]_]+$" ##
    seqIDs<-gsub(mpattern,"",seqID.full)
    seqIDs<-gsub(mpattern,"", seqIDs)
    strTmp<-strsplit(SppDistrRef,",")
    strTmp2<-unlist(strTmp)
    strTmp3<-as.data.frame(strTmp2)
    id.index<-seq(1,dim(strTmp3)[1],by=2)
    coor.index<-seq(2,dim(strTmp3)[1],by=2)
    spe<-strTmp3$strTmp2[id.index]
    spe<-as.vector(spe)
    longLat<-strTmp3$strTmp2[coor.index]
    longLat<-as.vector(longLat)
    longLat2<-unlist(strsplit(longLat," "))
    long<-as.numeric(longLat2[id.index])
    lat<-as.numeric(longLat2[coor.index])
    SppDistrRef<-data.frame(no.= c(1:length(spe)),
                            seqIDs =  seqIDs,
                            species = spe,
                            Lon = long,
                            Lat = lat
    )

  }else{### for query:
    SppDistrQue<-gsub("^[-\\.[:alnum:]]+,","",seqID.full)
    mpattern<-",[-\\.[:alnum:][:space:]_]+$" ##
    seqIDs<-gsub(mpattern,"",seqID.full)
    id.index<-seq(1,length(SppDistrQue)*2,by=2)
    coor.index<-seq(2,length(SppDistrQue)*2,by=2)
    longLat<-SppDistrQue
    longLat<-as.vector(longLat)
    longLat2<-unlist(strsplit(longLat," "))
    long<-as.numeric(longLat2[id.index])
    lat<-as.numeric(longLat2[coor.index])
    SppDistrQue<-data.frame(no.= c(1:length(seqIDs)),
                            seqIDs =  seqIDs,
                            species = "unknown",
                            Lon = long,
                            Lat = lat
    )
  }
}


# Test Start ###
test_that("class testing on information",{
  expect_equal(class(seqIDs),"character")
  expect_equal(class(long),"numeric")
  expect_equal(class(lat),"numeric")
})

test_that("length testing on long and lat",{
  expect_equal(length(long),length(lat))
})
