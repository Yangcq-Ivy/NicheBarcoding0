data(en.vir)
library(ape)
data(LappetMoths)
ref.seq<-LappetMoths$ref.seq

fas=ref.seq
dna.model="raw"
ecol.dist.method="euclidean"
mantel.method="spearman"
permutations=999

niche.stand<-function(x){
  (x-env.range["min",])/(env.range["max",]-env.range["min",])
}
env.range<-rbind(en.vir@data@min,en.vir@data@max)
rownames(env.range)<-c("min","max");
colnames(env.range)<-names(en.vir)
env.range
infor<-extractSpeInfo(labels(fas)) #;head(infor)
unique<-unique(infor$species)
nspe<-length(unique)
if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
  cat("Environmental layers downloading ... ")
  #envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10,lon=lon,lat=lat)
  envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
  en.vir<-raster::brick(envir)
  cat("Done!\n")
}
result<-list()
genet.matrix<-matrix(nrow=nspe,ncol=nspe)
ecol.matrix<-matrix(nrow=nspe,ncol=nspe)
for (spe1 in 1:(nspe-1)){
  #cat (paste(">>> ",spe1,"/",nspe," ",as.character(unique[spe1]),"\n",sep=""))
  for (spe2 in (spe1+1):nspe){
    sp1.fas<-fas[infor[,3] %in% unique[spe1],]
    sp1.infor<-extractSpeInfo(labels(sp1.fas))
    sp1.vari<-raster::extract(en.vir,sp1.infor[,4:5])
    if (nrow(sp1.vari) == 1){
      sp1.vari<-apply(sp1.vari,FUN=as.numeric,MARGIN=2)
    }else{
      sp1.vari<-apply(apply(sp1.vari,FUN=as.numeric,MARGIN=2),FUN=mean,MARGIN=2)
    }
    sp1.vari<-niche.stand(sp1.vari)
    sp2.fas<-fas[infor[,3] %in% unique[spe2],]
    sp2.infor<-extractSpeInfo(labels(sp2.fas))
    sp2.vari<-raster::extract(en.vir,sp2.infor[,4:5])
    if (nrow(sp2.vari) == 1){
      sp2.vari<-apply(sp2.vari,FUN=as.numeric,MARGIN=2)
    }else{
      sp2.vari<-apply(apply(sp2.vari,FUN=as.numeric,MARGIN=2),FUN=mean,MARGIN=2)
    }
    sp2.vari<-niche.stand(sp2.vari)
    sel.fas<-rbind(sp1.fas,sp2.fas)
    genet.dist<-as.matrix(ape::dist.dna(sel.fas,dna.model))
    genet.dist12<-genet.dist[1:nrow(sp1.fas),(nrow(sp1.fas)+1):(nrow(sp1.fas)+nrow(sp2.fas))]
    if (length(which(is.nan(genet.dist12))) != 0){
      warning ("Sequences of ",as.character(unique[spe1])," and ",as.character(unique[spe2]),
               " are very different, so that the model of dist.dna have been changed to \"raw\".\n")
      genet.dist<-as.matrix(ape::dist.dna(sel.fas,"raw"))
      genet.dist12<-genet.dist[1:nrow(sp1.fas),(nrow(sp1.fas)+1):(nrow(sp1.fas)+nrow(sp2.fas))]
    }
    genet.matrix[spe2,spe1]<-mean(genet.dist12)
    sel.envir<-rbind(sp1.vari,sp2.vari)
    ecol.dist<-as.matrix(stats::dist(sel.envir,method=ecol.dist.method))
    ecol.matrix[spe2,spe1]<-ecol.dist[2,1]
  }
}
row.names(genet.matrix)<-as.character(unique)
colnames(genet.matrix)<-as.character(unique)
row.names(ecol.matrix)<-as.character(unique)
colnames(ecol.matrix)<-as.character(unique)
test<-vegan::mantel(stats::as.dist(genet.matrix),stats::as.dist(ecol.matrix),
                    mantel.method,permutations);test
result$MantelStat.r<-test$statistic
result$p.value<-test$signif
result$genet.matrix<-genet.matrix
result$ecol.matrix<-ecol.matrix


# Test Start ###
test_that("class testing of the input",{
  expect_equal(class(ref.seq),"DNAbin")
})

test_that("check whether the matrices are matched",{
  expect_equal(nrow(genet.matrix),ncol(genet.matrix))
  expect_equal(nrow(ecol.matrix),ncol(ecol.matrix))
  expect_equal(nrow(genet.matrix),nspe)
  expect_equal(ncol(genet.matrix),nspe)
  expect_equal(dim(genet.matrix),dim(ecol.matrix))
})
