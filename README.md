# NicheBarcoding
# This is a quick guide to getting started with the two main functions.
# Complete examples can also be found in the help documentation of each functions.
# Users can also read the manual to learn more.


rm(list=ls())
library(NicheBarcoding)

### load the example bioclimatic layers for the subsequent processes ####
data(en.vir)
data(bak.vir)
### or if you want to download the complete bioclimatic layers ####
#envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
#en.vir<-raster::brick(envir)
#back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,
#                          excludep=TRUE,prob=FALSE,
#                          cellnumbers=FALSE,tryf=3,warn=2,
#                          lonlatCorrection=TRUE)
#bak.vir<-raster::extract(en.vir,back)

#################################################################
### Scenario 0
### NBSI  DNA barcodes + coordinates of species distribution 
###       available (using online climate data)
#################################################################

library(ape)
data(LappetMoths)
ref.seq<-LappetMoths$ref.seq
que.seq<-LappetMoths$que.seq
NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
               independence=TRUE,
               model="RF",variables="ALL",
               en.vir=en.vir,bak.vir=bak.vir)
NBSI.out

### when you have an additional reference coordinates information, run: ####
ref.add<-LappetMoths$ref.add
NBSI.out2<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                independence=TRUE,
                model="RF",variables="SELECT",
                en.vir=en.vir,bak.vir=bak.vir)
NBSI.out2


#################################################################
### Scenario 1 
### NBSI2   species identified by other methods or barcodes + 
###         coordinates of species distribution available 
###         (for using online climatic data)
#################################################################

data(LappetMoths)
barcode.identi.result<-LappetMoths$barcode.identi.result
ref.infor<-LappetMoths$ref.infor
que.infor<-LappetMoths$que.infor

NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                 barcode.identi.result=barcode.identi.result,
                 model="RF",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)
NBSI2.out


#################################################################
### Scenario 2
### NBSI2   species identified by other methods or barcodes + 
###         users possessing their own environmental data
#################################################################

data(LappetMoths)
barcode.identi.result<-LappetMoths$barcode.identi.result
ref.env<-LappetMoths$ref.env
que.env<-LappetMoths$que.env

NBSI2.out2<-NBSI2(ref.env=ref.env,que.env=que.env,
                  barcode.identi.result=barcode.identi.result,
                  model="RF",variables="ALL",
                  en.vir=en.vir,bak.vir=bak.vir)
NBSI2.out2

