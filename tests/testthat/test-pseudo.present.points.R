data(en.vir)
data<-data.frame(species=rep("Acosmeryx anceus",3),
                 Lon=c(145.380,145.270,135.461),
                 Lat=c(-16.4800,-5.2500,-16.0810))

outputNum=10
lonRange=2
latRange=1
map=TRUE

if (!is.data.frame(data)|dim(data)[2]!=3){
  stop ("The input data must be a dataframe with three columns
          (species name, lon, lat)!\n")
}else{
  lon=data[,2];lat=data[,3]

  if (nrow(data)>=outputNum){
    #stop ("You've got enough data!\n")

  }else{
    #Give a square range for simulated coordinates
    if (nrow(unique(data))==1){
      minLon=lon-lonRange/2;maxLon=lon+lonRange/2
      minLat=lat-latRange/2;maxLat=lat+latRange/2
    }else{
      minLon=min(lon);maxLon=max(lon)
      minLat=min(lat);maxLat=max(lat)
    }
    ifelse (minLon < (-180),-180,minLon)
    ifelse (maxLon > 180,180,maxLon)
    ifelse (minLat < (-90),-90,minLat)
    ifelse (maxLat > 90,90,maxLat)

    #Define empty matrices for storing data
    location<-matrix(NA,1,2)
    landPoint<-matrix()
    count<-0
    simNum=outputNum-nrow(data)

    if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
      cat("Environmental layers downloading ... ")
      envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
      en.vir<-raster::brick(envir)
      cat("Done!\n")
    }

    while (count<simNum|nrow(landPoint)==0){
      simLon<-stats::runif(floor(2*simNum),minLon,maxLon)
      simLat<-stats::runif(floor(2*simNum),minLat,maxLat)
      simLL<-cbind(simLon,simLat)

      eFactors<-raster::extract(en.vir,simLL);eFactors[,1]
      if (all(is.na(eFactors[,1]))==TRUE){
        return (data)
        warning ("The location of ",data[,1]," must be re-confirmed!\n")
        break
      }else{
        landPoint<-matrix(simLL[which(is.na(eFactors[,1])==FALSE),],ncol=2);
        landPoint
      }

      count<-count+nrow(landPoint);count
      location<-rbind(location,landPoint);location
    }

    #Sampling from redundant points
    simFrame<-data.frame("Simulation",location)
    n<-sample(count,simNum);n
    finalFrame<-simFrame[n+1,]

    #final format
    names(finalFrame)<-c("species","Lon","Lat")
    rownames(finalFrame)<-NULL
    colnames(data)<-c("species","Lon","Lat")
    output<-rbind(data,finalFrame)

    if (map == T){
      maps::map("world",xlim=c(min(output$Lon)-40,max(output$Lon)+40),
                ylim=c(min(output$Lat)-40,max(output$Lat)+40))
      graphics::points(data$Lon,data$Lat,col="blue",pch=19)
      graphics::points(output$Lon,output$Lat,col="blue",pch=1)
      graphics::legend(x="bottom",inset=-0.12,
                       col=c("blue","blue"),
                       pch=c(19,1),
                       legend=c("Present","Simulated"),
                       horiz=T,
                       xpd=T,
                       bty="n")
    }
  }
}


# Test Start ###
test_that("check whether the input data is not null",{
  expect_false(is.null(data))
})

test_that("check whether the coordinates are matched",{
  expect_equal(colnames(lon),colnames(lat))
})

test_that("check whether the boundary coordinates are out of range",{
  expect_gt(minLon,-180)
  expect_lt(maxLon,180)
  expect_gt(minLat,-90)
  expect_lt(maxLat,90)
})

test_that("check whether the simulation recordes are outputted as required",{
  expect_equal(nrow(output),outputNum)
})
