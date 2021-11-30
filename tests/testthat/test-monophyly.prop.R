library(ape)
tree<-ape::rtree(20)
tree$tip.label<-sample(tree$tip.label[1:10],size=20,replace = TRUE)
sppVector<-tree$tip.label

phy<-tree
singletonsMono=TRUE

mono<-spider::monophyly(phy, sppVector, pp = NA, singletonsMono = TRUE)
spp.unique<-unique(sppVector)
mono.list<-spp.unique[mono]
mono[mono==TRUE]<-1
mono[mono==FALSE]<-0
no.mono<-sum(mono)
non.mono.list<-spp.unique[!mono]
non.mono.list
mono.prop<-no.mono/length(mono)
OUT<-list(mono.prop=mono.prop,
          no.mono=no.mono,
          mono.list=mono.list,
          non.mono.list=non.mono.list)


# Test Start ###
test_that("class testing on input",{
  expect_equal(class(tree),"phylo")
})

test_that("length testing on phy$tip.label and sppVector",{
  expect_equal(length(phy$tip.label),length(sppVector))
})

test_that("class testing on output",{
  expect_equal(class(OUT),"list")
})
