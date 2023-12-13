source("read.input.R")

options(digits=22)
input <- readLines("Day 12/input.txt")
s <- strsplit(input," ")
onsen <- strsplit(sapply(s,\(x)x[1]),"")
arr <- lapply(strsplit(sapply(s,\(x)x[2]),","),as.integer)
res <- 0
check <- function(x,arr){
  r <- rle(x)
  identical(r$l[r$v=="#"],arr)
}
for(i in 1:1000){
  o <- onsen[[i]]
  s <- sum(o=="?")
  O <- matrix(o,nrow=2^s,ncol=length(o),byrow=T)
  S <- 2^((s:1)-1)
  q <- which(o=="?")
  for(j in seq_along(q)){
    O[,q[j]] <- rep(c(".","#"),each=S[j])
  }
  valid <-sum(apply(O,1,check,arr[[i]]))
  #cat(i,"\r")
  print(c(i, valid))
  res <- res + valid
}
res
#7195   #7173