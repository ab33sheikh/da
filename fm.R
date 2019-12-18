x=c(1,3,2,1,2,3,4,3,1,2,3,1)
h=function(var){
  return ((6*var+1)%%5)
}

r=c()
b=function(var1){
  l1=rev(intToBits(var1))
  l1=paste(as.integer(l1),collapse = "")
  return(l1)
}

lz=c()

leadzero=function(var2){
  var2=strsplit(var2,"")
  var2=rev(var2)
  for(i in length(var2[[1]]):1){
    if(var2[[1]][i]!="0"){
      print(var2[[1]][i])
      return(32-i)
    }
  }
  return(0)
}

hf=c()

for(i in 1:length(x)){
  temp=h(x[i])
  hf[i]=temp
}

hf

for(i in 1:length(hf)){
  temp=b(hf[i])
  r[i]=temp
}

r

for(i in 1:length(r)){
  temp=leadzero(r[i])
  lz[i]=temp
}

lz

max(lz)