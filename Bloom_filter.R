library(digest)

n=10
m=100
nh=4
l = rep(0,m)

words_present=c('abound.com','abounds.in','abundance.com','abundant.org','accessable.co.in',
                'bloom.edu','blossom.com','bolster.com','bonny.org','bonus.in', 'war.com')

print(words_present)

words_absent=c('bluff.com','cheater.com','hate.com','war.in','humanity.org',
               'racism.edu','hurt.org','nuke.in','gloomy.com','facebook.co.in')

print(words_absent)

get_hash = function(item,seed){
  hex_str=digest(object=item,
                 algo="murmur32",
                 serialize=F,
                 seed=seed)
  hex=paste('0x',hex_str,sep="")
  return(as.numeric(hex) %% m)
}

add = function(item){
  for (i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    l[hash_digest]<<-1
  }
}

check = function(item){
  for(i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    if(l[hash_digest] == 0)
    {
      return(FALSE)
    }
  }
  return(TRUE)
  
}

for(i in 1:n){
  add(words_present[i])
}

print(l)
test_set=c(words_present[1:5],words_absent)
print(test_set)

for(i in 1:length(test_set)){
  if(check(test_set[i]))
  {
    if(test_set[i] %in% words_absent)
    {
      cat(test_set[i]," this is  false positive ","\n")
    }
    else
    {
      cat(test_set[i]," this is probably present ","\n")
    }
  }
  else
  {
    cat(test_set[i]," this is not present !","\n")
  }
}