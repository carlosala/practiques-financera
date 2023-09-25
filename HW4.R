#Hw4
#Júlia Albero, Sergi Cucala, Marc Luque, carlo Sala
# exercise 1

# a. Building a tree

build_stock_tree = function(S,u,v,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j] = S*u**(j-1)*v**(i-j)
    }
  }
  return(tree)
}

build_stock_tree(S=10,u=1.1,v=0.9,N=2)

# b. Risk neutral probability

# 1 + r*dt = q*u + (1-q)*v
# 1 + r*dt - v = q*(u-v)

q_prob=function(r,u,v,dt){(1-v+r*dt)/(u-v)}

q_prob(0.1,1.1,0.9,1/256)

# c. Recursive valuation

call<-function(K,s){(s-K)*(s-K>0)}
put<-function(K,s){(K-s)*(K-s>0)}

value_binomial_option = function(tree,u,v,r,dt,K,type){
  q = q_prob(r,u,v,dt)
  
  option_tree = matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  if(type=="call"){
    for(i in 1:nrow(tree)){
      option_tree[nrow(tree),i] = call(K,tree[nrow(tree),i])
    }
  }
  
  if(type=="put"){
    for(i in 1:nrow(tree)){
      option_tree[nrow(tree),i] = put(K,tree[nrow(tree),i])
    }
  }
  
  for(i in (nrow(tree)-1):1){
    for(j in i:1){
      option_tree[i,j] = (option_tree[i+1,j+1]*q+option_tree[i+1,j]*(1-q))/(1+r*dt)
    }
  }
  
  return(option_tree)
}

S=10
N=5
u=1.01
v=0.99
r=0.1
dt=1/256
K=10
tree2 = build_stock_tree(S,u,v,N)
value_binomial_option(tree2,u,v,r,dt,K,type="call")

# d. Final price

binomial_option=function(type,u,v,dt,r,K,S,N){
  tree=build_stock_tree(S,u,v,N)
  option_tree=value_binomial_option(tree,u,v,r,dt,K,type)
  price=option_tree[1,1]
  return(price)
}

binomial_option(type="call",u,v,dt,r,K,S,N)

# exercise 2
binomial_option(type="call",u=1.01,v=0.99,dt=1/12,r=0.1,K=100,S=100,N=12)