# Homework 1
# PSCI 7381
# Prof. Dr. Santoro
# Student: Federico Ferrero

# loading data and libraries
library(statnet)
library(UserNetR)
data("FIFA_Nether")

?FIFA_Nether

# simple visualization
plot(FIFA_Nether, displaylabels=TRUE)

# network size
network.size(FIFA_Nether)
summary(FIFA_Nether,print.adj=TRUE)

# density
gden(FIFA_Nether)
# density calculated by hand in a directed network
den_hand <- 108/(11*10)
den_hand

# number of components (subgroup in which all actors are conected, directely or indirectely)
help(components)
components(FIFA_Nether)

# diameter
gd <- geodist(FIFA_Nether)
max(gd$gdist)

# clustering by transitivity
gtrans(FIFA_Nether,mode="graph")
gtrans(FIFA_Nether,mode="digraph")

# degree centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
degree(FIFA_Nether) # gmode is set to "digraph" by default.

# closeness centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
closeness(FIFA_Nether) # gmode is set to "digraph" by default.

# betweenness centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
betweenness(FIFA_Nether) # gmode is set to "digraph" by default.

#Homework  MORENO DATASET (pag 11)
install.packages('statnet')
library(statnet)
library(UserNetR)
data(Moreno)

#simple visualization
gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

#network size
network.size(Moreno)
summary(Moreno,print.adj=FALSE)

#density
#calculated by hand
den_hand <- 2*46/(33*32)
den_hand
# calculated with function
gden(Moreno)

# number of components (subgroup in which all actors are conected, directely or indirectely)
components(Moreno)

# diameter
lgc <- component.largest(Moreno,result="graph")
gd <- geodist(lgc)
max(gd$gdist)

# clustering by transitivity
gtrans(Moreno,mode="graph")

# centrality measures
degree(net, gmode="digraph") #gmode is set to "digraph" by default.

## Chapter 7: Actor prominence
## Degree centrality
library(statnet)
net <- network(net_mat)
net %v% 'vertex.names' #me da los nombres de la net
degree(net, gmode="graph")

# centrality measures
data(DHHS)
df.prom <- data.frame(
  deg = degree(DHHS),
  cls = closeness(DHHS),
  btw = betweenness(DHHS),
  evc = evcent(DHHS),
  inf = infocent(DHHS),
  flb = flowbet(DHHS)
)

# working with Bali Data
library(igraph)
?Bali
data(Bali)
str(degree(Bali))
summary(degree(Bali))
my_pal <- brewer.pal(5,"Set2")
rolecat <- Bali %v% "role"
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0,edge.col="grey25")
legend("topright",legend=c("BM","CT","OA","SB",
                           "TL"),col=my_pal,pch=19,pt.cex=2)
