# Social Network Data

# 1.1  Reading the data
  edges = read.csv("edges.csv")
  str(edges)
  users = read.csv("users.csv")
  str(users)
  all = c(edges$V1,edges$V2)
  allDF = as.data.frame(table(all))
  allDF
  
  # Total of friends relationships
  sum(allDF$Freq)
  
  # Total of friends relationships
  nrow(allDF$Freq)
  

  
# 1.2 Most common school
  table(users$locale, users$school)

# 1.3
  table(users$gender, users$school)
  
  
# 2.1 installing igraph
  install.packages("igraph")
  library(igraph)
  g = graph.data.frame(edges, FALSE, users)
  ?graph.data.frame

# 2.2 Ploting
  plot(g, vertex.size=5, vertex.label=NA)

  
# 2.3 computing the degreee
  sort(degree(g))
  frs = degree(g)
  frs
mean(frs)

  
# 2.4 refining
  V(g)$size = degree(g)/2+2
  plot(g, vertex.label=NA)
  max(V(g)$size)
  min(V(g)$size)
  
  
# 3.1 Coloring to better view
  V(g)$color = "black"
  V(g)$color[V(g)$gender == "A"] = "red"
  V(g)$color[V(g)$gender == "B"] = "gray"
  
  plot(g, vertex.label=NA)
  
  
# 3.2 Coloring based on the school
  V(g)$color = "green"
  V(g)$color[V(g)$school == "A"] = "red"
  V(g)$color[V(g)$school == "AB"] = "gray"
  plot(g, vertex.label=NA)
  
  table(  V(g)$school)
  
  
# 3.3 Coloring based on the locale
  V(g)$color = "black"
  V(g)$color[V(g)$locale == "A"] = "red"
  V(g)$color[V(g)$locale == "B"] = "green"
  plot(g, vertex.label=NA)
  
  table(  V(g)$locale)
  
  
# 4 - HELP
  ?igraph.plotting
