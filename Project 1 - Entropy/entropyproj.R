library(data.table)

#### Q1 ####
entropy <- function(input) {
  input <- factor(input)
  buckets <- tabulate(input)
  p <- buckets / length(input)
  
  ents <- p*log2(p)
  ents <- replace(ents, is.nan(ents), 0)
  totalent = -1 * sum(ents)

  return <- totalent
}

#### Q2 ####
infogain <- function(input, atr) {
  
  ed <- entropy(input)
  
  df <- data.frame(input,atr)
  
  atrfact <- factor(sort(atr))
  partitions <- tabulate(atrfact)
  m <- sort(unique(atr))

  splitents <- NULL
  for(i in 1:length(m))
  {
    sub <- subset(input, atr == m[i])
    splitents[i] <- partitions[i]/length(input)*entropy(sub) 
  }

  return <- ed - sum(splitents)
}


#### Entry Point ####
dataset = read.csv("entropy-test-file.csv")
input <- dataset$answer

ent <- entropy(input)
ent

input <- dataset
infog <- infogain(input$answer, dataset$attr1)
infog <- infogain(input$answer, dataset$attr2)
infog <- infogain(input$answer, dataset$attr3)
infog


















