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
  
  ed <- entropy(input) #calculate entropy of entire set with function from Q1
  
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

#### Q3 ####
decide <- function(input, a, target) {
  
  maxInfoGain = -9999 #an artifically low value
  maxInfoGainIndex = 0
  gainV <- NULL
  colNames <- NULL
  for(i in 1:length(a))
  {
    dfindex = a[i]
    
    infog <- infogain(input[[4]], input[[dfindex]]) # Use infogain funciton from Q2
    gainV[dfindex] <- infog
    colNames[dfindex] <- colnames(input)[dfindex] 
    if(infog > maxInfoGain)
    {
      maxInfoGain <- infog
      maxInfoGainIndex <- i 
    }
  }
  
  names(gainV) <- colNames
  
  ret <- list(gains = gainV, max = maxInfoGainIndex)
  
  return <- ret
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

infog <- infogain(input$answer, dataset[[1]])
infog

result <- decide(dataset, c(1,2,3), 4)
result


















