#1a

queue <- c("James", "Mary", "Steve", "Alex", "Patricia")
queue

#1b
queue <- append(queue, "Harold")
queue

#1c
queue <- queue[-1]
queue

#1d

insert <- function(v,e,pos){
  
  return(c(v[1:(pos-1)],e,v[(pos):length(v)])) }

queue <- insert(queue, "Pam", 2)
queue

#1e
queue <- queue[-length(queue)]
queue

#1f
queue <- queue[!queue %in% c("Alex")]
queue

#1g
index <- match(c("Patricia"), queue)
index

#1h
qLength <- length(queue)
qLength

#2
getroots <- function(a,b,c) {

  if(a > 0)
  {
    disc = b^2 - 4*a*c
    
    if(disc < 0)
    {
      result <- c("No Real Roots")
    }
    else if(disc == 0)
    {
      r1 = -b/(2*a)
      result <- c(r1)
    }
    else
    {
      r1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
      r2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)
      
      result <- c(r1, r2)
    }
  }
  else
  {
    result <- c("Not a Quadratic")
  }
  
  return <- result
}

roots <- getroots(1,-2,-4)
roots

#3
v1 <- 1:1000

vDiv <- (v1 %% 3 == 0) | (v1 %% 7 == 0)|(v1 %% 11 == 0)
vDiv <- vDiv[vDiv %in% c(TRUE)]
count <- length(vDiv)
count

#4
ispolytriple <- function(a,b,c) {
  result <- FALSE
  ordered <- sort(c(a,b,c), TRUE)
  
  if(ordered[1] ^ 2 == (ordered[2] ^ 2 + ordered[3]^2))
  {
    result <- TRUE
  }
  
  return <- result
}

triple <- ispolytriple(3,4,5)
triple







