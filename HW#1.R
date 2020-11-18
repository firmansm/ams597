# AMS597 2019: Homework #1.

####1. Consider the following weights 60, 72, 34, 56, 87, 80, 89, 95, 76, 28, 48, 59. Use the R script to finish the following questions.####
#(a) Assign all these weights as vector 'weight'.
####
weight <- c(60, 72, 34, 56, 87, 80, 89, 95, 76, 28, 48, 59)

#(b) Compute the mean of 'weight' and of the square of the weight.
mean_weight <- mean(weight)
square_weight <- weight^2

#(c) What is the length of the weight?
length(weight)

#(d) How many weights are larger than 55?
length(weight[weight>50])

#(e) Show if each weight is larger than 55 and smaller than 85.
weight_55_85 <- 55 < weight & weight < 85


####2. Using the following script, we can generate a 3×4 matrix####
tmp <- matrix(rnorm(12), 3, 4)
#Answer the following questions.
tmp

#(a) Compute the sum of the first and third column.
colSums(tmp[,c(1,3)])

#(b) Compute the product of the first and third row.
prod_1n3 <- tmp[1,]*tmp[3,]

#(c) Show the dimension of the matrix.
dim(tmp)

#(d) Use 'cat' function to output elements in the first row that are larger than 0.5
cat(tmp[1,])
cat(tmp[1,][tmp[1,]>0.5],'\n')


####3. How would you check whether two vectors are the same if they may contain missing (NA) values? (Use of the identical function is considered cheating!)####
podo <- function(a,b) {
  if(length(a) == length(b) &&
    all(is.na(a) == is.na(b)) &&
    all(a[!is.na(a)] == b[!is.na(b)])){
    print("Two vectors are the same")}
  else {
    print("Two Vectors are not the same")}
}

#checking the function
vecW <- c(4,5,7)
vecX <- c(4,5,7)
vecY <- c(4,5,7,9)
vecZ <- c(4,5,7,NA)
vecQ <- c(4,5,NA,NA)

podo(vecW,vecX)
podo(vecY,vecZ)
podo(vecZ,vecZ)
podo(vecZ,vecQ)
podo(vecQ,vecQ)


####4. If x is a factor with n levels and y is a length n vector, what happens if you compute y[x]?####
x <- factor(c("low", "medium", "high"))
y <- c("rambutan", "durian", "nanas")
y[x]

# It outputs the original of y vector but with different order. It also shows that y[x] implies y[x]=c(y[i]É) and y[i] stands for the ith value in y corresponding to the ith level in x.



#5. Using the following script, we will generate a toy DNA sequence
mydna <- paste(sample(c('a','t','c','g'), 1000, replace=T), collapse='')

#Write a function that takes a string as a input, counts the number of "cg" in the input string, and replace all "cg" with "XY". Apply your function to mydna. (Do not use any special R packages.)

pattern <- "cg"
newpattern <- "XY"

hitunggantipola <- function(sandi, pola, ganti){
  telik=gregexpr(pola, sandi, perl=T)
  print(length(telik[[1]]))
  print(gsub(pola, ganti, sandi, perl=T))
}
#sandi = string, pola = pattern to look for in the string, ganti = pattern to replace the pola

hitunggantipola(mydna, pattern, newpattern)



####6. Write a function which reads in the file from http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt and output rows containing valid phone numbers from the file. Valid phone numbers take one of the following form:####
# | ###-###-####
# | (###) ###-####
# | ### ### ####
# | ###.###.####
#  where # is a digit between 0-9

nomortelepon <- function(url){
  filenya <- read.csv(url, header=F)
  coba <- regmatches(t(filenya),gregexpr("\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}", t(filenya), perl=T))
  coba1 <- paste(coba)
  coba2 <- coba[coba!="character(0)"]
  paste(coba2)
}

nomortelepon("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt")


#orsubsetPhone <- function(dat){
p1 <- "[0-9]{3}[:.:]|-|[[:space:]][0-9]{3}[:.:]|-|[[:space:]][0-9]{4}"
p2 <- "([0-9]{3})[[:space:]][0-9]{3}-[0-9]{4}"

myfunc <- function(x){
  tmp1 <- grepl(p1,x)
  tmp2 <- grepl(p2,x)
  return(max(tmp1,tmp2))}

subsetPhone(mydat)



