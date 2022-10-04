setwd("C:/Users/surface/Documents/Rscripts/Groupwork18")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

# step 4
bible <- strsplit(a," ") # a vector of original bible text

# the split_punct function with the parameters of a word_vector and a punctuation vector
# to search for each word containing the punctuation mark, remove it from the word
# and add the mark as a new entry in the vector of words, after the word it came from
# note that "." is special punctuation, so we use if function to deal with it.
split_punct <- function(word_vector, punct_vector){
  for (punct in punct_vector){
    ii <- grep(punct, word_vector, fixed = TRUE)
    new_word <- rep("", length(ii) + length(word_vector))
    iis <- ii + 1:length(ii)
    new_word[iis] <- punct
    if (punct == "."){
      word_vector <- gsub("\\.","", word_vector)
    }else{
      word_vector <- gsub(punct, "", word_vector,fixed = TRUE)
    }
    new_word[-iis] <- word_vector
    word_vector <- new_word
  }
  word_vector
}

# step 5
bible_punct <- c(",",".", ";", "!", ":", "?") # a vector of punctuations in bible
a <- split_punct(bible, bible_punct) # bible text with splitted punctuations

# step 6
a <- tolower(a) # bible vector with lower case letters
b <- unique(a) # find the vector of unique words
index <- match(a,b) #find the vector of indicies indicating which element in the unique word vector each
#element in the (lower case) bible text corresponds to
tabulate(index)
freq_vector <- c(tabulate(index)) #把频率装进向量
freq_vector

#先确定b中元素个数。检验排在第500的频率是否是唯一的（如果第500和501等是相同频率，就全部纳入）
freq_sort <- sort(freq_vector, decreasing = TRUE)
b_number <- sum(freq_sort >= freq_sort[500])
b_number
#确定threshold
threshold <- freq_sort[b_number]
threshold

#再选取b_number个（虽然正好是500）
#下面这个500common的b是按照b原来的顺序把频率高于threshold的拿了出来，写为b
common_index <- which(freq_vector >= threshold)
b <- b[common_index]
b




# step 7

##ZMY
new_index <- match(a,b) #确定full bible中每个字符对应着的b的位置
new_index <- c(new_index)
triplet <- cbind(new_index[1:(length(new_index)-2)], new_index[2:(length(new_index)-1)], new_index[3:length(new_index)]) #应该是这个意思？
triplet
na_row_index <- which(rowSums(is.na(triplet)) != 0)
#删掉有na的行，得到新的triplet
triplet <- triplet[-na_row_index,]
triplet

# find array T
# need some comments
##之前那一版每个dim里的顺序好像不是很对，改了一下
array_T <- array(0,c(500,500,500))
for (x in 1:nrow(triplet)){
  array_T[triplet[x,1],triplet[x,2],triplet[x,3]]=array_T[triplet[x,1],triplet[x,2],triplet[x,3]]+1
}
array_T

test1 <- length(which(triplet[,1]==2 & triplet[,2]==1 & triplet[,3]==481))
test1 == array_T[2,1,481]

#find matrix A
pair <- cbind(new_index[1:(length(new_index)-1)], new_index[2:(length(new_index))]) #应该是这个意思？
pair
na_row_index <- which(rowSums(is.na(pair)) != 0)
pair <- pair[-na_row_index,]
pair

matrix_A <- matrix(0,nrow=500,ncol=500)
for (x in 1:nrow(pair)){
  matrix_A[pair[x,1],pair[x,2]]=matrix_A[pair[x,1],pair[x,2]]+1
}
matrix_A

test2 <- length(which(pair[,1]==2 & pair[,2]==484))
test2 == matrix_A[2,484]

#find vector S
single <- new_index
vector_S <- c(rep(0,500))
for (x in 1:500){
  vector_S[x]=length(which(single==x))
}
vector_S


# step 8
# try to find whether there is any commom word in b that is never followed by another word in b.
test_A <- c(rep(0,500))
for (i in 1:500){
  test_A[i] <- sum(matrix_A[i,])
}
test_A_result <- sum(which(test_A == 0))
test_A_result # equals 0, verifying that every word in b can be followed by another word in b.
# Our simulation won't be broken when using S and A to generate the first two words.
# But similar test for array T is difficult.
simulate_two_words <- function(vector_S, matrix_A){
  two_word <- c(0,0)
  two_word[1] <- sample(1:500, size = 1, prob = vector_S)
  two_word[2] <- sample(1:500, size = 1, prob = matrix_A[two_word[1],])
  two_word
} 
simulate_text <- function(vector_S, matrix_A, array_T, length_text=50){
  text <- c(rep(0,length_text))
  text[c(1,2)] <- simulate_two_words(vector_S, matrix_A)
  i <- 3
  repeat{
    if (sum(array_T[text[i-2],text[i-1],])!=0){
      text[i] <- sample(1:500, size = 1, prob = array_T[text[i-2],text[i-1],])
      i <- i+1
    }else{
      text[c(i,i+1)] <- simulate_two_words(vector_S, matrix_A)
      i <- i+2
    }
    if (text[length_text]!=0){break}
  }
  text
}
simulate_index_T <- simulate_text(vector_S, matrix_A, array_T)
cat(b[simulate_index_T])

# step 9
simulate_index_S <- sample(1:500, size = 50, prob = vector_S, replace = T)
cat(b[simulate_index_S])
