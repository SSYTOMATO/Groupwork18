setwd("D:/Rgroupwork/Groupwork18")
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
order_vector <- order(freq_vector,decreasing = TRUE) #直接返回位置参数
threshold <- freq_vector[order_vector[500]]
threshold
commom_index <- order_vector[1:500]#取排名前500个的 作为common
b <- b[commom_index] #set of m ≈ 500 most common words
b
