# Group member: Shangyun Sun(S2330859); Ziyi Gu(S2307232); Mengyao Zhang(S2049018).

# Shangyun Sun wrote codes of step 4 & 5 & 10.
# Ziyi Gu wrote codes of step 7 & 9.
# Mengyao Zhang wrote codes of step 6 & 8.
# Each of us double-checked and optimized others' codes.  
# Everyone undertakes roughly 1/3 of the work.

#-------------------------------------------------------------------------------

# step 3

setwd("C:/Users/surface/Documents/Rscripts/Groupwork18") # set the working directory
a <- scan("pg10.txt",what="character",skip=104) # skip contents
n <- length(a)
a <- a[-((n-2886):n)] # strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] # strip out verse numbers

#-------------------------------------------------------------------------------

# step 4

bible <- strsplit(a," ") # splite the original text into individual words

# a function to find the location of specific punctuation marks, remove them from the original text and
# add the mark as a new entry in the vector of words, after the word it came from
split_punct <- function(word_vector, punct_vector){
  
  for (punct in punct_vector){
    ii <- grep(punct, word_vector, fixed = TRUE) # indices of the punctuation in word_vector
    new_word <- rep("", length(ii) + length(word_vector)) # vector to store both words and punctuations
    iis <- ii + 1:length(ii) # decide where should punctuations go in new_word
    new_word[iis] <- punct # insert punctuations
    
    # handle full stop and other punctuation marks separately
    if (punct == "."){
      word_vector <- gsub("\\.","", word_vector) # remove full stops from word_vector
    }else{
      word_vector <- gsub(punct, "", word_vector,fixed = TRUE) # remove other punctuation marks
    }
    new_word[-iis] <- word_vector # insert words without punctuations
    word_vector <- new_word # update word_vector for the next round
  }
  word_vector
}

#-------------------------------------------------------------------------------

# step 5

# use the split_punct function to handle the original bible text
bible_punct <- c(",",".", ";", "!", ":", "?") # a vector of punctuations in bible
a_with_capital <- split_punct(bible, bible_punct) # bible text with splitted punctuations

#-------------------------------------------------------------------------------

# step 6
# In this step, we want to find a vector containing about 500 most common words.

a <- tolower(a_with_capital) # bible vector with lower case letters
b <- unique(a) # the vector of unique words
index <- match(a,b) # indicies indicating which element in b each element in a corresponds to

freq_vector <- c(tabulate(index)) # the frequency of b's elements in a
freq_sort <- sort(freq_vector, decreasing = TRUE) # a decreasingly ordered frequency vector

# Now determine the length of b, which is around 500.
# Considering that the 501th in freq_sort may have the same frequency with the 500th
# We contain all frequencies greater or equal to 500th.
b_number <- sum(freq_sort >= freq_sort[500]) # sum function helps count the number of TRUE

# a threshold number of occurrences at which a word should be included in the set of m≈500 most common words
threshold <- freq_sort[b_number]

common_index <- which(freq_vector >= threshold) # the index of word with occurrences >= threshold
b <- b[common_index] # the vector of the most common words

#-------------------------------------------------------------------------------

# step 7
#In this step, we start to find array T, matrix A and vector S

new_index <- c(match(a,b)) # indicies indicating which element in b each element in a corresponds to

# find array T
# We use cbind to create a 3 column matrix. 
# Remove a couple of entries from the end in new_index to form the first column, one from the start and one from the end to form the second column and two from the start for the third column. 
# Each row indexes a triplet of adjacent words 
triplet <- cbind(new_index[1:(length(new_index)-2)], new_index[2:(length(new_index)-1)], new_index[3:length(new_index)])
na_row_index <- which(rowSums(is.na(triplet)) != 0)#remove the rows containing NA
triplet <- triplet[-na_row_index,]

array_T <- array(0,c(500,500,500))#create an array with elements all equal to 0
#Use a loop to count the times each index of a triplet of words appears in the text. 
#Each index of array_T represents a triplet of element in one row of the matrix triplet
#So in the loop, every time we reach a new row of triplet, add one to the corresponding element of array_T and we can get the total frequency.
for (x in 1:nrow(triplet)){
  array_T[triplet[x,1],triplet[x,2],triplet[x,3]]=array_T[triplet[x,1],triplet[x,2],triplet[x,3]]+1
}

#find matrix A
#Similay to array T, we first find matrix pair, with each row indicating the index of a pair of adjacent words
pair <- cbind(new_index[1:(length(new_index)-1)], new_index[2:(length(new_index))]) 
na_row_index <- which(rowSums(is.na(pair)) != 0)#remove rows with NA
pair <- pair[-na_row_index,]

matrix_A <- matrix(0,nrow=500,ncol=500)
#The logic is the same as the for loop in array_T. matrix_A stores the times of index of a pair of word appearing in the text.
for (x in 1:nrow(pair)){
  matrix_A[pair[x,1],pair[x,2]]=matrix_A[pair[x,1],pair[x,2]]+1
}

#find vector S
single <- new_index
vector_S <- c(rep(0,500))
#count the times that each index of vector_S appears in vector single
for (x in 1:500){
  vector_S[x]=length(which(single==x))
}

#-------------------------------------------------------------------------------

# step 8
# In this step, we simulate 50-word sections from our model.
# The complete process is: firstly, randomly select a word according to the probability implied by S,
# then choose the second word according to the first word and the probability implied by A,
# and then choose the following words according to the last two words and the probability implied by T.
# We can do this by loop, but need to make sure that in the process of looping, 
# we will not fail to find the next word because the probability is 0.
# So verify matrix A and array T.
# For A, we try to find whether there is any commom word in b that is never followed by another word in b.
test_A <- c(rep(0,500))
for (i in 1:500){
  test_A[i] <- sum(matrix_A[i,])
}
test_A_result <- sum(which(test_A == 0))
test_A_result # equals 0, verifying that every word in b can be followed by another word in b.
# Our simulation won't be broken when using S and A to generate the first two words.

# a function to simulate the first two words (index)
simulate_two_words <- function(vector_S, matrix_A){
  two_word <- c(0,0)
  two_word[1] <- sample(1:500, size = 1, prob = vector_S) # randomly select one number in [1,500] based on the prob implied by vector_S
  two_word[2] <- sample(1:500, size = 1, prob = matrix_A[two_word[1],])
  two_word
} 

# However, similar test for array T is difficult.
# So we add if condition in our code to make sure that our loop can run without interruption.

# a function to simulate the text (index) with default length 50
simulate_text <- function(vector_S, matrix_A, array_T, length_text=50){
  text <- c(rep(0,length_text))
  text[c(1,2)] <- simulate_two_words(vector_S, matrix_A) # simulate the first two words
  i <- 3 # an index which is the position where we start loop
  
  repeat{
    # "!= 0" means simulating the following word by array T works well
    if (sum(array_T[text[i-2],text[i-1],])!=0){ 
      # simulate the i-th word based on the last two words and the corresponding prob
      text[i] <- sample(1:500, size = 1, prob = array_T[text[i-2],text[i-1],]) 
      # the next loop is to simulate a word for the next position in text
      i <- i+1} 
    
    # else ("= 0") means we cannot find the following word by T. The loop break and we need to "restart"
    else{ 
      text[c(i,i+1)] <- simulate_two_words(vector_S, matrix_A) 
      # the next position that needs to be filled is i+2
      i <- i+2
    }
    
    # when the last term in text is filled with "something", the loop should be stopped
    if (text[length_text]!=0){break}
  }
  text #return text
}

# generate a vector of word index by the above function
simulate_index_T <- simulate_text(vector_S, matrix_A, array_T)

# print the simulated words
cat(b[simulate_index_T]) 

#-------------------------------------------------------------------------------

# step 9
# Directly simulate 50 words based on probability implied by vector_S， with replacement
simulate_index_S <- sample(1:500, size = 50, prob = vector_S, replace = T)
cat(b[simulate_index_S])

#-------------------------------------------------------------------------------

# step 10
#In this step, we would like to replace the words that most often start with a capital letter with an uppercase initial form, 
#as opposed to the words in the previous b, which are all lowercase forms.

unique_a_with_capital <- unique(a_with_capital) # Find vector unique_a_with_capital of unique words in vectors containing words beginning with capital letters.
unique_a <- unique(a)# Find vector unique_a of unique words in vectors containing only words beginning with lowercase letters

# Using match funtion, returns a vector of the positions of (unique_a_with_capital) matches of unique_a_with_capital in unique_a.
# The positions that are not successfully matched are the positions of all words beginning with an capital letter, which returns NA. 
# Finally we can use these position index to get all the words that start with an capital letter, forming the vector capital.
capital <- unique_a_with_capital[is.na(match(unique_a_with_capital,unique_a))]

#returns a vector of the positions of (a_wth_capital) matches of a_with_capital in capital. 
#It is convenient for us to subsequently use the tabulate function 
#to count how many times each word beginning with capital letter appears.
capital_index <- match(a_with_capital,capital)

#Converting the capital vector to all lowercase and matching it with a, a vector containing all possible words, gives the position index. 
#It is convenient for us to subsequently use the tabulate function to calculate the total number of occurrences in the text 
#of all words that have had their initial letter be capital. 
lower_capital_index <- match(a, tolower(capital))
lower_capital_index

freq_capital <- tabulate(capital_index) #The total number of occurrences in the text of all words that have had their initial letter be capital.
freq_lower_capital <- tabulate(lower_capital_index)#Total frequency of the word which can start with both lowercase and capital letters.
prop <- freq_capital/freq_lower_capital#Calculate the proportion of start with capital letter's frequency in the total frequency of the word which can start with both lowercase and capital letters.  
most_often_index <- which(prop>0.5)# Find position index of the part whose proportion is larger than 50%, as the most common part.
most_often <- capital[most_often_index]# Using the position index, find the words that most often start with a capital letter
most_often_lower <- tolower(most_often)# Change all words in lower case in order to match with b_with_capital, to find the position index of the word start with capital letter.
b_with_capital <- b

# Returns a vector of the positions of (b_with_capital) matches of b_with_capital in most_often_lower.
match_index <- match(b_with_capital,most_often_lower)


# Find position index which not equals to na, because it means that 
# in this position we can find word in most_often_lower vector in b_with_capital vector. 
replace_index <- which(!is.na(match_index))

# Therefore, we should change these words in b_with_capital 
# which is same with most often start with capital letter to the form that start with capital letter
b_with_capital[replace_index] <- most_often[match_index[replace_index]]
cat(b_with_capital[simulate_index_T])


