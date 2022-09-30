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
#原方法选出来的500common的b是按照从频率最高到频率最低的顺序排的，改为b1
order_vector <- order(freq_vector,decreasing = TRUE) #直接返回位置参数
commom_index <- order_vector[1:500]#取排名前500个的 作为common
b1 <- b[commom_index] #set of m ≈ 500 most common words
b1
#下面这个500common的b是按照b原来的顺序把频率高于threshold的拿了出来，写为b2
common_index2 <- which(freq_vector >= threshold)
b2 <- b[common_index2]
b2
#检验了一下b1和b2包含的元素是一样的，个人更偏向用第二种方法，要不threshold没用上
equal <- sum(b2 %in% b1)
equal


#以下为新增部分 SSY
#寻找b在a中的位置 并返回a每个位置上对应的b的元素在b中的位置参数

a_index<-match(a,b1)
a_index <- c(a_index)

#寻找那些非b的位置 并给出在a中的位置参数
naa_index<-which(is.na(a_index))
naa_index <- c(naa_index)

#将非b的位置参数去掉 就是和b匹配的位置参数
anonan<-seq(1,length(a))
anonan<- anonan[-naa_index]
anonan

#建立triple
triple<-cbind(a_index[anonan],a_index[anonan+1],a_index[anonan+2])#以b中元素的位置参数为值
na_row_index<-which(rowSums(is.na(triple)) != 0)
na_row_index#有NA的行

unique(triple[,2:3])#挑出所有23列可以有的组合 再接着往前找at就可以
#这样的话 无论抽哪一组值都很方便 一个NA 两个NA 没有NA
