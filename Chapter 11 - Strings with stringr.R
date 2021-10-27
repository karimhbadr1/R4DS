# Chapter 11: Strings with stringr ----------------------------------------

# Prerequisites
library(tidyverse)
library(stringr)


# String Basics -----------------------------------------------------------

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes.'

writeLines(string2)

double_quote<-"\""
single_quote<-'\''
writeLines(single_quote)

x<-c("\"","\\")
x
writeLines(x)

x<-"\u00b5"
x
y<-c("one","two","three","\\")
y
writeLines(y)


# String Length -----------------------------------------------------------

str_length(c("a","R for data science",NA))


# Combining Strings -------------------------------------------------------
str_c("x","y","z")
str_c("x","y","z",sep=",")

x<-c("abc",NA)
str_c("|-",x,"-|")
str_length(str_c("|-",str_replace_na(x),"-|"))

str_c("prefix-",c("a","b","c"),"-suffix")

name<-"Hadley"
time_of_day<-"morning"
birthday<-TRUE

str_c("Good ",time_of_day," ",name,if(birthday) " and HAPPY BIRTHDAY",".")

str_c(c("x","y","z"),collapse=",")


# Subsetting Strings ------------------------------------------------------

x<-c("Apple","Banana","Pear")
str_sub(x,1,1)
str_sub(x,1,2)
str_sub(x,2,1)
str_sub(x,2,3)
str_sub("Apple",3,1)
str_sub(x,-3,-2)
str_sub(x,1,19)
str_sub("a,",1,5)
str_sub(x,1,1)
str_sub(x,1,1)<-str_to_lower(str_sub(x,1,1))
x


# Locales -----------------------------------------------------------------

str_to_upper(c("i", "i"))
str_to_upper(c("i", "i"),locale = "tr")

x<-c("apple","eggplant","banana")
str_sort(x)
str_sort(x,locale = "en")
str_sort(x,locale="haw")


# Exercises ---------------------------------------------------------------

#1

?paste
paste("a","b",sep="")
paste0("a","b")

str_c("a",NA)
paste("a",NA)
paste0("a",NA)

#2
str_c("a","b",sep=",")
str_c(c("a","b"),collapse = ",")

#3
x<-c("karim","michaEl","evan")
a<-str_length(x)
b<-ceiling(a/2)
str_sub(x,b,b)

#4
?str_wrap
#str_c(readLines(thanks_path))

thanks_path <- file.path(R.home("doc"), "THANKS")
thanks <- str_c(readLines(thanks_path), collapse = "\n")
thanks <- word(thanks, 1, 3, fixed("\n\n"))
thanks
cat(str_wrap(thanks),"\n")

#5
?str_trim
a<-" abc    "
str_trim(a,side="right")
b<-"hello"
str_length(str_pad("hello",7,side="right"))
str_pad("abc",5,side="both")

#6
str_commasep<-function(a,delim=",") {
  n<-length(a)
  if (n==0) {
    ""
  } else if (n==1) {
    a
  } else if (n==2) {
    str_c(a[1],"and",a[2],sep=" ")
  } else {
    not_last<-str_c(a[seq_len(n-1)],delim)
    last <- str_c("and", a[n], sep = " ")
    str_c(c(not_last, last), collapse = " ")
  }
}


# Matching Patterns with Regular Expressions ------------------------------

#Basic Matches
x<-c("pear","apple","banana")
str_view(x,"an")
str_view(x,".a.")

z<-"."
writeLines(z)

t<-c("a.bc","a.c","bef")
writeLines(t)

str_view(t,".\\..")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")


str_view("\"'\\", "\"'\\\\")
writeLines("\"'\\")

#Anchors

x<-c("apple","banana","pear")
str_view(x,"^a")
str_view(x,"a$")

x<-c("apple pie","apple","cake apple")
str_view(x,"apple$")
str_view(x,"apple")
str_view(x,"a")

x<-c(x,"A$\\^$lol")
x
#Exercises
str_view(x,"^\\$\\^\\$")
str_view(x,"\\\\\\$")

view(words)
str_view(words,"^y",match=TRUE)
str_view(words,"x$",match=TRUE)

str_view(words,"^...$",match=TRUE)
str_view(words,"^.......",match = TRUE)
str_view(words,".......$",match = TRUE)


# Character Classes and Alternatives --------------------------------------

# Look for a literal character that normally has special meaning in a regex
str_view(c("abc]", "a.c]", "a*c[", "a c"), "a.c\\[")
str_view(c("abc", "a.c", "a*c", "ac"), ".*c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")
str_view(c("grey", "gray"), "gr(ea)y")

str_view(words,"^[a|e|i|o|u|y]",match=TRUE)
str_view(words,"[aeiou]",match=FALSE)
str_subset(words,"[aeiou]",negate=TRUE)
str_subset(words,"^[a|e|i|o|u|y]")

str_view(words,"[^e]ed$",match=TRUE)
str_view(words,"ed$",match=TRUE)
str_view(words,"(^|[^e])ed$",match=TRUE)

str_view(words,"ing$|ize$",match=TRUE)
str_view(words,"i(ng|ze)$",match=TRUE)
str_view(words,"ei",match=TRUE)
str_view(words,"cei|[^c]ie",match=TRUE)
str_view(words,"(cie|[^c]ei)",match=TRUE)

str_view(words,"q[^u]",match=TRUE)
str_view(stringr::words, "q[^u]", match = TRUE)

str_view(words,"ou|ise$|ae|oe|yse$",match=TRUE)
x<-c("071-075","074-055","079-075","077-079","077-79")
# "07[1|"


# Repetition --------------------------------------------------------------

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

str_view(x,"CC?")
str_view(x,"CC*")
str_view(x,"CC+")

y<-c("color","colour","banana")
str_view(y,"colou?r")
y<-c(y,"per","penguin","pengui","penguinn")
str_view(y,"(pengui){0}n$")
str_view(y,"ba(na)+")
y<-c(y,"bana","ba","bananz","bananan","banananaa")
str_view(y,"ba(na)*")

str_view(x,"C{1,3}?")
str_view(x,"C[LX]+")
str_view(x,"C{2,3}?")

z<-c("\\\\\\\\")
z
str_view(z,"\\\\")


str_view(words,"^[^aeiou]{3}",match=TRUE)
str_view(words,"[aeiou]{3,}",match=TRUE)
str_view(words,"([aeiou][^aeiou]){2,}",match=TRUE)



# Grouping and Backreferences ---------------------------------------------

str_view(fruit,"(..)\\1",match=TRUE)
str_view("aaa","(.)\\2",match=TRUE)

str_view(words,"^(.)((.*\\1$)|(\\1?$))",match = TRUE)

str_subset(c("a","aa","aaba","aa a","aaa"),"^(.)((.*\\1$)|(\\1?$))")

str_view(words,"(..).*\\1",match=TRUE)


str_view(words,".*(.).*\\1.*\\1.*",match=TRUE)



# Tools -------------------------------------------------------------------


# Detect Matches ----------------------------------------------------------

x<-c("apple","banana","pear")
str_detect(x,"e")

sum(str_detect(words,"^t"))
mean(str_detect(words,"[aeiou]$"))

no_vowels_1<-!str_detect(words,"[aeiou]")
sum(no_vowels_1)

no_vowels_2<-str_detect(words,"^[^aeiou]+$")
sum(no_vowels_2)

identical(no_vowels_1,no_vowels_2)

words[str_detect(words,"x$")]

str_subset(words,"x$")

df<-tibble(
  word=words,
  i=seq_along(word)
)
view(df)

df %>% 
  filter(str_detect(word,"x$"))

str_count(x,"a")

df1<-tibble(
  word=words,
  count=str_count(words,"[aeiou]")
)

mean(df1$count)

df<-df %>% 
  mutate(
    vowels=str_count(word,"[aeiou]"),
    consonants=str_count(word,"[^aeiou]"),
    length=str_length(word)
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")


words[str_detect(words,"(^x)|(x$)")]

words

start_x<-str_detect(words,"^x")
end_x<-str_detect(words,"x$")
words[start_x|end_x]
words[start_x]
words[end_x]

vowel_start<-str_detect(words,"^[aeiou]")
cons_end<-str_detect(words,"[aeiou]$",negate = TRUE)
words[vowel_start&cons_end]

words[str_detect(words,"^[aeiou].*[^aeiou]$")]

identical(words[str_detect(words,"^[aeiou].*[^aeiou]$")],words[vowel_start&cons_end])

x<-c(x,"aeiou676609")
x[str_detect(x, "a") &
        str_detect(x, "e") &
        str_detect(x, "i") &
        str_detect(x, "o") &
        str_detect(x, "u")]

DF<-tibble(
  word=words,
  vowel=str_count(word,"[aeiou]"),
  length=str_length(word)
)

which.max(DF$vowel)

DF[which(vowel== max(vowel))]

DF %>% 
  filter(vowel==max(vowel))

DF<-DF %>% 
  mutate(prop=vowel/length)

DF %>% 
  filter(prop==max(prop))


length(sentences)

head(sentences)

view(sentences)

colours<-c("red","orange","yellow","green","blue","purple")

colour_match_exp<-str_c(colours,collapse = "|")

has_colour<-str_subset(sentences,colour_match_exp)

matches<-str_extract(has_colour,colour_match_exp)

more<-sentences[str_count(sentences,colour_match_exp)>1]
str_view_all(more,colour_match_exp)

str_extract(more,colour_match_exp)
str_extract_all(more,colour_match_exp)

ab1<-str_extract_all(more,colour_match_exp,simplify = TRUE)
view(ab1)

x<-c("a","b","c","a ","a b","ab","abc","a    bdl")
str_extract_all(x,"[a-z]",simplify=TRUE)

# 
# for(i in 1:length(colours)){
#   new_colours[i]=paste0("\\s",colours[i])
# }


new_colour_match_exp<-str_c("\\b(",colour_match_exp,")\\b")
new_colour_match_exp

a<-str_subset(sentences,new_colour_match_exp)

str_view_all(a,new_colour_match_exp)

b<-str_extract(sentences,"[A-Za-z][A-Za-z']*")
b
view(b)

str_view(sentences,"\\b\\S+ing\\b")
str_view(sentences,"\\b[A-Za-z]+ing\\b")

a<-str_detect(sentences,"\\b\\S+ing\\b")
a

# str_extract_all(str_detect(sentences,"\\b\\S+ing\\b"),"\\b\\S+ing\\b")

pattern<-"\\b\\S+ing\\b"
sentences_with_pattern<-str_detect(sentences,"\\b\\S+ing\\b") 
sentences_with_pattern<-sentences[sentences_with_pattern]
a<-str_extract_all(sentences_with_pattern,pattern)
a<-unlist(a)
unique(a)


#MODEL TEMPLATE
pattern<-"\\b[A-Za-z]{3,}s\\b"
sentences_with_pattern<-str_detect(sentences,pattern)
sentences_with_pattern<-sentences[sentences_with_pattern]
a<-str_extract_all(sentences_with_pattern,pattern)
a<-unlist(a)
a<-unique(a)
a


# Grouped Matches ---------------------------------------------------------

str_view(sentences,"(a|the) ([^ ]+)")

noun<-"(a|the|is) ([^ ]+)"
has_noun<-sentences %>% 
  str_subset(noun) %>% 
  head(10)

has_noun %>% 
  str_extract(noun)

has_noun %>% 
  str_match(noun)

tibble(sentence=sentences) %>% 
  extract(sentence,c("article","noun"),"(a|the) ([^ ]+)", remove=FALSE)

words_after_number<-"\\b(one|two|three|four|five|six|seven|eight|nine|ten) +(\\w+)"
str_view(sentences,words_after_number,match=TRUE)
str_match(sentences,words_after_number)

str_extract_all(sentences[str_detect(sentences,words_after_number)],words_after_number)

A<-tibble(sentence=sentences) %>% 
  extract(sentence,c("number","word"),words_after_number,remove=TRUE)


words_after_number<-"\\b(one|two|three|four|five|six|seven|eight|nine|ten) +(\\w+)"
S<-sentences[str_detect(sentences,words_after_number)]
str_match(S,words_after_number)


str_view(sentences,"([^ ]+)'([^ ]+)",match=TRUE)
pattern<-"([^ ]+)'([^ ]+)"
sen<-str_detect(sentences,pattern)
sen<-sentences[sen]
A<-str_match(sen,pattern)
A<-as_tibble(A)
A<-A %>% 
  rename("Sentence"="V1","Word"="V2","Contraction"="V3")

B<-tibble(
  sen,A
) %>% 
  rename("Sentence"="sen","Contracted Word"="Sentence")

X<-c("My house's board's aluminum.","Hello's word","HELLO")
sen1<-str_detect(X,pattern)
sen1<-X[sen1]
Z<-str_extract_all(sen1,pattern)
as_tibble(Z)
unlist(Z)


# Replacing Matches -------------------------------------------------------

x<-c("apple","pear","banana")

str_replace(x,"[aeiou]","-")
str_replace_all(x,"[aeiou]","-")

x<-c("1 house","2 cars","3 people 1 kid")
str_replace_all(x,c("1"="one","2"="two","3"="three"))

str_replace(sentences,"([^ ]+) ([^ ]+) ([^ ]+)","\\1 \\3 \\2")

a<-"A/ gentelman i/s cool."
str_replace_all(a,"/","\\\\")

str_to_lower(a)

replacements <- c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
                  "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
                  "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
                  "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
                  "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
                  "Z" = "z")
str_replace_all(a,replacements)

view(words)

myword<-"able"
swapped<-str_replace(words,"(^[^ ])(.*)([^ ]$)","\\3\\2\\1")
intersect<-intersect(words,swapped)

view(intersect)
str_view(myword,"^[^ ]")
str_view(myword,"[^ ]$")

str_view(myword,"(^[^ ])")


# Splitting ---------------------------------------------------------------

sentences %>% 
  head(5) %>% 
  str_split(" ") %>% 
  .[[1]]

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

a<-sentences %>% 
  head(5) %>% 
  str_split(" ",simplify=TRUE)

fields<-c("Name: Hadley","Country: NZ","Age: 35: n")
fields %>% str_split(": ",n=2,simplify=TRUE)
fields %>% str_split(": ",simplify=TRUE)

x<-"This is a sentence. This is another sentence."
str_view(x,boundary("word"))
str_split(x," ")[[1]]
str_split(x,boundary("word"))[[1]]             

y<-"apples,pears, and bananas"            
str_split(y,boundary("word"))
str_split(y,",+( and +)?")[[1]]

sentence<-"The quick (\"brown\") fox can't jump 32.3 feet, right?"
writeLines(sentence)
sentence<-"The quick ("brown") fox can't jump 32.3 feet, right?"
str_split(sentence," ")
str_split(sentence,boundary("word"))

a<-str_split("ab. cd|agt","")[[1]]
a<-as_tibble(a)
# pivot_longer(as_tibble(a))
view(transpose(a))


# Find Matches ------------------------------------------------------------
str_locate(x,"is")
str_locate_all(x,"is")
str_sub(x,10,-9)


# Other Types of Pattern --------------------------------------------------

str_view(fruit,"nana")
str_view(fruit,regex("nana"))

bananas<-c("banana","Banana","BANANA","BaNaNa")
str_view(bananas,"banana")
str_view(bananas,regex("banana",ignore_case = TRUE))

x<-"Line 1\nLine 2\nLines 3"
writeLines(x)
str_extract_all(x,regex("^Line",multiline=TRUE))

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)
print(phone)

str_match("(514)79189823",phone)

str_detect(sentences, fixed("the"))

str_detect(sentences, "the")

microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

install.packages("tictoc")
library(tictoc)

tic()
str_detect(sentences, fixed("the"))
toc()

tic()
str_detect(sentences, "the")
toc()

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1==a2

str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

i <- c("I", "I", "i", "i")
i

str_subset(i, coll("i", ignore_case = TRUE))

str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

stringi::stri_locale_info()

x <- "This is a sentence."
str_view_all(x, boundary("word"))

str_extract_all(x, boundary("word"))

A<-"This is \\ Karim."
str_detect(A, fixed("\\"))

str_detect(A,"\\\\")

B<-unlist(str_extract_all(sentences,boundary("word")))
B<-tibble(B)
B<-str_to_lower(B$B)
B %>% 
  count(B$V1)

B<-tibble(word = unlist(str_extract_all(sentences, boundary("word"))))
B<-B %>% 
  mutate(word=str_to_lower(word))

B %>% count(word,sort = TRUE)


# Other Uses of Regular Expressions ---------------------------------------


apropos("replace")

dir(pattern = "\\.Rmd")


# stringi -----------------------------------------------------------------

install.packages("stringi")
library(stringi)

stri_count_words(sentences)

stri_duplicated(c("the", "brown", "cow", "jumped", "over",
                  "the", "lazy", "fox"))
stri_rand_strings(4, 5)
stri_rand_shuffle("The brown fox jumped over the lazy cow.")
stri_rand_lipsum(1)
stri_rand_lipsum(3)
stri_rand_lipsum(100900)

string1 <- c("hladny", "chladny")
stri_sort(string1, locale = "pl_PL")
stri_sort(string1, locale = "sk_SK")

stri_sort(string1, opts_collator = stri_opts_collator(locale = "pl_PL"))
string2 <- c("number100", "number2")
stri_sort(string2)
stri_sort(string2, opts_collator = stri_opts_collator(numeric = TRUE))
