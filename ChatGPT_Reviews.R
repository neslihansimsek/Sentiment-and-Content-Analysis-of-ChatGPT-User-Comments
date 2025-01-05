#ChatGPT_Reviews
# Paketlerin yuklenmesi
install.packages("readxl")        #Excel dosyalarini okur ve R'a yukleme yapan paket.
install.packages("xlsx")          #Excel dosyalarini okumak , yazmak ve bicimlendirmek icin kullanilan paket. 
install.packages("stringi")       #Hizli ve tasinabilir karakter dizisi isleme tesisleri paketi.
install.packages("stringr")       #Karakter yapili veriler icin kullanilan paket.
install.packages("tm")            #Metin madenciliginde kullanilan paket.
install.packages("tidytext")      #Duzenli veri ilkelerini jullanmak bircok metin madenciligi gC6revini yapan paket.
install.packages("plyr")          #Verileri Bolme, Uygulama ve Birlestirme yapan paket.
install.packages("dplyr")         #Veri manipulasyon islemini yapan paket.
install.packages("magrittr")      #%>% komutu ile zincirleme yapan paket.
install.packages("ggplot2")       #Verileri gorsellestirmek icin kullanilan paket.
install.packages("wordcloud")     #Kelime bulutu icin kullanilan paket.
install.packages("RColorBrewer")  #Kelime bulutunun renklendirilmesi icin kullanilan paket.
install.packages("rJava")         #Nesnelerin olusturulmas??na, yontemlerin cagirilmasina ve alanlara erisime izin veren paket.
install.packages("readr")         #csv', 'tsv' ve 'fwf' gibi dosyalarin okunmasini saglayan paket.
install.packages("writexl")       #Veri cercevelerini Excel 'xlsx' formatina aktaran paket.
install.packages("ggeasy")        #'ggplot2' Komutlarina kolay erisim saglar.



# Paketlerin kurulmasi
library(rJava)
library(readxl)
library(xlsx)
library(stringi)
library(stringr)
library(tm)
library(tidytext)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(xlsx)
library(readr)
library(writexl)
library(ggeasy)


ChatGPT_Reviews<- read_xlsx(file.choose())  


# ingilizce kelimelerin secilmesi
ChatGPT_Reviews <- ChatGPT_Reviews %>%
  filter(str_detect(Review, "^[a-zA-Z\\s,\\.]+$")) 

# Duzenlenmis veriyi kaydetme
write_xlsx(ChatGPT_Reviews, "ChatGPT_Reviews_English1.xlsx")

# Bos satirlari silme
ChatGPT_Reviews <- ChatGPT_Reviews %>%
  mutate(Review = str_trim(Review)) %>%   # Satir basi ve sonundaki bosluklari temizle
  filter(Review != "")                   # Tamamen bos olan satirlari kaldir

# Baglantilar (URL) kaldiriliyor
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "http[^[:space:]]*", "")

# "#" ve "@" ifadelerinin kaldirilmasi
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "#\\S+", "")
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "@\\S+", "")

# Noktalama isaretlerinin kaldirilmasi
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "[[:punct:][:blank:]]+", " ")
ChatGPT_Reviews$Review <- str_to_lower(ChatGPT_Reviews$Review, "tr")

# Rakamlarin kaldirilmasi
ChatGPT_Reviews$Review <- removeNumbers(ChatGPT_Reviews$Review)

# ASCII formatina uymayan karakterlerin temizlenmesi
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "[<].*[<]", " ")
ChatGPT_Reviews$Review <- gsub("\uFFFD", "", ChatGPT_Reviews$Review, fixed = TRUE)

# Yeni satir karakterlerinin kaldirilmasi
ChatGPT_Reviews$Review <- gsub("\n", "", ChatGPT_Reviews$Review, fixed = TRUE)

# Alfabetik olmayan karakterlerin temizlenmesi
ChatGPT_Reviews$Review <- str_replace_all(ChatGPT_Reviews$Review, "[^[:alnum:]]", " ")



#stopwordslerin cekilmesi
Stopwords <- read_xlsx(file.choose())

# Stopwords sutununu al 
stopwords_list <- Stopwords$stopwords

# Metni temizle (stopwords'leri kaldir)
ChatGPT_Reviews$Review <- removeWords(ChatGPT_Reviews$Review, stopwords_list)



# Bos satirlari silme
ChatGPT_Reviews <- ChatGPT_Reviews %>%
  mutate(Review = str_trim(Review)) %>%   # Satir basi ve sonundaki bosluklari temizle
  filter(Review != "")                   # Tamamen bos olan satirlari kaldir


# Veri cercevesini Hazirlama ve Kelimelere Ayirma
ChatGPT_veri <- ChatGPT_Reviews %>%
  select(Review) %>%  # 'Review' sutununu seciyoruz
  mutate(linenumber = row_number()) %>%  # Her satira numara ekliyoruz
  unnest_tokens(word, Review) # Metni kelimelere ayiriyoruz


# Frekans Grafigi
ChatGPT_veri %>%
  count(word, sort = TRUE) %>%  # Kelimelerin frekanslarini sayiyoruz
  filter(n > 1500) %>%  # Yalnizca 3'ten fazla frekansa sahip kelimeleri seciyoruz
  mutate(word = reorder(word, n)) %>%  # Kelimeleri frekansa gore siraliyoruz
  ggplot(aes(word, n)) +
  geom_col() +  # Bar grafigi ciziyoruz
  xlab("Word") +  # X eksenine 'Kelime' etiketini ekliyoruz
  ylab("Number of Frequencies") +  # Y eksenine 'Frekans Sayisi' etiketini ekliyoruz
  coord_flip() +  # Barlari yatay yap??yoruz
  theme_minimal() +  # Minimal tema
  ggtitle("Most Used Word")  # Baslik ekliyoruz


# Kelime Bulutu
wordcloud(ChatGPT_Reviews$Review, min.freq = 1, max.words = 150,  # Minimum frekans 1, en fazla 80 kelime
          colors = brewer.pal(8, "Dark2"),  # Renk paleti
          random.color = TRUE,  # Rastgele renkler
          random.order = FALSE)  # Rastgele kelime siralamasi





#-------------------------------------------
# Duygu Analizi

# Pozitif ve negatif kelimeleri yukle
pos.words <- read_xlsx(file.choose())$PositiveWordList  # Pozitif kelimelerin oldugu sutun adi
neg.words <- read_xlsx(file.choose())$NegativeWordList  # Negatif kelimelerin oldugu sutun adi

# Kucuk harfe cevir ve temizle
pos.words <- tolower(trimws(pos.words))
neg.words <- tolower(trimws(neg.words))

# Duygu analizi fonksiyonu
score.sentiment <- function(sentences, pos.words, neg.words) {
  require(stringr)
  
  scores <- sapply(sentences, function(sentence) {
    # Cumle temizleme
    sentence <- gsub('[^[:alpha:][:space:]]', '', sentence)  # Noktalama isaretlerini kaldir
    sentence <- tolower(sentence)  # Kucuk harfe cevir
    
    # Kelimeleri bol
    words <- unlist(str_split(sentence, '\\s+'))
    
    # Pozitif ve negatif kelime eslesmeleri
    pos_matches <- sum(!is.na(match(words, pos.words)))
    neg_matches <- sum(!is.na(match(words, neg.words)))
    
    # Puan hesaplama
    score <- pos_matches - neg_matches
    return(score)
  })
  

  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

# Analiz
sentences <- ChatGPT_veri$word  
analysis <- score.sentiment(sentences, pos.words, neg.words)


#  Duygu analizi Frekans tablosu
table(analysis$score)


# Duyarlilik puanlarinin dagiliminin grafik seklinde gosterimi
analysis %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill ="red")+ 
  ylab("Frequency") + 
  xlab("Emotion Score") +
  ggtitle("ChatGPT Reviews Sensitivity Scores") +
  ggeasy::easy_center_title()

# Duygu analizi icin notr , pozitif ve negatif kelimelerin skor degerlerinin belirlenmesi
# Yuzde grafigi seklinde gosterimi
neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
toplam=positive+neutral+negative
Sentiment <- c("Positive","Neutral","Negative")

Count <- c((positive/toplam)*100,(neutral/toplam)*100,(negative/toplam)*100)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count,))+
  geom_bar(stat = "identity", aes(fill = Sentiment ))+
  ggtitle("Sentiment Analysis Percentage Graph of Positive, Negative and Neutral Words")
head((positive/toplam)*100,"Positive")
head((neutral/toplam)*100 ,"Neutral")
head((negative/toplam)*100 ,"Negative")


