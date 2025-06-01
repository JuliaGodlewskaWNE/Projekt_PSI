#' ---
#' title: "Recenzje e-sklepów z ubraniami: klasyfikacja"
#' author: "Julia Godlewska"
#' date:   "01.06.2025"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: lumen
#'     highlight: haddock
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: true
#' ---

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE)

#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(dplyr)
library(SnowballC)
library(LiblineaR)

#' # Dane tekstowe
# Dane tekstowe ----

# Wczytanie pliku csv
data <- read.csv("Womens_Clothing_E_Commerce_Reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Zmiana nazwm kolumn dla ułatwienia pracy
colnames(data)[colnames(data) == "Review.Text"] <- "Review_Text"
colnames(data)[colnames(data) == "Recommended.IND"] <- "Recommended"

# Usunięcie duplikatów
data <- data %>%
  distinct(Review_Text, .keep_all = TRUE)

# Korpus
corpus <- VCorpus(VectorSource(data$Review_Text))

#' # Przetwarzanie i oczyszczanie tekstu
# Przetwarzanie i oczyszczanie tekstu ----

# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usuń zbędne znaki lub pozostałości url, html itp.

corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")
corpus <- tm_map(corpus, toSpace, "ã¼")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
custom_stopwords <- c("one", "will", "got", "get", "just")
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

#' # Tokenizacja
# Tokenizacja ----

#' # Macierz częstości TDM
# Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

#' ## Zliczanie częstości słów
# Zliczanie częstości słów ----

# Zliczanie częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

#' ## Eksploracyjna analiza danych
# Eksploracyjna analiza danych ----

# Stylizacja
colour_palette <- c("#FFB6C1", "#D4A5FF", "#B5B9FF", "#A2E1FF", "#BFFCC6")

# Chmura słów
tdm_df_clean <- data.frame(
  word = tdm_df$word, 
  freq = tdm_df$freq) %>% 
  filter(freq >= 1200)

wordcloud(
  words = tdm_df_clean$word,
  freq = tdm_df_clean$freq,
  scale = c(4, 0.8),
  colors = rep(colour_palette, length.out = nrow(tdm_df_clean)),
  bg.color = "#FFFFFF",
  min.freq = 1,
  random.order = FALSE,
  rot.per = 0.35,
  fixed.asp = TRUE)

# Wyświetlenie top 10
print(head(tdm_df, 10))

#' # Macierz częstości TDM z TF-IDF
# Macierz częstości TDM z TF-IDF ----

tdm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_tfidf_m <- as.matrix(tdm_tfidf)

#' ## Zliczanie częstości słów
# Zliczanie częstości słów ----

# Zliczanie częstości słów w macierzach
v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)

#' ## Eksploracyjna analiza danych
# Eksploracyjna analiza danych ----

# Chmura słów
tdm_tfidf_df_clean <- data.frame(
  word = tdm_tfidf_df$word, 
  freq = tdm_tfidf_df$freq) %>% 
  filter(freq >= 1200)

wordcloud(
  words = tdm_tfidf_df_clean$word,
  freq = tdm_tfidf_df_clean$freq,
  scale = c(4, 0.8),
  colors = rep(colour_palette, length.out = nrow(tdm_tfidf_df_clean)),
  bg.color = "#FFFFFF",
  min.freq = 1,
  random.order = FALSE,
  rot.per = 0.35,
  fixed.asp = TRUE)

# Wyświetlenie top 10
print(head(tdm_tfidf_df, 10))

#' # Uczenie Maszynowe Nadzorowane
# Uczenie Maszynowe Nadzorowane ----

# Utworzenie ramki danych z macierzy TDM 
dtm_df <- as.data.frame(t(tdm_tfidf_m))
dtm_df$Recommended <- factor(data$Recommended, levels = c("0", "1"))
dim(dtm_df)

#' ## Podział na zbiór treningowy/testowy: LOSOWY
# Podział na zbiór treningowy/testowy: LOSOWY ----

# Podział na zbiór treningowy (uczący) i testowy (80/20)
set.seed(420)
train_indices <- sample(1:nrow(dtm_df), 0.8 * nrow(dtm_df))
trainData <- dtm_df[train_indices, ]
testData  <- dtm_df[-train_indices, ]

#' ### Model klasyfikacji
# Model klasyfikacji, podział LOSOWY ----

# Model z użyciem LiblineaR (dobra szybkość dla klasyfikacji binarnej)
x_train <- as.matrix(trainData[, -which(names(trainData) == "Recommended")])
y_train <- trainData$Recommended

model <- LiblineaR(
  data = x_train,
  target = y_train,
  type = 1,      # L2-regularized L2-loss SVM (fastest for binary)
  cost = 1,      # Default cost (tune if needed)
  bias = TRUE,   # Include bias term
  verbose = FALSE)

# Predykcje
predictions <- predict(model, as.matrix(testData[, -which(names(testData) == "Recommended")]))$predictions

#' ### Ocena modelu klasyfikacji
# Ocena modelu klasyfikacji, podział STRATYFIKOWANY ----

# Macierz pomyłek
confusion_matrix <- table(Predicted = predictions, Actual = testData$Recommended)
print(confusion_matrix)

# Wyciąganie TP, TN, FP, FN z confusion_matrix
# "1" jako pozytywna klasa
TP <- confusion_matrix["1", "1"]
TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["1", "0"]
FN <- confusion_matrix["0", "1"]

cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")

# Obliczenia metryk
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla '1'):", round(precision, 2),
    "\nRecall (dla '1'):", round(recall, 2),
    "\nSpecificity (dla '1'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")

#' ### Wizualizacja metryk
# Wizualizacja metryk, podział LOSOWY ----

# Przygotowanie danych do wykresu
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score))

ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "#3D225F") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_manual(values = colour_palette) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Przygotowanie do wizualizacji macierzy pomyłek
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")

# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradientn(
    colours = c("#FFFFFF", "#FFECF1", "#FFD1DC", "#FFB6C1", "#FF85A2"),
    values = scales::rescale(c(0, 300, 350, 400, 450, 500, 3000, 3500, 4000)),
    
    breaks = c(0, 300, 500, 1000, 2000, 3000, 4000),
    limits = c(0, 4000),
    name = "Count") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14) +
  theme(
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "right",
    legend.box.background = element_rect(color = "#3D225F", linewidth = 0.5),
    panel.border = element_rect(color = "#3D225F", fill = NA, linewidth = 0.7)) +
  guides(fill = guide_colorbar(
    frame.colour = "#3D225F",
    ticks.colour = "#3D225F",
    frame.linewidth = 0.5,
    ticks.linewidth = 0.5))

# Oznaczenie poprawności klasyfikacji (Correct / Incorrect)
confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")

# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "#B5F2B8",
  confusion_df$Label == "True Negative (TN)" ~ "#B5F2B8",
  confusion_df$Label == "False Positive (FP)" ~ "#FFE3B3",
  confusion_df$Label == "False Negative (FN)" ~ "#FFBFC1")

# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = paste(Label, "\n", Freq)), 
    size = 4, 
    fontface = "bold",
    lineheight = 0.8) +
  scale_fill_identity(
    guide = "legend",
    breaks = c("#B5F2B8", "#FFE3B3", "#FFBFC1"),
    labels = c("TP / TN (Correct)", 
               "False Positive (Incorrect)", 
               "False Negative (Incorrect)"),
    name = "Wynik klasyfikacji") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    aspect.ratio = 1) +
  coord_fixed()

#' ## Podział na zbiór treningowy/testowy: STRATYFIKOWANY
# Podział na zbiór treningowy/testowy: STRATYFIKOWANY ----

#' ### Model klasyfikacji
# Model klasyfikacji, podział STRATYFIKOWANY ----

# Podział na klasy
yes_class <- dtm_df[dtm_df$Recommended == "1", ]
no_class  <- dtm_df[dtm_df$Recommended == "0",  ]

# Podział na zbiór treningowy (uczący) i testowy (80/20)
# Stratyfikowane próbkowanie 80% z każdej klasy
set.seed(420)
yes_train_indices <- sample(1:nrow(yes_class), size = floor(0.8 * nrow(yes_class)))
no_train_indices  <- sample(1:nrow(no_class),  size = floor(0.8 * nrow(no_class)))

# Budowa zbioru treningowego (uczącego) i testowego utrzymując proporcje klas
trainData <- rbind(yes_class[yes_train_indices, ], no_class[no_train_indices, ])
testData  <- rbind(yes_class[-yes_train_indices, ], no_class[-no_train_indices, ])

# Model z użyciem LiblineaR (dobra szybkość dla klasyfikacji binarnej)
x_train <- as.matrix(trainData[, -which(names(trainData) == "Recommended")])
y_train <- trainData$Recommended

model <- LiblineaR(
  data = x_train,
  target = y_train,
  type = 1,      # L2-regularized L2-loss SVM (fastest for binary)
  cost = 1,      # Default cost (tune if needed)
  bias = TRUE,   # Include bias term
  verbose = FALSE)

# Predykcje
predictions <- predict(model, as.matrix(testData[, -which(names(testData) == "Recommended")]))$predictions

#' ### Ocena modelu klasyfikacji
# Ocena modelu klasyfikacji, podział STRATYFIKOWANY ----

# Macierz pomyłek (confusion matrix)
confusion_matrix <- table(Predicted = predictions, Actual = testData$Recommended)
print(confusion_matrix)

# Wyciąganie TP, TN, FP, FN z confusion_matrix
# "1" jako pozytywna klasa
TP <- confusion_matrix["1", "1"]
TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["1", "0"]
FN <- confusion_matrix["0", "1"]

cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")

# Obliczenie metryk
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla '1'):", round(precision, 2),
    "\nRecall (dla '1'):", round(recall, 2),
    "\nSpecificity (dla '1'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")

#' ### Wizualizacja metryk
# Wizualizacja metryk, podział STRATYFIKOWANY ----

# Przygotowanie danych do wykresu
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score))

ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "#3D225F") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_manual(values = colour_palette) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Przygotowanie do wizualizacji macierzy pomyłek
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")

# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradientn(
    colours = c("#FFFFFF", "#FFECF1", "#FFD1DC", "#FFB6C1", "#FF85A2"),
    values = scales::rescale(c(0, 300, 350, 400, 450, 500, 3000, 3500, 4000)),
    
    breaks = c(0, 300, 500, 1000, 2000, 3000, 4000),
    limits = c(0, 4000),
    name = "Count") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14) +
  theme(
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "right",
    legend.box.background = element_rect(color = "#3D225F", linewidth = 0.5),
    panel.border = element_rect(color = "#3D225F", fill = NA, linewidth = 0.7)) +
  guides(fill = guide_colorbar(
    frame.colour = "#3D225F",
    ticks.colour = "#3D225F",
    frame.linewidth = 0.5,
    ticks.linewidth = 0.5))

# Oznaczenie poprawności klasyfikacji (Correct / Incorrect)
confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")

# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "#B5F2B8",
  confusion_df$Label == "True Negative (TN)" ~ "#B5F2B8",
  confusion_df$Label == "False Positive (FP)" ~ "#FFE3B3",
  confusion_df$Label == "False Negative (FN)" ~ "#FFBFC1")

# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
# z przypisanymi kolorami i etykietami
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5, fontface = "bold") +
  scale_fill_identity(guide = "legend",
                      breaks = c("#B5F2B8", "#FFE3B3", "#FFBFC1"),
                      labels = c("TP / TN (Correct)",
                                 "False Positive (Incorrect)",
                                 "False Negative (Incorrect)"),
                      name = "Wynik klasyfikacji") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14)
