#Jezeli nazwa ramki danych inna, to w nawiasie dajemy te nazwe
library(readr)
library(plotly)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(broom)
library(car)
library(stats)
library(multcomp)

AnalizowaneDane <- read_csv("facebook+comment+volume+dataset/Dataset/Training/Features_Variant_1.csv", #Tak sie wgrywa baze danych jak cos
                               col_names = FALSE)
colnames(AnalizowaneDane)[1]<-"Page Popularity/likes" #Defines the popularity or support for the source of the document
colnames(AnalizowaneDane)[2]<-"Page Checkins" #Describes how many individuals so far visited this place. This feature is only associated with the places: some institution, place, theater etc
colnames(AnalizowaneDane)[3]<-"Page talking about" #Defines the daily interest of individuals towards source of the document/ Post. The people who actually come back to the page, after liking the page. This include activities such as comments, likes to a post, shares, etc by visitors to the page
colnames(AnalizowaneDane)[4]<-"Page Category" #Defines the category of the source of the document: place, institution, brand etc (1-106) <-LISTA Z TEMATYKAMI W PDF "Catagory_File - Feature 4"
colnames(AnalizowaneDane)[5]<-"CC1 min deviation" #Obliczone dla danej strony (chyba w danej kategorii stron)
colnames(AnalizowaneDane)[6]<-"CC1 max deviation"
colnames(AnalizowaneDane)[7]<-"CC1 average deviation"
colnames(AnalizowaneDane)[8]<-"CC1 median deviation"
colnames(AnalizowaneDane)[9]<-"CC1 standard deviation"
colnames(AnalizowaneDane)[10]<-"CC2 min deviation"
colnames(AnalizowaneDane)[11]<-"CC2 max deviation"
colnames(AnalizowaneDane)[12]<-"CC2 average deviation"
colnames(AnalizowaneDane)[13]<-"CC2 median deviation"
colnames(AnalizowaneDane)[14]<-"CC2 standard deviation"
colnames(AnalizowaneDane)[15]<-"CC3 min deviation"
colnames(AnalizowaneDane)[16]<-"CC3 max deviation"
colnames(AnalizowaneDane)[17]<-"CC3 average deviation"
colnames(AnalizowaneDane)[18]<-"CC3 median deviation"
colnames(AnalizowaneDane)[19]<-"CC3 standard deviation"
colnames(AnalizowaneDane)[20]<-"CC4 min deviation"
colnames(AnalizowaneDane)[21]<-"CC4 max deviation"
colnames(AnalizowaneDane)[22]<-"CC4 average deviation"
colnames(AnalizowaneDane)[23]<-"CC4 median deviation"
colnames(AnalizowaneDane)[24]<-"CC4 standard deviation"
colnames(AnalizowaneDane)[25]<-"CC5 min deviation"
colnames(AnalizowaneDane)[26]<-"CC5 max deviation"
colnames(AnalizowaneDane)[27]<-"CC5 average deviation"
colnames(AnalizowaneDane)[28]<-"CC5 median deviation"
colnames(AnalizowaneDane)[29]<-"CC5 standard deviation"
colnames(AnalizowaneDane)[30]<-"CC1" #The total number of comments before selected base date/time
colnames(AnalizowaneDane)[31]<-"CC2" #The number of comments in last 24 hours, relative to base date/time
colnames(AnalizowaneDane)[32]<-"CC3" #The number of comments in last 48 to last 24 hours relative to base date/time
colnames(AnalizowaneDane)[33]<-"CC4" #The number of comments in the first 24 hours after the publication of post but before base date/time
colnames(AnalizowaneDane)[34]<-"CC5" #The difference between CC2 and CC3
colnames(AnalizowaneDane)[35]<-"Base time" #Selected time in order to simulate the scenario
colnames(AnalizowaneDane)[36]<-"Post length" #Character count in the post
colnames(AnalizowaneDane)[37]<-"Post share count" #This features counts the no. of shares of the post, that how many peoples had shared this post on to their timeline
colnames(AnalizowaneDane)[38]<-"Post promotion status" #To reach more people with posts in News Feed, individual promote their post and this features tells that whether the post is promoted(1) or not(0)
colnames(AnalizowaneDane)[39]<-"H Local" #This describes the H hrs, for which we have the target variable/ comments received
colnames(AnalizowaneDane)[40]<-"Weekday Published (Sunday)" #This represents the day on which the post was published
colnames(AnalizowaneDane)[41]<-"Weekday Published (Monday)"
colnames(AnalizowaneDane)[42]<-"Weekday Published (Tuesday)"
colnames(AnalizowaneDane)[43]<-"Weekday Published (Wednesday)"
colnames(AnalizowaneDane)[44]<-"Weekday Published (Thursday)"
colnames(AnalizowaneDane)[45]<-"Weekday Published (Friday)"
colnames(AnalizowaneDane)[46]<-"Weekday Published (Saturday)"
colnames(AnalizowaneDane)[47]<-"Base DateTime weekday (Sunday)" #This represents the day on selected base Date/Time
colnames(AnalizowaneDane)[48]<-"Base DateTime weekday (Monday)"
colnames(AnalizowaneDane)[49]<-"Base DateTime weekday (Tuesday)"
colnames(AnalizowaneDane)[50]<-"Base DateTime weekday (Wednesday)"
colnames(AnalizowaneDane)[51]<-"Base DateTime weekday (Thursday)"
colnames(AnalizowaneDane)[52]<-"Base DateTime weekday (Friday)"
colnames(AnalizowaneDane)[53]<-"Base DateTime weekday (Saturday)"
colnames(AnalizowaneDane)[53]<-"Base DateTime weekday (Saturday)"

#Histogram polubien w zaleznosci od dnia tygodnia
polubienia_dni_tygodnia <- AnalizowaneDane[, c(1, 40:46)]
dane_dlugie <- pivot_longer(polubienia_dni_tygodnia, cols = c(2:8), names_to = "Weekday published", values_to = 'Czy opublikowane w danym dniu')
przefiltrowane_dni_tyg <- dane_dlugie %>% filter(`Czy opublikowane w danym dniu` == 1)
suma_polubien_tyg <- aggregate(przefiltrowane_dni_tyg$`Page Popularity/likes`, by = list(przefiltrowane_dni_tyg$`Weekday published`), FUN = sum)

ggplot(data = suma_polubien_tyg, aes(x = Group.1, y = x)) + 
  geom_col() +
  labs(x = "Dzień tygodnia opublikowania posta", y = "Łączna liczba polubień") +
  ggtitle("Liczba polubień w zależności od dnia tygodnia") +
  scale_y_continuous(labels = comma) +
  theme_minimal()
plot


# Test korelacji
# Przygotowanie danych do testu chi-kwadrat
dzien_tygodnia_likes <- AnalizowaneDane %>%
  select(`Page Popularity/likes`, `Weekday Published (Sunday)`, `Weekday Published (Monday)`, `Weekday Published (Tuesday)`, 
         `Weekday Published (Wednesday)`, `Weekday Published (Thursday)`, `Weekday Published (Friday)`, `Weekday Published (Saturday)`) %>%
  pivot_longer(cols = starts_with("Weekday Published"), names_to = "Weekday", values_to = "Published") %>%
  filter(Published == 1)

# Przeprowadzenie testu chi-kwadrat
table_dzien_likes <- table(dzien_tygodnia_likes$Weekday)
test_chi_kwadrat <- chisq.test(table_dzien_likes)

# Wynik testu
print(test_chi_kwadrat)


top_categories <- AnalizowaneDane %>%
  count(`Page Category`) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  pull(`Page Category`)  

filtered_data <- AnalizowaneDane %>%
  filter(`Page Category` %in% top_categories)

# Obliczanie korelacji dla każdej z top 20 kategorii
correlation_results <- filtered_data %>%
  group_by(`Page Category`) %>%
  summarise(correlation = cor(`Page Popularity/likes`, `Post length`, use = "complete.obs"))

# Wyniki testu korelacji
print(correlation_results)

# Histogram polubien w zaleznosci od tematu
polubienia_od_tematu <- filtered_data %>% 
  group_by(`Page Category`) %>% 
  summarise(suma_polubien = sum(`Page Popularity/likes`, na.rm = TRUE))

ggplot(data = polubienia_od_tematu, aes(x = `Page Category`, y = suma_polubien)) + 
  geom_col(fill = "skyblue") +
  labs(x = "Tematyka posta (1-106)", y = "Łączna liczba polubień") +
  ggtitle("Liczba polubień w zależności od tematyki posta") +
  scale_y_continuous(labels = comma) +
  theme_minimal()
plot


top_categories <- AnalizowaneDane %>%
  count(`Page Category`) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  pull(`Page Category`)  # Extract category numbers into a vector

srednia_dlugosc_posta <- Features_Variant_1 %>%
  filter(`Page Category` %in% top_categories) %>%
  group_by(`Page Category`) %>%
  summarise(srednia_dlugosc = mean(`Post length`, na.rm = TRUE)) %>%
  arrange(`Page Category`)  # Sort by category number


# Plotting the histogram
ggplot(srednia_dlugosc_posta, aes(x = factor(`Page Category`), y = srednia_dlugosc)) +
  geom_col(fill = "skyblue") +
  labs(x = "Kategoria Strony (1-106)", y = "Średnia Długość Posta") +
  ggtitle("Średnia Długość Posta wzgledem kategorii strony") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() 


filtered_data <- Features_Variant_1 %>%
  filter(`Page Category` %in% top_categories)

average_likes <- filtered_data %>%
  group_by(`Page Category`) %>%
  summarise(mean_likes = mean(`Page Popularity/likes`, na.rm = TRUE)) %>%
  arrange(`Page Category`)

# Wyświetlenie wyniku
print(average_likes)

# Wykres średniej liczby polubień w zależności od kategorii posta
ggplot(average_likes, aes(x = factor(`Page Category`), y = mean_likes)) +
  geom_col(fill = "skyblue") +
  labs(x = "Kategoria Strony (1-106)", y = "Średnia Liczba Polubień") +
  ggtitle("Średnia Liczba Polubień w Zależności od Kategorii Strony") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))


Features_Variant_1 <- Features_Variant_1 %>%
  mutate(Weekday_Published = case_when(
    `Weekday Published (Sunday)` == 1 ~ "Sunday",
    `Weekday Published (Monday)` == 1 ~ "Monday",
    `Weekday Published (Tuesday)` == 1 ~ "Tuesday",
    `Weekday Published (Wednesday)` == 1 ~ "Wednesday",
    `Weekday Published (Thursday)` == 1 ~ "Thursday",
    `Weekday Published (Friday)` == 1 ~ "Friday",
    `Weekday Published (Saturday)` == 1 ~ "Saturday",
    TRUE ~ NA_character_
  ))

# Usunięcie niepotrzebnych kolumn z dniami tygodnia
Features_Variant_1 <- Features_Variant_1 %>%
  select(-starts_with("Weekday Published"))

# Ustawienie kolejności dni tygodnia
Features_Variant_1$Weekday_Published <- factor(Features_Variant_1$Weekday_Published, 
                                               levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Sprawdzenie brakujących dni tygodnia
print(table(Features_Variant_1$Weekday_Published))

# Tworzenie histogramu liczby odwiedzin strony w zależności od dnia tygodnia
ggplot(Features_Variant_1, aes(x = Weekday_Published, y = `Page Checkins`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(x = "Dzień Tygodnia", y = "Średnia Liczba Odwiedzin Strony") +
  ggtitle("Średnia Liczba Odwiedzin Strony w Zależności od Dnia Tygodnia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


correlation <- cor(Features_Variant_1$`Page Popularity/likes`, Features_Variant_1$`Post length`, use = "complete.obs")
correlation


ggplot(Features_Variant_1, aes(x = `Post length`, y = `Page Popularity/likes`)) +
  geom_point(color = "skyblue") +
  labs(x = "Długość Posta", y = "Ilość Polubień") +
  ggtitle("Zależność Między Ilością Polubień a Długością Posta") +
  theme_minimal()+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))


correlation_test <- cor.test(Features_Variant_1$`Page Popularity/likes`, Features_Variant_1$`Post length`, method = "pearson")

# Wyświetl wyniki testu korelacji
print(correlation_test)


# Wybierz top 20 kategorii
top_categories <- Features_Variant_1 %>%
  count(`Page Category`) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  pull(`Page Category`)  # Pobierz nazwy kategorii do wektora

# Filtrowanie danych dla top 20 kategorii, które faktycznie występują
filtered_data <- Features_Variant_1 %>%
  filter(`Page Category` %in% top_categories)

# Oblicz średnią ilość polubień dla każdej kategorii
average_likes <- filtered_data %>%
  group_by(`Page Category`) %>%
  summarise(mean_likes = mean(`Page Popularity/likes`, na.rm = TRUE)) %>%
  ungroup()

# Upewnij się, że kategorie są posortowane rosnąco według numerów kategorii
average_likes$`Page Category` <- factor(average_likes$`Page Category`, levels = sort(unique(average_likes$`Page Category`)))

# Wygeneruj histogram
ggplot(average_likes, aes(x = `Page Category`, y = mean_likes)) +
  geom_col(fill = "skyblue") +
  labs(x = "Kategoria Strony", y = "Średnia Ilość Polubień") +
  ggtitle("Średnia Ilość Polubień Względem Kategorii Strony") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   # Obrócenie etykiet osi x dla lepszej czytelności
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))  # Formatowanie osi 

# Oblicz korelację i przeprowadź test
correlation_test <- cor.test(Features_Variant_1$`Page Popularity/likes`, Features_Variant_1$`Post length`, method = "pearson")

# Wyświetl wyniki testu korelacji
print(correlation_test)


# Biblioteki potrzebne do analizy
library(dplyr)
library(ggplot2)

# Obliczenie korelacji między długością posta a liczbą polubień
correlation_test <- cor.test(Features_Variant_1$`Page Popularity/likes`, Features_Variant_1$`Post length`, method = "pearson")

# Wyświetlenie wyników testu korelacji
print(correlation_test)


#WYKRES LICZBY KOM OD DLUGOSCI POSTU

dane_komentarze_dlugosc <- AnalizowaneDane %>%
  filter(!is.na(`Post length`)) %>%
  select(`Post length`, CC1)

dane_komentarze_dlugosc <- dane_komentarze_dlugosc %>%
  mutate(Post_length_category = case_when(
    `Post length` <= 200 ~ "Short",
    `Post length` <= 600 ~ "Medium",
    TRUE ~ "Long"
  ))
mediana_komentarze <- dane_komentarze_dlugosc %>%
  group_by(Post_length_category) %>%
  summarise(mediana_komentarzy = median(CC1))

print(mediana_komentarze)

kruskal_test_result <- kruskal.test(CC1 ~ Post_length_category, data = dane_komentarze_dlugosc)
print(kruskal_test_result)

ggplot(dane_komentarze_dlugosc, aes(x = Post_length_category, y = CC1)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(x = "Długość posta", y = "Liczba komentarzy") +
  ggtitle("Liczba komentarzy w zależności od długości posta") +
  theme_minimal()

ggplot(dane_komentarze_dlugosc, aes(x = Post_length_category, y = CC1)) +
  geom_bar(stat = "summary", fun = "mean", fill = "cyan", color = "black") +
  labs(x = "Długość posta", y = "Średnia liczba komentarzy") +
  ggtitle("Średnia liczba komentarzy w zależności od długości posta") +
  scale_y_continuous(labels = comma) +
  theme_minimal()


dane_kr_dl <- dane_komentarze_dlugosc %>%
  filter(Post_length_category %in% c("Short", "Long"))

ggplot(dane_kr_dl, aes(x = Post_length_category, y = CC1)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(x = "Długość posta", y = "Liczba komentarzy") +
  ggtitle("Liczba komentarzy w zależności od długości posta") +
  theme_minimal()
ggplot(dane_kr_dl, aes(x = Post_length_category, y = CC1)) +
  geom_bar(stat = "summary", fun = "mean", fill = "cyan", color = "black") +
  labs(x = "Długość posta", y = "Średnia liczba komentarzy") +
  ggtitle("Średnia liczba komentarzy w zależności od długości posta") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

test_manna_whitneya <- wilcox.test(CC1 ~ Post_length_category, 
                                   data = dane_kr_dl,
                                   subset = (Post_length_category %in% c("Short", "Long")))
print(test_manna_whitneya)

komentarze_dni_tygodnia <- AnalizowaneDane[, c(40:46, 30)]


dane_dlugie <- pivot_longer(komentarze_dni_tygodnia, 
                            cols = c(1:7), 
                            names_to = 'Weekday published', 
                            values_to = 'Czy opublikowane w danym dniu')

przefiltrowane_dni_tyg <- dane_dlugie %>% 
  filter(`Czy opublikowane w danym dniu` == 1)

suma_kom_tyg <- aggregate(przefiltrowane_dni_tyg$CC1, 
                          by = list(przefiltrowane_dni_tyg$`Weekday published`), 
                          FUN = sum)

colnames(suma_kom_tyg) <- c("Weekday published", "Suma komentarzy")

full_weekdays <- c("Weekday Published (Sunday)", "Weekday Published (Monday)", "Weekday Published (Tuesday)", 
                   "Weekday Published (Wednesday)", "Weekday Published (Thursday)", 
                   "Weekday Published (Friday)", "Weekday Published (Saturday)")

suma_kom_tyg <- suma_kom_tyg %>%
  complete(`Weekday published` = full_weekdays, fill = list(`Suma komentarzy` = 1))

suma_kom_tyg$`Weekday published` <- factor(suma_kom_tyg$`Weekday published`, levels = full_weekdays)

ggplot(data = suma_kom_tyg, aes(x = `Weekday published`, y = `Suma komentarzy`)) + 
  geom_col() +
  labs(x = "Dzień tygodnia opublikowania posta", y = "Łączna liczba komentarzy") +
  ggtitle("Dzień tygodnia a liczba komentarzy") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

anova_result <- aov(CC1 ~ `Weekday published`, data = przefiltrowane_dni_tyg)
summary(anova_result)
comments_per_day <- suma_kom_tyg$`Suma komentarzy`
print(comments_per_day)

data <- przefiltrowane_dni_tyg

anova_result <- aov(CC1 ~ `Weekday published`, data = przefiltrowane_dni_tyg)
str(przefiltrowane_dni_tyg)
# Test Tukey'a HSD na wynikach ANOVA
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
plot(tukey_result)

print(sumowane_dane1)

ggplot(dane_komentarze_dlugosc, aes(x = `Post length`, y = CC1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Długość posta", y = "Liczba komentarzy") +
  ggtitle("Relacja między długością posta a liczbą komentarzy") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

colnames(AnalizowaneDane)[37] <- "Post share count"
colnames(AnalizowaneDane)[30] <- "CC1"


dane <- AnalizowaneDane %>%
  filter(`Post share count` < 50000) %>%
  select(`Post share count`, CC1)

ggplot(dane, aes(x = `Post share count`, y = CC1)) +
  geom_point() +
  labs(x = "Ilość udostępnień", y = "Liczba komentarzy") +
  ggtitle("Liczba komentarzy a liczba udostępnień posta") +
  theme_minimal()

correlation_test <- cor.test(dane$`Post share count`, dane$CC1)
print(correlation_test)


#HIPOTEZA: wariancja liczby komentarzy w sporcie jest większa niz w innych kategoriach

filtered_data <- AnalizowaneDane 

sport_data <- filtered_data %>%
  filter(`Page Category` == 9) %>%
  select(CC1)

inne_kategorie_data <- filtered_data %>%
  filter(`Page Category` != 9) %>%
  select(CC1)

variance_sport <- var(sport_data$CC1)
variance_inne <- var(inne_kategorie_data$CC1)

cat("Wariancja w kategorii Sport:", variance_sport, "\n")
cat("Wariancja w innych kategoriach:", variance_inne, "\n")

comparison_df <- data.frame(
  Category = c("Sport", "Inne kategorie"),
  Variance = c(variance_sport, variance_inne)
)

ggplot(comparison_df, aes(x = Category, y = Variance, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(x = "Kategoria", y = "Wariancja liczby komentarzy",
       title = "Porównanie wariancji liczby komentarzy w kategorii 'druzyny sportowe' z innymi kategoriami") +
  theme_minimal()

ggplot(comparison_df, aes(x = Post_length_category, y = CC1)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(x = "Kategoria", y = "Wariancja liczby komentarzy") +
  ggtitle("Porównanie wariancji") +
  theme_minimal()

filtered_data <- AnalizowaneDane

sport_data <- filtered_data %>%
  filter(`Page Category` == 9) %>%
  select(CC1)

inne_kategorie_data <- filtered_data %>%
  filter(`Page Category` != 9) %>%
  select(CC1)

variance_sport <- var(sport_data$CC1)
variance_inne <- var(inne_kategorie_data$CC1)
x <- variance_inne/variance_sport
print(x)

# F-test
f_test_result <- var.test(sport_data$CC1, inne_kategorie_data$CC1)

print("Test równości wariancji (F-test):")
print(f_test_result)

