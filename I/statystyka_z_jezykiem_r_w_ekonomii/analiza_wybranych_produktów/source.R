library(tidyverse)
library(reshape2)
library(lubridate)
#pobranie listy potrzebnych plików
data = list.files(path='data', pattern = '*.csv', full.names="TRUE", recursive="FALSE")
df_list = list()

#tworzenie listy zawierającej dane z poszczególnych plików
for (i in 1:length(data)) {
  dataframe = read.csv(data[[i]], sep=";")
  df_list[[i]] = dataframe
}

#Łączenie listy w jeden zbiorczy dataframe i usuwanie niepotrzebnych danych
df = do.call(rbind, df_list)
df = subset(df, select = -c(Kod, Cena.i.wskaźniki, Jednostka.miary, Atrybut, X))

grouped = group_by(df, Rodzaje.towarów.i.usług, Nazwa)

ordered = grouped[order(grouped$Nazwa, grouped$Rok, grouped$Rodzaje.towarów.i.usług), ]
ordered = transform(ordered, Wartosc = as.numeric(sub(',', '.', as.character(Wartosc))))
ggplot()

for (voivodeship in unique(ordered$Nazwa)) {
  subdata <- ordered[ordered$Nazwa == voivodeship, ]
    products = c();
    meanPrice = c();
    year = c();
  for (product in unique(subdata$Rodzaje.towarów.i.usług)) {
    productSubdata = subdata[subdata$Rodzaje.towarów.i.usług == product, ]
    for (yr in unique(productSubdata$Rok)) {
      data = productSubdata[productSubdata$Rok == yr, ]
      mv = mean(data$Wartosc)
      products = c(products, product)
      year = c(year, yr)
      meanPrice = c(meanPrice, mv)
    }
  }
  data = data.frame(products, year, meanPrice)
  ggplot(data=data, aes(x=year)) + 
    geom_line(aes(y=meanPrice, col=products))
}

#TODO: Labelki, odchylenie standardowe, tytuł wykresu