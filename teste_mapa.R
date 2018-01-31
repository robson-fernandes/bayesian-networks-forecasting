library('forecast')
library('plotly')
library('MLmetrics')
library('MAPA')

df = read.table('data/data-sales.csv', header=TRUE, sep=";")

df_train = df[1:84,]
df_test = df[85:90,] 

serieTemporal <- ts(df_train['grupoSanduiche'], start=c(2010, 1), end=c(2016, 12), frequency=12)

mapafit <- mapaest(serieTemporal,paral=2, model = "Z")
forecas <- mapafor(serieTemporal,mapafit, h = 6)
f6 <- forecas$outfor[1:6]

#serieTemporalComp <- decompose(serieTemporal)
#plot(serieTemporalComp)

#serieTemporalSeasonAdj <- serieTemporal - serieTemporalComp$seasonal
#plot(serieTemporalSeasonAdj)

#serieTemporal <- serieTemporalSeasonAdj

# p-value < 0.05 indicates the TS is stationary
adf.test(serieTemporal)

#Ajuste - Modelo ARIMA
fitArima <- elm(serieTemporal, hd = 100, reps = 50)
fitArima
# Previsao do 1 Semestre de 2017
ts_forecast <- forecast::forecast(fitArima, 6)


p <- plot_ly() %>%
  layout(title = "Predição -  Grupo de Bebidas")  %>%
  add_lines(x = time(serieTemporal), y = serieTemporal,
            color = I("purple"), name = "observado",  fill = "tonexty") %>%
  add_ribbons(x = time(ts_forecast$mean), ymin = ts_forecast$lower[, 2], ymax = ts_forecast$upper[, 2],
              color = I("gray95"), name = "95% confiança") %>%
  add_ribbons(x = time(ts_forecast$mean), ymin = ts_forecast$lower[, 1], ymax = ts_forecast$upper[, 1],
              color = I("gray80"), name = "80% confiança") %>%
  add_lines(x = time(ts_forecast$mean), y = ts_forecast$mean, color = I("blue"), name = "predição")

ggplotly(p)


p1 <- plot_ly() %>%
  layout(title = "Predição -  Grupo de Bebidas")  %>%
  add_lines(x = c(1:6), y = df_test$grupoSanduiche,
            color = I("purple"), name = "observado") %>%
  add_lines(x = c(1:6), y = f6, color = I("blue"), name = "predição")

ggplotly(p1)


mape <- MAPE(f6, df_test$grupoSanduiche)
mape * 100
