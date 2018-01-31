library('forecast')
library('plotly')
library('MLmetrics')
library(prophet)

df = read.table('data/data-sales.csv', header=TRUE, sep=";")

df_train = df[1:84,]
df_test = df[85:90,] 

serieTemporal <- ts(df_train['grupoAcompanhamento'], start=c(2010, 1), end=c(2016, 12), frequency=12)


ds = as.Date(df_train$mes) 
y =  df_train$grupoAcompanhamento

df_train_prophet <- data.frame(ds, y)

m <- prophet(df_train_prophet, yearly.seasonality = TRUE, mcmc.samples = 23)

future <- make_future_dataframe(m, periods = 6, freq = 'month')
future
tail(future)

forecast <- predict(m, future)
ts_forecast <- tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


# Previsao do 1 Semestre de 2017
ts_forecast <- ts_forecast[1:6,'yhat']


p1 <- plot_ly() %>%
  layout(title = "Predição -  Grupo de Bebidas")  %>%
  add_lines(x = c(1:6), y = df_test$grupoAcompanhamento,
            color = I("purple"), name = "observado") %>%
  add_lines(x = c(1:6), y = ts_forecast, color = I("blue"), name = "predição")

ggplotly(p1)


mape <- MAPE(ts_forecast, df_test$grupoAcompanhamento)
mape * 100