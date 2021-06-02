indice <- nrow(df_lag_log) - 700:nrow(df_lag_log)
df_train_base <- df_lag_log[indice,]
df_test_base <- df_lag_log[-indice,]
df_date <- df_lag_date[-indice,] %>% pull(date)
ts.plot(df_train_temp$conc_od)

auto.arima(df_train_base$conc_od)
sarima_base <-
  sarima(
    df_train_base$conc_od,
    p = 2,
    d = 1,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )
acf2(residuals(sarima_base$fit)^2)
sarima_base_pred <-
  sarima.for(
    df_train_base$conc_od,
    n.ahead = 700,
    p = 2,
    d = 1,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )
rmse_base <- RMSE(sarima_base_pred$pred %>% as.vector(),df_test_base$conc_od)
mae_base <- MAE(sarima_base_pred$pred %>% as.vector(),df_test_base$conc_od)
sse_base <- var(df_test_base$conc_od-sarima_base_pred$pred %>% as.vector())
############################## sans température ###############################


auto.arima(df_train_base$conc_od,xreg= df_train_base[,-c(1:3)] %>% as.matrix)

sarimax_conc_od <-
  sarima(
    df_train_base$conc_od,
    p = 1,
    d = 0,
    q = 1,
    P = 0,
    D = 1,
    Q = 2,
    S = 24,
    xreg = df_train_base[, -c(1:3)] 
  )

acf2(residuals(sarimax_conc_od$fit)^2)

sarimax_pred_2bev <- sarima.for(
  df_train_base$conc_od,
  p = 1,
  d = 0,
  q = 1,
  P = 0,
  D = 1,
  Q = 2,
  S = 24,
  plot.all = FALSE,
  n.ahead = 700,
  newxreg =df_test_base[, -c(1:3)],
  xreg = df_train_base[, -c(1:3)]
)

sse_st <- var(df_test_base$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_st <- RMSE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od)
mae_st <- MAE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od)



plot(sarimax_pred_2bev$pred %>% as.vector(),col='red',type ="l")
lines(df_test_base$conc_od)
lines(sarima_base_pred$pred %>% as.vector(),col="blue")


############################## avec température ###############################
indice <- nrow(df_lag_log_temp) - 700:nrow(df_lag_log_temp)
df_train_temp <- df_lag_log_temp[indice,]
df_test_temp <- df_lag_log_temp[-indice,]
auto.arima(df_train_temp$conc_od,xreg = df_train_temp[,-c(1:3)] %>% as.matrix)
sarimax_conc_od_temp <-
  sarima(
    df_train_temp$conc_od,
    p = 2,
    d = 0,
    q = 0,
    P = 0,
    D = 0,
    Q = 0,
    S = 0,
    xreg = df_train_temp[, -c(1:3)]
  )

sarimax_conc_od_temp 

df_train_temp <- df_train_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))
df_test_temp <- df_test_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))

sarimax_pred_2bev_temp <- sarima.for(
  df_train_temp$conc_od,
  p = 2,
  d = 0,
  q = 0,
  P = 0,
  D = 0,
  Q = 0,
  S = 0,
  plot.all = FALSE,
  n.ahead = 700,
  newxreg = df_test_temp[, -c(1:3)],
  xreg = df_train_temp[, -c(1:3)]
)

sse_temp <- var(df_test_temp$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_temp <- RMSE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)
mae_temp <- MAE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)

plot(sarima_pred_2bev$pred %>% as.vector(),col='red',type ="l",ylim=c(4,10))
lines(sarimax_pred_2bev_temp$pred %>% as.vector(),col='green',type ="l",ylim=c(4,10))
lines(df_test_temp$conc_od)
lines(sarima_base_pred_temp$pred %>% as.vector(),col="blue")

data_lacanauG_abve_20_sarima_pred_1 <- bind_cols(
  ox_sarima = sarima_base_pred$pred,
  ox_sarimax_temp = sarimax_pred_2bev_temp$pred %>% as.vector(),
  ox_sarimax_sans_temp = sarimax_pred_2bev$pred %>% as.vector(),
  conc_od = df_test_temp$conc_od,
  date = df_date
) %>% gather(key = "pred",value="value",-date)

ggplot(data_lacanauG_exve_20_sarima_pred_1,aes(x=date,y=value,color = pred))+geom_line()










indice <- nrow(df_lag_log_temp) - 700:nrow(df_lag_log_temp)
df_train_temp <- df_lag_log_temp[indice,c(1:9)]  
df_test_temp <- df_lag_log_temp[-indice,c(1:9)] 
auto.arima(df_train_temp$conc_od,xreg = df_train_temp[,-c(1:3)] %>% as.matrix)
sarimax_conc_od_temp <-
  sarima(
    df_train_temp$conc_od,
    p = 0,
    d = 1,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0,
    xreg = df_train_temp[, -c(1:3)]
  )

sarimax_conc_od_temp 

df_train_temp <- df_train_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))
df_test_temp <- df_test_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))

sarimax_pred_2bev_temp <- sarima.for(
  df_train_temp$conc_od,
  p = 2,
  d = 0,
  q = 1,
  P = 0,
  D = 0,
  Q = 0,
  S = 0,
  plot.all = FALSE,
  n.ahead = 700,
  newxreg = df_test_temp[, -c(1:3)],
  xreg = df_train_temp[, -c(1:3)]
)

sse_temp <- var(df_test_temp$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_temp <- RMSE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)
mae_temp <- MAE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)
