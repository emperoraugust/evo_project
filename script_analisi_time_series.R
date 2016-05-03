if (!require("forecast")) install.packages("forecast")
library(forecast)

dir.create(file.path(getwd(), "Figures"), showWarnings = FALSE)

value <- 1:nrow(store_master)
value <- value[value!=8]
for(i in value){
    assign("data",get(paste("data_store",i,sep="")))
    vec<-colnames(data)[4:ncol(data)]
    par(mfcol=c(4,2))

    for(col in vec){
        time_series<-ts(data[[col]],start = 1, frequency = 1)
        plot(time_series,xlab = "week",ylab = col)
        fit <- tslm(time_series ~ trend)
        lines(fitted(fit),col="blue")
    }

    dev.copy(pdf,paste(paste("Figures/ts_store",i, sep=""),"pdf",sep="."))
    dev.off()
}
#Analizzando in maniera molto rude le serie temporali e guardando il loro grafico
# e sapendo il loro significato scelto tra queste variabili come predittori
# avg_price, avg_markdown, traffic