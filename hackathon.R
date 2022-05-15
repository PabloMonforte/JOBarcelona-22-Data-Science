# Lo primero que tenemos que hacer es importar los datos con los que construiremos nuestro modelo
# y los datos que tenemos que predecir

data <- read.csv("/Users/pablo/Desktop/Hackatons/Barcelona_bichos/train.csv",header=T,sep=",")
test_oficial <- read.csv("/Users/pablo/Desktop/Hackatons/Barcelona_bichos/test_x.csv",header=T,sep=",")

# Importamos el paquete C50 para poder utiliazr un árbol de decisión. 

if(!require(C50)){
  install.packages('C50', repos='http://cran.us.r-project.org')
  library(C50)
}

# Seleccionamos nuestra variable a predecir, en este caso a "y" y las variables que utilizaremos
# para realizar realizar las predicciones. En este caso nueztra "X" serán todas las variables menos
# la columna X en la que se encuentra el índice.

y <- data[,10]
X <- data[,2:9]

# Construimos nuestro primer modelo

y = as.factor(y)
model <- C5.0(X, y, rules = TRUE)
summary(model)

# Observamos las variables que más influyen a la hora de predecir nuestra variable objetivo son:

# Sensor_alpha_plus
# Sensor_beta
# Hour
# Sensor_gamma
# Sensor_gamma_plus

# Así que construiremos el modelo otra vez pero utilizando solamente estas variables.

X <-  data[, c(2,5,6,7,9)]

# A continuación dividiremos nuestros datos en dos partes para poder crear un modelo y posteriormente 
# evaluar la precisión de este.

split_prop <- 3 
indexes = sample(1:nrow(data), size=floor(((split_prop-1)/split_prop)*nrow(data)))
trainX<-X[indexes,]
trainy<-y[indexes]
testX<-X[-indexes,]
testy<-y[-indexes]

# Entremanos nuestro nuevo modelo con los datos de entrenamiento divididos y utilizando solamente 
# las variables más relevantes.

trainy = as.factor(trainy)
model <- C5.0(trainX, trainy)

# Realizamos las predicciones

predicted_model <- predict( model, testX, type="class" )

# Finalmente comprobamos cuál ha sido el porcenteje de acierto de nuestro modelo.

print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == testy) / length(predicted_model)))

# Nuestro mmodelo tiene un porcentaje de acierto de en torno al 86%

# Vamos a intentar mejorar el porcentaje utilizando adaptative boosting, para esto introduciremos el 
# parametro trials.

trainy = as.factor(trainy)
model <- C5.0(trainX, trainy, trials = 50)

# Realizamos las predicciones

predicted_model <- predict( model, testX, type="class" )

# Comprobamos cuál ha sido el porcenteje de acierto de nuestro modelo.

print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == testy) / length(predicted_model)))

# Vemos que nuestro mmodelo ha mejorado su precisión y que ahora tiene un porcentaje de acierto
# de en torno al 90%

# Por último, vamos a intentar mejorar nuestra precisión a la hora de determinar el tipo de bicho
# utilizando el algoritmo random forest.

if(!require("randomForest")) install.packages("randomForest"); library("randomForest")

# Seleccionamos nuestros datos para realizar las predicciones, aquí seleccionamos tanto la "y" como 
# la "X".

datarf <-  data[, c(2,5,6,7,9,10)]

# Dividiremos otra vez nuestros datos en dos partes para poder crear un modelo y posteriormente 
# evaluar la precisión de este.

split_prop <- 3 
indexes = sample(1:nrow(datarf),size=floor(((split_prop-1)/split_prop)*nrow(datarf)))
trainXrf<-datarf[indexes,]
testXrf<-datarf[-indexes,]

# Entrenamos nuestro modelo especificando la variable que queremos predecir y con que datos la predeciremos

random_forest <- randomForest(as.factor(trainXrf$Insect)~., data=trainXrf)

# Realizamos las predicciones

predicciones <- predict(random_forest, testXrf)

# Por último vamos a evaluar el modelo para ver la precisión de este.

(mc <- with(testXrf,table(predicciones, testXrf$Insect)))
print(sprintf("La precisión del random forest es: %.4f %%",100 * sum(diag(mc)) / sum(mc)))

# Con este algoritmo obtenemos una precisión de acierto de en torno al 90%.

# En este caso utilizaremos el random forest para realizar nuestras predicciones finales, ya que es un 
# algorimtmo que suele ser más preciso que el árbol de decisión, aunque en este caso han demostrado unos
# niveles de precisión bastante similares. 

# Realizamos las predicciones demandadas utilizando los datos de test_x y el random forest.

datarf_oficiales <-  test_oficial[, c(2,5,6,7,9)]
predicciones_finales <- predict(random_forest, datarf_oficiales)

# Finalmente exportamos el resultado de las predicciones

write.csv(predicciones_finales, "results.csv")















