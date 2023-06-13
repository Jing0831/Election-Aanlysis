data <- read.csv("projectdata.csv", as.is=T, header = T)

vote16 <- data[-9:-17]

vote16$Trumpwin  <- '.'
vote16 <- na.omit(vote16)
vote16$Trumpwin[which(vote16$percentage16_Donald_Trump>=vote16$percentage16_Hillary_Clinton)] <- "Yes"
vote16$Trumpwin[which(vote16$percentage16_Donald_Trump<vote16$percentage16_Hillary_Clinton)] <- "No"

vote16 <- na.omit(vote16)

table(vote16$Trumpwin)

vote16 <- vote16[-1:-5]

truetrumpwin <- vote16$Trumpwin

summary(vote16)

vote16 <- vote16[,-38]

d_norm <- function(x) { ((x-min(x))/(max(x)-min(x)))}

vote16_norm <- as.data.frame(lapply(vote16, d_norm))
vote16_norm$Trump_Win <- truetrumpwin

vote16_norm$rndnum = runif(3109, 1,100)
vote_train = vote16_norm [ vote16_norm[, "rndnum"] <= 95    ,     ]
vote_test = vote16_norm [vote16_norm[, "rndnum"] > 95   ,     ]

target <- vote_train$Trump_Win
targettest <- vote_test$Trump_Win

vote_train <- vote_train[-38:-39]
vote_test <- vote_test[-38:-39]

library(class)
vote_pred <- knn(vote_train, vote_test, target, k= 55, prob = T)

table(vote_pred, targettest)
table(targettest)
summary(vote_train)
