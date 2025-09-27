library(xgboost)
library(data.table)
library(stringr)

# Petit dataset de tokens
tokens <- c("KÖNIG:", "Bonjour", "LE ROI:", "DAME:","Salut","(küsst die Hand)","KING:","SERVANT.","wir gehen raus jetzt","gehen wir doch, oder bleiben","(gehen raus.)",
            "und der könig geht (König ab.)","KÖNIGIN: (leise.)","und der könig geht (Königin ab.)","KÖNIG: (im gehen.)","gehen wir doch, oder bleiben noch länger","vielleicht aber gehen wir doch, oder bleiben","KÖNIGIN:", "Bonjour le queen", "LE KÖNIGIN:", "DAME 2:","Salut, madame","(küsst ihr die Hand)","KING A:","SERVANTES.","wir gehen raus jetzt aber wirklich","gehen wir doch, oder bleiben wir noch ein biszchen","(gehen jetzt wirklich raus.)",
            "und der könig geht (König ab.)","KÖNIGIN: (leise.)","und der könig geht (Königin ab.)","KÖNIG: (im gehen.)","gehen wir doch, oder bleiben noch länger","vielleicht aber gehen wir doch, oder bleiben","und der king ab (König geht.)","DAME: (jetzt laut.)","da geht er hin der könig geht (König ab.)","da geht er hin der könig geht (König ab.)","da geht er hin der könig geht (König ab.)")
tags <- c("speaker", "p", "speaker", "speaker", "p","stage","speaker","speaker","p","p","stage","speaker", "p", "speaker", "speaker", "p","stage","speaker","speaker","p","p","stage",
          "stage_spc","stage_spk","stage_spc","stage_spk","p","p",
          "stage_spc","stage_spk","stage_spc","stage_spk","p","p",
          "stage_spc","stage_spk","stage_spc","stage_spk","stage_spc")

#traindf<-data.frame(tokens,tags)
#train2<-fix(traindf)
#write.csv(train2,"~/Documents/GitHub/ETCRA5_dd23/bgltr/dracorTEI/lx/traindf.csv",row.names = F)
traindf<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/dracorTEI/lx/traindf.csv")
train2<-traindf
train2$tokens<-gsub("König","Bender",train2$tokens)
train2$tokens<-gsub("^(...)[a-z]",paste0("\\1",letters[sample(26,1)]),train2$tokens)
train3<-rbind(traindf,train2)
traindf<-train3
tokens<-traindf$tokens
tags<-traindf$tags


spk<-"Iwanette"
capt<-"^[A-ZÀ-Ö]"
allcaps<-"^[A-Z0-9]+\\w+$"
lc<-"[a-z]"
colon<-"[.:]$"
comma<-"[,]"
stage<-"^\\(.+\\.?\\)$"
number<-unlist(lapply(strsplit(tokens," "),length)>4)
stage_spk<-paste0(spk,"[.:]? \\(.+\\.?\\)")
stage_spc<-"(\\w+) \\((.+)\\.?\\)( \\w+)"
#tokens<-"this is a (stage between) text"
is_stage_spc = as.integer(grepl(stage_spc, tokens))
# Features simples
df <- data.table(
  token = tokens,
  is_capitalized = as.integer(grepl(capt, tokens)),
  is_allcaps = as.integer(grepl(allcaps, tokens)),
  is_lowercase = as.integer(grepl(lc, tokens)),
  is_colon = as.integer(grepl(colon, tokens)),
  is_comma = as.integer(grepl(comma, tokens)),
  is_number = as.integer(unlist(lapply(strsplit(tokens," "),length)>4)),
  is_stage = as.integer(grepl(stage, tokens)),
  is_stage_spk = as.integer(grepl(stage_spk, tokens)),
  is_stage_spc = as.integer(grepl(stage_spc, tokens)),
  len = nchar(tokens),
  label = as.integer(factor(tags)) - 1L
)
features<-c("is_capitalized","is_allcaps","is_lowercase","is_colon","is_comma","is_number","is_stage","is_stage_spk","is_stage_spc","len")
# Matrice XGBoost
# X <- as.matrix(df[, .(is_capitalized, 
#                       is_allcaps, 
#                       is_lowercase,
#                       is_colon,
#                       is_comma,
#                       is_number,
#                       is_stage,
#                       is_stage_spk,
#                       is_stage_spc,
#                       len)])
#X <- as.matrix(df[, .(..features)])
dtrain <- xgb.DMatrix(data = as.matrix(df[,..features]), label = df$label)
print(sum(is.na(df$label)))
y <- df$label

y
# Entraînement XGBoost
#dtrain <- xgb.DMatrix(data = X, label = y)
params <- list(objective = "multi:softprob", num_class = length(unique(y)), eval_metric = "mlogloss")
model <- xgb.train(params, dtrain, nrounds = 20, verbose = 0)



# Exporter le premier arbre en texte
xgb.dump(model, with_stats = TRUE)#[1:100]

library(Ckmeans.1d.dp) # nécessaire pour xgb.plot.tree
#library(xgboost)

xgb.plot.tree(model = model, trees = 0) # arbre 0

# test_df <- data.table(
#   token = "PRINCE:",
#   is_capitalized = 1,
#   is_allcaps = 1,
#   is_lowercase=0,
#   is_colon=1,
#   is_comma =0,
#   is_cleft = 1,
#   is_number =0,
#   len = nchar("PRINCE (singt.):")
# )
create_test.df<-function(token){
  test_df<-data.table(
    token=token,
    is_capitalized = as.integer(grepl(capt, token)),
    is_allcaps = as.integer(grepl(allcaps, token)),
    is_lowercase = as.integer(grepl(lc, token)),
    is_colon = as.integer(grepl(colon, token)),
    is_comma = as.integer(grepl(comma, token)),
    is_number = as.integer(unlist(lapply(strsplit(token," "),length)>4)),
    is_stage = as.integer(grepl(stage, token)),
    is_stage_spk = as.integer(grepl(stage_spk, token)),
    is_stage_spc = as.integer(grepl(stage_spc, token)),
    len = nchar(token)
    
  )
}
# test_df<-create_test.df("KÖNIG: is mir egal (geht ab.)")
# test_df<-create_test.df("KÖNIG: (im gehen.)")
# #test_df<-create_test.df("KÖNIG: is mir egal was du sagst.")
# #test_df<-create_test.df("First Act")
# #dtest <- xgb.DMatrix(as.matrix(test_df[, .(..features)])) 
# dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))

                                 # 
                                           # dtest <- xgb.DMatrix(as.matrix(test_df[, .(is_capitalized, 
                                           # is_allcaps, 
                                           # is_lowercase,
                                           # is_colon,
                                           # is_comma,
                                           # 
                                           # is_number,
                                           # is_cleft,
                                           # len)]))


call.model<-function(model,dtest){
pred <- predict(model, dtest)
pred

# softmax <- function(x) {
#   exp_x <- exp(x - max(x))   # stabilité numérique
#   exp_x / sum(exp_x)
# }

probs <- pred
label_map <- levels(factor(tags))

names(probs) <- label_map
print(probs)
cat("Label choisi:", label_map[which.max(probs)], "\n")
}

### training
### training
get.train.model<-function(traindf){
set.seed(42)
n <- nrow(traindf)
train_idx <- sample(1:n, size = round(0.6 * n))
val_idx <- sample(setdiff(1:n, train_idx), size = round(0.2 * n))
test_idx <- setdiff(1:n, c(train_idx, val_idx))
### add weight
# Create sample weights based on your conditions
df<-traindf
df$weight <- 1.0  # Default weight
df$len<-unlist(lapply(df$tokens,function(x){strsplit(x,"")%>%unlist%>%length}))
# Higher weight for examples that are likely speakers but might be mislabeled
df$weight <- ifelse(
  grepl(allcaps, df$token) & grepl(colon, df$token) & df$len < 25,
  3.0,  # Triple weight for strong speaker candidates
  df$weight
)

# Higher weight for clear stage directions
df$weight <- ifelse(
  grepl(stage, df$token) & grepl("^\\(.+\\)$", df$token),
  2.5,  # Higher weight for clear stage directions
  df$weight
)
traindf<-df
train_df <- traindf[train_idx, ]
val_df <- traindf[val_idx, ]
test_df <- traindf[test_idx, ]

create_feature_matrix <- function(tokens) {
  df <- data.table(
    token = tokens,
    is_capitalized = as.integer(grepl(capt, tokens)),
    is_allcaps = as.integer(grepl(allcaps, tokens)),
    is_lowercase = as.integer(grepl(lc, tokens)),
    is_colon = as.integer(grepl(colon, tokens)),
    is_comma = as.integer(grepl(comma, tokens)),
    is_number = as.integer(unlist(lapply(strsplit(tokens, " "), length) > 4)),
    is_stage = as.integer(grepl(stage, tokens)),
    is_stage_spk = as.integer(grepl(stage_spk, tokens)),
    is_stage_spc = as.integer(grepl(stage_spc, tokens)),
    len = nchar(tokens)
  )
  
 
  return(as.matrix(df[, ..features]))
}

X_train <- create_feature_matrix(train_df$tokens)
X_val <- create_feature_matrix(val_df$tokens)
X_test <- create_feature_matrix(test_df$tokens)

y_train <- as.integer(factor(train_df$tags)) - 1L
y_val <- as.integer(factor(val_df$tags)) - 1L
y_test <- as.integer(factor(test_df$tags)) - 1L
w_train <- as.integer(factor(train_df$weight)) - 1L
w_val <- as.integer(factor(val_df$weight)) - 1L
w_test <- as.integer(factor(test_df$weight)) - 1L

dtrain <- xgb.DMatrix(X_train, label = y_train,weight=w_train)
# Apply weights in XGBoost
# dtrain <- xgb.DMatrix(
#   data = as.matrix(df[, ..features]), 
#   label = df$label,
#   weight = df$weight  # This is the key parameter
# )
dval <- xgb.DMatrix(X_val, label = y_val,weight=w_val)
dtest <- xgb.DMatrix(X_test, label = y_test,weight=w_test)

params <- list(
  objective = "multi:softprob",
  num_class = length(unique(y_train)),
  eval_metric = "mlogloss"
)

watchlist <- list(train = dtrain, eval = dval)

model <- xgb.train(
  params,
  dtrain,
  nrounds = 100,
  watchlist = watchlist,
  early_stopping_rounds = 10,
  verbose = 1
)

pred_test <- predict(model, dtest, reshape = TRUE)
pred_labels <- max.col(pred_test) - 1L

# Map back to tag names
label_map <- levels(factor(traindf$tags))
pred_tag <- label_map[pred_labels + 1L]
true_tag <- label_map[y_test + 1L]

# Confusion matrix
library(caret)
confusionMatrix(factor(pred_tag, levels = label_map),
                factor(true_tag, levels = label_map))


# Identify misclassified examples
misclassified <- which(pred_tag != true_tag)
if (length(misclassified) > 0) {
  feedback_df <- test_df[misclassified, ]
  # Add these to training set
  train_df <- rbind(train_df, feedback_df)
  # Retrain model with expanded training set
  X_train_new <- create_feature_matrix(train_df$tokens)
  y_train_new <- as.integer(factor(train_df$tags)) - 1L
  dtrain_new <- xgb.DMatrix(X_train_new, label = y_train_new)
  
  model <- xgb.train(
    params,
    dtrain_new,
    nrounds = 100,
    watchlist = watchlist,
    early_stopping_rounds = 10,
    verbose = 1
  )
}

}

test_df<-create_test.df("KÖNIG: is mir egal (geht ab.)")
#test_df<-create_test.df("KÖNIG: (im gehen.)")
test_df<-create_test.df("KÖNIG: is mir egal was du sagst.")
test_df<-create_test.df("König: is mir egal was du sagst.")
test_df<-create_test.df("First Act")
test_df<-create_test.df("Second Scene")
#dtest <- xgb.DMatrix(as.matrix(test_df[, .(..features)])) 
dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))

model<-get.train.model(traindf)
call.model(model,dtest)

