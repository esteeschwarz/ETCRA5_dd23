library(xgboost)
library(data.table)
library(stringr)
library(gsubfn)
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
train.df<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/dracorTEI/lx/traindf.csv")
train2<-train.df
traindf<-train.df
train2$tokens<-gsub("König","Benderin",train2$tokens)
train2$tokens<-gsub("KÖNIG","KOENIG",train2$tokens)
# increase vector size: replace every 4th character after a word boundary with a random string of random length 
train2$tokens<-gsubfn("^(\\w+[.:] )\\w+", function(x){
  #print(x)
  paste0(x,paste0(letters[sample(26,sample(3:15,1))],collapse = ""))
}, train2$tokens, perl = TRUE)


#train2$tokens<-gsub("^(...)[a-z]",paste0("\\1",letters[sample(26,1)]),train2$tokens)
train3<-rbind(traindf,train2)
traindf<-train3
tokens<-traindf$tokens
tags<-traindf$tags
metadf<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/dracorTEI/lx/metadf-mlx-03.csv")
act<-paste0(unique(metadf$h1),collapse = "|")
scene<-paste0(unique(metadf$h2),collapse = "|")


spk<-"\\w+"
capt<-"^[A-ZÀÖÜÄ]"
allcaps<-"^[A-ZÄÖÜ]{2,}[.:]$"
lc<-"[a-zäöü]"
colon<-"[.:]"
comma<-"[,]"
stage<-"^\\(.+\\.?\\)$"
spk_1<-paste0(spk,"[.:]? ")
#number<-unlist(lapply(strsplit(tokens," "),length)>4)
stage_spk<-paste0(spk,"[.:]? \\(.+\\.?\\)")
stage_spc<-"(\\w+) \\((.+)\\.?\\)( \\w+)?"
#tokens<-"this is a (stage between) text"
is_stage_spc = as.integer(grepl(stage_spc, tokens))
# Features simples
create_feature_matrix <- function(tokens) {
  df <- data.table(
    token = tokens,
    is_capitalized = as.integer(grepl(capt, tokens)),
    is_allcaps = as.integer(grepl(allcaps, tokens)),
    is_lowercase = as.integer(grepl(lc, tokens)),
    is_spk_1 = as.integer(grepl(spk_1, tokens)),
    
    is_colon = as.integer(grepl(colon, tokens)),
    is_comma = as.integer(grepl(comma, tokens)),
    is_number = as.integer(unlist(lapply(strsplit(tokens, " "), length) <2 )),
    len_t = unlist(lapply(strsplit(tokens," "),length)),
    
    is_stage = as.integer(grepl(stage, tokens)),
    is_stage_spk = as.integer(grepl(stage_spk, tokens)),
    is_stage_spc = as.integer(grepl(stage_spc, tokens)),
    len = nchar(tokens),
    is_scene = as.integer(grepl(scene, tokens)),
    is_act = as.integer(grepl(act, tokens))
  )
  
  
  return(as.matrix(df[, ..features]))
}

# df <- data.table(
#   token = tokens,
#   is_capitalized = as.integer(grepl(capt, tokens)),
#   is_allcaps = as.integer(grepl(allcaps, tokens)),
#   is_lowercase = as.integer(grepl(lc, tokens)),
#   is_spk_1 = as.integer(grepl(spk_1, tokens)),
#   is_colon = as.integer(grepl(colon, tokens)),
#   is_comma = as.integer(grepl(comma, tokens)),
#   is_number = as.integer(unlist(lapply(strsplit(tokens," "),length)<2)),
#   len_t = unlist(lapply(strsplit(tokens," "),length)),
#   is_stage = as.integer(grepl(stage, tokens)),
#   is_stage_spk = as.integer(grepl(stage_spk, tokens)),
#   is_stage_spc = as.integer(grepl(stage_spc, tokens)),
#   len = nchar(tokens),
#   label = as.integer(factor(tags)) - 1L,
#   is_scene = as.integer(grepl(scene, tokens)),
#   is_act = as.integer(grepl(act, tokens))
#   
# )
features<-c("is_capitalized","is_allcaps","is_lowercase","is_spk_1","is_colon","is_comma","is_number","len_t","is_stage","is_stage_spk","is_stage_spc","is_scene","is_act","len")

df<-create_feature_matrix(tokens)
sum(is.na(df))
sum(is.na(tags))
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
#dtrain <- xgb.DMatrix(data = as.matrix(df[,..features]), label = df$label)
#dtrain <- xgb.DMatrix(data = df, label = dftr)
y_tags<- as.integer(factor(tags)) - 1L

dtrain <- xgb.DMatrix(df, label = y_tags)
#print(sum(is.na(dtrain)))
#y <- df$label

y<-tags
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
#stage_spc<-"(\\w+) \\((.+)\\.?\\)( \\w+)?"
#tokens<-"this is a (stage between) text"
#is_stage_spc = as.integer(grepl(stage_spc, tokens))
#token<-"KÖNIG: is mir egal (geht ab.)"
create_test.df_dep<-function(token){
  test_df<-data.table(
    token=token,
    is_capitalized = as.integer(grepl(capt, token)),
    is_allcaps = as.integer(grepl(allcaps, token)),
    is_lowercase = as.integer(grepl(lc, token)),
    is_spk_1 = as.integer(grepl(spk_1, token)),
    
    is_colon = as.integer(grepl(colon, token)),
    is_comma = as.integer(grepl(comma, token)),
    is_number = as.integer(unlist(lapply(strsplit(token," "),length)<2)),
    len_t = unlist(lapply(strsplit(token," "),length)),
    
    is_stage = as.integer(grepl(stage, token)),
    is_stage_spk = as.integer(grepl(stage_spk, token)),
    is_stage_spc = as.integer(grepl(stage_spc, token)),
    len = nchar(token),
    is_scene = as.integer(grepl(scene, token)),
    is_act = as.integer(grepl(act, token))
    
  )
}
# test_df<-create_test.df("KÖNIG: is mir egal (geht ab.)")
# test_df<-create_test.df("KÖNIG: (im gehen.)")
# #test_df<-create_test.df("KÖNIG: is mir egal was du sagst.")
# #test_df<-create_test.df("First Act")
# #dtest <- xgb.DMatrix(as.matrix(test_df[, .(..features)])) 
# dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))

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
library(Ckmeans.1d.dp) # nécessaire pour xgb.plot.tree

xgb.plot.tree(model = model, trees = 1) # arbre 0

cat("Label choisi:", label_map[which.max(probs)], "\n")
return(label_map[which.max(probs)])
}

### training
### training
# create_feature_matrix <- function(tokens) {
#   df <- data.table(
#     token = tokens,
#     is_capitalized = as.integer(grepl(capt, tokens)),
#     is_allcaps = as.integer(grepl(allcaps, tokens)),
#     is_lowercase = as.integer(grepl(lc, tokens)),
#     is_spk_1 = as.integer(grepl(spk_1, tokens)),
#     
#     is_colon = as.integer(grepl(colon, tokens)),
#     is_comma = as.integer(grepl(comma, tokens)),
#     is_number = as.integer(unlist(lapply(strsplit(tokens, " "), length) <2 )),
#     len_t = unlist(lapply(strsplit(tokens," "),length)),
#     
#     is_stage = as.integer(grepl(stage, tokens)),
#     is_stage_spk = as.integer(grepl(stage_spk, tokens)),
#     is_stage_spc = as.integer(grepl(stage_spc, tokens)),
#     len = nchar(tokens),
#     is_scene = as.integer(grepl(scene, tokens)),
#     is_act = as.integer(grepl(act, tokens))
#   )
#   
#   
#   return(as.matrix(df[, ..features]))
# }

get.train.model<-function(traindf){
set.seed(42)
n <- nrow(traindf)
train_idx <- sample(1:n, size = round(0.5 * n)) #0.6 not wks.
val_idx <- sample(setdiff(1:n, train_idx), size = round(0.2 * n))
test_idx <- setdiff(1:n, c(train_idx, val_idx))
### add weight
# Create sample weights based on your conditions
df<-traindf
df$weight <- 1.0  # Default weight
df$len<-unlist(lapply(df$tokens,function(x){strsplit(x,"")%>%unlist%>%length}))
# Higher weight for examples that are likely speakers but might be mislabeled
df$weight <- ifelse(
  grepl(allcaps, df$tokens) & grepl(colon, df$tokens) & df$len < 25,
  3.0,  # Triple weight for strong speaker candidates
  df$weight
)

# Higher weight for clear stage directions
df$weight <- ifelse(
  grepl(stage, df$tokens),
  3.5,  # Higher weight for clear stage directions
  df$weight
)
df$weight <- ifelse(
  !grepl("[)(]", df$tokens),
  3.5,  # Higher weight for clear p
  df$weight
)
traindf<-df
train_df <- traindf[train_idx, ]
val_df <- traindf[val_idx, ]
test_df <- traindf[test_idx, ]


X_train <- create_feature_matrix(train_df$tokens)
X_val <- create_feature_matrix(val_df$tokens)
X_test <- create_feature_matrix(test_df$tokens)

y_train <- as.integer(factor(train_df$tags)) - 1L
y_val <- as.integer(factor(val_df$tags)) - 1L
y_test <- as.integer(factor(test_df$tags)) - 1L
w_train <- as.integer(factor(train_df$weight)) - 1L
sum(w_train)
w_val <- as.integer(factor(val_df$weight)) - 1L
sum(w_val)
w_test <- as.integer(factor(test_df$weight)) - 1L
sum(w_test)
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
#print(pred_test)

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

return(list(train=train_df,dval=dval,test=test_df,dtest=dtest,fail=misclassified,model=model))
}

eval.m<-function(train_df,dval,dtest,test_df,misclassified){
#if (length(misclassified) > 10) {
  cat("----- FAIL (loop): ",length(misclassified),"------\n")
  feedback_df <- test_df[misclassified, ]
  # Add these to training set
  train_df <- rbind(train_df, feedback_df)
  # Retrain model with expanded training set
  X_train_new <- create_feature_matrix(train_df$tokens)
  y_train_new <- as.integer(factor(train_df$tags)) - 1L
  y_test <- as.integer(factor(test_df$tags)) - 1L
  
  dtrain_new <- xgb.DMatrix(X_train_new, label = y_train_new)
  watchlist <- list(train = dtrain_new,eval=dval)
  
  model <- xgb.train(
    params,
    dtrain_new,
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
  # library(caret)
  # confusionMatrix(factor(pred_tag, levels = label_map),
  #                 factor(true_tag, levels = label_map))
  # 
  
  # Identify misclassified examples
  misclassified <- which(pred_tag != true_tag)
  
# }

#}
return(list(train=dtrain_new,dval=dval,dtest=dtest,test=test_df,model=model))
}
model.d<-get.train.model(traindf)
cat("----- FAIL (top): ",length(model.d$fail),"------\n")

while (length(model.d$fail)>0) {
  model.d<-eval.m(model.d$train,model.d$dval,model.d$dtest,model.d$test,model.d$fail)
  cat("----- FAIL (while): ",length(model.d$fail),"------\n")
}
model<-model.d$model
###############################
create_test.df<-create_feature_matrix
test_df<-create_test.df("KÖNIG: is mir egal (geht ab.)")
test_df<-create_test.df("KÖNIG: (im gehen.)")
test_df<-create_test.df("KÖNIG: is mir egal was du sagst.")
test_df<-create_test.df("König: is mir egal was du sagst.")
test_df<-create_test.df("First Act")
test_df<-create_test.df("reiner p Akt")
test_df<-create_test.df("(reine stage)")
#test_df<-create_test.df("Second Scene")
# test_df<-create_test.df("1. Szene")
# test_df<-create_test.df("1. Akt")
#dtest <- xgb.DMatrix(as.matrix(test_df[, .(..features)])) 
#dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))



call.model(model,test_df)
ma<-unlist(lapply(train.df$tokens, function(x){
  dtest<-create_test.df(x)
  #dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))
  m<-call.model(model,dtest)
}))
m<-ma==train.df$tags
sum(m)
q<-sum(m)/length(train.df$tokens)
print(q)
train.df$tokens[!m]
#library(Ckmeans.1d.dp) # nécessaire pour xgb.plot.tree
xgb.plot.tree(model = model, trees = 0) # arbre 0

