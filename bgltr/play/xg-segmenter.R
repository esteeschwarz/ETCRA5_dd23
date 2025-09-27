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
test_df<-create_test.df("KÖNIG: is mir egal (geht ab.)")
#test_df<-create_test.df("KÖNIG: (im gehen.)")
test_df<-create_test.df("KÖNIG: is mir egal was du sagst.")
#dtest <- xgb.DMatrix(as.matrix(test_df[, .(..features)])) 
dtest <- xgb.DMatrix(data = as.matrix(test_df[,..features]))

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

