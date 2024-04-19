from transformers import AutoModel, AutoTokenizer

tokenizer = AutoTokenizer.from_pretrained('dicta-il/dictabert-morph')
model = AutoModel.from_pretrained('dicta-il/dictabert-morph', trust_remote_code=True)

model.eval()

sentence = 'בשנת 1948 השלים אפרים קישון את לימודיו בפיסול מתכת ובתולדות האמנות והחל לפרסם מאמרים הומוריסטיים'
print(model.predict([sentence], tokenizer))


