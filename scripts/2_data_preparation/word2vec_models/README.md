# DELEGATES
## Preprocessing and training the word2vec models

Build the docker image:
```bash
docker build src/preprocess/ -t delegates_preprocess:0.1
```

Run the docker container:
```bash
docker run -it -u 1000:1000 --rm --name=delegates_preprocess -v $PWD/src/preprocess:/src \
-v $PWD/data:/data -v $PWD/models:/models delegates_preprocess:0.1
```

Once inside the container, you can extract the content of the raw data with

```bash
python3 make_word2vec_models_delegates.py content
```

then process the text (tokenization, removing stopwords, etc.) with

```bash
python3 make_word2vec_models_delegates.py tokenize
```

and finally train the models with

```bash
python3 make_word2vec_models_delegates.py train --window [number_of_years]
```

This will train the models and save them in the `models` folder.