# libraries
import argparse
import pandas as pd
from tqdm import tqdm
from numpy import linspace
import os
from nltk import word_tokenize
import nltk
import string
import numpy as np
import sys
import csv

current_directory = os.getcwd()
print("Current Working Directory:", current_directory)

# Check if system is 64 or 32 bit
is_64bits = sys.maxsize > 2**30 - 1 

if is_64bits:
    csv.field_size_limit(2**30 - 1)
else:
    csv.field_size_limit(2**31 - 1)

from collections import defaultdict
from gensim import corpora
import gensim 
print('Gensim version: ', gensim.__version__) # 4.3.1

def get_content(args):
    df = pd.read_csv('raw/Letters of Delegates.csv')
    #del df['EEBO'], df['VID'], df['STC'], df['Status'], df['Terms'], df['Pages'], df['Author']
    df.rename(columns={'ID': 'TCP'}, inplace=True)
    start_index = 0
    end_index = df.shape[0] - 1
    df['Content'] = ''
    for index in tqdm(linspace(start_index, end_index - 1, end_index - start_index)):
        folder = f"raw/{df.loc[index, 'TCP'][3:5]}"
        file = f"{df.loc[index, 'TCP']}.txt"
        with open(f'{folder}/{file}', 'r') as f:
            content = f.read()
        df.loc[index, 'Content'] = content
    #years = list(set(df['Year']))[1:]
    years = list(set(df['Year']))
    for year in years[start_index:end_index]:
        df_data = df[df['Year'] == year]
        df_data.to_csv(f'processed/content/{year}.csv')
         
def split_into_sentences(text):
    text = text.replace('—', ' -- ')
    return nltk.tokenize.sent_tokenize(text)

# Split each sentence into words. Careful to change the language to the one used at the top of the script:
def split_into_words(text):
    """
        text:   list of strings
        return: list of lists of strings
    """
    document = []
    for sentence in text:
        document.append(word_tokenize(text=sentence, language='english'))
    return document

# Remove stopwords and punctuation marks. Again, pay attention to the language:
def remove_stopwords_and_punctuation(text, stopwords):
    """
        text:   list of lists of strings
        return: list of lists of strings
    """
    document = []
    for sentence in text:
        words = word_tokenize(' '.join(sentence))
        filtered_words = [''.join(char for char in word if char not in ['[', ']']).lower() for
                          word in words if word.lower() not in stopwords and
                          # does string.punctuation work?
                          word.lower() not in string.punctuation]
        document.append(filtered_words)
        # for word_id in range(len(sentence)):
        #     word = sentence[word_id]
        #     sentence[word_id] = ''.join(char for char in word if char not in ['[', ']'])
        # document.append([new_word.lower() for new_word in sentence if
        #                  not new_word.lower() in stopwords
        #                  and new_word not in string.punctuation])
    return document


# Join the remaining words into sentences, and sentences into documents
def join_words(text):
    """
        text:   list of lists of strings
        return: string
    """
    document = []
    for sentence in text:
        document.append(" ".join(sentence))
    return ". ".join(document)

def tokenize(args):
    input_path = 'processed/content'
    output_path = 'processed/tokenized'
    input_files = sorted(os.listdir(input_path))
    for input_file in tqdm(input_files):
        print(f"Processing year {input_file[:-4]}")
        df = pd.read_csv(f'{input_path}/{input_file}', index_col=0, header=0,
                         names=['TCP', 'Year', 'Title', 'Content'],
                         dtype={'Year': int, 'Content': str})
        corpus = df['Content']
        corpus.dropna(inplace=True)
        corpus = corpus.apply(lambda x: split_into_sentences(x))
        corpus = corpus.apply(lambda x: split_into_words(x))
        stop_words = nltk.corpus.stopwords.words('english')
        corpus = corpus.apply(lambda x: remove_stopwords_and_punctuation(x, stop_words))
        corpus = corpus.apply(lambda x: join_words(x))

        # Save the corpus as a new column in the DataFrame:
        df['Corpus'] = corpus

        # Save all documents from a particular year in the appropriate file:
        df['Corpus'].to_csv(f'{output_path}/{input_file}', header=False)


# def tokenize(text):
#     document = []
#     for sentence in text:
#         document.append(re.split('\W+', sentence.lower()))
#     return document


# def get_csv_files(path):
#     for item in os.listdir(path):
#         if not item.startswith('sensors'):
#             full_item = os.path.join(path, item)
#             if os.path.isfile(full_item) and full_item.endswith("csv"):
#                 yield full_item
#             elif os.path.isdir(full_item):
#                 yield from get_csv_files(full_item)
#             else:
#                 print("found non-csv file")

def get_min_max_year():
    """Retrieves the first and last year in the data set."""
    root_folder = os.path.abspath(f'processed/tokenized')
    files = os.listdir(root_folder)
    years = [int(files[i].split('.')[0]) for i in range(len(files))]
    return np.array(years).min(), np.array(years).max()


def get_sentences_for_year(year):
    """
    Return list of lists of strings.
    Return list of sentences in given year.
    Each sentence is a list of words.
    Each word is a string."""
    sentences = []

    file_name = f"processed/tokenized/{year}.csv"
    #csv.field_size_limit(sys.maxsize)
    if not os.path.isfile(file_name):
        print(f"Year {year} does not exist")
        return []
    with open(file_name, 'rt') as file:
        try:
            text = csv.reader(file)
        except:
            print(text)
        for line in text:
            line = line[1]
            sentence_list = line.split('. ')
            for sentence in sentence_list:
                list_of_words = sentence.split(" ")
                sentences.append(list_of_words)

    return sentences


def get_sentences_in_range(start_y, end_y):
    """Return list of lists of strings.
    Return list of sentences in given year.
    Each sentence is a list of words.
    Each word is a string."""
    return [s for year in range(start_y, end_y)
            for s in get_sentences_for_year(year)]


def train(args):
    #model_folder = '../../Shico_delegates'
    model_folder = 'models'
    years_in_model = 3
    #years_in_model = int(args.window) if args.window else 5
    step_years = 1
    y_0, y_n = get_min_max_year()
    for year in tqdm(range(y_0, y_n - years_in_model + 1, step_years)):
        start_y = year
        end_y = year + years_in_model
        model_name = f'{model_folder}/{year}_{year + years_in_model}.w2v'

        print('Building model: ', model_name)

        sentences = get_sentences_in_range(start_y, end_y)

        frequency = defaultdict(int)
        for sentence in sentences:
            for word in sentence:
                frequency[word] += 1

        processed_corpus = [[word.lower() for word in sentence if frequency[word] > 1] for sentence in sentences]
        dictionary = corpora.Dictionary(processed_corpus)
        dictionary.save('letters.dict')
        bow_corpus = [dictionary.doc2bow(text) for text in processed_corpus]

        model = gensim.models.Word2Vec(min_count=5, workers=4)
        model.build_vocab(processed_corpus)
        try:
            model.train(processed_corpus, total_examples=model.corpus_count, epochs=model.epochs)
        except RuntimeError as e:
            continue

        print('...saving')
        model.wv.save_word2vec_format(model_name, binary=True)

def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(help='sub-command help')

    parser_content = subparsers.add_parser('content',
                                          help='extract content from raw online data')
    parser_content.set_defaults(func=get_content)

    parser_tokenize = subparsers.add_parser('tokenize',
                                          help='tokenize documents')
    parser_tokenize.set_defaults(func=tokenize)

    parser_train = subparsers.add_parser('train', help='train word2vec models')
    parser_train.add_argument("-w", "--window", action="store", dest="window",
                            help="window (in years) for a model (default: 5)")
    parser_train.set_defaults(func=train)


    args = parser.parse_args()
    try:
        args.func(args)
    except AttributeError:
        parser.print_help()


if __name__ == '__main__':
    main()
