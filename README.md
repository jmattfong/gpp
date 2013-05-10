### CS388 Final Project

## What is it?

This the final project for CS388 (Natural Language Processing) by Hassaan Markhiani and Matt Fong. It attempts to predict the return of a stock based on news articles.

## To run:

First, build the project:

    ./build package
    
Then, run:

    ./bin/gpp exp --train data/nytimes/up_down_articles_full_1day_train.xml --eval data/nytimes/up_down_articles_full_1day_test.xml --cost 0.1
