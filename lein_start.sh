#!/bin/bash

PROJECT_HOME=/home/cclaudiu/MyProgramming/Clojure/Projects/clojure-learning

echo "switching to clojure-learning:: home..."
cd $PROJECT_HOME

echo "starting lein repl..."
lein repl &

