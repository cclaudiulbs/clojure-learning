#!/bin/bash

java -Xmx4000M -Xss100M -cp $CLOJURE_BIN/clojure-1.8.0.jar clojure.main "$@" -r
