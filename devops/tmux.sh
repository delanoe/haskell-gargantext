#!/bin/bash

tmux new -d -s gargantext './server' \; \
            split-window -h -d 'cd ./purescript-gargantext ; ./server' \; \
            select-pane -t 1 \; \
            split-window -d    'cd deps/CoreNLP ; ./startServer.sh' \; \
