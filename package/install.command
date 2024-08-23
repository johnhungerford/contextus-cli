#!/bin/zsh

SCRIPT_PATH="${0:A:h}"
CONTEXTUS_ORIGIN=$SCRIPT_PATH/contextus-cli
CONTEXTUS_HOME=$HOME/.contextus
CONTEXTUS_BIN=$CONTEXTUS_HOME/bin
mkdir -p $CONTEXTUS_BIN
CONTEXTUS_TARGET=$CONTEXTUS_BIN/contextus
cp $CONTEXTUS_ORIGIN $CONTEXTUS_TARGET
DOCS_ORIGIN=$SCRIPT_PATH/documentation.pdf
DOCS_TARGET=$CONTEXTUS_HOME/documentation.pdf
cp $DOCS_ORIGIN $DOCS_TARGET

ZSHRC_LINE="export PATH=\$PATH:$CONTEXTUS_BIN"

if grep -Fxq $ZSHRC_LINE $HOME/.zshrc
then
    echo "PATH correction already added to .zshrc"
else
    echo "$ZSHRC_LINE" >> $HOME/.zshrc
    echo "PATH correction added to .zshrc"
fi
