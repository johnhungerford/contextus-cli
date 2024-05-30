#!/bin/zsh

SCRIPT_PATH="${0:A:h}"
CONTEXTUS_ORIGIN=$SCRIPT_PATH/contextus-cli
CONTEXTUS_BIN=$HOME/.contextus/bin
CONTEXTUS_TARGET=$CONTEXTUS_BIN/contextus
mkdir -p $CONTEXTUS_BIN
cp $CONTEXTUS_ORIGIN $CONTEXTUS_TARGET

ZSHRC_LINE="export PATH=\$PATH:$CONTEXTUS_BIN"

if grep -Fxq $ZSHRC_LINE $HOME/.zshrc
then
    echo "PATH correction already added to .zshrc"
else
    echo "$ZSHRC_LINE" >> $HOME/.zshrc
    echo "PATH correction added to .zshrc"
fi
