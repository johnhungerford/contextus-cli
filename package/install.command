#!/bin/zsh

SCRIPT_PATH="${0:A:h}"
CONTEXTUS_ORIGIN=$SCRIPT_PATH/contextus-cli
CONTEXTUS_TARGET=$HOME/.contextus
CONTEXTUS_BIN=$CONTEXTUS_TARGET/bin
mkdir -p $CONTEXTUS_BIN
cp $CONTEXTUS_ORIGIN $CONTEXTUS_TARGET/contextus
cp $SCRIPT_PATH/documentation.pdf $CONTEXTUS_TARGET/

ZSHRC_LINE="export PATH=\$PATH:$CONTEXTUS_BIN"

if grep -Fxq $ZSHRC_LINE $HOME/.zshrc
then
    echo "PATH correction already added to .zshrc"
else
    echo "$ZSHRC_LINE" >> $HOME/.zshrc
    echo "PATH correction added to .zshrc"
fi
