#!/bin/bash
#
# usage: setup.sh [-d] domain machine
#
# Set up a new Emacs installation the way I like it.
#
# "domain" and "machine" are dirs/subdirs in bootstrap/. If domain is
# missing, then the list of available domains will be printed out. Likewise
# if domain is specified but machine is missing.

cd $(dirname $0)
HERE=$(pwd)
bd=$HERE/bootstrap

backup_if_exists() {
    if [ -h $1 ] ; then         # link
        $debug rm $1
    elif [ -f $1 ] ; then       # regular file
        $debug mv $1 $1.bak
    fi
}

# ==== command line arg handling ====

# echo commands if -d specified.
if [ "$1" = "-d" ] ; then
    debug=echo
    shift
fi

domain=$1
if [ -z "$domain" ] ; then
    echo usage: setup-home domain machine
    echo domains: $(ls $bd)
    exit 1
fi

machine=$2
if [ -z "$machine" ] ; then
    echo usage: setup-home domain machine
    echo machines in domain $domain: $(ls $bd/$domain)
    exit 1
fi

# ==== main ====

# Create ~/.emacs
backup_if_exists ~/.emacs
$debug ln -s $bd/$domain/$machine/dot_emacs ~/.emacs

# Create ~/.emacs.d
$debug mkdir -p ~/.emacs.d
$debug cd ~/.emacs.d

# Link snippits dir
backup_if_exists ~/.emacs.d/snippets
$debug ln -s $HERE/snippets snippets

# Check out elixir-lang mode
if [ ! -d emacs-elixir ] ; then
    which -s git && \
        $debug git clone git@github.com:elixir-lang/emacs-elixir.git
fi
