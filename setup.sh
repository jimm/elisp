#!/bin/bash
#
# usage: setup.sh [-d] domain machine
#
# Set up a new Emacs installation the way I like it.
#
# "domain" and "machine" are dirs/subdirs in bootstrap/. If domain is
# missing, then the list of available domains will be printed out. Likewise
# if domain is specified but machine is missing.

cd "$(dirname "$0")"
HERE="$(pwd)"
bd="$HERE/bootstrap"
newsrc_dir=$dbox/Miscellaneous/newsrc.eld

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
    echo usage: setup.sh domain machine
    echo domains: $(ls $bd)
    exit 1
fi

machine=$2
if [ -z "$machine" ] ; then
    echo usage: setup.sh domain machine
    echo machines in domain $domain: $(ls $bd/$domain)
    exit 1
fi

# ==== main ====

# Create init file
backup_if_exists init.el
$debug ln -s $bd/$domain/$machine/init.el ../init.el

# Link snippets
$debug ln -s $HERE/snippets "../snippets"

# Link newsrc
if [ -f $newsrc_dir ] ; then
    backup_if_exists ~/.newsrc.eld
    $debug ln -s $newsrc_dir ~/.newsrc.eld
fi
