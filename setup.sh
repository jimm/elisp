#!/bin/bash
#
# usage: setup.sh [-d] [-e user-emacs-directory] domain machine
#
# Set up a new Emacs installation the way I like it. Creates a small init.el
# file in the emacs init directory. By default that is ~/.emacs.d; override
# that directory with the `-e` flag.
#
# "domain" and "machine" are dirs/subdirs in bootstrap/. If domain is
# missing, then the list of available domains will be printed out. Likewise
# if domain is specified but machine is missing.

cd "$(dirname "$0")"
HERE="$(pwd)"
user_emacs_dir="~/.emacs.d"
bd="$HERE/bootstrap"
newsrc_dir=$dbox/Miscellaneous/newsrc.eld

usage() {
    cat <<EOS
usage: setup.sh [-d] [-e user-emacs-directory] domain machine

    -d      Debug: print but do not execute commands
    -e dir  Override default user-emacs-directory ~/.emacs.d
EOS
    exit $1
}

backup_if_exists() {
    if [ -h $1 ] ; then         # link
        $debug rm $1
    elif [ -f $1 ] ; then       # regular file
        $debug mv $1 $1.bak
    fi
}

# ==== command line arg handling ====

# echo commands if -d specified.
while getopts "de:" opt ; do
    case $opt in
        d) debug=echo ;;
        e) user_emacs_dir="$OPTARG" ;;
        h) usage 0 ;;
        *) usage 1 ;;
    esac
done
shift $((OPTIND-1))

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
if [ "$debug" = "echo" ] ; then
    echo creating init.el
else
    cat > "$user_emacs_dir/init.el" <<EOS
(load-file "$HERE/bootstrap-init.el")
(bootstrap-init "$domain" "$machine")
EOS
    $debug ln -s $bd/$domain/$machine/init.el ../init.el
fi

# Link newsrc
if [ -f $newsrc_dir ] ; then
    backup_if_exists ~/.newsrc.eld
    $debug ln -s $newsrc_dir ~/.newsrc.eld
fi
