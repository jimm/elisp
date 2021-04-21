#!/bin/bash
#
# usage: fix-macos.sh [-f]
#
# Note: this script is no longer necessary as of the
# https://emacsformacosx.com/ build of Emacs 27.2-2 (and possibly earlier).
#
# See https://spin.atomicobject.com/2019/12/12/fixing-emacs-macos-catalina/
# This version contains an update mentioned in the comments.

MACOS_DIR=/Applications/Emacs.app/Contents/MacOS
EMACS_EXE_PREFIX=Emacs-x86_64-

if [ "$1" != "-f" ] ; then
    echo "warning: this script isn't necessary for Emacs v27.2-2 and higher"
    echo "re-run with '-f' to ignore this warning and run this script"
    exit 1
fi

if [ ! -d "$MACOS_DIR" ] ; then
    echo error: "$MACOS_DIR" does not exist, exiting
    exit 1
fi

cd "$MACOS_DIR"
emacs_exe="$(/bin/ls -1 ${EMACS_EXE_PREFIX}* | grep -v pdmp | tail -1)"
if [ -z "$emacs_exe" ] ; then
    echo error: No Emacs launcher 'Emacs-x86_64-*' found, exiting
    exit 1
fi

mv Emacs Emacs-launcher
ln -s "$emacs_exe" Emacs
cd ..
rm -rf _CodeSignature
