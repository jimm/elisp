#!/bin/bash
#
# See https://spin.atomicobject.com/2019/12/12/fixing-emacs-macos-catalina/
#
# This version contains an update mentioned in the comments.

MACOS_DIR=/Applications/Emacs.app/Contents/MacOS
EMACS_EXE_PREFIX=Emacs-x86_64-

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
