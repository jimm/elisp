#!/bin/bash
#
# See https://spin.atomicobject.com/2019/12/12/fixing-emacs-macos-catalina/
#
# This version contains an update mentioned in the comments.

cd /Applications/Emacs.app/Contents/MacOS
mv Emacs Emacs-launcher
ln -s Emacs-x86_64-10_14 Emacs
cd /Applications/Emacs.app/Contents/
rm -rf _CodeSignature
