#!/bin/sh

echo "#ifndef _EMACSIM_HANDLER_LIST_H_";
echo "#define _EMACSIM_HANDLER_LIST_H_";
for dep in "$@"; do echo "#include \"$dep\""; done;
echo "#endif";

