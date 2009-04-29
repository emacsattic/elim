#!/bin/sh
# Copyright © 2009 Vivek Dasmohapatra 
echo "#ifndef _ELIM_OPS_H_";
echo "#define _ELIM_OPS_H_";

for dep in "$@";
do
    ops=$(basename $dep);
    echo "#include \"$ops\"";
done;
echo "#endif"
