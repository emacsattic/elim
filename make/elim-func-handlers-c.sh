#!/bin/sh

echo    "#include \"elim-func-handlers.h\"";
echo    "func_handler handlers[] = ";
echo -n "  {";
FILLER="";
for FILE in "$@";
do
    BN=$(basename $FILE .c);
    FN=$(echo -n $BN | sed -e 's/_/-/g');
    printf "$FILLER { %-15s, _h_elim_%-11s } ,\n" \"$FN\" $BN;
    FILLER="   ";
done;
printf "$FILLER {  %-14s, %-19s } };\n" NULL NULL;
