#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"

for f in $(ls *.md | cut -d . -f 1 ); do
    fu=$(echo $f | tr '[:lower:]' '[:upper:]')
    echo $f
    #pos=$(git grep -n $fu . |  grep -v "\[$fu" | grep -v "# ") 
    #echo $fu | sed -i '' -e 's/\n/\|/g'
    #echo $pos
    #line=$(echo $pos | cut -d : -f 1| sed -i '' -e 's/\.md//')
    #echo $line
    #file=$(echo $pos | cut -d : -f 2)
    #echo $file
    #char=$(echo $pos | cut -d : -f 3)
    #echo $char
    #word=$(echo $char | cut -d" " -f 1)
    #if [ $fu .ne. $word ]; then
    #    echo $line
    #fi
    
    #git grep -n $fu . |grep -v "\[$fu" | grep -v "# " |grep -v ":3:"
    git grep -n $fu . |grep -v "\[$fu" | grep -v "# " 
    #git grep -n $fu . |grep -v "\[$fu" | grep -v "# " 
done



