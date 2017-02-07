noSuffix=${1%.tex}
name=${noSuffix##*/}

pdfname=$name.pdf

if [ $# -eq 1 ];   then source=$1
                        imgname=$name.png
elif [ $# -eq 3 ]; then source=$2
                        imgname=$3.png
else echo "Wrong number of parameters"
     exit 1
fi

echo "pdflatex -synctex=1 -interaction=nonstopmode \"$source\""
    
pdflatex -synctex=1 \
         -interaction=nonstopmode \
         -output-directory=build \
         "$source"

convert -density 300 build/$pdfname \
        -quality 90  png/$imgname
