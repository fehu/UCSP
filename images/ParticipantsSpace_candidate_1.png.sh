basename="ParticipantsSpace_candidate_1"

sh tex2png.sh "$basename.tikz.alone.tex" \
              "\def\stage{$1} \input{$basename.tikz.alone.tex}" \
              "${basename}_$1"
