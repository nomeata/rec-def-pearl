function check () {
	aspell -l $1 -p ./spellcheck.$1 -t \
	  --add-tex-command="cref p" \
	  --add-tex-command="cpageref p" \
	  --add-tex-command="Cref p" \
	  --add-tex-command="citep p" \
	  --add-tex-command="citet p" \
	  --add-tex-command="settopmatter p" \
	  --add-tex-command="acmISBN p" \
	  --add-tex-command="citestyle p" \
	  --add-tex-command="usetikzlibrary p" \
	  --add-tex-command="sethscode p" \
	  --add-tex-command="grantnum pp" \
	  -c $2
}
check en_US rec-def-pearl.tex
