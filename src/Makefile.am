# scripts that should be installed (not distributed by default)
bin_SCRIPTS = csv2sc

# additional files that should be distributed (i.e., included in the tar file)
EXTRA_DIST = csv2sc.scm

csv2sc: csv2sc.scm
	ln -s csv2sc.scm csv2sc

clean-local:
	-rm -f csv2sc

