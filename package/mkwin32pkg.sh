if [ -z "$1" ]; then
	echo "Usage: $0 VERSION"
	exit 1
fi

pkgdir="rrb-$1-mswin32"
mkdir $pkgdir
mkdir $pkgdir/lib
mkdir $pkgdir/lib/rrb
mkdir $pkgdir/ext/
mkdir $pkgdir/bin
mkdir $pkgdir/elisp

cp ../lib/rrb/*.rb $pkgdir/lib/rrb
cp ../ext/ripper/rrb_ripper.so $pkgdir/ext
cp ../ext/reflection/rrb_reflection.so $pkgdir/ext
cp ../elisp/*.el $pkgdir/elisp
cp -r ../doc $pkgdir
cp ../README.* ../NEWS.* $pkgdir
cp win32bin/*.exe $pkgdir/bin

zip -r rrb-$1-mswin32.zip $pkgdir

rm -r $pkgdir
