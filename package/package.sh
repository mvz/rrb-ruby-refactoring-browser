#!/bin/sh

echo "-- RubyRefactoringBrowser package maker --"

# - usage
usage() {
  echo "usage: $0 VERSION [TAG]"
  exit
}
if [ $# = 0 ]; then
  usage
elif [ $1 = "--help" ]; then
  usage
fi
VERSION=$1

# - check directories
if [ -e "rrb" ]; then
  echo "error: file or directory 'rrb' already exists."
  exit
fi
if [ -e "rrb-$VERSION" ]; then
  echo "error: file or directory 'rrb-$VERSION' already exists."
  exit
fi

# - checkout
if [ $# = 1 ]; then
  cvs -z3 -d:pserver:anonymous@rubyforge.org:/var/cvs/rrb export -D now rrb
else
  cvs -z3 -d:pserver:anonymous@rubyforge.org:/var/cvs/rrb export -r $2 rrb
fi

# - move files
cp setup.rb rrb/
mkdir rrb/ext
# - ripper
mv rrb/ripper rrb/ext/
cd rrb/ext/ripper/
touch MANIFEST
bison ripper.y -o ripper.c
ruby create.rb
cp ripper.rb ../../lib/rrb/
cd ../../../
# - reflection
mv rrb/reflection rrb/ext/
touch rrb/ext/reflection/MANIFEST

# - make package
mv rrb rrb-$VERSION
tar zcf rrb-$VERSION.tar.gz rrb-$VERSION
rm -r rrb-$VERSION/
echo "made package 'rrb-$VERSION.tar.gz'."

