# $Id$

RUBY=${RUBY:-ruby}
status=0

$RUBY tools/list-parse-event-ids.rb parse.y | awk '{print "on__" $1}' > /tmp/a
$RUBY test/list-called-events.rb | sort -u > /tmp/b
diff -u /tmp/a /tmp/b | grep '^-on' | sed 's/^-on__//' > /tmp/c
if [ -s /tmp/c ]
then
    cat /tmp/c
    status=1
fi
rm -f /tmp/[abc]
exit $status
