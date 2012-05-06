#!/bin/sh -e

<!-- TMPL_VAR opts -->

set -e

COMMAND="$1"
ECHO="echo"
CHECK1="ps axo user,pid,ppid,command"
CHECK2="grep $LISP"
CHECK3="grep $PROJECTNAME"

if [ "$1" = "" ] ; then
    $ECHO "Usage $0 start|stop|status"
    exit 1
fi
shift


case $COMMAND in
start)
	if [ $USER = "`$CHECK1|$CHECK2|$CHECK3|grep ' [1] '|awk '{ print $1}'`" ] ; then
	    $ECHO 'already started' $PROJECTNAME
	else
	    $ECHO 'running lisp process'
	    $LISP $PREOPT $PROJECTPATH/$LOADFILE $POSTOPT $PROJECTNAME
	fi
	;;
stop)
	if [ $USER = "`$CHECK1|$CHECK2|$CHECK3|grep ' [1] '|awk '{ print $1}'`" ] ; then
	    $ECHO 'quiting' $PROJECTNAME
	    kill -9 "`$CHECK1|$CHECK2|$CHECK3|grep ' [1] '|awk '{ print $2}'`"
	else
	    $ECHO 'not started yet'
	fi
	;;
status)
	if [ $USER = "`$CHECK1|$CHECK2|$CHECK3|grep ' [1] '|awk '{ print $1}'`" ] ; then
	    $ECHO $PROJECTNAME 'is running pid = ' "`$CHECK1|$CHECK2|$CHECK3|grep ' [1] '|awk '{ print $2}'`"
	else
	    $ECHO $PROJECTNAME 'is not running'
	fi
	;;
*)
	exit 1
esac
