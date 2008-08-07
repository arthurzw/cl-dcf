#!/bin/sh

#
# This script generates the GData compiler.
#
# It requires that install.sh has run successfully.
#
# It accomplishes the following:
# - Launch CMUCL.
# - Load Catharsis.
# - Generate the compiler.
#

# This script has to be executed from its directory.

if ! [ $0 = "./save-core.sh" ]; then
    echo This script has to be executed from its directory;
    exit;
fi

# Run.

cmucl -quiet -eval "(progn (asdf:operate 'asdf:load-op 'catharsis) (save-lisp \"../gdata/codegen/compiler/catharsis.core\") (quit))"


#DIR=`echo $0 | sed -n 's/\(.*\)\/[^\/]*/\1/p'`
#CORE=$1/gdata.core
#shift
#
#echo Building core $CORE...
#
#for SRC in $*
#do
#  if [[ `echo $SRC | grep asd` ]]; then
#    ASDF_PATH=`echo ./$SRC | sed -n 's/\(.*\/\)[^\/]*.asd/\1/p'`
#    ASDF_REGISTRY_CMD=`echo $ASDF_REGISTRY_CMD "(push #p\"$ASDF_PATH\" asdf:*central-registry*)"`
#    ASDF_NAME=`echo ./$SRC | sed -n 's/.*\/\([^\/]*\).asd/\1/p'`
#    LOAD_CMD=`echo $LOAD_CMD "(asdf:operate 'asdf:load-op :$ASDF_NAME)"`
#  fi
#done
#
# $DIR/lisp -quiet -eval "(progn (setf asdf:*central-registry* nil) $ASDF_REGISTRY_CMD $LOAD_CMD (save-lisp \"$CORE\" :init-function (symbol-function (find-symbol \"MAIN\" (find-package \"GDATA\")))) (quit))"
