#!/bin/sh

CONF=/chalmers/sw/sup64/fix_personal_www_rights-1.1/etc
cd /chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2016/course/DAT151
find . -type d                -print0 | xargs -r -0 nfs4_setfacl -S ${CONF}/dir.acl
find . -type f   -perm /ugo+x -print0 | xargs -r -0 nfs4_setfacl -S ${CONF}/xfile.acl
find . -type f ! -perm /ugo+x -print0 | xargs -r -0 nfs4_setfacl -S ${CONF}/file.acl

# EOF
