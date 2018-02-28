#! /bin/bash
SITE_BRANCH=gh-pages

git checkout $SITE_BRANCH
git checkout master

racket dth.rkt

git checkout $SITE_BRANCH
mv build/*.html .
mv today.html index.html # temporary
git add *.html
git commit -m "`date`"
git push origin $SITE_BRANCH
git checkout master
