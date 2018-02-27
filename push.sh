#! /bin/bash
SITE_BRANCH=gh-pages

git checkout $SITE_BRANCH
rm *.html
git checkout master

racket dth.rkt
mv today.html index.html # temporary

git checkout $SITE_BRANCH
git add *.html
git commit -m "`date`"
git push origin $SITE_BRANCH
git checkout master
