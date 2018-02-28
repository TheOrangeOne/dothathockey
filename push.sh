#! /bin/bash
SITE_BRANCH=gh-pages

git checkout $SITE_BRANCH
rm *.html
git checkout master

git checkout $SITE_BRANCH
mv build/*.html .
cp today.html index.html # temporary
git add *.html
git commit -m "`date`"
git push origin $SITE_BRANCH
git checkout master
