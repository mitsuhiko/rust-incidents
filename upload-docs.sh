#!/bin/bash

# Make a new repo for the gh-pages branch
rm -rf .gh-pages
mkdir .gh-pages
cd .gh-pages
git init

# Copy over the documentation
cp -r ../target/doc/* .
cat <<EOF > index.html
<!doctype html>
<title>incidents</title>
<meta http-equiv="refresh" content="0; ./incidents/">
EOF

# Add, commit and push files
git add -f --all .
git commit -m "Built documentation"
git co -b gh-pages
git remote add origin git@github.com:mitsuhiko/rust-incidents.git
git push -qf origin gh-pages

# Cleanup
cd ..
rm -rf .gh-pages
