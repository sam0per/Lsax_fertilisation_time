Code adjusted from this [source](https://gist.github.com/maxogden/97190db73ac19fc6c1d9beee1a6e4fc8)

Once you have `.md` and `.bib` files you can generate a PDF like this:  
```
pandoc --filter pandoc-citeproc --bibliography=FLs_references.bib --variable classoption=twocolumn --variable papersize=a4paper -s README.md -o FLs_readme.pdf
```

Or generate the intermediate Latex source like this:  
```
pandoc --filter pandoc-citeproc --bibliography=FLs_references.bib --variable classoption=twocolumn --variable papersize=a4paper -s README.md -t latex -o FLs_readme.txt
```
