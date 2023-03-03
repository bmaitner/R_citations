# Python Implementation

## Functionality

- Downloads the Google Sheet as csv for doi links
- Scrap the paper from doi links (to html or pdf)
- Analyze by link or keywords and output scores

## TODO
- [x] Downloads the Google Sheet as csv
- [x] Scrap papers with `open_access=1`
  - [x] Attempt to scrap the others because they might also be open access
  - [x] Find the paper on other website (e.g. libkey/Unpaywall)
- [ ] Find Github links
  - [ ] Check repo code type
- [ ] Analyze by links
- [ ] Analyze by keywords

## Credits
- [Requests](https://requests.readthedocs.io/en/master/)
- [Unpaywall](https://unpaywall.org/)
- [pypdf](https://github.com/py-pdf/pypdf)