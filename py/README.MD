# Python Implementation

## Functionality

- Downloads the Google Sheet as csv for doi links
- Scrap the paper from doi links (to html or pdf)
- Analyze by link or keywords and output scores

## Instructions

- Install dependencies
  - `pip install -r dependencies.txt`
- Run the script
  - `python main.py`
  - Use `REFRESH_GOOGLE_SHEET` to refresh the locally cached Google Sheet
  - Use `SKIP_PDF_DOWNLOAD` to skip downloading the pdfs, if they were already downloaded and you wish to skip this slow process
  - Use `SKIP_GITHUB_LINK` to skip Github link search

## TODO
- [x] Downloads the Google Sheet as csv
- [x] Scrap papers with `open_access=1`
  - [x] Attempt to scrap the others because they might also be open access
  - [x] Find the paper on other website (e.g. libkey/Unpaywall)
- [x] Find Github links
  - [x] Check repo code type
- [x] Analyze by links
- [ ] Analyze by keywords
  - [x] Find keywords in the paper

## Credits
- [Unpaywall](https://unpaywall.org/)
- [Requests](https://requests.readthedocs.io/en/master/)
- [pypdf](https://github.com/py-pdf/pypdf)
- [PyCryptodome](https://www.pycryptodome.org/)
- [textdistance](https://github.com/life4/textdistance)