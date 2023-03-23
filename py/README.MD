# Python Implementation

## Functionality

- Downloads the Google Sheet as csv for doi links
- Scrap the paper from doi links (to html or pdf)
- Analyze by link or keywords and output scores

## Instructions

- Install dependencies
  - `pip install -r dependencies.txt`
- Run the modules
  - `google_sheet.py` for downloading the Google Sheet
  - `paper_pdf.py` for downloading the paper as pdf
  - `github_link_finder.py` for finding the GitHub links
  - `keyword_counter.py` for generating the keyword count
  - `keyword_analyzer.py` for analyzing the keywords

## TODO
- [x] Downloads the Google Sheet as csv
- [x] Scrap papers with `open_access=1`
  - [x] Attempt to scrap the others because they might also be open access
  - [x] Find the paper on other website (e.g. libkey/Unpaywall)
- [x] Find Github links
  - [x] Check repo code type
- [x] Analyze by links
- [x] Analyze by keywords
  - [x] Find keywords in the paper

## Credits
- [Unpaywall](https://unpaywall.org/)
- [Requests](https://requests.readthedocs.io/en/master/)
- [pypdf](https://github.com/py-pdf/pypdf)
- [PyCryptodome](https://www.pycryptodome.org/)
- [textdistance](https://github.com/life4/textdistance)
- [matplotlib](https://matplotlib.org/)
- [pytorch](https://pytorch.org/)