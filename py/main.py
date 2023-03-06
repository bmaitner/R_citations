import sys
import google_sheet
import paper_pdf
import github_link_finder

if "REFRESH_GOOGLE_SHEET" in sys.argv:
    google_sheet.download()

if "SKIP_PDF_DOWNLOAD" not in sys.argv:
    paper_pdf.download()

if "SKIP_GITHUB_LINK" not in sys.argv:
    github_link_finder.run()