import csv
import os

import requests

cached_sheets = None

# Download the Google Sheet as a CSV file and save it in run/r_citations_sheet.csv
def download():
    sheet_id = "1pYB_oJt-Sx__OKJdmlgEpBmDFLmGLxh9Rddp9qKNooE"

    response = requests.get(f"https://docs.google.com/spreadsheet/ccc?key={sheet_id}&output=csv")
    response.raise_for_status()

    with open("run/r_citations_sheet.csv", "w") as f:
        f.write(response.text)


# Read the Google Sheet from as dictionary, download if not exist
def read():
    global cached_sheets
    if cached_sheets is not None:
        return cached_sheets

    if not os.path.exists("run/r_citations_sheet.csv"):
        download()

    cached_sheets = []

    with open("run/r_citations_sheet.csv") as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            cached_sheets.append(dict(zip(header, row)))

    return cached_sheets
