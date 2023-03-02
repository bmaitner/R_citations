import csv
import os

import requests


# Download the Google Sheet as a CSV file and save it in run/r_citations_sheet.csv
def download():
    sheet_id = "1pYB_oJt-Sx__OKJdmlgEpBmDFLmGLxh9Rddp9qKNooE"

    response = requests.get(f"https://docs.google.com/spreadsheet/ccc?key={sheet_id}&output=csv")
    response.raise_for_status()

    with open("run/r_citations_sheet.csv", "w") as f:
        f.write(response.text)


# Read the Google Sheet from as dictionary, download if not exist
def read():
    if not os.path.exists("run/r_citations_sheet.csv"):
        download()

    result = []

    with open("run/r_citations_sheet.csv") as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            result.append(dict(zip(header, row)))

    return result
