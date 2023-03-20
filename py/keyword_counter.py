import concurrent.futures
import csv
import multiprocessing
import os
import re

import pypdf

import google_sheet

keywords = []
try:
    with open("run/keywords.txt") as keyword_file:
        keywords = [l.rstrip() for l in keyword_file.readlines()]
except:
    keywords = []

if len(keywords) == 0:
    print("Please enter your keywords in run/keywords.txt.")
    exit()

result_keys = ["uid"] + keywords
compiled_keywords = [re.compile(f"\\b{keyword}\\b", re.IGNORECASE) for keyword in keywords]

def count_keywords_in_text(result, text):
    for keyword, regex in zip(keywords, compiled_keywords):
        result[keyword] += len(re.findall(regex, text))

def count_keywords_in_pdf(entry):
    uid = entry["uid"]
    result = dict(zip(result_keys, [uid] + [0] * len(keywords)))

    try:
        reader = pypdf.PdfReader(f"run/paper_pdf/{uid}.pdf", strict=False)
        for page in reader.pages:
            count_keywords_in_text(result, page.extract_text())
    except Exception as e:
        print(f"Error in uid_{uid}: {e}")

    return result

def run():
    # Get the paper entries with pdf
    results = []
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]
    sheets_with_pdf.sort(key=lambda x: int(x["uid"]))

    print("Keywords:", keywords)
    print(f"Counting keywords in {len(sheets_with_pdf)} papers...")

    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
        for entry, result in zip(sheets_with_pdf, pool.map(count_keywords_in_pdf, sheets_with_pdf)):
            results.append(result)

    with open("run/keyword_count.csv", "w") as f:
        writer = csv.DictWriter(f, fieldnames=result_keys)
        writer.writeheader()
        for result in results:
            writer.writerow(result)

if __name__ == "__main__":
    run()