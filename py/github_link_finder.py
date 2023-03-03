import concurrent.futures
import multiprocessing
import os
import re
import csv

import pypdf

import google_sheet


def find_links_in_str(s):
    return list(re.findall(r"\b\S*github\.com/\S+", s))


def find_links_in_pdf(entry):
    try:
        uid = entry["uid"]
        file_path = f"run/paper_pdf/{uid}.pdf"

        if not os.path.exists(file_path): return []

        reader = pypdf.PdfReader(file_path, strict=True)
        urls = []
        for page in reader.pages:
            urls += find_links_in_str(page.extract_text())

        if len(urls) > 0:
            print(f"Found link in uid_{uid}: {urls}")

        return urls
    except Exception as e:
        print(f"Error in uid_{uid}: {e}")
        return []


total = {}
sheets = google_sheet.read()

with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
    for e, urls in zip(sheets, pool.map(find_links_in_pdf, sheets)):
        if len(urls) == 0: continue
        total[e["uid"]] = urls

print(f"Found links in {len(total)} papers")

with open("run/github_links.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["uid", "urls"])
    for uid, urls in total.items():
        writer.writerow([uid, ";".join(urls)])
