import concurrent.futures
import multiprocessing
import os
import re

import pypdf

import google_sheet


def find_links_in_str(s):
    return list(re.findall(r"\b\S*github\.com\S+", s))


def find_links_in_pdf(entry):
    uid = entry["uid"]
    file_path = f"run/paper_pdf/{uid}.pdf"

    if not os.path.exists(file_path): return

    reader = pypdf.PdfReader(file_path)
    for page in reader.pages:
        urls = find_links_in_str(page.extract_text())
        if len(urls) > 0:
            print(f"Found link in uid_{uid}: {urls}")


sheets = google_sheet.read()
with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
    for e in sheets:
        pool.submit(find_links_in_pdf, e)
