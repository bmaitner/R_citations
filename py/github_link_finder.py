import concurrent.futures
import multiprocessing
import os
import re
import csv

import pypdf

import google_sheet


def find_links_in_str(s):
    return list(re.findall(r"github\.com/\S+", s))


def find_links_in_pdf(entry):
    try:
        uid = entry["uid"]
        file_path = f"run/paper_pdf/{uid}.pdf"

        reader = pypdf.PdfReader(file_path, strict=False)
        urls = []
        for page in reader.pages:
            for url in find_links_in_str(page.extract_text()):
                urls.append(f"https://{url}")

        if len(urls) > 0:
            print(f"Found link in uid_{uid}: {urls}")

        return urls
    except Exception as e:
        print(f"Error in uid_{uid}: {e}")
        return []


def run():
    total = {}
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]

    print(f"Finding links in {len(sheets_with_pdf)} papers...")

    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
        for e, urls in zip(sheets, pool.map(find_links_in_pdf, sheets_with_pdf)):
            if len(urls) == 0: continue
            total[e["uid"]] = urls

    print(f"Found links in {len(total)} papers")

    with open("run/github_links.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(["uid", "urls"])
        for uid, urls in total.items():
            for url in urls:
                writer.writerow([uid, url])
