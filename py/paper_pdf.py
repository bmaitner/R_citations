import concurrent.futures
import multiprocessing
import os

import requests

import google_sheet
import utils

# Read email address from file
# noinspection PyBroadException
try:
    with open("run/email.txt") as email_file:
        email_address = email_file.read().strip()
except Exception as e:
    email_address = ""

if email_address == "":
    print("Please enter your email address in run/email.txt for unpaywall api access.")
    exit()

# Create folder for pdfs
utils.create_if_not_exist("run/paper_pdf")


# Download pdf for a paper entry using unpaywall
def download_entry(entry):
    uid = entry["uid"]
    doi = entry["doi_url"]

    if os.path.exists(f"run/paper_pdf/{uid}.pdf"): return
    if doi == "": return

    # Request info from unpaywall api
    try:
        url = f"https://api.unpaywall.org/v2/{doi}?email={email_address}"
        response = requests.get(url, timeout=15)
        response.raise_for_status()
        doi_obj = response.json()
    except requests.exceptions.Timeout:
        print(f"Timeout while requesting info for uid_{uid}")
        return

    pdf_link = doi_obj["best_oa_location"]["url_for_pdf"]
    if not pdf_link or pdf_link == "":
        return

    # Download pdf
    try:
        response = requests.get(pdf_link, timeout=15)
        response.raise_for_status()

        # Save pdf
        with open(f"run/paper_pdf/{uid}.pdf", "wb") as f:
            f.write(response.content)

        print(f"Downloaded uid_{uid}")
    except requests.exceptions.Timeout:
        print(f"Timeout while downloading uid_{uid}")


# Download all pdfs
def download():
    print("Downloading pdfs...")
    sheet = google_sheet.read()

    with concurrent.futures.ThreadPoolExecutor(multiprocessing.cpu_count() * 8) as pool:
        for e in sheet:
            pool.submit(download_entry, e)
