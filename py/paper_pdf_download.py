import requests
import google_sheet
import os
import utils
import concurrent.futures

silent_error = True

# noinspection PyBroadException
try:
    with open("run/email.txt") as email_file:
        email_address = email_file.read().strip()
except:
    email_address = ""

if email_address == "":
    print("Please enter your email address in run/email.txt for unpaywall api access.")
    exit()

# Download using unpaywall
def download_entry(entry):
    uid = entry["uid"]
    doi = entry["doi_url"]

    try:
        # Request info from unpaywall api
        url = f"https://api.unpaywall.org/v2/{doi}?email={email_address}"
        response = requests.get(url)
        response.raise_for_status()
        doi_obj = response.json()

        # Download pdf
        pdf_link = doi_obj["best_oa_location"]["url_for_pdf"]
        response = requests.get(pdf_link)
        response.raise_for_status()

        # Save pdf
        with open(f"run/paper_pdf/{uid}.pdf", "wb") as f:
            f.write(response.content)

        print(f"Downloaded uid_{uid}")
    except:
        if not silent_error:
            print(f"Could not download uid_{uid}")


def download():
    utils.create_if_not_exist("run/paper_pdf")
    sheet = google_sheet.read()

    with concurrent.futures.ThreadPoolExecutor(max_workers=64) as pool:
        for e in sheet:
            uid = e["uid"]

            if os.path.exists(f"run/paper_pdf/{uid}.pdf"): continue
            if e["doi_url"] == "": continue

            pool.submit(download_entry, e)

        pool.shutdown(wait=True)