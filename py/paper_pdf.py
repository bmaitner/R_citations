import concurrent.futures
import multiprocessing
import os
import time

import pypdf
import requests

import google_sheet
import utils

silent_errors = False

lock = multiprocessing.Lock()

# Read email address from file
# noinspection PyBroadException
email_address = utils.try_read_text("run/tokens/email.txt")
if email_address == "":
    print("Please enter your email address in run/email.txt for unpaywall api access.")
    exit()

elsevier_api_key = utils.try_read_text("run/tokens/elsevier.txt")
if elsevier_api_key == "":
    print(
        "Elsevier API key not found. Please enter your Elsevier API key in run/elsevier.txt for Elsevier API access.")
    print("This is required for downloading papers from Elsevier services such as Science Direct.")
    print("See https://dev.elsevier.com/ for more information.")

# Create folder for pdfs
utils.create_if_not_exist("run/paper_pdf")

spoofed_headers = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"
}


def print_error(*s):
    if not silent_errors:
        print(*s, sep="")


def download_elsevier_api(entry):
    if elsevier_api_key == "":
        return None

    with lock:
        while True:
            print(f"Downloading uid_{entry['uid']} using Elsevier API...")
            url = f"https://api.elsevier.com/content/article/doi/{entry['doi']}?apiKey={elsevier_api_key}"
            headers = {
                "Accept": "application/pdf",
            }
            response = requests.get(url=url, timeout=15, headers=headers)

            try:
                response.raise_for_status()
            except requests.exceptions.HTTPError as http_error:
                if http_error.response.status_code == 429:
                    print("Rate limit exceeded, waiting 5 seconds...")
                    time.sleep(5)
                    continue
                else:
                    raise http_error

            return response


def validate_pdf(file_path):
    valid = True
    try:
        pypdf.PdfReader(file_path, strict=False)
    except:
        os.remove(file_path)
        valid = False

    return valid


# Get pdf links for a paper entry using Unpaywall API
def get_pdf_links(entry):
    uid = entry["uid"]
    doi = entry["doi"]
    file_path = f"run/paper_pdf/{uid}.pdf"

    if os.path.exists(file_path):
        if validate_pdf(file_path):
            return
        else:
            print_error(f"uid_{uid} is corrupted, redownloading...")
    if doi == "":
        return

    doi_obj = None

    # Request info from unpaywall api
    try:
        url = f"https://api.unpaywall.org/v2/{doi}?email={email_address}"
        response = requests.get(url, timeout=15, headers=spoofed_headers)
        response.raise_for_status()
        doi_obj = response.json()
    except requests.exceptions.Timeout:
        print_error(f"Timeout while requesting info for uid_{uid}")
    except requests.exceptions.HTTPError as http_error:
        if http_error.response.status_code == 404:
            print_error(f"uid_{uid} not found on Unpaywall")
        else:
            print_error(f"HTTP error while requesting info for uid_{uid}", http_error)
    except Exception as request_info_e:
        print_error(f"Exception while requesting info for uid_{uid}", request_info_e)

    if doi_obj is None:
        return

    best_oa_location = doi_obj["best_oa_location"]
    if not best_oa_location:
        return

    pdf_links = [oa["url_for_pdf"] for oa in [best_oa_location] + doi_obj["oa_locations"]]
    pdf_links.append(doi_obj["best_oa_location"]["url"])

    return list(set(pdf_links))


def try_site_api(i, entry, url):
    if i != 0:
        return None

    if "elsevier" in url or "sciencedirect" in url:
        return download_elsevier_api(entry)


def download_entry(entry, pdf_link, trying_next):
    uid = entry["uid"]
    file_path = f"run/paper_pdf/{uid}.pdf"

    # Try downloading pdf directly before using site specific API
    response = None

    for i in range(2):
        try:
            if i == 0:
                response = requests.get(pdf_link, allow_redirects=True, headers=spoofed_headers, timeout=15)

            response.raise_for_status()

            # Save pdf
            with open(file_path, "wb") as f:
                f.write(response.content)

            if not validate_pdf(file_path):
                new_response = try_site_api(i, entry, response.url)
                if new_response:
                    response = new_response
                    continue

                print_error(f"uid_{uid} from {response.url} is corrupted", trying_next)
                return False

            print(f"Downloaded uid_{uid}")
            return True
        except requests.RequestException as request_exception:
            new_response = try_site_api(i, entry, response.url)
            if new_response:
                response = new_response
                continue
            else:
                raise request_exception


# Attempt download pdf for a paper entry
def try_download_entry(entry):
    uid = entry["uid"]

    pdf_links = get_pdf_links(entry)
    if pdf_links is None:
        return

    for i in range(len(pdf_links)):
        pdf_link = pdf_links.pop()
        if pdf_link is None:
            continue
        trying_next = ", trying next link..." if (len(pdf_links) > 0) else ", no more links to try."

        # Download pdf
        try:
            if download_entry(entry, pdf_link, trying_next):
                break
        except requests.exceptions.Timeout:
            print_error(f"Timeout while downloading uid_{uid}", trying_next)
        except requests.exceptions.HTTPError as http_error:
            if http_error.response.status_code == 403:
                print_error(f"Unable to download uid_{uid} from {http_error.response.url} due to 403", trying_next)
            else:
                print_error(f"HTTP error while downloading uid_{uid}", trying_next, http_error)
        except Exception as any_e:
            print_error(f"Exception while downloading uid_{uid}", trying_next, any_e)


def download_entries(entries):
    for entry in entries:
        try:
            try_download_entry(entry)
        except Exception as e:
            print(f"Unexpected error while downloading uid_{entry['uid']}", e, sep="")
            exit()


# Download all pdfs
def download():
    print("Downloading pdfs...")
    sheet = google_sheet.read()
    thread_count = multiprocessing.cpu_count() * 8

    with concurrent.futures.ProcessPoolExecutor(thread_count) as process_pool:
        lists = [[] for _ in range(thread_count)]
        for i, entry in enumerate(sheet):
            if entry["open_access"] == "0":
                continue

            lists[i % thread_count].append(entry)

        for list_for_process in lists:
            process_pool.submit(download_entries, list_for_process)


def get_sheets_with_pdf():
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]
    sheets_with_pdf.sort(key=lambda x: int(x["uid"]))
    return sheets_with_pdf


if __name__ == "__main__":
    download()
