import concurrent.futures
import multiprocessing
import os
import re
import csv

import pypdf
import requests

import google_sheet

github_link_result_keys = ["uid", "url", "page_found", "invalid", "is_repo", "contains_r_code"]

# noinspection PyBroadException
try:
    with open("run/tokens/github.txt") as github_token_file:
        github_token = github_token_file.read().strip()
except:
    github_token = ""

if github_token == "":
    print(
        "Please enter your Github token in run/github.txt for Github API access. The token only needs to have the 'repo' scope permission.")
    print(
        "Document for generating a token: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token")
    print(
        "The code will continue to run without Github API access, but won't be able to retrieve details information for repos/users.")

github_api_headers = {
    "Authorization": f"token {github_token}"
}

github_link_regex = re.compile(r"github.com/[^\s/]+(?:/[^\s/]*)?")
github_repo_regex = re.compile(r"github.com/([^\s/]+)/([^\s/]+)")


def fix_link(link):
    if link.endswith("."):
        link = link[:-1]

    if link.endswith("/"):
        link = link[:-1]

    if link.endswith(".git"):
        link = link[:-4]

    link = link.replace("(", "").replace(")", "")

    # Don't know why, but some links have invalid characters
    fixed = link.replace("ﬁ", "fi").replace("ﬂ", "fl")
    if fixed != link:
        print(f"Fixed invalid characters in link: {link} -> {fixed}")
        link = fixed

    return link


# Find all Github links in a string
def find_links_by_text(page):
    for link in re.findall(github_link_regex, page.extract_text()):
        yield fix_link(link)


def get_or(obj, key, default):
    if key in obj.keys():
        return obj[key]
    else:
        return default


def find_links_by_annotation(page):
    for annotation in get_or(page, "/Annots", []):
        ank = get_or(annotation.get_object(), "/A", {})
        for link in re.findall(github_link_regex, str(get_or(ank, "/URI", ""))):
            yield fix_link(link)

    return []


# Check if a repo contains R code using GitHub API
def check_repo(result, user, repo):
    if github_token == "":
        return

    try:
        url = f"https://api.github.com/repos/{user}/{repo}/languages"
        response = requests.get(
            url,
            headers=github_api_headers,
            timeout=15
        )

        response.raise_for_status()
        languages = response.json()
        result["contains_r_code"] = "y" if "R" in languages else "n"
        result["invalid"] = "n"
    except requests.exceptions.HTTPError as e:
        print(f"HTTPError while requesting info for Github repo {user}/{repo}", e)
        result["invalid"] = "y"
    except requests.exceptions.Timeout:
        print(f"Timeout while requesting info for Github repo {user}/{repo}")


# Analyze a Github link
def process_link(link_result):
    link = link_result["url"][8:]

    repo_match = re.fullmatch(github_repo_regex, link)

    if repo_match:
        link_result["is_repo"] = "y"
        check_repo(link_result, repo_match.group(1), repo_match.group(2))
    else:
        link_result["is_repo"] = "n"

    link_result["page_found"] = ",".join([str(page) for page in link_result["page_found"]])

    return link_result


# Find all Github links in a pdf
def find_links_in_pdf(entry):
    uid = entry["uid"]

    try:
        reader = pypdf.PdfReader(f"run/paper_pdf/{uid}.pdf", strict=False)
        links_found = {}

        def add_link_to_dict(link, page_num):
            link_result = links_found.get(link, None)

            if link_result is None:
                link_result = dict(zip(github_link_result_keys, [
                    entry["uid"],
                    f"https://{link}",
                    set(),
                    "",
                    "",
                    ""
                ]))
                links_found[link] = link_result

            link_result["page_found"].add(page_num)

        for page_num, page in enumerate(reader.pages):
            for link in find_links_by_annotation(page):
                add_link_to_dict(fix_link(link), page_num + 1)

            # for link in find_links_by_text(page):
            #     add_link_to_dict(fix_link(link), page_num + 1)

        return [process_link(link) for link in links_found.values()]
    except Exception as e:
        print(f"Error in uid_{uid}: {e}")
        return []


def run():
    # Get the paper entries with pdf
    paper_with_link_count = 0
    results = []
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]
    sheets_with_pdf.sort(key=lambda x: int(x["uid"]))

    print(f"Finding links in {len(sheets_with_pdf)} papers...")

    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
        for entry, link_entries in zip(sheets_with_pdf, pool.map(find_links_in_pdf, sheets_with_pdf)):
            if len(link_entries) > 0:
                print(f"Found link in uid_{entry['uid']}:")
                for info in link_entries:
                    print(f"    {info}")
                results += link_entries
                paper_with_link_count += 1

    print(f"Found {len(results)} links in {paper_with_link_count} papers")

    with open("run/github_links.csv", "w") as f:
        writer = csv.DictWriter(f, fieldnames=github_link_result_keys)
        writer.writeheader()
        for link_entry in results:
            writer.writerow(link_entry)


def plot():
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]
    sheets_with_pdf.sort(key=lambda x: int(x["uid"]))

    total = {}
    r_code_on_github = {}

    for entry in sheets_with_pdf:
        k = int(entry["date"][:4])
        total[k] = total.get(k, 0) + 1

    counted_uid = set()

    with open("run/github_links.csv", "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            uid = int(row["uid"])
            if uid not in counted_uid:
                counted_uid.add(uid)
                k = int(sheets[uid - 1]["date"][:4])
                r_code_on_github[k] = r_code_on_github.get(k, 0) + 1

    import matplotlib.pyplot as plt

    x = list(total.keys())
    x.sort()
    y = [(r_code_on_github.get(k, 0) / total.get(k, 0)) * 100.0 for k in x]

    plt.figure(figsize=(10, 5))
    plt.plot(x, y)
    plt.xlabel("Year")
    plt.ylabel("% of papers with R code on Github")
    plt.xticks(x)
    plt.show()

if __name__ == "__main__":
    run()

