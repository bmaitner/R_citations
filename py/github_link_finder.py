import concurrent.futures
import multiprocessing
import os
import re
import csv

import pypdf
import requests

import google_sheet

# noinspection PyBroadException
try:
    with open("run/github_token.txt") as github_token_file:
        github_token = github_token_file.read().strip()
except:
    github_token = ""

if github_token == "":
    print(
        "Please enter your Github token in run/github_token.txt for Github API access. The token only needs to have the 'repo' scope permission.")
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

    # Don't know why, but some links have invalid characters
    fixed = link.replace("ﬁ", "fi").replace("ﬂ", "fl")
    if fixed != link:
        print(f"Fixed invalid characters in link: {link} -> {fixed}")
        link = fixed

    return link

# Find all Github links in a string
def find_links_in_str(s):
    return [fix_link(link) for link in re.findall(github_link_regex, s)]


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
def analyze_link(entry, link):
    result = {
        "uid": entry["uid"],
        "url": f"https://{link}",
        "invalid": "",
        "is_repo": "",
        "contains_r_code": ""
    }

    repo_match = re.fullmatch(github_repo_regex, link)

    if repo_match:
        result["is_repo"] = "y"
        check_repo(result, repo_match.group(1), repo_match.group(2))
    else:
        result["is_repo"] = "n"

    return result


# Find all Github links in a pdf
def find_links_in_pdf(entry):
    uid = entry["uid"]

    try:
        file_path = f"run/paper_pdf/{uid}.pdf"

        reader = pypdf.PdfReader(file_path, strict=False)
        links_found = set()
        for page in reader.pages:
            for link in find_links_in_str(page.extract_text()):
                links_found.add(link)

        results = [analyze_link(entry, link) for link in links_found]

        if len(results) > 0:
            print(f"Found link in uid_{uid}:")
            for info in results:
                print(f"    {info}")

        return results
    except Exception as e:
        print(f"Error in uid_{uid}: {e}")
        return []


def run():
    paper_with_link_count = 0
    results = []
    files = [f for f in os.listdir("run/paper_pdf/")]
    sheets = google_sheet.read()
    sheets_with_pdf = [sheets[int(name.split(".")[0]) - 1] for name in files]

    print(f"Finding links in {len(sheets_with_pdf)} papers...")

    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as pool:
        for link_entries in pool.map(find_links_in_pdf, sheets_with_pdf):
            if len(link_entries) > 0:
                results += link_entries
                paper_with_link_count += 1

    print(f"Found {len(results)} links in {paper_with_link_count} papers")

    with open("run/github_links.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(["uid", "url", "invalid", "is_repo", "contains_r_code"])
        for link_entry in results:
            writer.writerow(link_entry.values())


if __name__ == "__main__":
    run()
