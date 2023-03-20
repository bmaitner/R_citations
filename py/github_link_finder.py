import concurrent.futures
import multiprocessing
import os
import re
import csv

import pypdf
import requests
import textdistance

import google_sheet

github_link_result_keys = [
    "uid",
    "url",
    "page_found",
    "user",
    "repo",
    "valid",
    "contains_r_code",
    "readme_score",
    "description_score"
]

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


def request_github(url):
    response = requests.get(url, headers=github_api_headers, timeout=15)
    response.raise_for_status()
    return response.json()


def validate_link(link_result):
    # noinspection PyBroadException
    try:
        requests.get(link_result["url"], timeout=15).raise_for_status()
        link_result["valid"] = "y"
    except Exception:
        link_result["valid"] = "n"


def extract_user_repo_info(link_result):
    split = link_result["url"][8:].split("/")  # Removes https:// and splits into user and repo
    link_result["user"] = split[1]
    link_result["repo"] = split[2] if len(split) > 2 else ""


# Check if a repo contains R code using GitHub API
def check_repo(link_result):
    if github_token == "":
        return

    user = link_result["user"]
    repo = link_result["repo"]

    try:
        languages = request_github(f"https://api.github.com/repos/{user}/{repo}/languages")
        link_result["contains_r_code"] = "y" if "R" in languages else "n"
    except requests.exceptions.HTTPError as e:
        print(f"HTTPError while requesting languages info for Github repo {user}/{repo}", e)
    except requests.exceptions.Timeout:
        print(f"Timeout while requesting languages info for Github repo {user}/{repo}")


def compute_score(input_text, expected_text):
    if not input_text:
        return 0.0
    l = len(expected_text)
    levenshtein = textdistance.levenshtein.similarity(input_text, expected_text)
    lcsstr = textdistance.lcsstr.similarity(input_text, expected_text)
    return (levenshtein / l * 0.5) + (lcsstr / l * 0.5)


def compute_title_match_score(entry, link_result):
    if github_token == "":
        return

    user = link_result["user"]
    repo = link_result["repo"]

    try:
        json_response = request_github(f"https://api.github.com/repos/{user}/{repo}")
        default_branch = json_response["default_branch"]
        readme_request = requests.get(f"https://raw.githubusercontent.com/{user}/{repo}/{default_branch}/README.md", timeout=15)

        readme = readme_request.text
        description = json_response["description"]
        title = entry["title"]

        link_result["readme_score"] = compute_score(readme, title)
        link_result["description_score"] = compute_score(description, title)
    except requests.exceptions.HTTPError as e:
        print(f"HTTPError while requesting info for Github repo {user}/{repo}", e)
    except requests.exceptions.Timeout:
        print(f"Timeout while requesting info for Github repo {user}/{repo}")


# Analyze a Github link
def process_link(entry, link_result):
    link_result["page_found"] = ",".join([str(page) for page in link_result["page_found"]])

    extract_user_repo_info(link_result)
    validate_link(link_result)

    if link_result["repo"] and link_result["valid"] == "y":
        check_repo(link_result)
        compute_title_match_score(entry, link_result)

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
                    "",
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

        return [process_link(entry, link) for link in links_found.values()]
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

    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count() * 4) as pool:
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


def plot_percentage_github():
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


def plot_score_scatter():
    import matplotlib.pyplot as plt

    x = []
    y = []

    with open("run/github_links.csv", "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row["readme_score"] and row["description_score"]:
                x.append(float(row["readme_score"]))
                y.append(float(row["description_score"]))

    plt.title("Readme score vs Description score, total count: " + str(len(x)))
    plt.scatter(x, y, s=2.0)
    plt.xlabel("Readme score")
    plt.ylabel("Description score")
    plt.show()


def plot_score_histo():
    import matplotlib.pyplot as plt

    readme_scores = []
    description_scores = []

    with open("run/github_links.csv", "r") as f:
        reader = csv.DictReader(f)

        for row in reader:
            if row["readme_score"]:
                readme_scores.append(float(row["readme_score"]))

            if row["description_score"]:
                description_scores.append(float(row["description_score"]))

    plt.title("Readme score, total count: " + str(len(readme_scores)))
    plt.hist(readme_scores, bins=20)
    plt.xlabel("Readme score")
    plt.ylabel("Count")
    plt.show()

    plt.title("Description score, total count: " + str(len(description_scores)))
    plt.hist(description_scores, bins=20)
    plt.xlabel("Description score")
    plt.ylabel("Count")
    plt.show()

if __name__ == "__main__":
    run()
    plot_percentage_github()
    plot_score_scatter()
    plot_score_histo()