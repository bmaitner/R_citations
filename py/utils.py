import os


def create_if_not_exist(path):
    if not os.path.exists(path):
        os.makedirs(path)


# Attempt to read the text file, if it fails, return an empty string
def try_read_text(path):
    # noinspection PyBroadException
    try:
        with open(path) as file:
            result = file.read().strip()
    except Exception:
        result = ""
    return result


create_if_not_exist("run")
