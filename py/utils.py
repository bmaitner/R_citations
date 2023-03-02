import os


def create_if_not_exist(dir):
    if not os.path.exists(dir):
        os.makedirs(dir)


create_if_not_exist("run")
