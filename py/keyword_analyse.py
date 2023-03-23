import csv

import torch

import google_sheet
import keyword_counter
import paper_pdf
import math

# Set the default dtype to float32
torch.set_default_dtype(torch.float32)

# Read the data
all_sheets = google_sheet.read()
sheets_with_pdf = {int(entry["uid"]): entry for entry in paper_pdf.get_sheets_with_pdf()}
keyword_count = {int(entry["uid"]): entry for entry in keyword_counter.read_keyword_count()}
for entry, e in zip(sheets_with_pdf.values(), keyword_count.values()):
    assert e.pop('uid') == entry['uid']

# TODO: Need a better way to normalize the word count
keyword_count = {k: [math.sqrt(float(e)) for e in v.values()] for k, v in keyword_count.items()}


def predict(model, uid):
    if uid not in keyword_count:
        return 0.0
    X = torch.tensor(keyword_count[uid])
    return model(X).item()


def train(model, epoch):
    # Filter uid with r_scripts_available labeled
    labeled_uids = []
    for uid, entry in sheets_with_pdf.items():
        if len(entry["r_scripts_available"]) > 0:
            labeled_uids.append(uid)

    # Prepare the data
    X = torch.tensor([keyword_count[uid] for uid in labeled_uids])
    Y = torch.tensor([1.0 if sheets_with_pdf[uid]["r_scripts_available"] == "yes" else 0.0 for uid in labeled_uids])
    Y = torch.reshape(Y, (-1, 1))

    print(f"Training with {len(labeled_uids)} labeled data")

    loss_fn = torch.nn.BCELoss()
    learning_rate = 0.001
    optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

    for t in range(1, epoch + 1):
        # Forward pass: compute predicted y by passing x to the model.
        y_pred = model(X)

        # Compute and print loss.
        loss = loss_fn(y_pred, Y)
        print(f"Epoch {t}: loss = {loss.mean():.4f}")

        # Zero gradients, perform a backward pass, and update the weights.
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()


if __name__ == '__main__':
    keyword_list_size = len(keyword_counter.keywords)
    model = torch.nn.Sequential(
        torch.nn.Linear(keyword_list_size, keyword_list_size**2),
        torch.nn.ReLU(),
        torch.nn.Linear(keyword_list_size**2, keyword_list_size),
        torch.nn.ReLU(),
        torch.nn.Linear(keyword_list_size, 1),
        torch.nn.Sigmoid(),
    )

    train(model, 200)

    # Output the prediction scores
    result = []
    for uid, entry in sheets_with_pdf.items():
        result.append({
            "uid": uid,
            "score": predict(model, uid),
        })

    with open("run/keyword_analyse_result.csv", "w") as f:
        writer = csv.DictWriter(f, fieldnames=["uid", "score"])
        writer.writeheader()
        writer.writerows(result)
