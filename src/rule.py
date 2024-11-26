import pandas as pd

# Define the data for the effect rules
data = {
    "Shape Name": ["Circle", "Rectangle", "Diamond", "Arbitrary"],
    "Parameters": [
        "radius",
        "width, height",
        "radius",
        "list of offsets"
    ],
    "Effect Range Calculation": [
        "Includes all positions (dx, dy) where dx^2 + dy^2 <= radius^2.",
        ("Width = 1: All columns in current row.\n"
         "Width even: Expand equally above/below, prioritizing upward.\n"
         "Width odd: Symmetric above/below.\n"
         "Height = ':': Covers entire column."),
        "Includes all positions (dx, dy) where |dx| + |dy| <= radius.",
        ("Directly uses the provided offsets (dx, dy):\n"
         "x > 0: Downward, x < 0: Upward.\n"
         "y > 0: Rightward, y < 0: Leftward.\n"
         "(0, 0): Always included.")
    ]
}

# Create a DataFrame
df = pd.DataFrame(data)

file_path = "./effect_rules.txt"
df.to_string(buf=file_path, index=False)

file_path