"""Simply exists to convert JSON file to an escaped string that can be referenced as an elisp variable"""

import json

with open("src/builtins.json") as f:
    d = json.load(f)
print(json.dumps(json.dumps(d)))
