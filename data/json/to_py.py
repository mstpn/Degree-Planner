import json

f = open("all_courses.json")
data = json.load(f)
for item in data.items():
    print(item)
