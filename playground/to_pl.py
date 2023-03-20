import pandas as pd


def convert_to_with_sem(file_starter):
    in_file = file_starter + ".csv"
    data = pd.read_csv(in_file)
    data["Semester"] = file_starter
    out_file = file_starter + "_new" + ".csv"
    data.to_csv(out_file, index=False)
    for row in data.itertuples(index=False):
        print(row)
    return data


def update_fall_winter(starters=["fall", "winter"]):
    sems = {}
    for item in starters:
        # print(item)
        sems[item] = convert_to_with_sem(item)
    return sems


def format_tuple_row(row):
    row_type = None
    lab_str = '\nlab("{}","{}",{},{},{},"{}",{},{},"{}").'
    course_str = '\nlecture("{}","{}",{},{},{},"{}",{},{},"{}").'
    tut_str = '\ntut("{}","{}",{},{},{},"{}",{},{},"{}").'
    fmt_str = None
    if row.Comp > 500:
        fmt_str = tut_str
        row_type = "tut"
    elif row.Comp > 400 and row.Comp < 500:
        fmt_str = lab_str
        row_type = "lab"
    else:
        fmt_str = course_str
        row_type = "lec"

    return (
        row_type,
        fmt_str.format(
            row.Dept,
            row.Course,
            row.Section,
            row.Comp,
            row.Del,
            row.ClassDayOfWeek,
            row.ClassHour,
            row.ClassDuration,
            row.Semester,
        ),
    )


def courses_to_pl(data):
    lecs = []
    labs = []
    tuts = []
    for item in data.items():
        # print(item[1])
        for row in item[1].itertuples(index=False):
            temp = format_tuple_row(row)
            if row.Dept == "MACO":
                print(temp)
            if temp[0] == "lab":
                labs.append(temp[1])
            elif temp[0] == "lec":
                lecs.append(temp[1])
            elif temp[0] == "tut":
                tuts.append(temp[1])
    return [lecs, labs, tuts]


def write_lines(lines):
    f = open("courses.pl", "w")
    for item in lines:
        f.writelines(item)
    f.close()


if __name__ == "__main__":
    data = update_fall_winter()
    print(data)
    lines = courses_to_pl(data)

    write_lines(lines)
