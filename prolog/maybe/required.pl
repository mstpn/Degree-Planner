program_course("MATH1200",required).
program_course("MATH1203",required).
program_course("MATH1271",required).
program_course("MATH2232",required).

program_course("COMP1631",required).
program_course("COMP1633",required).
program_course("COMP2613",required).
program_course("COMP2631",required).
program_course("COMP2633",required).
program_course("COMP2655",required).
program_course("COMP3309",required).
program_course("COMP3614",required).
program_course("COMP3649",required).
program_course("COMP3659",required).

program_course("GEOG1101",cognate).
program_course("GEOG1105",cognate).
program_course("GEOG2553",cognate).
program_course("GEOG3553",cognate).

%program_course("GEOG1101",option).
%program_course("GEOG1105",option).
%program_course("GEOG2553",option).
%program_course("GEOG3553",option).

program_one_of_prereq("COMP3309","GNED1301").
program_one_of_prereq("COMP3309","GNED1303").
program_one_of_prereq("COMP3309","GNED1304").

course_prereq("COMP1633","COMP1631").
course_prereq("MATH2232","MATH1200").
course_prereq("COMP2613","MATH1271").
course_prereq("COMP2613","COMP1633").
course_prereq("COMP2631","COMP1633").
course_prereq("COMP2633","COMP2631").
course_prereq("COMP2655","COMP1633").
course_prereq("COMP2659","COMP2655").
course_prereq("COMP2659","PHIL1179").
course_prereq("COMP3614","MATH1200").
course_prereq("COMP3614","COMP2613").
course_prereq("COMP3614","COMP2631").
course_prereq("COMP3649","COMP2631").
course_prereq("COMP3649","PHIL1179").
course_prereq("COMP3659","COMP2659").

course_prereq("GEOG2553","GEOG1105").
course_prereq("GEOG3553","GEOG2553").