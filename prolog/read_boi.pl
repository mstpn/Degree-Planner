
%rules 

%student(name(),sem(),courses_taken(),chosen_sen_options(),chosen_jun_options(),cog(),choosen_cog(),years_to_grad(8),max_course(8))

course(dep(gned),code(1301),or([]),and([])).
course(dep(gned),code(1303),or([]),and([])).
course(dep(gned),code(1304),or([]),and([])).

course(dep(comp),code(1631),or([]),and([])).
course(dep(phil),code(1179),or([]),and([])).
course(dep(math),code(1200),or([]),and([])).
course(dep(math),code(1203),or([]),and([])).
course(dep(math),code(1271),or([]),and([])).
course(dep(math),code(2232),or([]),and([course(dep(math),code(1200),or([]),and([]))])).
course(dep(comp),code(1631),or([]),and([])).
course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))])).
course(dep(comp),code(2613),or([]),and([course(dep(math),code(1271),or([]),and([])),course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])).
course(dep(comp),code(2631),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])).
course(dep(comp),code(2633),or([]),and([course(dep(comp),code(2631),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))]))])).
course(dep(comp),code(2655),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])).
course(dep(comp),code(2659),or([]),and([course(dep(phil),code(1179),or([]),and([])),course(dep(comp),code(2655),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))]))])).
course(dep(comp),code(3309),or([course(dep(gned),code(1301),or([]),and([])),course(dep(gned),code(1303),or([]),and([])),course(dep(gned),code(1304),or([]),and([]))]),and([])).
course(dep(comp),code(3614),or([]),and([course(dep(math),code(1200),or([]),and([])),course(dep(comp),code(2613),or([]),and([course(dep(math),code(1271),or([]),and([])),course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))]),course(dep(comp),code(2631),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])))])).
course(dep(comp),code(3649),or([]),and([course(dep(comp),code(2631),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])),course(dep(phil),code(1179),or([]),and([]))])).
course(dep(comp),code(3659),or([]),and([course(dep(comp),code(2659),or([]),and([course(dep(comp),code(2631),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))]),course(dep(phil),code(1179),or([]),and([])),course(dep(comp),code(2655),or([]),and([course(dep(comp),code(1633),or([]),and([course(dep(comp),code(1631),or([]),and([]))]))])))]))])).


course(D,C,_,_) :- D=dep(comp),C=code(1631).
get_and_for(D,C,A) :- course(dep(D),code(C),_,A).