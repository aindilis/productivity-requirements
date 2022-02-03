%% 2022-01-11 13:50:24 <aindilis> say I have a bunch of "derived predicates"
%%       expressed as Prolog clauses.  Is there an easy way to extract the truth
%%       value of all of the intermediate expressions?
%% 2022-01-11 13:50:31 <aindilis> so like, say I say
%%       hasCrisisHangingOverHead(Person) :- unableToPayUpcomingBills(Person) ;
%%       hasPain(Person,_).  and say both of these goals in the body evaluate to
%%       true.
%% 2022-01-11 13:50:33 <aindilis> I would like to extract a list like:
%%       [hasCrisisHangingOverHead(me),unableToPayUpcomingBills(me),hasPain(me,toothache)].
%% 2022-01-11 13:51:21 <aindilis> and I guess, if say, hasPain(me,_) was false,
%%       then maybe have neg(hasPain(me,_)) in the list instead.
%% 2022-01-11 13:52:11 <aindilis> I guess in thinking about it maybe it would be
%%       possible to stick all these in a module and do goal expansion?
%% 2022-01-11 13:53:02 <aindilis> The issue is I have a lot of (chained together
%%       of) these 
%% 2022-01-11 13:53:38 <aindilis> so like another thing would be
%%       cannotConcentrate(Person) :- hasCrisisHangingOverHead(Person) ; ....
%% 2022-01-11 14:01:49 <aindilis> another thing would be just to use a
%%       meta-interpreter

%% :- module(productivity_requirements,[]).

:- dynamic test_data/3.
:- dynamic saved_output/1.
:- dynamic autotest_tests/1.
:- dynamic mode/0.
:- dynamic classifiedTerm/1.

:- discontiguous derived/2.

:- discontiguous airFilterFiltersInstalled/1, avolition/1, broken/1,
computersAreWorking/0, disorderedEnvironment/2,
disorderedEnvironment/2, disturbedSleepSchedule/1,
furnanceFilterInstalled/1, hasBiologicalIssues/1, hasCapability/1,
hasClearConceptionOfWhatNeedsToBeDone/1, hasConvienetAccessToAStore/1,
hasDistractions/1, hasDistractions/1, hasFacility/1,
hasInterruptions/1, hasItemOfType/2, hasOtherCommitments/1, hasPain/2,
hasService/1, hasSomethingForToDo/2, hasToothache/1,
hasUpcomingSocietyEvent/2, isDreading/2, isEmotionallyUnfitToWork/1,
isEssentialPartOfEnvironment/1, isInEnvironment/2, isInMaterialWant/1,
isInPain/1, isMentallyUnfitToWork/1, isPhysicallyUnfitToWork/1,
lackingSomeUtilities/1, lacksAmenities/1, lacksFortitude/1,
lacksProperShelter/3, lacksProperWorkEnvironment/3, lacksProvisions/1,
lacksProvisions/1, languishing/1, recentlyMissedTakingMedications/1,
sick/1, supplyChainIsSecure/1, tired/1, unableToBeProductive/1,
unableToPayUpcomingBills/3, unableToSleep/1, workSetupAvailable/0.

:- dynamic airFilterFiltersInstalled/1, avolition/1, broken/1,
computersAreWorking/0, disorderedEnvironment/2,
disorderedEnvironment/2, disturbedSleepSchedule/1,
furnanceFilterInstalled/1, hasBiologicalIssues/1, hasCapability/1,
hasClearConceptionOfWhatNeedsToBeDone/1, hasConvienetAccessToAStore/1,
hasDistractions/1, hasDistractions/1, hasFacility/1,
hasInterruptions/1, hasItemOfType/2, hasOtherCommitments/1, hasPain/2,
hasService/1, hasSomethingForToDo/2, hasToothache/1,
hasUpcomingSocietyEvent/2, isDreading/2, isEmotionallyUnfitToWork/1,
isEssentialPartOfEnvironment/1, isInEnvironment/2, isInMaterialWant/1,
isInPain/1, isMentallyUnfitToWork/1, isPhysicallyUnfitToWork/1,
lackingSomeUtilities/1, lacksAmenities/1, lacksFortitude/1,
lacksProperShelter/3, lacksProperWorkEnvironment/3, lacksProvisions/1,
lacksProvisions/1, languishing/1, recentlyMissedTakingMedications/1,
sick/1, supplyChainIsSecure/1, tired/1, unableToBeProductive/1,
unableToPayUpcomingBills/3, unableToSleep/1, workSetupAvailable/0.

:- discontiguous hasTV/2.
:- dynamic hasTV/2.

%% :- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/resource-manager/data-git/states/load.pl').
:- ensure_loaded('load.pl').

%% :- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/aop-swipl/lib.pl').
:- ensure_loaded('lib.pl').

%% /var/lib/myfrdcsa/codebases/minor/aop-swipl/alt22.pl

%% :- use_module('/var/lib/myfrdcsa/codebases/minor/resource-manager/predicate_streams.pl').
:- use_module('predicate_streams.pl').

:- module(predicate_streams).

:- multifile quietly/1.
:- discontiguous quietly/1.
:- dynamic quietly/1.

:- use_module(library(regex)).

%% :- module(user).

%% :- module(productivity_requirements).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_mode(Mode) :-
	retractall(mode(_)),
	assert(mode(Mode)).
my_normal :-
	retractall(mode(_)),
	assert(mode(normal)).
my_trace :-
	retractall(mode(_)),
	assert(mode(trace)).
my_tests :-
	retractall(mode(_)),
	assert(mode(tests)).

:- assert(mode(normal)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

view(X) :-
	write_term(X,[]).

string_match_p(String,Regex) :-
	regex(Regex, [], String, []).

hasTruthValue(Expression,Value) :-
	(   Expression -> (Value = true) ; (Value = fail)),
	view([expression,Expression,value,Value]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unableToBeProductive(Person) :-
	hasDistractions(Person).

%% energyDrinks notNecessarilySugarKind

hasDistractions(Person) :-
	isInPain(Person) ;
	lacksProvisions(Person) ;
	%% hasCrisisHangingOverHead(Person) ;
	isDreading(Person,_) ;
	%% has(Person,stressors) ;
	isInMaterialWant(Person) ;
	hasSocialAnxiety(Person) ;
	unableToSleep(Person) ;
	hasInterruptions(Person) ;
	hasOtherCommitments(Person) ;
	recentlyMissedTakingMedications(Person).

%% Target a certain number of days in a row free of distractions.

%% daysUntilExpectedChangeOfState, free from dread of
%% change in situation, or responsibility to plan for
%% changes

hasOtherCommitments(Person) :-
	hasSomethingForToDo(_,Person).

isInPain(Person) :-
	hasPain(Person,_) ;
	hasToothache(Person) ;
	hasBiologicalIssues(Person).

hasCrisisHangingOverHead(Person) :-
	unableToPayUpcomingBills(Person) ;
	hasPain(Person,_).

hasSocialAnxiety(Person) :-
	hasUpcomingSocietyEvent(Person,_).

lackingSomeUtilties(Environment) :-
	not((
	     hasService(Internet),
	     hasService(Water),
	     hasFacility(Bathroom),
	     hasService(Electricity)
	    )).

lacksAmenities(Environment) :-
	lackingSomeUtilities(Environment) ;
	lacksProvisions(Environment) ;
	not(hasService(Environment,drinkingWater)) ;
	not(hasConvienetAccessToAStore(Environment)) ;
	disorderedEnvironment(Person,Environment).

isInMaterialWant(Person) :-
	not(supplyChainIsSecure(Person)) ;
	(   
	    isInEnvironment(Person,Environment),
	    lacksProvisions(Environment)
	).

lacksProvisions(Environment) :-
	not(hasItemOfType(Environment,water)).

disorderedEnvironment(Person,Environment) :-
	not((
	     furnanceFilterInstalled(Environment),
	     airFilterFiltersInstalled(Environment),
	     supplyChainIsSecure(Person),
	     not((
		  isEssentialPartOfEnvironment(Environment,Object),
		  broken(Object)
		 ))
	    )).

lacksProperWorkEnvironment(Person,Environment) :-
	isInEnvironment(Person,Environment),
	not((
	     not(hasItemOfType(Person,funds)),
	     not(lacksAmenities(Environment)),
	     workSetupAvailable,
	     hasCapability(Person,imakSmartGloves),
	     has(Person,operational(Headset)),
	     isa(Headset,headset),
	     has(Person,operational(Phone)),
	     isa(Phone,phone),
	     hasProperty(phone,on(charger)),
	     computersAreWorking,
	     not(hasCaffeine(Person))
	    )).

lacksProperShelter(Person,Shelter) :-
	lacksAmenities(Shelter).

isPhysicallyUnfitToWork(Person) :-
	(   sick(Person) ;
	    tired(Person) ;
	    disturbedSleepSchedule(Person) ;
	    not(hasProperty(Person,healthy)) ;
	    hasTinglyPuffyLethargicFeelingsInHands(Person)).

isEmotionallyUnfitToWork(Person) :-
	burntOut(Person) ;
	anxious(Person) ;
	depressed(Person) ;
	avolition(Person) ;
	languishing(Person) ;
	lacksFortitude(Person).

isMentallyUnfitToWork(Person) :-
	not(hasClearConceptionOfWhatNeedsToBeDone(Person)) ;
	avolition(Person) ;
	languishing(Person) ;
	not(has(Person,and([geist,motivation]))) ;
	not(has(Person,stimulant)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

should_be_wrapped(Predicate,R) :-
	(   Predicate = ',' ->
	    true ; %% maplist([A]>>(aop_advice(A)),R) ; 
	    (	
		not(my_member(Predicate,[:-,=..,!,in,consult,module,use_module,module_file,call,include,load_files,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open,listing,is,asserta,saved_output,with_output_to_predicate])),
		not(my_member(Predicate,[error,error_nl,wot,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal,generate_for_file_new])),
		not(my_member(Predicate,[nl,write,write_term,write_data_to_file,writeq_data_to_file,string_match_p])),
		not(my_member(Predicate,[repeatingTimer,resource_manager_load_state])),
		not(my_member(Predicate,[declassified,isDeclassified,scry,scryList,sec_write_list,term_contains_subterm,term_contains_subterm_nonvar,tsScry,write_footer,write_header,write_header_footer,write_list,write_result_length])),
		length(R,Arity),
		view([1]),nl,
		(   
		    (	
			view([2]),nl,
			M:Predicate/Arity = M:F/A, functor(P,F,A), source_file(M:P,File)
		    ) ;   
		    (	
			view([3]),nl,
			Predicate/Arity = F/A, functor(P,F,A),
			predicate_property(M:P,file(File)),
			\+ predicate_property(M:P,imported_from(_))
		    )
		),
		view([4,File]),nl,
		string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$'),
		view([5]),nl
	    )).

goal_expansion(Goal,aop_advice(Goal)) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    (
	     member(R1,R),
	     R1 =.. [P2|R2],
	     view([0,R1]),nl,
	     should_be_wrapped(P2,R2),
	     view([6,P2,R2]),nl,
	     Goal2 =.. [P2|R2],
	     view([7,Goal2]),nl,
	     goal_expansion(Goal2,aop_advice(Goal2)),
	     fail
	    ) ;
	    should_be_wrapped(P,R)).

wot(X,A) :-
	with_output_to(atom(A),write_term(X,[quoted(true)])).

%% errorq(Item) :-
%% 	with_output_to(user_error,write_term(Item,[quoted(true)])).

%% error(Item,Options) :-
%% 	with_output_to(user_error,write_term(Item,Options)).

%% error(Item) :-
%% 	with_output_to(user_error,write_term(Item,[])).

%% error_nl :-
%% 	with_output_to(user_error,nl).

errorq(Item) :- true.

error(Item,Options) :- true.

error(Item) :- true.

error_nl :- true.

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	(   
	    mode(normal) -> (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail ) ;
	    (	
		with_output_to_predicate([X]>>(
					       asserta(saved_output(X))
					      ),
					 (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail )),
		findall(X,saved_output(X),Xs),
		retractall(saved_output(_)),
		atomic_list_concat(Xs,'',SavedOutput)
	    %% error([savedOutput,SavedOutput]),error_nl,
	    )
	),
	do_aop_code_after(Goal,Result,SavedOutput),
	Result \== fail.

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	(   
	    mode(normal) -> (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail ) ;
	    (	
		with_output_to_predicate([X]>>(
					       asserta(saved_output(X))
					      ),
					 (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail )),
		findall(X,saved_output(X),Xs),
		retractall(saved_output(_)),
		atomic_list_concat(Xs,'',SavedOutput)
	    %% error([savedOutput,SavedOutput]),error_nl,
	    )
	),
	do_aop_code_after(Goal,Result,SavedOutput),
	Result \== fail.


do_aop_code_before(Goal) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   Call: (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    true
		)
	    )).

do_aop_code_after(Goal,Result,SavedOutput) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   ',Result,': (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    Goal =.. [Pred|Args],
		    length(Args,Arity),
		    atomic_list_concat([Pred,Arity],'__',Predicate),
		    error(test(Predicate) :- tester(Goal,Result,SavedOutput),[quoted(true)]),
		    error('.'), error_nl, error_nl,
		    (	(   
			    (	
				%% M = user,
				(   
				    M:Pred/Arity = M:F/A, functor(P,F,A), source_file(M:P,File),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- tester(Goal,Result,SavedOutput)))
				) ;   
				(   
				    Pred/Arity = F/A, functor(P,F,A),
				    predicate_property(M:P,file(File)),
				    \+ predicate_property(M:P,imported_from(_)),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- tester(Goal,Result,SavedOutput)))
				)
			    )
			) -> true ; true)
		)
	    )).

do_aop_code_redo(Goal) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   Redo: (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    true
		)
	    )).

generate_tests_for_goal(Goal) :-
	mode(Mode),
	my_mode(tests),
	call(Goal),
	setof(File,A^B^test_data(File,A,B),Files),
	member(File,Files),
	string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$'),
	error([file,File]),
	atomic_list_concat([File,'t'],'',OutputFile),
	error_nl,
	wotp(generate_for_file_new(File),Atom),
	error([atom,Atom]),error_nl,
	%5 write_data_to_file(Atom,OutputFile),
	view([atom,Atom]),
	assert(autotest_tests(File)),
	error('Wrote to file: '),
	error(OutputFile),
	error_nl,
	fail.
generate_tests_for_goal(Goal) :-
	true.

wotp(Goal,Atom) :-
	(   with_output_to_predicate([X]>>(assert(saved_output(X))),call(Goal)) -> true ; true),
	findall(X,saved_output(X),Xs),
	atomic_list_concat(Xs,'',Atom),
	%% saved_output(Atom),
	retractall(saved_output(_)).

generate_for_file_new(File) :-
	setof(MPA,A^test_data(File,MPA,A),MPAs),
	write_term(MPAs,[quoted(true)]).

%% generate_for_file(File) :-
%% 	write_term(:- begin_tests(File),[quoted(true)]),
%% 	write('.'), nl, nl,
%% 	setof(MPA,A^test_data(File,MPA,A),MPAs),
%% 	member(MPA,MPAs),
%% 	error([mpa,MPA]),error_nl,
%% 	generate_for_mpa(File,MPA),
%% 	fail.
%% generate_for_file(File) :-
%% 	write_term(:- end_tests(File),[quoted(true)]),
%% 	write('.'), nl,
%% 	true.

%% generate_for_mpa(File,MPA) :-
%% 	write('%% '),
%% 	write_term(MPA,[quoted(true)]),
%% 	nl,nl,
%% 	setof(Test,test_data(File,MPA,Test),Tests),
%% 	member(Test,Tests),
%% 	error([test,Test]),error_nl,
%% 	generate_for_test(File,MPA,Test).

%% generate_for_test(File,MPA,Test) :-
%% 	write_term(Test,[quoted(true)]),
%% 	write('.'), nl, nl.
%% generate_for_test(File,MPA,Test) :-
%% 	true.

scry(Item) :-
        findall(Item,(call(Item),declassified(Item)),Result),
        write_header(Item,Result),nl,
        write_list(Result),
        write_footer(Item,Result),nl,nl.

tsScry(Item) :-
        findall(Item,Item,Result),
        write_header(Item,Result),nl,
        sec_write_list(Result),
        write_footer(Item,Result),nl,nl.

scryList(X,List) :-
        findall(X,X,List).

write_header(Item,Result) :-
        write_header_footer(Item,Result).

write_footer(Item,Result) :-
        write_header_footer(Item,Result).

write_header_footer(Item,Result) :-
        write_result_length(Result),write(' for Scrying for: '),
        write_term(Item,[quoted(true)]).

write_result_length(Result) :-
        length(Result,Length),
        write(Length),write(' results').

write_list(List) :-
        member(Term,List),
        isDeclassified(Term),
        tab(4),write_term(Term,[quoted(true)]),write('.'),nl,
        fail.
write_list(_).

sec_write_list(List) :-
        member(Term,List),
        tab(4),write_term(Term,[quoted(true)]),write('.'),nl,
        fail.
sec_write_list(_).

declassified(Goal) :-
        call(Goal),
        forall(term_contains_subterm(X,Goal),isDeclassified(X)).

isDeclassified(Term) :-
	forall(classifiedTerm(Sub),free_of_term(Sub, Term)).

term_contains_subterm(SubTerm, Term) :-
        not(compound(Term)),
        SubTerm=Term.
term_contains_subterm(SubTerm, Term) :-
        compound(Term),
        compound_name_arguments(Term, SubTerm, _).
term_contains_subterm(SubTerm, Term) :-
        compound(Term),
        arg(_, Term, SubSubTerm),
        term_contains_subterm(SubTerm, SubSubTerm).

term_contains_subterm_nonvar(SubTerm, Term) :-
        not(compound(Term)),
        nonvar(Term),
        SubTerm=Term.
term_contains_subterm_nonvar(SubTerm, Term) :-
        compound(Term),
        nonvar(Term),
        compound_name_arguments(Term, SubTerm, _).
term_contains_subterm_nonvar(SubTerm, Term) :-
        compound(Term),
        nonvar(Term),
        arg(_, Term, SubSubTerm),
        term_contains_subterm_nonvar(SubTerm, SubSubTerm).

p(x).

hasTruthValue(Expression,Value) :-
        (   Expression -> (Value = true) ; (Value = fail)),
        view([expression,Expression,value,Value]).

run :-
	hasTruthValue(unableToPayUpcomingBills(andrewDougherty),TV),
	generate_tests_for_goal(unableToBeProductive(andrewDougherty)),
	scry(test_data(X,Y,Z)).