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

:- module(productivity_requirements,[]).

:- dynamic test_data/3.
:- dynamic saved_output/1.
:- dynamic autotest_tests/1.
:- dynamic mode/0.

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

:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/resource-manager/data-git/states/load.pl').

:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/aop-swipl/lib.pl').

%% /var/lib/myfrdcsa/codebases/minor/aop-swipl/alt22.pl

:- use_module('/var/lib/myfrdcsa/codebases/minor/resource-manager/predicate_streams.pl').
:- module(predicate_streams).

:- multifile quietly/1.
:- discontiguous quietly/1.
:- dynamic quietly/1.

:- use_module(library(regex)).
:- module(productivity_requirements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
	hasCrisisHangingOverHead(Person) ;
	isDreading(Person,_) ;
	has(Person,stressors) ;
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
		not(my_member(Predicate,[error,error_nl,wot,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
		not(my_member(Predicate,[nl,write,write_term,write_data_to_file,writeq_data_to_file,string_match_p])),
		not(my_member(Predicate,[repeatingTimer,resource_manager_load_state])),
		length(R,Arity),
		(   
		    (	
			M:Predicate/Arity = M:F/A, functor(P,F,A), source_file(M:P,File)
		    ) ;   
		    (	
			Predicate/Arity = F/A, functor(P,F,A),
			predicate_property(M:P,file(File)),
			\+ predicate_property(M:P,imported_from(_))
		    )
		),
		string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$')
	    )).

goal_expansion(Goal,aop_advice(Goal)) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    (
	     member(R1,R),
	     R1 =.. [P2|R2],
	     should_be_wrapped(P2,R2),
	     Goal2 =.. [P2|R2],
	     goal_expansion(Goal2,aop_advice(Goal2)),
	     fail
	    ) ;
	    should_be_wrapped(P,R)).

wot(X,A) :-
	with_output_to(atom(A),write_term(X,[quoted(true)])).

errorq(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

error(Item,Options) :-
	with_output_to(user_error,write_term(Item,Options)).

error(Item) :-
	with_output_to(user_error,write_term(Item,[])).

error_nl :-
	with_output_to(user_error,nl).

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

mode(trace).

p(x).

