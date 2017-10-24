% Natural Language schedule
% Corey Wilson - 17400110
% Lyndon Won - 19386144

% Persistence Logic
% =================

:- use_module(library(persistency)).

:- persistent
        manager(name:atom),
        employee(name:atom),
        hours(name:atom, amount:integer),
        works_on(name:atom, oneof([monday, tuesday, wednesday, thursday, friday, saturday, sunday]) ),
        works_in(name:atom, oneof([morning, afternoon, evening])),
        has_dept(name:atom, oneof([grocery, deli, checkout, customer_service])).

:- initialization(init).

init :-
  absolute_file_name('fact.db', File, [access(write)]),
  db_attach(File, []).

% Natural Language Logic
% ======================

noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).

det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([an | T],T,_,C,C).
det(T,T,_,C,C).

% Adjective Logic
% ---------------

adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% Modifying Phrases
% -----------------

mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([to|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% Department Adjectives
% ---------------------

adj([grocery,department | T],T,Ind,[has_dept(Ind, grocery)|C],C).
adj([grocery | T],T,Ind,[has_dept(Ind, grocery)|C],C).
adj([deli,department | T],T,Ind,[has_dept(Ind, deli)|C],C).
adj([deli | T],T,Ind,[has_dept(Ind, deli)|C],C).
adj([checkout | T],T,Ind,[has_dept(Ind, checkout)|C],C).
adj([cashier | T],T,Ind,[has_dept(Ind, checkout)|C],C).

% Hours Adjectives
% ----------------

adj([full,time | T],T,Ind,[full_time(Ind)|C],C).
adj([part,time | T],T,Ind,[part_time(Ind)|C],C).

% Role Nouns
% ----------

noun([employee | T],T,Ind,[employee(Ind)|C],C).
noun([manager | T],T,Ind,[manager(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- employee(Ind).
noun([Ind | T],T,Ind,C,C) :- manager(Ind).

% Shifts and Hours Nouns
% ----------------------

noun([day | T],T,Ind,[day(Ind)|C],C).
noun([shift | T],T,Ind,[shift(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- day(Ind).
noun([Ind | T],T,Ind,C,C) :- shift(Ind).
noun([Ind | T],T,Ind,C,C) :- integer(Ind).
noun([Ind | T],T,Ind,C,C) :- role(Ind).
noun([Ind | T],T,Ind,C,C) :- department(Ind).

% Question/Action Relations
% -------------------------

reln([works, in | T],T,I1,I2,[works_in(I1,I2)|C],C).
reln([working, in | T],T,I1,I2,[works_in(I1,I2)|C],C).
reln([works, on | T],T,I1,I2,[works_on(I1,I2)|C],C).
reln([working, on | T],T,I1,I2,[works_on(I1,I2)|C],C).
reln([work, on | T],T,I1,I2,[work_on(I1,I2)|C],C).
reln([work, in | T],T,I1,I2,[work_in(I1,I2)|C],C).
reln([hours, to | T],T,I1,I2,[change_hours(I1,I2)|C],C).
reln([role, to | T],T,I1,I2,[change_role(I1,I2)|C],C).
reln([department, to | T],T,I1,I2,[change_dept(I1,I2)|C],C).

% Questions
% ---------

question([is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    adjectives(T0,T1,Ind,C0,C1).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,[is|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).

ask(Q,A) :-
    question(Q,[],A,C,[]),
    prove_all(C).

prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

% Actions
% -------

action([promote | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2),
    promote(Ind).
action([demote | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2),
    demote(Ind).
action([schedule | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
action([change | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).

% Because demanding is better than asking
demand(Q) :-
  action(Q,[],_,C,[]),
  prove_all(C).

% Added promote/demote just for fun. Can use change role instead.
promote(X) :-
  retractall_employee(X),
  assert_manager(X).

demote(X) :-
  retractall_manager(X),
  assert_employee(X).

work_on(X, Y) :-
  retractall_works_on(X, Y),
  assert_works_on(X, Y).

work_in(X, Y) :-
  retractall_works_in(X, Y),
  assert_works_in(X, Y).

change_hours(X, Y) :-
  retractall_hours(X, _),
  assert_hours(X, Y).

% Could easily extend this to more roles based on case.
change_role(X, Y) :-
  Y == manager ->
    retractall_employee(X),
    assert_manager(X);
  Y == employee ->
    retractall_manager(X),
    assert_employee(X).

% Could easily extend this to more departments based on case.
change_dept(X, Y) :-
    retractall_has_dept(X, _),
    assert_has_dept(X, Y).

% General Rules
% -------------

day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).
day(saturday).
day(sunday).

shift(morning).
shift(afternoon).
shift(evening).

role(manager).
role(employee).

department(grocery).
department(deli).
department(checkout).
department(customer_service).

full_time(E):-
    hours(E,H),
    H >= 37.5.

part_time(E):-
    hours(E,H),
    H < 37.5.

% Testing Suite
% =============

:- begin_tests(schedule_interface_dl).

% Fact Tests
% ----------

test(full_time) :-
  full_time(E),
  assertion(E == corey).

test(part_time) :-
  part_time(E),
  assertion(E == lyndon).

% Basic Ask Tests
% ---------------

test(ask_full_time) :-
  ask([who,is,a,full,time,employee],E),
  assertion(E == mary).

test(ask_grocery_deptartment) :-
  ask([who,is,the,grocery,department,manager],E),
  assertion(E == corey).

test(ask_who_is_working_in) :-
  ask([who,is,working,in,the,morning],E),
  assertion(E == mary).

test(ask_who_is_working_on) :-
  ask([who,is,working,on,monday],E),
  assertion(E == john).

% Compound Relation Ask Tests
% ---------------------------

test(ask_who_is_employee_working_on_working_in) :-
  ask([who,is,an,employee,working,on,monday,working,in,afternoon],E),
  assertion(E == john).

test(ask_what_employee_works_on_works_in) :-
  ask([what,employee,works,on,monday,works,in,afternoon],E),
  assertion(E == john).

test(ask_what_employee_works_in_works_on) :-
  ask([what,employee,works,in,afternoon,works,on,monday],E),
  assertion(E == john).

test(ask_what_manager_works_in_works_on) :-
  ask([what,manager,works,in,evening,works,on,monday],E),
  assertion(E == corey).

test(ask_grocery_dept_employee_works_on_monday_works_in_morning) :-
  ask([what,deli,employee,works,on,monday,works,in,afternoon],E),
  assertion(E == lyndon).

test(ask_what_grocery_employee_works_on_monday_works_on_wednesday) :-
  ask([what,grocery,manager,works,on,monday,works,on,friday],E),
  assertion(E == corey).

test(ask_what_full_time_deli_department_manager_works_on_monday_work_in_evening) :-
  ask([what,full,time,grocery,department,manager,works,on,monday,works,in,evening],E),
  assertion(E == corey).

% Not Working
% -----------

% something wrong when works in is added
% test(ask_grocery_dept_employee_works_on_monday_works_on_wednesday_works_in_evening) :-
%   ask([what,grocery,department,manager,works,on,monday,works,on,friday,works,in,evening],E),
%   assertion(E == corey).

% same 'ask' works when starting with 'what'
% test(ask_who_is_working_on_working_in) :-
%   ask([who,is,working,on,monday,working,in,afternoon],E),
%   assertion(E == john).

% Manual Tests (So you don't corrupt the DB with automated tests ;-) )
% --------------------------------------------------------------------

% test(change_employee_to_manager) :-
% demand([demote, corey]),
% assertion(employee(corey)).

% test(change_manager_to_employee) :-
% demand([promote, lyndon]),
% assertion(manager(lyndon)).

% test(change_role) :-
% demand([change, corey, department, to, customer_service]),
% assertion(has_dept(corey, customer_service)).

% test(change_hours) :-
% demand([change, corey, hours, to, 20]),
% assertion(hours(corey, 20)).

% test(change_shift) :-
% demand([change, corey, work, in, afternoon]),
% assertion(works_in(corey,afternoon).

% test(change_day) :-
% demand([change, corey, work, on, thursday]),
% assertion(works_on(corey, thursday).

:- end_tests(schedule_interface_dl).
