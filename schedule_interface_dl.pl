% Natural Language schedule
% Corey Wilson - 17400110
% Lyndon Won -

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
adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% Modifying Phrases
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
adj([grocery,department | T],T,Ind,[has_dept(Ind, grocery)|C],C).
adj([grocery | T],T,Ind,[has_dept(Ind, grocery)|C],C).
adj([deli,department | T],T,Ind,[has_dept(Ind, deli)|C],C).
adj([deli | T],T,Ind,[has_dept(Ind, deli)|C],C).
adj([checkout | T],T,Ind,[has_dept(Ind, checkout)|C],C).
adj([cashier | T],T,Ind,[has_dept(Ind, checkout)|C],C).

% Hours Adjectives
adj([full,time | T],T,Ind,[full_time(Ind)|C],C).
adj([part,time | T],T,Ind,[part_time(Ind)|C],C).

% Role Nouns
noun([employee | T],T,Ind,[employee(Ind)|C],C).
noun([manager | T],T,Ind,[manager(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- employee(Ind).
noun([Ind | T],T,Ind,C,C) :- manager(Ind).

% Shifts and Hours Nouns
noun([day | T],T,Ind,[day(Ind)|C],C).
noun([shift | T],T,Ind,[shift(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- day(Ind).
noun([Ind | T],T,Ind,C,C) :- shift(Ind).
noun([Ind | T],T,Ind,C,C) :- integer(Ind).
noun([Ind | T],T,Ind,C,C) :- role(Ind).
noun([Ind | T],T,Ind,C,C) :- department(Ind).

% Question Relations
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
question([is | T0],T2,Ind,C0,C2) :-
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

% Action Logic
% ------------

% Actions
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

demand(Q) :-
  action(Q,[],_,C,[]),
  prove_all(C).

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

change_role(X, Y) :-
  Y == manager ->
    retractall_employee(X),
    assert_manager(X);
  Y == employee ->
    retractall_manager(X),
    assert_employee(X).

change_dept(X, Y) :-
    retractall_has_dept(X, _),
    assert_has_dept(X, Y).

% General Rules
% =============
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

% Testing
% =======
:- begin_tests(schedule_interface_dl).

test(change_employee_to_manager) :-
  manager(X),
  assertion(X == corey).

test(change_manager_to_employee) :-
  manager(X),
  assertion(X == corey).

:- end_tests(schedule_interface_dl).
