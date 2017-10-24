% :- dynamic employee/1, manager/1.

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

noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).

det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([an | T],T,_,C,C).
det(T,T,_,C,C).

adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% Modifying phrases
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

% Department adjectives
adj([grocery,department | T],T,Ind,[dept(Ind, grocery)|C],C).
adj([grocery | T],T,Ind,[dept(Ind, grocery)|C],C).
adj([deli,department | T],T,Ind,[dept(Ind, deli)|C],C).
adj([deli | T],T,Ind,[dept(Ind, deli)|C],C).
adj([checkout | T],T,Ind,[dept(Ind, checkout)|C],C).
adj([cashier | T],T,Ind,[dept(Ind, checkout)|C],C).

% Hours adjectives
adj([full,time | T],T,Ind,[full_time(Ind)|C],C).
adj([part,time | T],T,Ind,[part_time(Ind)|C],C).

% Roles
noun([employee | T],T,Ind,[employee(Ind)|C],C).
noun([manager | T],T,Ind,[manager(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- employee(Ind).
noun([Ind | T],T,Ind,C,C) :- manager(Ind).

% Shifts and Days
noun([day | T],T,Ind,[day(Ind)|C],C).
noun([shift | T],T,Ind,[shift(Ind)|C],C).
noun([Ind | T],T,Ind,C,C) :- day(Ind).
noun([Ind | T],T,Ind,C,C) :- shift(Ind).

% Relations
reln([works, in | T],T,I1,I2,[works_in(I1,I2)|C],C).
reln([working, in | T],T,I1,I2,[works_in(I1,I2)|C],C).
reln([works, on | T],T,I1,I2,[works_on(I1,I2)|C],C).
reln([working, on | T],T,I1,I2,[works_on(I1,I2)|C],C).

% Action Relation
reln([work, on | T],T,I1,I2,[work_on(I1,I2)|C],C).
reln([work, in | T],T,I1,I2,[work_in(I1,I2)|C],C).

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

demand(Q, A) :-
  action(Q,[],A,C,[]),
  prove_all(C).

prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

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

promote(X) :-
  retractall_employee(X),
  assert_manager(X).

demote(X) :-
  retractall_manager(X),
  assert_employee(X).

work_on(X, Y) :-
  retractall_works_on(works_on(X, Y)),
  assert(works_on(X, Y)).

work_in(X, Y) :-
  retractall_works_in(works_in(X, Y)),
  assert(works_in(X, Y)).

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

full_time(E):-
    hours(E,H),
    H >= 37.5.

part_time(E):-
    hours(E,H),
    H < 37.5.

/* Try the following queries
| ?- ask([is,john,enrolled,in,cs312],_).
| ?- question([is,john,enrolled,in,cs312],[],A,C,[]).
| ?- ask([who,is,a,student],A).
| ?- question([who,is,a,student],[],A,C,[]).
| ?- ask([who,is,tall],A).
| ?- ask([is,john,enrolled,in,a,computer,science,course],_).
| ?- question([is,john,enrolled,in,a,computer,science,course],[],A,C,[]).
| ?- ask([who,is,enrolled,in,a,computer,science,course],A).
| ?- question([who,is,enrolled,in,a,computer,science,course],[],A,C,[]).
| ?- ask([who,is,a,tall,student,enrolled,in,a,computer,science,course],A).
| ?- question([who,is,a,tall,student,enrolled,in,a,computer,science,course],[],A,C,[]).
| ?- ask([what,student,is,enrolled,in,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course],A).
| ?- ask([what,student,enrolled,in,a,math,course,passed,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course,enrolled,in,a,math,course],A).
| ?- ask([what,student,passed,cs312],A).
| ?- question([what,student,passed,a,computer,science,course,enrolled,in,a,math,course],[],A,C,[]).
| ?- question([what,student,enrolled,in,a,math,course,passed,a,computer,science,course],[],A,C,[]).
*/

% ask([who,is,works,in,afternoon],X).
% ask([what,manager,works,in,evening],X).
% ask([what,manager,works,on,monday,works,in,evening], X).
% ask([what,employee,works,on,monday,works,in,evening], X).

:- begin_tests(schedule_interface_dl).

test(manager) :-
  manager(X),
  assertion(X == corey).

:- end_tests(schedule_interface_dl).
