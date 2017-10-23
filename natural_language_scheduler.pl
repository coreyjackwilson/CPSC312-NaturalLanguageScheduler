% Natural Language Shift Scheduler
% Corey Wilson
% 17400110
% Lyndon Won
% 19386144

% Rules for the natural language parsing
% ======================================

% Example Questions
% -----------------
% When does X work?
% Who works today?
% Who works in department X?
% Who manages department X?

% Question that work with current format:
% Who is working in the morning?
% Who is working in the afternoon?
% Is Corey working in the morning?

noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).

det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% HELPER FUNCTIONS
adj([computer, science | T],T,Ind,C,[dept(Ind,comp_sci)|C]).

% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([employee | T],T,Ind,[employee(Ind)|C],C).
noun([work_shift | T],T,Ind,[work_shift(Ind)|C],C).
% The following are for proper nouns. Note that the name affects the grammar
noun([Ind | T],T,Ind,C,C) :- employee(Ind).
noun([Ind | T],T,Ind,C,C) :- work_shift(Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([working, in | T],T,I1,I2,[working_in(I1,I2)|C],C).

question([is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    adjectives(T0,T1,Ind,C0,C1).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,C,[]),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).


% Props for the data design
% =========================

% Shifts
% ------
work_shift(morning).
work_shift(afternoon).

% Employees
% ---------
employee(corey).
employee(lyndon).

% Working
% -------
working_in(corey,morning).
working_in(lyndon,afternoon).

% Departments
% -----------
% department(electronics, 1234).

% Stores
% ------
% store(1, downtown_vancouver, '123 Howe St, Vancouver').

% Jobs
% ----
% job(1, clerk, 12).
