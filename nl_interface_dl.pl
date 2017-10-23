% Prolog representation of a grammar to build a query for a database
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is slightly expanded code of Figures 12.10 and 12.11 in Section 12.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2010.

% Copyright (c) David Poole and Alan Mackworth 2010. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0-C4 define the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).

% Try:
%?- noun_phrase([a,student],L1,I1,C0,C1).
%?- noun_phrase([a,tall,student],L1,I1,C0,C1).
%?- noun_phrase([a,computer,science,course],L2,I2,C0,C3).
%?- noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],T3,I3,C0,C3).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% DICTIONARY

% adj(T0,T1,Ind,C0,C1) is true if T0-T1 is an adjective that provides properties C1-C0 to Ind
adj([computer, science | T],T,Ind,[dept(Ind,comp_sci)|C],C).
adj([math | T],T,Ind,[dept(Ind,math)|C],C).
adj([female | T],T,Ind,[female(Ind)|C],C).
adj([male | T],T,Ind,[male(Ind)|C],C).
adj([tall | T],T,Ind,[tall(Ind)|C],C).

% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([course | T],T,Ind,[course(Ind)|C],C).
noun([student | T],T,Ind,[student(Ind)|C],C).
noun([building | T],T,Ind,[building(Ind)|C],C).
% The following are for proper nouns. Note that the name affects the grammar
noun([Ind | T],T,Ind,C,C) :- course(Ind).
noun([Ind | T],T,Ind,C,C) :- student(Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([enrolled, in | T],T,I1,I2,[enrolled_in(I1,I2)|C],C).
reln([passed | T],T,I1,I2,[passed(I1,I2)|C],C).

% Some Example Queries
%?- noun_phrase([a,student],R,Ind,C,[]).
%?- noun_phrase([a,tall,student],R,Ind,C,[]).
%?- noun_phrase([a,computer,science,course],R,Ind,C,[]).
%?- noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],R,Ind,C,[]).

% question(Question,QR,Indect,Q0,Query) is true if Query-Q0 provides an answer about Indect to Question-QR
question([is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    adjectives(T0,T1,Ind,C0,C1).
question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
    noun_phrase(T0,[is|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,C,[]),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(T).

%  The Database of Facts to be Queried

% course(C) is true if C is a course
course(cs312).
course(cs322).
course(math315).

dept(cs312,comp_sci).
dept(cs322,comp_sci).
dept(math315,math).

enrolled_in(john,cs312).
enrolled_in(mary,cs312).
enrolled_in(jane,math315).
enrolled_in(sally,cs322).
enrolled_in(sam,math315).

passed(S,C):-
    grade(S,C,G),
    G >= 50.

grade(sam,cs312,93).
grade(chris,cs312,82).

female(mary).
female(jane).
female(sally).
male(john).

tall(mary).
tall(jane).
tall(john).
tall(jordan).

student(mary).
student(jane).
student(sally).
student(john).
student(sam).
student(chris).

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
