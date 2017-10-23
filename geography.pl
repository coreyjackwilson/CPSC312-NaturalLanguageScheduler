% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% Determiners are ignored in this oversimplified example.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% adjectives consist of a sequence of adjectives.
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adjective(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).

% An optional modifying phrase / relative clause is either
% nothing or
% a prepositional phrase or
% that followed by a verb phrase
mp(T,T,_,C,C).
mp(T0,T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).

% DICTIONARY

adjective([Lang,speaking | T],T,Obj,C,[language(Obj,Lang)|C]).

noun([country | T],T,Obj,C,[country(Obj)|C]).
noun([city | T],T,Obj,C,[city(Obj)|C]).
noun([X | T],T,X,C,C) :- country(X).
noun([X | T],T,X,C,C) :- langauge(X).

reln([borders | T],T,O1,O2,C,[borders(O1,O2)|C]).
reln([the,capital,of | T],T,O1,O2,C,[capital(O2,O1)|C]).
reln([next,to | T],T,O1,O2,C,[borders(O1,O2)|C]).

% Some Example Queries
%?- noun_phrase([a, country, that,borders,chile],R,Obj,[],C).

% question(Question,QR,Object,Q0,Query) is true if Query provides an answer about Object to Question
question([is | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([what,is | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([what,is | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([what | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

%  The Database of Facts to be Queried

country(argentina).
country(chile).
country(peru).
country(brazil).

language(spanish).
langauge(portugese).

language(argentina,spanish).
language(chile,spanish).
language(peru,spanish).
language(brazil,portugese).

capital(argentina,buenos_aires).
capital(chile,santiago).
capital(peru,lima).
capital(brazil,brasilia).

borders(peru,chile).
borders(chile,peru).
borders(argentina,chile).
borders(chile,argentina).
borders(brazil,chile).
borders(chile,brazil).
borders(argentina,brazil).
borders(brazil,argentina).


/* Try the following queries:
?- ask([what,is,a,country],A).
?- ask([what,is,a,spanish,speaking,country],A).
?- ask([what,is,the,capital,of, chile],A).
?- ask([what,is,the,capital,of, a, country],A).
?- ask([what,is, a, country, that, borders,chile],A).
?- ask([what,is, a, country, that, borders,a, country,that,borders,chile],A).
?- ask([what,is,the,capital,of, a, country, that, borders,chile],A).
?- ask([what,country,borders,chile],A).
*/
