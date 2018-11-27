use_module(library(clpfd)).



% Empty Cell:
empty(-).

% Empty Column:
col(_,true,empty,0,[]).


% Empty Board:
% - 
initBoard :-
    col(1,true,empty,0,[]),
    col(2,true,empty,0,[]),
    col(3,true,empty,0,[]),
    col(4,true,empty,0,[]),
    col(5,true,empty,0,[]),
    col(6,true,empty,0,[]),
    col(7,true,empty,0,[]).




boardState :-
    col(1,Free,TP,TN,[Bot|Top]),
    col(2,Free,TP,TN,[Bot|Top]),
    col(3,Free,TP,TN,[Bot|Top]),
    col(4,Free,TP,TN,[Bot|Top]),
    col(5,Free,TP,TN,[Bot|Top]),
    col(6,Free,TP,TN,[Bot|Top]),
    col(7,Free,TP,TN,[Bot|Top]).





/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI DISPLAY SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


welcomeScreen :- print('**************************************'),
    nl,   
    print('*                                             *'),
    nl,     
    print('*        WELCOME TO             *'),
    nl, 
    print('*   PROLOG CONNECT 4!     *'),
    nl,     
    print('*                                             *'),
    nl,      
    print('**************************************'),
    nl,
    print(' play at your own risk...').



mainOptionsScreen :- nl, 
	print('START MENU:'),
    nl,  
    nl,       
    print('Player VS Player [ Enter 1 ]'),
    nl, 
    print('Player VS AI [ Enter 2 ]'),
    nl, 
    print('Exit [ Enter any other key ]'),
    nl.    
     

play :- welcomeScreen, 
    nl,
    mainOptionsScreen,
    nl,
    read(X),
    play_mode(X).

play_mode(1) :- print('Playing against your friend'),
    nl,
    print('Press any key to continue'),
    nl.
play_mode(2) :- print('Playing against AI'),
    nl,
    print('Press any key to continue'),
    nl.
play_mode(_) :- print('Good bye').

/*
Pieces are either
empty cell:   '-', empty 
piece of red player:   'X', red
piece of blue player:   'O', blue
 */

displayPiece(empty) :- print('-').
displayPiece(red) :- print('X').
displayPiece(blue) :- print('O').

win(Board, Player) :- win_column(Board, Player).
win(Board, Player) :- win_row(Board, Player).
win(Board, Player) :- win_diag(Board, Player).

win_row(Board, Player).
% TODO: Figure out the win condition for horizontals

win_column(Board, Player).
% TODO: Figure out the win condition for verticals

win_diag(Board, Player).
% TODO: Figure out the win condition for diagonals
    

