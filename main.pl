use_module(library(clpfd)).



% Empty Cell:
empty(-).

% Empty Column:
col(_,true,empty,0,[]).


% Empty Board:
% - 
/*
initBoard :-
    col(1,true,empty,0,[]),
    col(2,true,empty,0,[]),
    col(3,true,empty,0,[]),
    col(4,true,empty,0,[]),
    col(5,true,empty,0,[]),
    col(6,true,empty,0,[]),
    col(7,true,empty,0,[]).
*/


/*
boardState :-
    col(1,Free,TP,TN,[Bot|Top]),
    col(2,Free,TP,TN,[Bot|Top]),
    col(3,Free,TP,TN,[Bot|Top]),
    col(4,Free,TP,TN,[Bot|Top]),
    col(5,Free,TP,TN,[Bot|Top]),
    col(6,Free,TP,TN,[Bot|Top]),
    col(7,Free,TP,TN,[Bot|Top]).
*/




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
    print('Player VS Player [ Press 1 ]'),
    nl, 
    print('Player VS AI [ Press 2 ]'),
    nl, 
    print('Exit [ Press any other key ]'),
    nl.    
     

play :- welcomeScreen, 
    nl,
    mainOptionsScreen,
    % TODO: PAUSE FOR USER INPUT HERE
    nl.   

/*
Pieces are either
empty cell:   '-', empty 
piece of red player:   'X', red
piece of blue player:   'O', blue
 */

displayPiece(empty) :- print('-').
displayPiece(red) :- print('X').
displayPiece(blue) :- print('O').


    

