use_module(library(clpfd)).



% Empty Cell:
empty(-).

/*
   Each column is a term col(Num,Free,TP,TN,Ps), where:
      Num: column number
      Free: yes/no, whether a piece can be placed "on top" (= at the end)
      TP: Colour of topmost piece
      TN: max. number of consecutive topmost pieces of same colour
      Ps: Pieces in this column
 */ 

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
    col(1,Free,TP,TN,[Top|Bot]),
    col(2,Free,TP,TN,[Top|Bot]),
    col(3,Free,TP,TN,[Top|Bot]),
    col(4,Free,TP,TN,[Top|Bot]),
    col(5,Free,TP,TN,[Top|Bot]),
    col(6,Free,TP,TN,[Top|Bot]),
    col(7,Free,TP,TN,[Top|Bot]).
*/


standardRows(6).  
standardCols(7). 


%% a Game takes a move and the current state and returns a result game state
%%type Game = Move -> State -> Result

%%-- a Move is a piece drop to a column with room in it.
%%-- a Move is a piece drop to a column with room in it.
move(red, col(Num,free,TP,TN, P)) :- 
    col(Num,free,red,TN,[red|P]).
move(blue, col(Num,free,TP,TN, P)) :-
    col(Num,free,blue,TN,[blue|P]).
move(concede).

%%-- the State keeps track of the internal state of the game and the available moves
%%data State = State InternalState [Move]

%%-- a Result is what happens after each move
%%data Result = EndOfGame PlayerType GameBoard    -- end of game, value, ending state
%            | MyTurn State              -- continue current player's turn with new state
%            | YourTurn State            -- continue next player's turn with new state
 %           | InvalidMove State

%-- Current Game State:
%-- - Gameboard
%-- - Player's turn



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


    

