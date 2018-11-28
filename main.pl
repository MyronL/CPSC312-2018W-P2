use_module(library(clpfd)).



% Empty Cell:
%empty(-).

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

initBoard :-
    col(1,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(2,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(3,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(4,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(5,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(6,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(7,true,empty,0,[empty,empty,empty,empty,empty,empty]).



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




%% a Game takes a move and the current state and returns a result game state
%%type Game = Move -> State -> Result

%%-- a Move is a piece drop to a column with room in it.
%%-- a Move is a piece drop to a column with room in it.
move(red, col(Num,free,_,TN, P)) :- 
    col(Num,free,red,TN,[red|P]).
move(blue, col(Num,free,_,TN, P)) :-
    col(Num,free,blue,TN,[blue|P]).
move(concede).

%%-- the State keeps track of the internal state of the game and the available moves
%%data State = State InternalState [Move]

%%-- a Result is what happens after each move
%%%data Result = EndOfGame PlayerType GameBoard    -- end of game, value, ending state
%            | MyTurn State              -- continue current player's turn with new state
%            | YourTurn State            -- continue next player's turn with new state
 %           | InvalidMove State



%-- Current Game State:
%-- - Gameboard
%-- - Player's turn
%data InternalState = GameState GameBoard PlayerType

/*
%-- a Player takes the current state of the game and returns a move
type Player = State -> Move
type IOPlayer = State -> IO Move
*/

%-- PlayerType is the two players in the game
%data PlayerType = North | South
%  deriving (Eq, Show)

playerType(red).
playerType(blue).
player(blue).
player(red).


/*
-- flipPlayer returns the opposite of the given player type
flipPlayer :: PlayerType -> PlayerType
flipPlayer North = South
flipPlayer South = North
*/

flipPlayer(red) :- player(blue).
flipPlayer(blue) :- player(red).

/*

%Pieces are either
%empty cell:   '-', empty 
%piece of red player:   'X', red
%piece of blue player:   'O', blue
 */

standardRows(6).  
standardCols(7). 


col(1).
col(2).
col(3).
col(4).
col(5).
col(6).
col(7).

row(1).
row(2).
row(3).
row(4).
row(5).
row(6).

%%Square is a single square tile on a Connect4 grid
%It can host either players' piece, 
%or be empty.
/*
square(col(_),row(_)) :- empty.
square(col(_),row(_)) :- red.
square(col(_),row(_)) :- blue.
*/
% test data
square(col(1),row(_),x).
square(col(2),row(_),x).
square(col(3),row(_),o).
square(col(4),row(_),o).
square(col(5),row(_),x).
square(col(6),row(_),x).
square(col(7),row(_),x).

red.
blue.
empty.
/*
-- 
data Square = Square Int Int
    deriving (Show)

instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

instance Ord Square where
    (Square a b) <= (Square c d) = a < c || (a == c && b <= d)
*/


/*
-- GameBoard is a map from a square to the piece in that square
type GameBoard = Map Square Piece
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



%play_player(Board)
%play_player(Board)
%play_player(Board)
/*
play_player(Board) :-
    displayBoardExample,
    print('Player X turn:'),
    nl,
    read(N),
%    insert_piece(Board, x, N, BoardX),
%    display_board(BoardN),
    displayBoardExample, 
    print('Player O turn:'),
    nl,
    read(N),
%    insert_piece(Board, o, N, BoardO),
%    display_board(BoardO),
    play_player(BoardO).
*/


%insert_piece(Board, Colour, Column, BoardX).


displayPiece(empty) :- write('-').
displayPiece(red) :- write('X').
displayPiece(blue) :- write('O').



exampleBoard :-
    col(1,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(2,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(3,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(4,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(5,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(6,true,empty,0,[empty,empty,empty,empty,empty,empty]),
    col(7,true,empty,0,[empty,empty,empty,empty,empty,empty]).

displayBoardExample :- 
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| '),
    nl,    
    print(' |_|_|_|_|_|_|_| ').

% display board row by row (?)

displayRow(row(Y)) :- 
    write(' |'),         
    displaySquares(col(1),row(Y)).

displaySquares(col(7),row(Y)) :-
    square(col(7),row(Y),Piece),
    write(Piece),
    write('|').
displaySquares(col(X),row(Y)) :-
    square(col(7),row(Y),Piece),
    write(Piece),
    write('|'),
    X1 is X + 1,
    displaySquares(col(X1), row(Y)).





    /*
*/
