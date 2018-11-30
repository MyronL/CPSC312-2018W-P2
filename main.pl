:- use_module(library(clpfd)). %for transpose
:- use_module(library(lists)). %for nth1

%GLOBAL VARS FOR DELAYS
superQuickDelay(0.6).
littleDelay(0.9).

% Player piece definitions:
playerPiece(red, x). % Player 1
playerPiece(blue, o). % Player 2
playerPiece(empty, '_'). %empty piece

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI + CONTROLS PLAY SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%%%% TO LOAD GAME:
% simply enter ?-play.
play :- welcomeScreen,
    nl,
    mainOptionsScreen.


%print the initial welcome screen
welcomeScreen :- print('**************************************'),
    nl,
    print('*                                   *'),
    nl,
    print('*        WELCOME TO                 *'),
    nl,
    print('*   PROLOG CONNECT 4!               *'),
    nl,
    print('*                                   *'),
    nl,
    print('**************************************'),
    nl,
    print(' play at your own risk...').

%print the initial Main Options Screen
mainOptionsScreen :- nl,
    print('START MENU:'),
    nl,
    nl,
    print('Player VS Player [ Enter 1 ]'),
    nl,
    print('Player VS AI [ Enter 2 ]'),
    nl,
    print('Exit [ Enter any other key ]'),
    nl,
    nl,
    read(X),
    play_mode(X).    


% handling for PVP mode when selected:
play_mode(1) :- print('Playing against your friend'),
    nl,
    print('Ready... 3. 2.. 1...'),
    superQuickDelay(G),
    sleep(G),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurn(B, red).

% handling for VS AI mode when selected:
play_mode(2) :- print('Playing against AI'),
    nl,
    aiLevelOptions,
    read(Difficulty),
    checkInvalidLevel(Valid, Difficulty),
    nl,
    print('Ready... 3. 2.. 1...'),
    superQuickDelay(G),
    sleep(G),
    nl,
    print('GO!'),
    nl,
    initBoardFull(B),
    displayBoard(B),
    gameTurnMachine(B, red, human, Valid).

% handling for exit-game when selected:
play_mode(_) :-
    print('Good bye'),
    abort.

% Helper: handling for AI difficulty selection
checkInvalidLevel(D, 4) :- D = insane, write('Ah insane mode I see...'), nl.
checkInvalidLevel(D, 3) :- D = hard.
checkInvalidLevel(D, 2) :- D = easy.
checkInvalidLevel(D, 1) :- D = supereasy.
checkInvalidLevel(_, _) :- mainOptionsScreen.


aiLevelOptions :-
    write('please enter AI difficulty:'),nl,
    write('[enter 1]: Super Easy'),nl,
    write('[enter 2]: Easy'),nl,
    write('[enter 3]: Hard'),nl,
    write('[enter anything else]: back to menu'),nl.

% GameTurn Handling: 

% GameTurn: PVP Player 1 wins scenario
gameTurn(Board, _) :- win(Board, x), 
    write('Congratulations Player 1 (red) Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.

% GameTurn: PVP Player 2 wins scenario
gameTurn(Board, _) :- win(Board, o), 
    write('Congratulations Player 2 (blue) Wins!'),
    nl,    
    littleDelay(S),
    sleep(S),
    nl,
    play.

% GameTurn: Draw scenario
gameTurn(Board, _) :- full(Board), write('It\'s a tie!').

% GameTurn: Turn-by-turn scenario:
gameTurn(Board, Player) :-
    nl,
    write(Player),
    write(' player\'s turn:'),
    nl,
    write('Moves Available:'),
    getListOfAvailMoves(Board, ListTotal),
    write(ListTotal),
    nl,
    write('Please select a column. (Or enter qq to quit)'),
    nl,
    read(Move),
    getMove(Player, Move, ListTotal, Board, BoardAfter),
    displayBoard(BoardAfter),
    flipPlayer(Player, OtherPlayer),
    gameTurn(BoardAfter, OtherPlayer).

%Use this for getting opposite player.
%Use as flipPlayer(Player, OtherPlayer).
flipPlayer(red, blue).
flipPlayer(blue, red).
flipPlayer(p1, p2).
flipPlayer(p2, p1).


%=============== ============
% Winning Conditions
win(Board, Player) :- rowWin(Board, Player).
win(Board, Player) :- columnWin(Board, Player).
win(Board, Player) :- diagonalWin(Board, Player).

% Win by 4 consecutive pieces in a column
columnWin(Board, Player) :-
    append(_, [Column|_], Board),
    append(_, [Player, Player, Player, Player|_], Column).

% Win by 4 consecutive pieces in a row
rowWin(Board, Player) :-
    transpose(Board, Board1),
    columnWin(Board1, Player).

% Win by 4 consecutive pieces in a diagonal (\)
diagonalWin(Board, Player) :- diagonalRight(Board, Player).

% Win by 4 consecutive pieces in a diagonal (/)
diagonalWin(Board, Player) :- diagonalLeft(Board, Player).

% helper for checking diagonal (\)
diagonalRight(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

% helper for checking diagonal (/)
diagonalLeft(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.


% FOR TIE GAMES:
% full(Board) - Checks if board is full
full(Board) :- full(Board, 7).
full(_, 0). 
%CASE: board full, recursive
full(Board, N) :-
    nth1(N, Board, Col),
    playerPiece(empty, EmptySpace),
    \+ member(EmptySpace, Col),
    N1 is N-1,
    full(Board, N1).


% Prints the full list of moves available, unused currently
showListTotal([]).
showListTotal([H|T]) :-
    write(H),
    showListTotal(T).

% getListOfAvailMoves(Board, ListTotal)
% Get list of free, available columns in a board
getListOfAvailMoves(Board, ListTotal) :-
    getListOfAvailMoves(7, Board, [], ListTotal).

getListOfAvailMoves(0, _, ListTotal, ListTotal).
%CASE: col has available move
getListOfAvailMoves(Count, Board, Acc, ListTotal) :-
    nth1(Count,Board,Col),  %get nth col from board
    columnFree(Col),  %incl if col has empties in it
    Count1 is Count-1,
    getListOfAvailMoves(Count1, Board, [Count|Acc], ListTotal).
%CASE: col DOESNT has available move
getListOfAvailMoves(Count, Board, Acc, ListTotal) :-
    Count1 is Count-1,
    getListOfAvailMoves(Count1, Board, Acc, ListTotal).

%check if this column is free (has space)
columnFree(Column) :- member('_',Column).


% UI+Control handler to get moves
% OK Case
getMove(Player, Move, ListOfMoves, Board, BoardAfter) :-
    member(Move, ListOfMoves),
    insertToBoard(Board, Move, Player, BoardAfter).

% P2 Blue Concede Case
getMove(blue, Move, _, _, _) :-
    isConcede(Move),
    write('Player 2 (blue) has conceded'),
    nl,
    nl,
    play.

% P1 Red Concede Case
getMove(red, Move, _, _, _) :-
    isConcede(Move),
    write('Player 1 (red) has conceded'),
    nl,
    nl,
    play.

% Invalid Input Case
getMove(Player, _, ListOfMoves, Board, BoardAfter) :-
    write('Not valid move. Please select a valid move '),
    write(ListOfMoves),
    nl,
    read(NewMove),
    nl,
    getMove(Player, NewMove, ListOfMoves, Board, BoardAfter).

% Helper: check if move is concede input
isConcede(Move) :- Move == qq.

% ======= ========= ===========

% Updating Board By Player Move: 
insertToBoard(Board, Move, Player, BoardAfter) :-
    nth1(Move, Board, Col),
    insertColumn(Col, Player, ColNew),
    updateBoard(Board, Move, ColNew, BoardAfter).

% Updating column during Player Move: 
% CASE: inserting player piece in column top
insertColumn([EmptySpace,X|T], Player, [PlayerPiece, X|T]) :-
    playerPiece(Player, PlayerPiece),
    playerPiece(empty, EmptySpace),
    \+ X == EmptySpace.
    
% CASE: inserting FIRST piece in column
insertColumn([EmptySpace], Player, [PlayerPiece]) :-
    playerPiece(Player, PlayerPiece),
    playerPiece(empty, EmptySpace).
    
% CASE: recursive step, skipping present pieces
insertColumn([H|T], Player, [H|R]) :-
    insertColumn(T, Player, R).


% Updates board with new column:
updateBoard([_|T], 1, ColNew, [ColNew|T]). %BASE
updateBoard([H|T], Move, ColNew, [H|X]) :- 
    Move1 is Move-1, 
    updateBoard(T, Move1, ColNew, X). %RECURSIVE

%================ ================ 
% VS AI GAME HANDLING:

% Human turn win condition
gameTurnMachine(Board, _, _, _) :- win(Board, x), 
    write('Congratulations Player 1 Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.

% AI turn win condition
gameTurnMachine(Board, _, _, _) :- win(Board, o), 
    write('Nice try... AI Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.

% Tie
gameTurnMachine(Board, _, _, _) :- full(Board), write('It\'s a tie!').

% % Human's turn
gameTurnMachine(Board, Player, human, Difficulty) :-
    nl,
    write(Player),
    write(' player\'s turn:'),
    nl,
    write('Moves Available: '),
    getListOfAvailMoves(Board, ListTotal),
    write(ListTotal),
    nl,
    read(Move),
    getMove(Player, Move, ListTotal, Board, BoardAfter),
    displayBoard(BoardAfter),
    gameTurnMachine(BoardAfter, Player, ai, Difficulty).

% % AI's turn
gameTurnMachine(Board, Player, ai, Difficulty) :-
    nl,
    write('Machine\'s turn'),
    nl,
    machineTurn(blue, Board, BoardMachine, Difficulty),
    nl,
    displayBoard(BoardMachine),
    gameTurnMachine(BoardMachine, Player, human, Difficulty).


%Use this for getting opposite player type.
%Use as flipPlayerType(PlayerType, OtherPlayerType).
flipPlayerType(human, ai).
flipPlayerType(ai, human).



% TODO
%================ ================ 
%AI Version 1
% Initiate machine turn
% Winning condition
machineTurn(_, Board, Board, _) :- win(Board, x).


% Super Easy AI (just randomly places pieces lol)
machineTurn(Player, Board, BoardAfter, supereasy) :-
    getListOfAvailMoves(Board, ListTotal),
    length(ListTotal, N),
    random(1, N, Pick),
    nth1(Pick, ListTotal, Move),   
    machineMove(Board, Move, Player, BoardAfter).

% Easy AI
machineTurn(Player, Board, BoardAfter, easy) :-
    selectMove(Board, 2, o, Move),
    machineMove(Board, Move, Player, BoardAfter).

% Hard AI
machineTurn(Player, Board, BoardAfter, hard) :-
    selectMove(Board, 4, o, Move),
    write('Machine Played '),
    write(Move),
    superQuickDelay(S),
    sleep(S),  %slight delay so user comprehends what AI move is    
    nl,
    insertToBoard(Board, Move, Player, BoardAfter).

% Insane AI
machineTurn(Player, Board, BoardAfter, insane) :-
    selectMove(Board, 6, o, Move),
    write('Machine Played '),
    write(Move),
    superQuickDelay(S),
    sleep(S),  %slight delay so user comprehends what AI move is    
    nl,
    insertToBoard(Board, Move, Player, BoardAfter).

% Common Helper to facilitate AI moves
machineMove(Board, Move, Player, BoardAfter) :-
    write('Machine Played '),
    write(Move),
    superQuickDelay(S),
    sleep(S),  %slight delay so user comprehends what AI move is    
    nl,
    insertToBoard(Board, Move, Player, BoardAfter).

% Select Move gets best move by searching each column and assigning
% a score for each move.
% depth is how far you want to search into a path
% Player is actually the piece rather than colour
selectMove(Board, Depth, Player, Move) :-
    columnScore(Board, Depth, Player, 1, Score1),
    columnScore(Board, Depth, Player, 2, Score2),
    columnScore(Board, Depth, Player, 3, Score3),
    columnScore(Board, Depth, Player, 4, Score4),
    columnScore(Board, Depth, Player, 5, Score5),
    columnScore(Board, Depth, Player, 6, Score6),
    columnScore(Board, Depth, Player, 7, Score7),
    selectBestScore([Score1,Score2,Score3,Score4,Score5,Score6,Score7], Move).


% columnScore(Board, Depth, Player, Column, Score)
% Assigns a score to each column recursively
% Stop at depth = 0
columnScore(_,0,_,_,0).
% When Board is full
columnScore(Board,_,_,_,0) :- full(Board).
% When machine finds an invalid move
columnScore(Board,_,_,Column,-42) :- \+ validCpuMove(Board, Column).
% If the opponent is close to winning
columnScore(Board,_,Player,Column,Score) :-
    canWin(Board, Player, Column),
    movesLeft(Board, MovesLeft),
    Score is ((MovesLeft - 1)//2).
% Recurse through each column and path to find the optimum move
% Blue and Red will alternate every recursion TODO: find a better name
columnScore(Board, Depth, Player, Column, Score) :-
    validCpuMove(Board, Column),
    playerPiece(Blue, Player),
    insertToBoard(Board, Column, Blue, BoardAfter),
    flipPlayer(Red, Blue),
    playerPiece(Red, Player2),
    Depth1 is Depth-1,
    columnScore(BoardAfter, Depth1, Player2, 1, S1),
    columnScore(BoardAfter, Depth1, Player2, 2, S2),
    columnScore(BoardAfter, Depth1, Player2, 3, S3),
    columnScore(BoardAfter, Depth1, Player2, 4, S4),
    columnScore(BoardAfter, Depth1, Player2, 5, S5),
    columnScore(BoardAfter, Depth1, Player2, 6, S6),
    columnScore(BoardAfter, Depth1, Player2, 7, S7),
    selectMax([S1,S2,S3,S4,S5,S6,S7], Max),
    Score is -1 * Max.

% Checks if the move is valid for machine
validCpuMove(Board, Move) :-
    integer(Move),
    Move >= 1,
    Move =< 7,
    nth1(Move, Board, [H|_]),
    H = '_'.

% Selects the best score by picking the maximum
selectBestScore(ScoreList, Move) :-
    selectMax(ScoreList, Max),
    findBestMove(Move, ScoreList, Max).

% Finds the best move within a list of scores, each index corresponds to a column
findBestMove(Index, ScoreList, Score) :-
    findall(N, nth1(N, ScoreList, Score), Indexes),
    length(Indexes, Length),
    Bounds is Length+1,
    random(1, Bounds, Random),
    nth1(Random, Indexes, I),
    Index is I.

% Selects the maximum
selectMax([], -42).
selectMax([H|T], Max) :-
    selectMax(T, Max2),
    Max is max(H,Max2).

% Finds how many moves are left on the board
movesLeft([], 0).
movesLeft([H|T], Moves) :-
    movesLeftColumn(H, Move1),
    movesLeft(T, Move2),
    Moves is Move1 + Move2.

% Finds how many moves are left in the column
movesLeftColumn([], 0).
movesLeftColumn([H|T], Moves) :-
    dif(H, '_'),
    movesLeftColumn(T, Moves).
movesLeftColumn(['_'|T], Moves) :-
    movesLeftColumn(T, Move1),
    Moves is Move1 + 1.

% Checks if the human is within winning range, Player is the piece represented, 
% Player1 is the colour
canWin(Board, Player, Column) :-
    playerPiece(Player1, Player),
    validCpuMove(Board,Column),
    insertToBoard(Board, Column, Player1, BoardAfter),
    win(BoardAfter, Player).



flipBetType(green, gold).
flipBetType(gold, green).

gameTurnVegas(Board, _, _, _, _, _) :- win(Board, x), 
    write('Congratulations Machine 1 (red) Wins!'),
    nl,
    littleDelay(S),
    sleep(S),
    nl,    
    play.

% GameTurn: PVP Player 2 wins scenario
gameTurnVegas(Board, _, _, _, _, _):- win(Board, o), 
    write('Congratulations Machine 2 (blue) Wins!'),
    nl,    
    littleDelay(S),
    sleep(S),
    nl,
    play.

% GameTurn: Draw scenario
gameTurnVegas(Board, _, _, _, _, _) :- full(Board), write('It\'s a tie!').

% GameTurn: Turn-by-turn scenario:
gameTurnVegas(Board, Player, ai, AiRedLvl, AIBlueLvl) :-
    nl,
    write(Player),
    write(' Machine\'s turn:'),
    nl,
    vegasAILVLMatch(Player, AiRedLvl, AIBlueLvl, Difficulty),
    machineTurn(Player, Board, BoardMachine, Difficulty),
%    write('Moves Available:'),
%    getListOfAvailMoves(Board, ListTotal),
%    write(ListTotal),
%    nl,
%    write('Please select a column. (Or enter qq to quit)'),
%    nl,
%    read(Move),
%    getMove(Player, Move, ListTotal, Board, BoardAfter),
%    displayBoard(BoardAfter),
    displayBoard(BoardMachine),
    flipPlayer(Player, OtherPlayer),
    gameTurnVegas(BoardMachine, OtherPlayer, ai, AiRedLvl, AIBlueLvl).    

% returns the correct AI lvl for Las Vegas Mode (AI vs AI)
% use as vegasAILVLMatch(Player, Difficulty).
vegasAILVLMatch(red, AiRedLvl, _, AiRedLvl).
vegasAILVLMatch(blue, _, AIBlueLvl, AIBlueLvl).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UI BOARD RENDERING SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


% Board Headers:
boardHeaders(' |1|2|3|4|5|6|7| ').

% INITIAL BOARD SETUP:
initBoardFull([
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).


% transpose board to make it easier to print rows
displayBoard(Board) :-
    boardHeaders(BH),
    write(BH),nl,nl,
    transpose(Board, BoardNew),
    displayRows(BoardNew).


% display single row
displayRows([]).
displayRows([H|T]) :-
    write(' |'),
    displaySquares(H),
    nl,
    displayRows(T).

% displays each cell in the row
displaySquares([]).
displaySquares([H|T]) :-
    write(H),
    write('|'),
    displaySquares(T).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TESTING SECTION:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


% Game Board: x wins, with connected-4 as column
testBoardColWinX([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', 'x', 'x', 'x', 'x'],
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'o', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

% Game Board: o wins, with connected-4 as row
testBoardRowWinO([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', 'x', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).    

%testBoardColWinX(B), win(B, x). %B= matrix from testBoardColWinX
%testBoardColWinX(B), win(B, o). %false
%testBoardRowWinO(B), win(B, o). %B= matrix from testBoardRowWinO
%testBoardRowWinO(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardColWinX(B), gameTurn(B, _). %Player1 wins
%testBoardRowWinO(B), gameTurn(B, _). %Player2 wins
%
%testBoardColWinX(B), gameTurnMachine(B, red, ai, hard). %Player1 wins
%testBoardColWinX(B), gameTurnMachine(B, red, human, hard). %Player1 wins
%testBoardRowWinO(B), gameTurnMachine(B, red, ai, hard). %AI wins
%testBoardRowWinO(B), gameTurnMachine(B, red, human, hard). %AI wins


% Game Board: x wins, with connected-4 as diagonal (\)
testBoardDiagWinX([
    ['_', '_', 'x', 'o', 'o', 'x'],
    ['_', '_', '_', 'x', 'o', 'o'],
    ['_', '_', '_', '_', 'x', 'o'],
    ['_', '_', '_', '_', '_', 'x'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

% % Game Board: o wins, with connected-4 as diagonal (/)
testBoardDiagWinO([
    ['_', '_', '_', '_', '_', 'o'],
    ['_', '_', '_', '_', 'o', 'x'],
    ['_', '_', '_', 'o', 'x', 'x'],
    ['_', '_', 'o', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

%testBoardDiagWinX(B), win(B, x). %B= matrix from testBoardDiagWinX
%testBoardDiagWinX(B), win(B, o). %false
%testBoardDiagWinO(B), win(B, o). %B= matrix from testBoardDiagWinO
%testBoardDiagWinO(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardDiagWinX(B), gameTurn(B, _). %Player1 wins
%testBoardDiagWinO(B), gameTurn(B, _). %Player2 wins

%testBoardDiagWinX(B), gameTurnMachine(B, red, human, hard). %Player1 wins
%testBoardDiagWinX(B), gameTurnMachine(B, red, ai, hard). %Player1 wins
%testBoardDiagWinO(B), gameTurnMachine(B, red, human, hard). %AI wins
%testBoardDiagWinO(B), gameTurnMachine(B, red, ai, hard). %AI wins

% Game Board: draw; no one wins; filled board
testBoardIsFullDraw([
    ['x', 'x', 'x', 'o', 'o', 'o'],
    ['x', 'o', 'o', 'x', 'x', 'x'],
    ['o', 'x', 'x', 'x', 'o', 'o'],
    ['x', 'x', 'o', 'o', 'x', 'o'],
    ['o', 'o', 'x', 'o', 'x', 'o'],
    ['o', 'o', 'x', 'o', 'x', 'x'],
    ['x', 'x', 'o', 'x', 'o', 'o']
    ]).

%testBoardIsFullDraw(B), win(B, o). %false
%testBoardIsFullDraw(B), win(B, x). %false

%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardIsFullDraw(B), gameTurn(B, _). %Tie Game

%testBoardIsFullDraw(B), gameTurnMachine(B, red, human, hard). %Tie Game
%testBoardIsFullDraw(B), gameTurnMachine(B, red, ai, hard). %Tie Game

% Game Board: game in progress; no one wins; 
% half-filled board
testBoardHalfFull([
    ['x', 'x', 'x', 'o', 'o', 'o'],
    ['x', 'o', 'o', 'x', 'x', 'x'],
    ['o', 'x', 'x', 'x', 'o', 'o'],
    ['_', '_', '_', 'x', 'x', 'o'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_']
    ]).

%testBoardHalfFull(B), win(B, o). %false
%testBoardHalfFull(B), win(B, x). %false
%
%%CHECKING WIN CONDITIONS LIVE:
%(input these as ?- queries.)
%testBoardHalfFull(B), gameTurnMachine(B, red, human, hard).
%testBoardHalfFull(B), gameTurnMachine(B, red, ai, hard).