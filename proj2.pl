/* Written by Tharun Dharmawickrema
   6th September 2021
   This program implements a predicate representation for a fill-in 
   puzzle and solves the puzzle. */
  
/* This file 'proj2.pl' is the main and only file. The file assumes
   that the puzzle and list of words(which are both ground) is represented as 
   a list of lists. It also assumes that the puzzle has at least one solution.
   
   The code will find the fillable spaces called slots both horizontally
   and vertically. It will then recursively fit the words from the list
   into the slots. The slots are chosen in order of which slot has the
   least number of matching words. The word list is also sorted in order
   of length. The code will find a solution or return False if none exists.
*/


% clpfd library is loaded to ensure that the correct version of transpose is
% used. ctypes library is loaded to use the is_alpha predicate later.
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(ctypes)).


%% puzzle_solution(+Puzzle, +WordList).
%
% The main predicate that gets called with the puzzle and the word list. The 
% horizontal slots are found, puzzle gets transposed and the vertical slots are 
% found. Slots (maximal sequence of fillable squares) are combined into one 
% list. The word list and slots are sorted, and a solution is found, if the 
% puzzle is solvable.
puzzle_solution(Puzzle, WordList):- 
                            find_all_slots(Puzzle, RowSlots),
                            transpose(Puzzle, TransposedPuzzle),
                            find_all_slots(TransposedPuzzle, ColumnSlots),
                            append(RowSlots, ColumnSlots, ListofSlots), 
                            sort_slots(ListofSlots, WordList, SortedSlots),
                            sort_list(WordList, SortedWordList),
                            find_solution(SortedWordList, SortedSlots).


%% find_all_slots(+Puzzle, ?SlotList). 
%
% Predicate is true when SlotList is a list of lists of all the row slots in 
% the given Puzzle (also a list of lists). Slots are found row by row using
% maplist. Append is used to remove the extra layer of brackets 
% (list of list of lists -> list of lists).
find_all_slots(Puzzle, SlotList) :- maplist(find_slots, Puzzle, List),
                                    append(List, SlotList).


%% find_slots(+Row, ?Slots).
%
% Predicate is true when Slots is a list of lists of all the slots in that Row, 
% which is a list of elements. It calls a tail-recursive predicate of the same
% name to do this.
find_slots(Row, Slots):- find_slots1(Row, [], Slots).


%% find_slots1(+Row, ?CurrentSlot, ?Slots).
%
% Predicate is true when Row is a list of elements, CurrentSlot is a list of 
% elements from Row that are part of the current slot & Slots is a list of all
% the slots in that Row. Each fillable square (pre-filled letter or unbound
% variable) is appended to the current slot. This slot is added into the Slots
% list when a '#' or the end of the row is found.
find_slots1([], [], []).
find_slots1([], CurrentSlot, Slots) :- 
                               CurrentSlot \== [],      % end of row found
                               length(CurrentSlot, N),
                               ( N > 1 ->      % CurrentSlot still has squares
                                   Slots = [CurrentSlot]
                               ; Slots = []
                               ).
find_slots1([X|Xs], CurrentSlot, Slots) :- 
                               (fillable_square(X) ->
                                   append(CurrentSlot, [X], NewCurrentSlot), 
                                   find_slots1(Xs, NewCurrentSlot, Slots)
                               
                               % if not fillable & CurrentSlot still has 
                               % squares, add to Slots
                               ; length(CurrentSlot, N), N > 1 ->
                                   Slots = [CurrentSlot|Tail],
                                   find_slots1(Xs, [], Tail)
                               
                               % otherwise, just recurse
                               ; find_slots1(Xs, [], Slots)
                               ).


%% fillable_square(+Element).
%
% Predicate is true when given element is either an unbound variable or is an
% alphabetical character.
fillable_square(Element) :- var(Element); 
                            is_alpha(Element).


%% sort_slots(+List, +WordList, ?SortedList).
%
% Predicate is true when SortedList is a list of sorted slots(from List) where
% the first element is the slot with the fewest matching words to fill. The
% list is first changed to include this matching word count, sorted, then
% recreated without the word count.
sort_slots(List, WordList, SortedList) :- 
                        slot_length_list(List, WordList, [], SlotLenList),
                        keysort(SlotLenList, SortedLenList),
                        maplist(slot_list, SortedLenList, SortedList).


%% slot_length_list(+SlotList, +WordList, ?List, ?FinalList).
%
% Predicate is true when SlotList is a list of slots, WordList is a list of 
% words, List contains the new list of slot elements currently being created & 
% FinalList holds the final created list. To each slot, a count of the number 
% of matching words is added to create a new element of type [Count-Slot].
slot_length_list([], _, List, List).
slot_length_list([X|Xs], WordList, List, FinalList) :- 
                        matching_word_count(X, WordList, 0, Count),
                        append(List, [Count-X], NewList),
                        slot_length_list(Xs, WordList, NewList, FinalList).


%% matching_word_count(+Slot, +WordList, ?N, ?Len).
%
% Predicate is true when Slot is the current slot,WordList is a list of words,
% N is the current number of words in WordList that match the Slot & Len holds 
% the final number of matching words. To check if the word matches the slot, 
% the length is checked for equality.
matching_word_count(_, [], N, N).                                                 
matching_word_count(Slot, [Word|Tail], N, Len) :- 
                                length(Slot, Len1),
                                length(Word, Len2),
                                ( Len1 =:= Len2 ->
                                    N1 is N+1, 
                                    matching_word_count(Slot, Tail, N1, Len)
                                ; matching_word_count(Slot, Tail, N, Len)
                                ).


%% slot_list(+(_-Element), ?Element).
%
% Predicate is true when Element is in both arguments. This is used to convert
% the slot-length list elements of the form [Count-Slot] into the normal slot
% list elments of the from [Slot].
slot_list(_-Element, Element).


%% sort_list(+List, ?SortedList).
%
% Predicate is true when List is the list of words and SortedList is List
% sorted in descending of word length (longest word first). A new list is 
% created with new elements of type [Length-Word], the list is sorted & the
% original list with elements of type [Word] is recreated.
sort_list(List, SortedList) :- maplist(length_list, List, LenList),
                               keysort(LenList, SortedLenList),
                               maplist(length_list, SortedList, SortedLenList). 


%% length_list(?Element, ?(N1-Element)).
%
% Predicate is true when the second argument is of type N1-Element where
% Element is the first argument and N1 is the negative value of the length
% of Element. Either the first or second argument will be unbound and this
% predicate is used to change the form of the given argument.
length_list(Element, N1-Element) :- N1 #= -N, 
                                    length(Element, N).


%% find_solution(+WordList, ?Slots).
%
% Predicate is true when the WordList is a list of words and Slots is a list
% of slots that are filled with the matching words. Select will add a word
% from WordList into Slots. The predicate will work recursively with the
% remaining Slots to find a solution where the words are fitted into the 
% correct slots or will return false if there is no solution.
find_solution([],[]).
find_solution([Word|Tail], Slots) :- select(Word, Slots, TempSlots),
                                     find_solution(Tail, TempSlots).
