signature SEARCH = sig
  type 'a collection;
  val create : ('a * 'a -> order) -> 'a list -> 'a collection;
  val traverse : 'a collection -> 'a list;
  val search : ('a * 'a -> order) -> 'a collection -> 'a -> bool;
end;

structure LinearSearch : SEARCH = struct
  type 'a collection = 'a list;

  fun insert _ x [] = [x]
    | insert cmp x (y::ys) = case cmp(x,y) of
                               GREATER => y::insert cmp x ys
                             | _ => x::y::ys;

  fun create _ [] = []
    | create cmp (x::xs) = insert cmp x (create cmp xs);

  fun traverse xs = xs;

  fun search _ [] _ = false
    | search cmp (x::xs) y = case cmp(x,y) of
                               LESS => search cmp xs y
                             | EQUAL => true
                             | GREATER => false;
end;

signature SORT = sig
  val sort : ('a * 'a -> order) -> 'a list -> 'a list;
end;

structure QuickSort : SORT = struct
  fun partition _ _ [] = ([], [])
    | partition cmp x (y::ys) = let
                                  val (smaller, larger) = partition cmp x ys
                                in
                                  case cmp(x,y) of
                                    GREATER => (y::smaller, larger)
                                  | _ => (smaller, y::larger)
                                end;

  fun sort _ [] = []
    | sort cmp (x::xs) = let
                           val (smaller, larger) = partition cmp x xs
                         in
                           sort cmp smaller @ [x] @ sort cmp larger
                         end;
end;

functor SearchSort(Search : SEARCH) : SORT = struct
  fun sort cmp xs = Search.traverse (Search.create cmp xs);
end;

signature TEST = sig
  val test : unit -> bool;
end;

fun genData 0 _ = []
  | genData n seed = seed :: genData (n-1) ((seed * 12345 + 6789) mod 32768);

val data = genData 20000 0;

val sortedData = QuickSort.sort Int.compare data;

functor TestSearch(Search : SEARCH) : TEST = struct
  fun test () = let
                  val collection = Search.create Int.compare data;
                in
                  Search.search Int.compare collection 6789
                  andalso
                  not (Search.search Int.compare collection 6794)
                end;
end;

functor TestSort(Sort : SORT) : TEST = struct
  fun test () = Sort.sort Int.compare data = sortedData;
end;

(* To test LinearSearch, for example, do the following:
 *
 * - structure T = TestSearch(LinearSearch);
 * - T.test();
 * - structure U = TestSort(SearchSort(LinearSearch));
 * - U.test();
 *
 * The output of each test() should be true.
 *)
