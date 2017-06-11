(ns clojure-learning.algorithms.week5.balanced-binary-trees)

;; TODO:: implement operations on BBT::
;; min/max, search, select, predecessor/successor, insertion/deletion
;; try with worst running times(O(height)) on not balanced binary search trees.
;; binary searfch tree property:: everything smaller than root => left branch, evertyhing
;; bigger than root -> right  branch
;; insertion/deletion => swap nodes or mutate back/fw pointers <= if swap => will
;; break the rule for binary-search-trees(left<root, right>root)
;; O(height) is the single variable which governs the running time of a binary-search-tree
;; the height should be ideally small.
;; ensure that height always O(logn)
;; red-black trees are the same as binary-balanced-searched-trees + some invariants!
;; invariants:
;; 1. each node stores an additional bit of information:: red/black
;; 2. the root is always black!
;; 3. NO 2 reds in a row!:: red node => only black children
;; 4. every path you take to search for a NON existent node should
;;    pass through exactly the same NUMBER of root-NULL nodes.(2, or 3)
;;   (searching for unexesitent1 -> passes through 1 black for instance, while searching
;; root-null path == unsuccessfull search!
;; for unexistent5 -> passes through 2 blacks... -> violation!
;; a red-black tree can have no red-colored nodes! invariant 3 is valid.

;; a chain with 3 nodes cannot be a red-black-tree!
;;
;; dynamic =~ having insertions & deletions
;; 
;; if you satisfy all 4 invariants => will ensure height will be small => all operations
;; will be fast!

;; when inserting:: always do a root-null path and insert the new node, as the LEAF
;; node;
;; whatever color we choose for the new node:: we have the potention of destroying the
;; Red Black Treee invariance!
;; a row ~= left branch red & it's parent is also red => NOT valid(3rd rule is broken)
;; if the new node to be inserted is black => the 4rth rule is MORE global => can
;; break the rest of the paths...
;; chose always the option where the damage is more isolated => making the new node
;; red(although it might be that its parent is already red <= the 3rd rule is broken
;; however the damage is more localized. => it might be that the parent is black
;; and we're done there... nothing is violated.
;; the solution is to recolor it's immediate parent together with its grandfather
;; and other grandfather son all black!
;;      grandf-black
;;  son1-black     son2-black
;;              new-node-red
;; previously the son2-black was RED remember.
;; red-black trees have logarithmic HEIGHT!!!

;; rbt have logarithmic insertions: have to either switch colors(going through 
;; the parents...in ct time switching colors

;;;;;;;;;;;
;; Heaps type of data-structure
;; two questions when it comes to a new data-structure::
;; what are the operations the data-structure supports?
;; what is the running-time of those operations?

;; a heap is always concerned with two operations::
;; 1. insertion
;; 2. extracting the minimum object by the key( sorted-map?, treemap?)
;; heap:: a container foreach of the objects, and each of the objs have a key 
;; <= associative.
;; a heap can support EITHER extract-max, or extract-min.
;; heap operations should be of logn running time; where n=numb of objects in the heap.
;; there's also a "heapify()" operation which should take a collection and transform
;; that into a heap datastructure. normally the heapify on a 1st attempt can do in::
;; nlogn, where n -> number of items from the source coll, and logn the insertion
;; running time. but a heap should provide some batch operations for heapifying.

;; dijkstra can be optimized using heaps.

;; when to use a heap? when you identify that your system performs repeated
;; computations for extracting the minimum, specially via exhaustive search.
;; via:: sorting, more exactly "selection sort" => find the minium, put that in a
;; container, scan for the n-1 elements, find the miniumum among them => put that
;; in a second position...and so on...
;; selection sort:: quadtratic running time algorithm(O(n^2))
;; => HEAP-SORT:: insert all elements in the heap => extract one/each
;; every operation should operate in logn => insert+extract in heap::
;; nlogn -> for heap-sort.
;; nlogn for merge-sort! which is quite quick! randomized quick sort as well.
;; it does NOT exist a sorting by comparison algorithm better than:: nlogn.
;; if we use a comparison based sorting(like heap-sort) algorithm 
;; the best running time is nlogn.
;; O(n^2) => O(nlogn)
;; not quite as fast as quick sort but good enough.

;; problem:: median maintenance::
;; a faster(than (O(N))) running time to compute the median(key) and extract the
;; median smallest number::
;; using two heaps:: 1 heap will contain the 1st half of the items, the 2nd heap
;; will contain the half-rest of the items.
;; the only problem is to re-balance the heaps:
;; heap1:: 1..10
;; heap2:: 11..20
;; 
;; dijkstra can be improved using heaps from: O(n x m) -> where n: number of vertices
;; in graph, and m: number of edges(adjacents/connections) => m takes the amount
;; of time to find and extract the minimum => using heaps:: dijkstra can be
;; improved to O(nlogn)

;;;;;
;; how to code a Heap datastructure from scratch??
;; what are the operations, what s the data-structure good for, what's the running time
;; of those operations?
;; inserts:: logarithmic
;; extract-min:: logarithmic
;; allow duplicates
;; heap:: one view as a binary-tree as implementation of those ops, 
;; one view as an array.
;; heap property:: any node X, should have the key <= than ANY of the keys/val 
;; for all it's children
;; heap structure imposes usefull structure on how the objects can be arranged.
;; the root also has a minim value key!!!
;; heap is implemented as an array structure:: considering a binary tree, each level
;; of the tree will map to an array position: level-1 root => maps at index 0,
;; level-2 the children of root, will map to array positions: 1, 2
;; => think of a heap in terms of a tree structure(mental mapping) however implement
;; the heaps as an "organized" array.
;; identifying the root of an ith node is quite simple:
;; 1 2 3 4 5 6  7 8...n
;; 4 4 8 9 4 12 9 11 
;; parent(of 8) -> if position of 8 is odd? 3 => parent(8, idx:3) = idx/2 => floor = 1

;; parent(of 9) -> idx(4 even) / 2 => 4

;; the children of a parent(i) can be easily determined by: 2*i and (2*i + 1)
;; children(8) @index 3:: 2*3 -> index 6, index 7:: 12 & 9 are the children of 8
;; => instead of identifying the parents and children in a tree based style, by using
;; these simple calculations can determine the structure of the heap
;; => no traversing, but simple calculations(divide by 2) and multiply by 2
;; HEAPs are REALLY implemented as ARRAY, but you can see them as a binary-search-tree

;; insertion implementation:: always insert at the last level! the inserted value
;; should have the key-value higher than its parent.
;; if the inserted "node" has a value smaller than the last-parent? then recursively
;; swap the parent<=>inserted-child, the parent will become the child, and the child
;; (newly inserted node) will become the parent. NO NEED TO BOTHER about the other
;; child of the initial parent, we're interested here in PRESERVING the "binary-tree"
;; mental-structure, which is an array in the end...so flattenized
;; recursively "bubble-up" until root.
;; running time:: logarithmic number of levels => O(logn)

;; extract-min!!! + bubble-down
;; KNOW that the min is the ROOT -> removing/retunring it(extracting)
;; will leave the heap structure without any root, 
;; so heaps should be perfectly balanced-binary-trees.
;; 1. delete root!
;; 2. pick the last LEAF and promote it to be ROOT
;; 3. the heap-property is broken, because the last LEAF is higher than the children
;;    keys =>
;;    compare the children of the new root, and choose the SMALLER one, to bubble
;;    down the swap. the heap violation resulted from promoting the last node
;; as root takes logarithmic time to fix:: BUBBLE DOWN
;; NOTE: if you consider to swap the new root with the higher value => the work to
;; fix the perfectly balanced binary tree is harder!!! and more traversals are required!

;; extracting min itself is a constant time operation, however the re-arrangement
;; of the heap structure after deleting it, takes logarithmic running time.

