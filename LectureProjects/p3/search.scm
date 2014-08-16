(define (loadsearch) (load "search.scm"))
;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #f)
;; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))  ; mit-schemen function first

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...
(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'v '(v) '(words for node v))
   (make-graph-element 'x '(v y) '(words for node x))
   (make-graph-element 'y '(w z) '(words for node y))
   (make-graph-element 'w '(x z u) '(words for node w))
   (make-graph-element 'z '() '(words for node z))
   (make-graph-element 'u '(w) '(words for node u)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
      (cond ((null? still-to-do)
             (if *search-debug*
                 (write-line (list 'can-not 'reach 'from initial-state)))
             #f)
            (else
             (let ((current (car still-to-do)))
               (if *search-debug*
                   (write-line (list 'now-at current)))
               (cond ((goal? current)
                      (if *search-debug*
                          (write-line (list 'reach current)))
                      #t)
                     (else
                      (search-inner
                       (merge (successors graph current) (cdr still-to-do)))))))))

  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))

  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))


;; (DFS-simple 'a
;;             (lambda (node) (eq? node 'l))
;;             test-graph)

(define (BFS-simple start goal? graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))

  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))


;; (BFS-simple 'a
;;             (lambda (node) (eq? node 'l))
;;             test-graph)

;; you will need to write a similar search procedure that handles cycles
(define (GDFS start goal? graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))
    
  ;; List of visited nodes to avoid revisit
  (let ((visitedlist (list)))

    ;; Merge strategy will check the visitedlist first and then decide whether to add the node
    (define (merge new old)
      (let ((still-to-do old))
        (map (lambda (node)
               (cond ((memq node visitedlist) #f)
                     (else (set! still-to-do (cons node still-to-do)))))
             new)
;;        (write-line new)
;;        (write-line still-to-do)
        still-to-do))

    ;; Successor will mark the current node on the visitedlist
    (define (successor graph current)
      (set! visitedlist (cons current visitedlist))
      (find-node-children graph current))

    ;; Search
    (search start goal? successor merge graph)))

;;(GDFS 'x (lambda (node) (eq? node 'z)) test-cycle)

(define (GBFS start goal? graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))

  ;; List of visited nodes to avoid revisit
  (let ((visitedlist (list)))

    ;; Merge strategy will check the visitedlist first and then decide whether to add the node
    (define (merge new old)
      (let ((still-to-do old))
        (map (lambda (node)
               (cond ((memq node visitedlist) #f)
                     (else (set! still-to-do (append still-to-do (list node))))))
             new)
;;        (write-line new)
;;        (write-line still-to-do)
        still-to-do))

    ;; Successor will mark the current node on the visitedlist
    (define (successor graph current)
      (set! visitedlist (cons current visitedlist))
      (find-node-children graph current))

    ;; Search
    (search start goal? successor merge graph)))

;; (GBFS 'x (lambda (node) (eq? node 'z)) test-cycle)

;; Search with action
(define (action-search initial-state goal? successors merge action graph)
    ;; initial-state is the start state of the search
    ;;
    ;; goal? is the predicate that determines whether we have
    ;; reached the goal
    ;;
    ;; successors computes from the current state all successor states
    ;;
    ;; merge combines new states with the set of states still to explore
    (define (search-inner still-to-do)
        (cond ((null? still-to-do)
               (if *search-debug*
                   (write-line (list 'can-not 'reach 'from initial-state)))
               #f)
              (else
               (let ((current (car still-to-do)))
                 (if *search-debug*
                     (write-line (list 'now-at current)))
                 (cond ((goal? current)
                        (if *search-debug*
                            (write-line (list 'reach current)))
                        #t)
                       (else
                        (action current)
                        (search-inner
                         (merge (successors graph current) (cdr still-to-do)))))))))

  (search-inner (list initial-state)))


(define (DFS-action start goal? action graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))

    (action-search start
                   goal?
                   find-node-children
                   (lambda (new old) (append new old))
                   action
                   graph))


;; (DFS-action 'a
;;             (lambda (node) (eq? node 'l))
;;             (lambda (node) (write-line (find-node-contents test-graph node)))
;;             test-graph)

(define (BFS-action start goal? action graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))

    (action-search start
            goal?
            find-node-children
            (lambda (new old) (append old new))
            action
            graph))


;; (BFS-action 'a
;;             (lambda (node) (eq? node 'l))
;;             (lambda (node) (write-line (find-node-contents test-graph node)))
;;             test-graph)

;; you will need to write a similar search procedure that handles cycles
(define (GDFS-action start goal? action graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))
    
  ;; List of visited nodes to avoid revisit
  (let ((visitedlist (list)))

    ;; Merge strategy will check the visitedlist first and then decide whether to add the node
    (define (merge new old)
      (let ((still-to-do old))
        (map (lambda (node)
               (cond ((memq node visitedlist) #f)
                     (else (set! still-to-do (cons node still-to-do)))))
             new)
;;        (write-line new)
;;        (write-line still-to-do)
        still-to-do))

    ;; Successor will mark the current node on the visitedlist
    (define (successor graph current)
      (set! visitedlist (cons current visitedlist))
      (find-node-children graph current))

    ;; Search
    (action-search start goal? successor merge action graph)))

;; (GDFS-action 'x
;;              (lambda (node) (eq? node 'z))
;;              (lambda (node) (write-line (find-node-contents test-cycle node)))
;;              test-cycle)

(define (GBFS-action start goal? action graph . searchname)
    (if (and (not (null? searchname)) *search-debug*)
        (write-line searchname))
    
  ;; List of visited nodes to avoid revisit
  (let ((visitedlist (list)))

    ;; Merge strategy will check the visitedlist first and then decide whether to add the node
    (define (merge new old)
      (let ((still-to-do old))
        (map (lambda (node)
               (cond ((memq node visitedlist) #f)
                     (else (set! still-to-do (append still-to-do (list node))))))
             new)
;;        (write-line new)
;;        (write-line still-to-do)
        still-to-do))

    ;; Successor will mark the current node on the visitedlist
    (define (successor graph current)
      (set! visitedlist (cons current visitedlist))
      (find-node-children graph current))

    ;; Search
    (action-search start goal? successor merge action graph)))

;; (GBFS-action 'x
;;              (lambda (node) (eq? node 'z))
;;              (lambda (node) (write-line (find-node-contents test-cycle node)))
;;              test-cycle)

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))

; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))

;; TO BE IMPLEMENTED
(define (add-to-index! index key value) ; Index,Key,Val -> Index
 (let ((index-entry (find-entry-in-index index key)))
   (if (null? index-entry)
       ;; no entry -- create and insert a new one...
       ;;... TO BE IMPLEMENTED
       (set-cdr! index (cons (list key (list value)) (cdr index)))
       ;; entry exists -- insert value if not already there...
       ;;... TO BE IMPLEMENTED
       (set-car! (cdr index-entry) (cons value (cadr index-entry)))))
 index)

;; ;; Testing
;; (define test-index (make-index))
;; (add-to-index! test-index 'key1 'value1)
;; (write-line test-index)
;; (add-to-index! test-index 'key2 'value2)
;; (add-to-index! test-index 'key1 'another-value1)
;; (write-line test-index)
;; (newline)
;; (write-line (find-in-index test-index 'key1))
;; (write-line (find-in-index test-index 'key2))



;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.
(load "generate.scm")

;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies
;; (newline)
;; (write-line 'Deep-First-Search)
;; (GDFS 'http://sicp.csail.mit.edu/ (lambda (x) #f) the-web)
;; (write-line 'Broad-First-Search)
;; (GBFS 'http://sicp.csail.mit.edu/ (lambda (x) #f) the-web)


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; TO BE IMPLEMENTED
;; add-document-to-index!: Index, Web, URL
(define (add-document-to-index! index web url)
    (let ((urlContent (find-URL-text web url)))
      (map (lambda (word) (add-to-index! index word url))
           urlContent))
  index)


;; Example use

;; (define the-web-index (make-index))

;; (add-document-to-index! the-web-index
;;                         the-web
;;                         'http://sicp.csail.mit.edu/)
;; (write-line the-web-index)
;; 
;; (write-line (find-in-index the-web-index 'help))
;; ;Value: (http://sicp.csail.mit.edu/)
;; 
;; (write-line (find-in-index the-web-index '*magic*))
;; ;Value: #f


(define (make-web-index the-web start)
    ;; make-web-index takes in a graph and start node, and return a procedure that could be used to lookup works in the index.
    (let ((web-index (make-index)))
      (GBFS-action start
                   (lambda (url) #f)
                   (lambda (url) (add-document-to-index! web-index
                                                         the-web
                                                         url))
                   the-web)
      (lambda (word) (find-in-index web-index word))))

;; (define find-document (make-web-index the-web 'http://sicp.csail.mit.edu/))

;; (write-line (find-document '|0|))

;; A dynamic web search
(define (search-any web start-url word)
    ;; dynamically search the whole internet, return the node if find one node.
    (let ((result-url (list)))

      (define (goal? url)
          (let ((return (memq word (find-URL-text web url))))
            (if return
                (set! result-url url))))

      (GBFS start-url
        goal?
        web)

      result-url))

(define (search-all web start-url word)
    ;; return all the node
    (let ((nodeList (list)))
      (define (goal? url) #f)

      (define (action url)
          (if (memq word (find-URL-text web url))
              (set! nodeList (cons url nodeList))))

      (GBFS-action start-url
                   goal?
                   action
                   web)
      nodeList))

;; Test
;; (write-line (search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative))
;; (write-line (search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative))
;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
    (let ((start (runtime)))
      (let ((val (do ((val (apply f args)
                           (apply f args))
                      (i 0 (+ i 1)))
                     ((= i 1000) val))))
        (newline)
        (display "time expended: ")
        (display (- (runtime) start))
        val)))

;; ;; Comparison a web size of 100
;; (define test-web (generate-random-web 199))

;; ;; find 'help using search any
;; (timed search-any test-web '*start* 'help)

;; ;; find non-exist word 'Susan-hockfield using search any
;; (timed search-any test-web '*start* 'Susan-hockfield)

;; ;; create a index for test-web
;; (define find-document (make-web-index test-web '*start*))

;; ;; find 'help using find document
;; (timed find-document 'help)
;; ;; find non-exsit word 'Susan-hockfield using find document
;; (timed find-document 'Susan-hockfield)

;; Print-out result for each line above ;(
;; time expended: 0.
;; time expended: 0.
;; ;; time expended: 3.67
;; time expended: 0.
;; time expended: 0.

;; Add it to a thousand times and delete the creating
;; time expended: 1.0000000000000009e-2
;; time expended: 1.0000000000000009e-2
;; time expended: 1.9999999999999574e-2
;; time expended: 9.999999999999787e-3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimized Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making the index into an optimized index

(define (make-optimized-index index)
    (let ((item-vect (list->vector (cdr index))))
      ;;(display (length index))
      ;;(write-line (cdr index))
      (define (compare item1 item2)
          (let ((key1 (car item1))
                (key2 (car item2)))
            (symbol<? key1 key2)))

      (sort! item-vect compare)
      (list 'optimized-index item-vect)))

(define (optimized-index-look-up index key)
    (if (and (list? index) (eq? 'optimized-index (car index)))
        (let ((result (vector-binary-search (cadr index) symbol<? car key)))
          (cond ((not result)
                 (cond (*search-debug*
                        (display "Can't find value with key--")
                        (write-line key)))
                 result)
                (else
                 (cadr result))))))

(define (make-optimized-web-index the-web start)
    ;; make-web-index takes in a graph and start node, and return a procedure that could be used to lookup works in the index.
    (let ((web-index (make-index)))
      (GBFS-action start
                   (lambda (url) #f)
                   (lambda (url) (add-document-to-index! web-index
                                                         the-web
                                                         url))
                   the-web)
      (make-optimized-index web-index)))

;; ;; Test
;; (define op (make-optimized-web-index the-web 'http://sicp.csail.mit.edu/))
;; (define opv (cadr op))
;; (write-line (optimized-index-look-up op '|0|))

;; (define op-test-web (make-optimized-index test-web))

;; (write-line (timed optimized-index-look-up op-test-web 'help))

;; (write-line (timed optimized-index-look-up op-test-web 'Susan-hockfield))


;; time expended: 0.#f

;; time expended: 9.999999999999787e-3#f
