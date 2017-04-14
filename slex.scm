;;; SLeX - Yet another lex generator written in Scheme.
;;;
;;; DeathKing <dk@hit.edu.cn>
;;; 2017-04-08

(load-option 'format)
(load-relative "ext.scm")

;;; Useful char set
(define slex:digit      (map digit->char (iota 10)))
(define slex:upper-case (map ascii->char (iota 26 (char->ascii #\A))))
(define slex:lower-case (map ascii->char (iota 26 (char->ascii #\a))))
(define slex:alpha      (append slex:upper-case slex:lower-case))
(define slex:alphanum   (append slex:digit slex:alpha))
(define slex:whitespace (list #\space #\tab #\page #\linefeed #\return))
(define slex:standard
  (append slex:alphanum
          (list #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
                #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
                #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~)))
(define slex:graphic `(,@slex:standard #\space))

;;; Primitive RE constructor
(define eps (cons 'eps '()))

(define (sig . cs)
  (cons 'sig
        (cond ((null? cs)
               (error "Argument of sig should not be null!"))
              ((all? char? cs)
               (sort cs char<?))
              (else 
               (error "Argument ~A is not a char type."
                      (any (lambda (e) (not (char? e))) cs)
                      cs)))))

(define (sig* . css)
  (apply sig (list-flatten css)))

(define (exact s)     (cons 'exact s))
(define (exact-ci s)  (cons 'exact-ci s))

(define (alt . r)     (cons 'alt r))
(define (seq . r)     (cons 'seq r))
(define (kleen re)    (cons 'kleen re))
(define (kleen+ re)   (seq re (kleen re)))

;;; Primitive DFA/NFA constructor

; A node is a recursive data structure and could refer to both 
; a DFA node or a NFA node.
; FANode : DFANode | NFANode
;        : (Object, List<DFAEdge>) | (Object, List<NFAEdge>)
;(define (new-node) (cons '() '()))
(define (new-node edges) (cons '() edges))
(define (new-node2 tag edges) (cons tag edges))


; A edge connect two nodes 
; FAEdge : DFAEdge | NFAEdge
;          (List<char>, DFANode) | (List<char>, NFANode)
(define (new-edge chars dest) (cons chars dest))
(define (new-eps-edge dest) (cons 'eps dest))
(define (get-edges node) (cdr node))
(define (get-chars edge) (car edge))
(define (get-dest node) (cdr node))

(define (add-new-edge! chars from dest)
  (set-cdr! from
            (append (get-edges from)
                    (list (new-edge chars dest)))))

; get-alphabet : FANode -> List<char>
(define (get-alphabet node)
  (then (get-edges node)
     -> (lambda (e) (map get-chars e))
     -> list-flatten
     -> list-uniq))

(define (get-neighbours node)
  (then (get-edges node)
     -> (lambda (e) (map get-dest e))
     -> list-flatten
     -> list-uniq))

(define (make-new-NFA start accept) (cons start accept))
(define (start-of-NFA N) (car N))
(define (accept-of-NFA N) (cdr N))

(define (make-new-DFA start accepts) (cons start accepts))
(define (start-of-DFA M) (car M))
(define (accepts-of-DFA M) (cdr M))

; make-eps-NFA : void -> (NFANode, NFANode)
(define (make-eps-NFA)
  (let ((accept (new-node '())))
    (add-new-edge! 'eps accept accept)
    (make-new-NFA accept accept)))

; make-just-accept-NFA : void -> (NFANode, NFANode)
(define (make-just-accept-NFA)
  (let ((accept (new-node '())))
    (make-new-NFA accept accept)))

; make-sig-NFA : List<char> -> (NFANode, NFANode)
(define (make-sig-NFA chars)
  (if (null? chars)
      (make-eps-NFA)
      (let* ((accept (new-node '()))
             (start (new-node (list (new-edge chars accept)))))
        (make-new-NFA start accept))))

; make-sig-NFA : List<char> -> (NFANode, NFANode)
(define (make-exact-NFA char-seq)
  (if (null? char-seq)
      (make-just-accept-NFA)
      (let* ((N (make-exact-NFA (cdr char-seq)))
             (accept (accept-of-NFA N))
             (start
              (new-node (list (new-edge (list (car char-seq)) (start-of-NFA N))))))
        (make-new-NFA start accept))))

; make-exact-ci-NFA : List<char> -> (NFANode, NFANode)
(define (make-exact-ci-NFA char-seq)
  (if (null? char-seq)
      (make-just-accept-NFA)
      (let* ((c (car char-seq))
             (start (new-node '()))
             (N (make-exact-ci-NFA (cdr char-seq)))
             (accept (accept-of-NFA N))
             (start1 (start-of-NFA N)))
        (if (char-alphabetic? c)
            (begin
              (add-new-edge! (list (char-upcase c)) start start1)
              (add-new-edge! (list (char-downcase c)) start start1))
            (add-new-edge! (list c) start start1))
        (make-new-NFA start accept))))

;;; translation rules for RE combinator
(define (make-alt-NFA . N)
  (if (= (length N) 1)
      (car N)
      (let ((T (car N))
            (M (apply make-alt-NFA (cdr N))))
        (add-new-edge! 'eps (start-of-NFA T) (start-of-NFA M))
        (add-new-edge! 'eps (accept-of-NFA M) (accept-of-NFA T))
        T)))

(define (make-seq-NFA . N)
  (if (= (length N) 1)
      (car N)
      (let ((head (car N))
            (rest (apply make-seq-NFA (cdr N))))
        (add-new-edge! 'eps (accept-of-NFA head) (start-of-NFA rest))
        (make-new-NFA (start-of-NFA head) (accept-of-NFA rest)))))
  
(define (make-kleen-NFA N)
  (add-new-edge! 'eps (start-of-NFA N) (accept-of-NFA N))
  (add-new-edge! 'eps (accept-of-NFA N) (start-of-NFA N))
  N)
  

;;; NFA/eps-closre : NFANode -> List<NFANode>
;;;
;;;   compute the eps-closure of a node d of a NFA machine N.
(define (NFA/eps-closure d)
  (let iter ((open (list d)) (result (list d)))
    (if (null? open)
        (reverse result)
        (begin
          (forall trans in (get-edges (car open))
            (let ((char (get-chars trans))
                  (dest (get-dest trans)))
              (cond ((not (eq? char 'eps))
                     'continue)
                    ((not (or (memq dest open) (memq dest result)))
                     (append! open (list dest))
                     (set! result (cons dest result)))
                    (else
                      'continue))))
          (iter (cdr open) result)))))

;;; NFA/forward-step : NFANode -> List<char> -> List<NFANode>
;;;
;;;  forward a state under some input characters.
(define (NFA/forward-step d cs)
  (then (mapall e in (get-edges d)
          (if (equal? (get-chars e) cs)
              (get-dest e)
              '()))
     -> list-compact
     -> list-uniq))

;;; NFA/nodes->alist : NFA Machine -> List<(NFA Node . int)>
;;;
;;;   walk through all the graph, generate the a assocation list of the walk
;;;   trace.
;;;
;;; * noticed *
;;;
;;;   the car of the node currently been visited will be set to count.
(define (NFA/nodes->alist N)
  (let iter ((open (list (start-of-NFA N)))
             (visited '())
             (count 0))
    (cond ((null? open) visited)
          (else
            (let* ((current (car open))
                   (adj-nodes
                     (list-uniq
                       (filter (lambda (n) (and (not (assq n visited))
                                                (not (memq n open))))
                               (map get-dest (get-edges current))))))
              (iter (cdr (append open adj-nodes))
                    (cons (cons current count) visited)
                    (+ count 1)))))))

;;; NFA/alist->fvec : List<(NFA Node . int)> -> Vector<NFA Node>
;;;
;;;  transform a association list a vector, which is a reverse map for that
;;;  alist.
(define (NFA/alist->fvec alist)
  (let ((fvec (make-vector (length alist))))
    (forall entry in alist
      (vector-set! fvec (cdr entry) (car entry)))
    fvec))

;;; NFA/fast-serials->nodes : List<int> -> Vector<NFA Node> -> List<NFA Node>
(define (NFA/fast-serials->nodes serials fvec)
  (let iter ((rest serials) (result '()))
    (if (null? rest)
        (reverse result)
        (iter (cdr rest)
              (cons (vector-ref fvec (car rest)) result)))))

;;; NFA/nodes->serials : List<NFA Node> -> List<(NFA Node . int> -> List<int>
(define (NFA/nodes->serials ds alist)
  (let iter ((open ds) (result '()))
      (if (null? open)
          (sort result <)
          (iter (cdr open)
                (cons (cdr (assq (car open) alist)) result)))))

(define (RE->NFA RE)
  (let ((tag (car RE)) (cont (cdr RE)))
    (cond ((eq? tag 'eps)      (make-eps-NFA))
          ((eq? tag 'sig)      (make-sig-NFA cont))
          ((eq? tag 'exact)    (make-exact-NFA (string->list cont)))
          ((eq? tag 'exact-ci) (make-exact-ci-NFA (string->list cont)))
          ((eq? tag 'alt)      (apply make-alt-NFA (map RE->NFA cont)))
          ((eq? tag 'seq)      (apply make-seq-NFA (map RE->NFA cont)))
          ((eq? tag 'kleen)    (make-kleen-NFA (RE->NFA cont)))
          (else
            (error "wrong type of regular expression.")))))

(define (serials->string srs)
    (join "," (map number->string srs)))

(define (NFA->DFA N)
  (let* ((alist (NFA/nodes->alist N))
         ; nfa-accept-sr : int
         ; we need this to mark all the accept state of DFA
         (nfa-accept-sr (cdr (assoc (accept-of-NFA N) alist)))
         ; d0-epsclo : List<NFA Node>
         (d0-epsclo (NFA/eps-closure (start-of-NFA N)))
         ; d0-epsclo-sr : List<int>
         (d0-epsclo-sr (NFA/nodes->serials d0-epsclo alist))
         ; s0 is the start state of DFA
         (dfa-start (new-node '()))
         ; correspondence table
         (map-table (list (cons dfa-start d0-epsclo-sr))))
    (let iter ((open (list (cons d0-epsclo d0-epsclo-sr))))
      ;(if (not (null? open))
          ;(format #t "Open: ~A ~%" (cdar open)))
      (if (null? open)
          (let mark-accept ((to-check map-table)
                            (dfa-accepts '()))
            (if (null? to-check)
                (cons (cons dfa-start dfa-accepts) map-table)
                (mark-accept (cdr to-check)
                             (if (memq nfa-accept-sr (cdar to-check))
                                 (cons (caar to-check) dfa-accepts)
                                 dfa-accepts))))
          (let* ((current-nfa-nodes (caar open))
                 (current-dfa-node (car (rassoc (cdar open) map-table)))
                 (all-possible-input
                   (then (mapall node in current-nfa-nodes
                           (mapall edges in (get-edges node)
                             (get-chars edges)))
                      -> (lambda (r) (list-join r '()))
                      -> (lambda (l)
                           (filter (lambda (e) (not (eq? 'eps e))) l))
                      -> list-uniq)))
            (forall d in current-nfa-nodes
              (forall cs in all-possible-input
                (let* ((fs (then (map NFA/eps-closure (NFA/forward-step d cs))
                              -> (lambda (l) (list-join l '()))
                              -> list-unique))
                       (fs-serials (NFA/nodes->serials fs alist)))
                  (cond ((rassoc fs-serials map-table) =>
                         (lambda (cached-dfa-node)
                           (add-new-edge! cs
                                          current-dfa-node
                                          (car cached-dfa-node))))
                        ((null? fs)
                         'nothing)
                        (else
                         (let ((new-dfa-node (new-node '())))
                           (add-new-edge! cs
                                          current-dfa-node
                                          new-dfa-node)
                           (set! map-table (cons (cons new-dfa-node fs-serials)
                                                 map-table))
                           (append! open (list (cons fs fs-serials)))))))))
                
            (iter (cdr open)))))))

(define (DFA/is-accept? D st)
  (not (not (memq st (accepts-of-DFA D)))))

(define (DFA/find-eqv-class D alist fvec)
  (let* ((dfa-nodes (map car alist))
         (table (make-2D-mirror-table (vector-length fvec)
                                      (vector-length fvec)
                                      '()))
         (serial (lambda (n) (cdr (assq n alist))))
         (to-check '()))
    ; initialize the mirror table
    (forall p in dfa-nodes
      (forall q in dfa-nodes
        (let ((sp (serial p)) (sq (serial q)))
          (cond ((not (null? (mirror-table-get table sp sq)))
                 'continue)
                ((eq? (DFA/is-accept? D p) (DFA/is-accept? D q))
                 (if (not (eq? 'added (mirror-table-get table sp sq)))
                     (begin
                       (mirror-table-put! table (serial p) (serial q) 'added)
                       (set! to-check (cons (cons p q) to-check)))))
                (else
                  (mirror-table-put! table (serial p) (serial q) 'x))))))
    (let iter ((changed #f))
      (forall (pair cont brk) in to-check
        (let* ((p (car pair)) (sp (serial p))
               (q (cdr pair)) (sq (serial q)))
          (if (not (equal? (get-alphabet p) (get-alphabet q)))
              (begin
                (mirror-table-put! table sp sq 'x)
                (delq! pair to-check)
                (cont 'nothing))
              (forall c in (get-alphabet p)
                (let ((nsp (serial (DFA/forward-step p c)))
                      (nsq (serial (DFA/forward-step q c))))
                  (if (eq? 'x (mirror-table-get table nsp nsq))
                      (begin
                        (mirror-table-put! table sp sq 'x)
                        (delq! pair to-check)
                        (set! changed #t)
                        (cont 'nothing)))))))) 
      (if changed (iter #f) to-check))))

(define (trace-root d)
  (cond ((null? (car d)) d)
        ((eq? d (car d)) d)
        (else
         (trace-root (car d)))))

(define (merge-edges! d)
  (let ((alist '()))
    (forall edge in (get-edges d)
      (let ((dest (get-dest edge))
            (chars (get-chars edge)))
        (cond ((assq dest alist) =>
               (lambda (entry)
                 (set-cdr! entry (cons chars (cdr entry)))))
              (else
               (set! alist (cons (cons dest (list chars)) alist))))))
    (set-cdr! d 
              (map
                (lambda (entry)
                  (cons (sort (list-uniq (list-flatten (cdr entry))) char<?)
                        (car entry)))
                alist))))

(define (DFA/simplify! D)
  (let* ((alist (NFA/nodes->alist D))
         (fvec (NFA/alist->fvec alist))
         (serial (lambda (n) (cdr (assq n alist))))
         (eqv-class (DFA/find-eqv-class D alist fvec)))
    (forall pair in eqv-class
      (let* ((p (car pair)) (sp (serial p))
             (q (cdr pair)) (sq (serial q))
             (m (if (< sp sq) p q)) ; m is the smaller one
             (n (if (< sp sq) q p)))
        (set-car! n m)
        (if (null? (car n))
            (set-car! n n))))
    (forall entry in alist
      (forall edge in (get-edges (car entry))
        (set-cdr! edge (trace-root (get-dest edge)))))
    (forall entry in alist
      (let ((root (trace-root (car entry))))
        (if (not (eq? root (car entry)))
            (set-cdr! root (append (get-edges root) (get-edges (car entry)))))))
    (forall entry in alist
      (if (eq? (trace-root (car entry)) (car entry))
          (merge-edges! (car entry))))
    D))

(define (DFA/forward-step d c)
  (call-with-current-continuation
    (lambda (K)
      (forall edge in (get-edges d)
        (if (member c (get-chars edge))
            (K (get-dest edge))))
      (K #f))))

(define (run-DFA D str)
  
  (let ((accepts (accepts-of-DFA D)))
    (let iter ((current-state (start-of-DFA D))
               (to-check (string->list str))
               (stack '()))
      (cond ((and (not (null? to-check))
                  (DFA/forward-step current-state (car to-check))) =>
             (lambda (next-state)
               (iter next-state (cdr to-check) (cons (car to-check) stack))))
            (else
             (list (and (memq current-state accepts) #t)
                   (reverse stack)
                   to-check))))))

(define (RE-compile RE)
  (car (NFA->DFA (RE->NFA RE))))

(define (RE-matches? RE str)
  (let ((D (RE-compile RE)))
    (car (run-DFA D str))))

(define (NFA->DOT N filename)
  (define (plot-body port alist)
    (for-each
      (lambda (entry)
        (let ((from (cdr entry)))
          (format port "d~A [label=\"d_{~A}\"];~%" from from)
          (for-each
            (lambda (edge)
              (let ((to (cdr (assq (cdr edge) alist))))
                (format port
                        "d~A -> d~A [label=\"~A\"];~%"
                        from
                        to
                        (stringfy (get-chars edge)))))
            (get-edges (car entry)))))
      alist))
  
  (let ((port (open-output-file filename))
        (alist (NFA/nodes->alist N)))
    (format port "digraph fsm {~%")
    (format port "d2toptions=\"-c -ftikz -tmath --autosize --edgeoptions=auto --tikzedgelabels\"; ~%")
    (format port "rankdir=LR; ~%")
    (format port
            "node [shape = doublecircle]; d~A; ~%"
            (cdr (assq (accept-of-NFA N) alist)))
    (format port "node [shape = circle];~%")
    (plot-body port alist)
    (format port "}")
    (close-output-port port)))

(define (DFA->DOT D map-table filename)
  
  (define (plot-body port alist)
    (forall entry in alist
      (let ((from (cdr entry)))
        (format port
                "d~A [label=\"d_{~A}\"];~%"
                from
                ;(serials->string (cdr (assq (car entry) map-table)))
                from
                )
          (forall edge in (get-edges (car entry))
            (let ((dest (cdr (assq (get-dest edge) alist))))
              (format port
                      "d~A -> d~A [label=\"~A\"];~%"
                      from
                      dest
                      (stringfy (car edge))))))))
  
  (let ((port (open-output-file filename))
        (alist (NFA/nodes->alist D)))
    (format port "digraph fsm {~%")
    (format port "d2toptions=\"-c -ftikz -tmath --autosize --edgeoptions=auto --tikzedgelabels\"; ~%")
    (format port "rankdir=LR; ~%")
    (format port "node [shape = doublecircle]; ~%")
    (forall accept-node in (accepts-of-DFA D)
      (format port
            "d~A [label=\"d{~A}\"]; ~%"
            (cdr (assq accept-node alist))
            (cdr (assq accept-node alist))
            ;(serials->string (cdr (assq accept-node map-table)))
            ))
    (format port "node [shape = circle];~%")
    (plot-body port alist)
    (format port "}")
    (close-output-port port)))

(define (compact chars)
  (let ((cs (sort chars char<?)))
    (reverse
      (call-with-current-continuation
        (lambda (K)
          (let merge ((result '()) (current '()) (rest cs))
            (cond ((null? rest)
                   (K (cons current result)))
                  ((null? current)
                   (merge result
                          (cons (car rest) (car rest))
                          (cdr rest)))
                  ((equal? 1 (- (char-code (car rest))
                                (char-code (cdr current))))
                   (merge result
                          (cons (car current) (car rest))
                          (cdr rest)))
                  (else
                   (merge (cons current result)
                          '()
                          rest)))))))))

(define (stringfy c)
  (cond ((eq? 'eps c) "\\\\epsilon")
        ((list? c)
         (let ((cs (compact c)))
           (format #f "\\\\texttt{~A}" (join "," (pp-edgelabel cs)))))
        (else (error "type error!"))))

(define latex-escape
  (let ((escape-table '((#\$ . "\\\\$") (#\\ . "\\\\textbackslash")
                        (#\% . "\\\\%") (#\# . "\\\\#")
                        (#\_ . "\\\\_") (#\& . "\\\\&")
                        (#\{ . "\\\\{") (#\} . "\\\\}")
                        (#\~ . "\\\\textasciitilde")
                        (#\^ . "\\\\textasciicircum"))))
    (lambda (c)
      (let ((entry (assq c escape-table)))
        (if entry
            (cdr entry)
            (char->string c))))))

(define pp-edgelabel
  (let* ((e latex-escape)
         (f (lambda (interval)
              (let ((x (car interval)) (y (cdr interval)))
                (cond ((char=? x y) (e x))
                      (else (format #f "[~A-~A]" (e x) (e y))))))))
    (lambda (cs) (map f cs))))

