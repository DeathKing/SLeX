;;; SLEX - Yet another lex generator written in Scheme.
;;;
;;; DeathKing <dk@hit.edu.cn>
;;; 2017-04-08

(load-option 'format)
(load-relative "ext.scm")

(define (sig c)
  (cons 'sig
        (cond ((char? c) (list c))
              ((list? c) (sort c char<?))
              (else (error "unexcept type")))))

(define (exact s)
  (cons 'exact s))

(define (exact-ci s)
  (cons 'exact-ci s))

(define (alt left right)
  (cons 'alt (cons left right)))

(define (seq first later)
  (cons 'seq (cons first later)))

(define (kleen re)
  (cons 'kleen re))

(define (new-node edges) (cons '() edges))
(define (new-node2 tag edges) (cons tag edges))
(define (new-edge chars dest) (cons chars dest))
(define (new-eps-edge dest) (cons 'eps dest))
(define (get-edges node) (cdr node))
(define (get-chars edge) (car edge))
(define (get-dest node) (cdr node))

(define (make-new-NFA start accept) (cons start accept))
(define (start-of-NFA N) (car N))
(define (accept-of-NFA N) (cdr N))

(define (make-new-DFA start accepts) (cons start accepts))
(define (start-of-DFA M) (car M))
(define (accepts-of-DFA M) (cdr M))

(define (add-new-edge! chars from dest)
  (set-cdr! from (append (cdr from) (list (new-edge chars dest)))))

(define (make-single-char-NFA chars)
  (let* ((accept (new-node '()))
         (start (new-node (list (new-edge chars accept)))))
    (make-new-NFA start accept)))

(define (make-alt-NFA N1 N2)
  (let ((start
          (new-node (list (new-eps-edge (start-of-NFA N1))
                          (new-eps-edge (start-of-NFA N2)))))
        (accept (new-node '())))
    (set-cdr! (accept-of-NFA N1) (list (new-eps-edge accept)))
    (set-cdr! (accept-of-NFA N2) (list (new-eps-edge accept)))
    (make-new-NFA start accept)))

(define (make-seq-NFA N1 N2)
  (let ((start (new-node (list (new-eps-edge (start-of-NFA N1)))))
        (accept (new-node '())))
    (set-cdr! (accept-of-NFA N1) (list (new-eps-edge (start-of-NFA N2))))
    (set-cdr! (accept-of-NFA N2) (list (new-eps-edge accept)))
    (make-new-NFA start accept)))

(define (make-kleen-NFA N)
  (let* ((accept (new-node '()))
         (start (new-node (list (new-eps-edge (start-of-NFA N))
                                (new-eps-edge accept)))))
    (set-cdr! (accept-of-NFA N) (list (new-eps-edge accept)
                                      (new-eps-edge (start-of-NFA N))))
    (make-new-NFA start accept)))

;;; NFA/eps-closre : NFA Node -> List<NFA Node>
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

;;; NFA/forward-step : NFA Node -> List<char> -> List<NFA Node>
;;;
;;;  forward a state under some input characters.
(define (NFA/forward-step d cs)
  (then (mapall e in (get-edges d)
          (if (equal? (get-chars e) cs)
              (get-dest e)
              '()))
     -> list-compact
     -> list-unique))

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
                     (filter (lambda (n) (and (not (assq n visited))
                                              (not (memq n open))))
                             (map get-dest (get-edges current)))))
              (set-car! current count)
              (iter (cdr (append open adj-nodes))
                    (cons (cons current count) visited)
                    (+ count 1)))))))

;;; NFA/alist->fastvec : List<(NFA Node . int)> -> Vector<NFA Node>
;;;
;;;  transform a association list a vector, which is a reverse map for that
;;;  alist.
(define (NFA/alist->fastvec alist)
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

;;; RE->NFA : RE -> NFA Machine
(define (RE->NFA re)
  (pmatch re
          (`(sig . ,chars)
            (make-single-char-NFA chars))
          (`(alt . (,left . ,right))
            (make-alt-NFA (RE->NFA left) (RE->NFA right)))
          (`(seq . (,first . ,later))
            (make-seq-NFA (RE->NFA first) (RE->NFA later)))
          (`(kleen . ,re)
            (make-kleen-NFA (RE->NFA re)))
          (`,__
            (error "wrong regular expression."))))

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
                      -> list-unique)))
            ;(format #t "all-possible-input: ~% ~A ~%" all-possible-input)
            ;(format #t "current-nfa-nodes: ~% ~A ~%" current-nfa-nodes)
            (forall d in current-nfa-nodes
              (forall cs in all-possible-input
                ;(format #t "current-nfa-node: ~A ~%" (cdr (assq d alist)))
                ;(format #t "current-input: ~A ~%" cs)
                (let* ((fs (then (map NFA/eps-closure (NFA/forward-step d cs))
                              -> (lambda (l) (list-join l '()))
                              -> list-unique))
                       (fs-serials (NFA/nodes->serials fs alist)))
                  ;(format #t "fs-srs: ~A ~%" (serials->string fs-serials))
                  (cond ((rassoc fs-serials map-table) =>
                         (lambda (cached-dfa-node)
                           ;(format #t "hit a cache! ~%")
                           (add-new-edge! cs
                                          current-dfa-node
                                          (car cached-dfa-node))))
                        ((null? fs)
                         ;(format #t "missed! ~%")
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

(define (DFA/forward-step d c)
  (call-with-current-continuation
    (lambda (K)
      (forall edge in (get-edges d)
        ;(format #t "Input: ~A ~% Accept: ~A ~%" c (get-chars edge))
        (if (member c (get-chars edge))
            (K (get-dest edge))))
      (K #f))))
  
(define (run-DFA D str)
  
  (let ((accepts (accepts-of-DFA D)))
    (let iter ((current-state (start-of-DFA D))
               (to-check (string->list str))
               (stack '()))
      ;(format #t "to-check: ~A ~%" to-check)
      (cond ((and (not (null? to-check))
                  (DFA/forward-step current-state (car to-check))) =>
             (lambda (next-state)
               (iter next-state (cdr to-check) (cons (car to-check) stack))))
            (else
             (list (and (memq current-state accepts) #t)
                   (reverse stack)
                   to-check))))))

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
                        (stringfy (car edge)))))
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
                "d~A [label=\"~A\"];~%"
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
            "d~A [label=\"~A\"]; ~%"
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
                  ((equal? 1 (- (char->integer (car rest))
                                (char->integer (cdr current))))
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
  (let ((escape-table '((#\$ . "\\\\$")
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

