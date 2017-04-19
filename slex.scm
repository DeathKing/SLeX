;;; SLeX - Yet another lex generator written in Scheme.
;;;
;;; DeathKing <dk@hit.edu.cn>
;;; 2017-04-08

(load-option 'format)
(load-relative "ext.scm")

;;; Useful char set
(define slex:digit       char-set:numeric)
(define slex:upper-case  char-set:upper-case)
(define slex:lower-case  char-set:lower-case)
(define slex:alpha       char-set:alphabetic)
(define slex:alphanum    char-set:alphanumeric)
(define slex:whitespace  char-set:whitespace)
(define slex:standard    char-set:standard)
(define slex:graphic     char-set:graphic)

;;; Primitive RE constructor
(define eps (cons 'eps '()))

(define (sig cs)
  (cons 'sig cs))

(define (sig* . css)
  (sig (apply char-set css)))

(define (exact s)     (cons 'exact s))
(define (exact-ci s)  (cons 'exact-ci s))

(define (alt . rs)    (cons 'alt rs))
(define (seq . rs)    (cons 'seq rs))
(define (kln* r)      (cons 'kln r))
(define (kln? r)      (alt r eps))
(define (kln+ r)      (seq r (kln* r)))

;;; Primitive DFA/NFA constructor

; A node is a recursive data structure and could refer to both 
; a DFA node or a NFA node.
; FANode : DFANode | NFANode
;        : (Object, List<DFAEdge>) | (Object, List<NFAEdge>)
(define (make-node)      (cons '() '()))
(define (make-node1 tag) (cons tag '()))

; A edge connect two nodes 
; FAEdge : DFAEdge | NFAEdge
;          (List<char-set>, DFANode) | (List<char-set>, NFANode)
(define (make-edge charset dest) (cons charset dest))
(define (make-eps-edge dest)     (cons 'eps dest))
(define (get-edges node)         (cdr node))
(define (get-char-set edge)      (car edge))
(define (get-dest edge)          (cdr edge))
(define (eps-edge? edge)         (eq? 'eps (get-char-set edge)))

; add-edge! : char-set -> FANode -> FANode -> #unspecific
(define (add-edge! charset from dest)
  (set-cdr! from (cons (make-edge charset dest) (get-edges from))))

; add-eps-edge! : FANode -> FANode -> #unspecific
(define (add-eps-edge! from dest)
  (set-cdr! from (cons (make-eps-edge dest) (get-edges from))))

; get-alphabet : FANode -> char-set
(define (get-node-alphabet node)
  (then (get-edges node)
     -> (lambda (e) (map get-char-set e))
     -> list-flatten
     -> (lambda (l) (apply char-set-union l))))

; get-neighbours : FANode -> List<FANode>
; FIXME: list-flatten
(define (get-neighbours node)
  (then node -> get-edges => get-dest -> list-flatten -> list-uniq))

;;; NFA and DFA routines
;;; 
;;; NFA and DFA are conceptually equivalent, only for that a DFA has multiple
;;; accept nodes instead

; make-NFA : NFANode -> NFANode -> (NFANode . NFANode)
(define (make-NFA start accept) (cons start accept))
(define (NFA/get-start N) (car N))
(define (NFA/get-accept N) (cdr N))

; make-DFA : DFANode -> List<DFANode> -> (DFANode . List<DFANode>)
(define (make-DFA start accepts) (cons start accepts))
(define (DFA/get-start M) (car M))
(define (DFA/get-accepts M) (cdr M))

; make-eps-NFA : void -> (NFANode, NFANode)
(define (make-eps-NFA)
  (let ((start (make-node))
        (accept (make-node)))
    (add-eps-edge! start accept)
    (make-NFA start accept)))

; make-just-accept-NFA : void -> (NFANode, NFANode)
(define (make-just-accept-NFA)
  (let ((accept (make-node)))
    (make-NFA accept accept)))

; make-sig-NFA : charset -> (NFANode, NFANode)
(define (make-sig-NFA charset)
  (if (null? charset)
      (make-eps-NFA)
      (let ((start (make-node)) (accept (make-node)))
        (add-edge! charset start accept)
        (make-NFA start accept))))

; make-sig-NFA : List<char> -> (NFANode, NFANode)
(define (make-exact-NFA char-seq)
  (if (null? char-seq)
      (make-just-accept-NFA)
      (let* ((N (make-exact-NFA (cdr char-seq)))
             (accept (NFA/get-accept N))
             (start (make-node)))
        (add-edge! (char-set (car char-seq)) start (NFA/get-start N))
        (make-NFA start accept))))

; make-exact-ci-NFA : List<char> -> (NFANode, NFANode)
(define (make-exact-ci-NFA char-seq)
  (if (null? char-seq)
      (make-just-accept-NFA)
      (let* ((c (car char-seq))
             (start (make-node))
             (N (make-exact-ci-NFA (cdr char-seq)))
             (accept (NFA/get-accept N))
             (start1 (NFA/get-start N)))
        (add-edge! (if (char-alphabetic? c)
                       (char-set (char-upcase c) (char-downcase c))
                       (char-set c))
                   start
                   start1)
        (make-NFA start accept))))

;;; translation rules for RE combinator
(define (make-alt-NFA . Ns)
  (let ((start (make-node))
        (accept (make-node)))
    (forall n in Ns
      (add-eps-edge! start (NFA/get-start n))
      (add-eps-edge! (NFA/get-accept n) accept))
    (make-NFA start accept)))

(define (make-seq-NFA . N)
  (if (= (length N) 1)
      (car N)
      (let ((head (car N))
            (rest (apply make-seq-NFA (cdr N))))
        (add-eps-edge! (NFA/get-accept head) (NFA/get-start rest))
        (make-NFA (NFA/get-start head) (NFA/get-accept rest)))))

(define (make-kleene-NFA N)
  (let ((start (make-node))
        (accept (make-node)))
    (add-eps-edge! start accept)
    (add-eps-edge! start (NFA/get-start N))
    (add-eps-edge! (NFA/get-accept N) accept)
    (add-eps-edge! (NFA/get-accept N) (NFA/get-start N))
    (make-NFA start accept)))  

;;; NFA/eps-closres : List<NFANode> -> List<NFANode>
;;;
;;;   compute the eps-closure of a node d of a NFA machine N.
(define (NFA/eps-closures ds)
  (let iter ((open ds) (result ds))    
    (until (null? open)
      (forall edge in (then open -> car -> get-edges)
        (let ((dest (get-dest edge)))
          (cond ((not (eps-edge? edge))
                 'continue)
                ((not (or (memq dest open) (memq dest result)))
                 (set! open (append open (list dest)))
                 (set! result (cons dest result)))
                (else
                 'continue))))
      (set! open (cdr open)))
    result))

;;; NFA/forward-step : NFANode -> List<char> -> List<NFANode>
;;;
;;;   forward a state under some input characters.
(define (NFA/forward-step d cs)
  (then (mapall e in (get-edges d)
          (if (equal? (get-chars e) cs)
              (get-dest e)
              '()))
     -> list-compact
     -> list-uniq))

;;; NFA/nodes->alist : FAMachine -> List<(NFA Node . int)>
;;;
;;;    walk through all the graph, generate the a assocation list of the walk
;;;    trace.
(define (NFA/nodes->alist N #!optional map-table)
  (let iter ((open (list (NFA/get-start N))) ; : List<NFANode>
             (visited '())                   ; : List<(NFANode . int)>
             (count 0))                      ; : int
    (cond ((null? open) visited)
          (else
            (let* ((current (car open))
                   (adjecents (then current -> get-edges => get-dest
                                 %> (lambda (n)
                                      (and (not (assq n visited))
                                           (not (memq n open))))
                                 -> list-uniq))) 
              (iter (cdr (append open adjecents))
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
        (iter (cdr rest) (cons (vector-ref fvec (car rest)) result)))))

;;; NFA/nodes->serials : List<NFA Node> -> List<(NFA Node . int> -> List<int>
(define (NFA/nodes->serials ds alist)
  (let iter ((open ds) (result '()))
    (if (null? open)
        (sort result <)
        (iter (cdr open) (cons (cdr (assq (car open) alist)) result)))))

(define (RE->NFA RE)
  (let ((tag (car RE)) (cont (cdr RE)))
    (cond ((eq? tag 'eps)      (make-eps-NFA))
          ((eq? tag 'sig)      (make-sig-NFA cont))
          ((eq? tag 'exact)    (make-exact-NFA (string->list cont)))
          ((eq? tag 'exact-ci) (make-exact-ci-NFA (string->list cont)))
          ((eq? tag 'alt)      (apply make-alt-NFA (map RE->NFA cont)))
          ((eq? tag 'seq)      (apply make-seq-NFA (map RE->NFA cont)))
          ((eq? tag 'kln)      (make-kleene-NFA (RE->NFA cont)))
          (else
            (error "wrong type of regular expression.")))))

(define (serials->string srs)
  (join "," (map number->string srs)))

;;; make-binary-partions : T => List<T> -> List<(List<T> . List<T>)>
;;;
;;;    given L : List<T>, generate all the pairs p of form  (S . R), such that:
;;;        1) S union R = L
;;;        2) S intersection R = EMPTY-SET
(define (make-binary-partitions ns)
  (cond ((null? ns) '())
        ((equal? 1 (length ns)) (list (cons ns '())))
        (else
         (let ((r '()) (n (car ns))
               (partitions (make-binary-partitions (cdr ns))) )
          (forall entry in partitions
            (set! r (cons (cons (cons n (car entry)) (cdr entry)) r))
            (set! r (cons (cons (car entry) (cons n (cdr entry))) r)))
          r))))

(define (NFA->DFA N #!optional node-seq)
  ; 
  (define alist
    (if (default-object? node-seq) (NFA/nodes->alist N) node-seq))
  ; nfa-accept-sr : int
  ; we need this to mark all the accept state of DFA
  (define nfa-accept-sr (cdr (assq (NFA/get-accept N) alist)))
  ; d0-epsclo : List<NFANode>
  (define d0-epsclo (NFA/eps-closures (list (NFA/get-start N))))
  ; d0-epsclo-sr : List<int>
  (define d0-epsclo-sr (NFA/nodes->serials d0-epsclo alist))
  ; s0 is the start state of DFA
  (define dfa-start (make-node))
  ; map-table : List<(DFANode . List<int>)>
  (define map-table (list (cons dfa-start d0-epsclo-sr)))
  
  ; open : List<(List<NFANode> . List<int>)>
  (let iter ((open (list (cons d0-epsclo d0-epsclo-sr))))
    (if (null? open)
        (let mark-accept ((to-check map-table) (dfa-accepts '()))
          (if (null? to-check)
              (cons (make-DFA dfa-start dfa-accepts) map-table)
              (mark-accept (cdr to-check)
                           (if (memq nfa-accept-sr (cdar to-check))
                               (cons (caar to-check) dfa-accepts)
                               dfa-accepts))))
        (let* (; current-nfa-nodes : List<NFANode>
               (current-nfa-nodes (caar open))
               ; the initial value must be cached
               (current-dfa-node (car (rassoc (cdar open) map-table)))
               ; all-xtions : List<(NFANode . char-set)>
               (all-xtions (then current-nfa-nodes      
                              ; : List<List<(char-set . NFANode)>>
                              => get-edges
                              ; : List<(char-set . NFANode)>           
                              -> (lambda (l) (fold-left append '() l))
                              ; filter out those eps edge
                              %> (lambda (e) (not (eps-edge? e)))
                              ; : List<(NFANode . char-set)>
                              -> alist-reverse-all-pairs))
               (ralist '()))
          
          ;;; break if there's no transitions
          (if (null? all-xtions)
              (iter (cdr open))
              (begin
                ; ralist : List<(NFANode . char-set)>
                (set! ralist (alist-merge all-xtions char-set-union))
                ; partition : (List<NFANode> . List<NFANode>)
                (forall partition in (make-binary-partitions (map car ralist))
                  (let* ((d1 (car partition))   ; : List<NFANode>
                         (d2 (cdr partition))   ; : List<NFANode>
                         (d1-epsclo (NFA/eps-closures d1))   ; : List<NFANode>
                         (d2-epsclo (NFA/eps-closures d2))   ; : List<NFANode>
                         (d1-epsclo-sr (NFA/nodes->serials d1-epsclo alist))  ; : List<int>
                         (d2-epsclo-sr (NFA/nodes->serials d2-epsclo alist))) ; : List<int>
                    ;;; Code too ugly here in order to slightly boost our computation
                    (and-let* (((not (null? d1)))
                               (ss1 (assq-values d1 ralist #f))
                               (ss2 (assq-values d2 ralist (list char-set:empty)))
                               (sigma1 (reduce char-set-intersection char-set:universe ss1))
                               ((not (empty-char-set? sigma1)))
                               (sigma2 (reduce char-set-union char-set:empty ss2))
                               ((not (universe-char-set? sigma2)))
                               (sigma3 (char-set-intersection sigma1 (char-set-invert sigma2)))
                               ((not (empty-char-set? sigma3))))
                      (cond ((rassoc d1-epsclo-sr map-table) =>
                             (lambda (dest-dfa-node)
                               (add-edge! sigma3 current-dfa-node (car dest-dfa-node))))
                            (else
                             (let ((dest-dfa-node (make-node)))
                               (add-edge! sigma3 current-dfa-node dest-dfa-node)
                               (set! map-table (cons (cons dest-dfa-node d1-epsclo-sr) map-table))
                               (set! open (append open (list (cons d1-epsclo d1-epsclo-sr))))))))
                    
                    (and-let* (((not (null? d2)))
                               (ss1 (assq-values d2 ralist #f))
                               (ss2 (assq-values d1 ralist (list char-set:empty)))
                               (sigma1 (reduce char-set-intersection char-set:universe ss1))
                               ((not (empty-char-set? sigma1)))
                               (sigma2 (reduce char-set-union char-set:empty ss2))
                               ((not (universe-char-set? sigma2)))
                               (sigma3 (char-set-intersection sigma1 (char-set-invert sigma2)))
                               ((not (empty-char-set? sigma3))))
                      (cond ((rassoc d2-epsclo-sr map-table) =>
                             (lambda (dest-dfa-node)
                               (add-edge! sigma3 current-dfa-node (car dest-dfa-node))))
                            (else
                             (let ((dest-dfa-node (make-node)))
                               (add-edge! sigma3 current-dfa-node dest-dfa-node)
                               (set! map-table (cons (cons dest-dfa-node d2-epsclo-sr) map-table))
                               (set! open (append open (list (cons d2-epsclo d2-epsclo-sr))))))))                    
                    ))
                  (iter (cdr open))))
          ))))

;(define (DFA/is-accept? D st)
;  (not (not (memq st (DFA/get-accepts D)))))

;(define (DFA/find-eqv-class D alist fvec)
;  (let* ((dfa-nodes (map car alist))
;         (table (make-2D-mirror-table (vector-length fvec)
;                                      (vector-length fvec)
;                                      '()))
;         (serial (lambda (n) (cdr (assq n alist))))
;         (to-check '()))
;    ; initialize the mirror table
;    (forall p in dfa-nodes
;      (forall q in dfa-nodes
;        (let ((sp (serial p)) (sq (serial q)))
;          (cond ((not (null? (mirror-table-get table sp sq)))
;                 'continue)
;                ((eq? (DFA/is-accept? D p) (DFA/is-accept? D q))
;                 (if (not (eq? 'added (mirror-table-get table sp sq)))
;                     (begin
;                       (mirror-table-put! table (serial p) (serial q) 'added)
;                       (set! to-check (cons (cons p q) to-check)))))
;                (else
;                  (mirror-table-put! table (serial p) (serial q) 'x))))))
;    (let iter ((changed #f))
;      (forall (pair cont brk) in to-check
;        (let* ((p (car pair)) (sp (serial p))
;               (q (cdr pair)) (sq (serial q)))
;          (if (not (equal? (get-alphabet p) (get-alphabet q)))
;              (begin
;                (mirror-table-put! table sp sq 'x)
;                (delq! pair to-check)
;                (cont 'nothing))
;              (forall c in (get-alphabet p)
;                (let ((nsp (serial (DFA/forward-step p c)))
;                      (nsq (serial (DFA/forward-step q c))))
;                  (if (eq? 'x (mirror-table-get table nsp nsq))
;                      (begin
;                        (mirror-table-put! table sp sq 'x)
;                        (delq! pair to-check)
;                        (set! changed #t)
;                        (cont 'nothing)))))))) 
;      (if changed (iter #f) to-check))))

;(define (trace-root d)
;  (cond ((null? (car d)) d)
;        ((eq? d (car d)) d)
;        (else
;         (trace-root (car d)))))

;(define (merge-edges! d)
;  (let ((alist '()))
;    (forall edge in (get-edges d)
;      (let ((dest (get-dest edge))
;            (chars (get-chars edge)))
;        (cond ((assq dest alist) =>
;               (lambda (entry)
;                 (set-cdr! entry (cons chars (cdr entry)))))
;              (else
;               (set! alist (cons (cons dest (list chars)) alist))))))
;    (set-cdr! d 
;              (map
;                (lambda (entry)
;                  (cons (sort (list-uniq (list-flatten (cdr entry))) char<?)
;                        (car entry)))
;                alist))))

;(define (DFA/simplify! D)
;  (let* ((alist (NFA/nodes->alist D))
;         (fvec (NFA/alist->fvec alist))
;         (serial (lambda (n) (cdr (assq n alist))))
;         (eqv-class (DFA/find-eqv-class D alist fvec)))
;    (forall pair in eqv-class
;      (let* ((p (car pair)) (sp (serial p))
;             (q (cdr pair)) (sq (serial q))
;             (m (if (< sp sq) p q)) ; m is the smaller one
;             (n (if (< sp sq) q p)))
;        (set-car! n m)
;        (if (null? (car n))
;            (set-car! n n))))
;    (forall entry in alist
;      (forall edge in (get-edges (car entry))
;        (set-cdr! edge (trace-root (get-dest edge)))))
;    (forall entry in alist
;      (let ((root (trace-root (car entry))))
;        (if (not (eq? root (car entry)))
;            (set-cdr! root (append (get-edges root) (get-edges (car entry)))))))
;    (forall entry in alist
;      (if (eq? (trace-root (car entry)) (car entry))
;          (merge-edges! (car entry))))
;    D))

(define (DFA/forward-step d c)
  (call-with-current-continuation
    (lambda (K)
      (forall edge in (get-edges d)
        (if (char-set-member? (get-char-set edge) c)
            (K (get-dest edge))))
      (K #f))))

(define (DFA/run D char-seq)
  (let ((start (DFA/get-start D))
        (accepts (DFA/get-accepts D)))
    (let iter ((current-state start) (sequence char-seq) (stack '()))
      (cond ((and (not (null? sequence))
                  (DFA/forward-step current-state (car sequence))) =>
             (lambda (next-state)
               (iter next-state (cdr sequence) (cons (car sequence) stack))))
            (else
             (list (and (memq current-state accepts) #t)
                   (reverse stack)
                   sequence))))))

(define (RE/compile RE)
  (car (NFA->DFA (RE->NFA RE))))

(define (RE/matches? RE str)
  (let ((D (RE/compile RE)))
    (car (DFA/run D (string->list str)))))

(define (RE/scan RE str)
  (let ((D (RE/compile RE)) (char-seq (string->list str)))
    (let iter ((run (DFA/run D char-seq)) (result '()))
      (if (and (null? (list-ref run 1)) (null? (list-ref run 2)))
          (reverse result)
          (if (list-ref run 0)
              (iter (DFA/run D (list-ref run 2)) (cons (list-ref run 1) result))
              (iter (DFA/run D (cdr (append (list-ref run 1) (list-ref run 2)))) result))))))

(define (NFA->DOT N filename #!optional node-seq)
  
  (define sequence
    (if (default-object? node-seq) (NFA/nodes->alist N) node-seq))
  
  (let ((port (open-output-file filename))
        (start-time (get-universal-time))
        (start-clock (real-time-clock)))
    
    (format #t "** [NFA->DOT] **~%")
    (format #t "  * Started at ~A:~%" 
            (universal-time->local-time-string start-time))
    (format #t "  * Writing common header ... ")  
    (format port "digraph fsm {~%")
    (format port "d2toptions=\"-c -ftikz -tmath --autosize --edgeoptions=auto --tikzedgelabels\"; ~%")
    (format port "rankdir=LR; ~%")
    
    (format #t "Done! ~%  * Writing accept vertex ... ")
    (format port
            "node [shape = doublecircle]; d~A; ~%"
            (cdr (assq (NFA/get-accept N) sequence)))
    
    (format #t "Done! ~%  * Writing vertexs and edges (this may take a while) ... ")
    (format port "node [shape = circle];~%")
    (forall pair in sequence
      (let ((from (cdr pair)))
        (format port "d~A [label=\"d_{~A}\"];~%" from from)
        (forall edge in (then pair -> car -> get-edges)
          (let ((to (cdr (assq (get-dest edge) sequence))))
            (format port "d~A -> d~A [label=\"~A\"];~%" from to
                    (stringfy (get-char-set edge)))))))
    
    (format #t "Done! ~%  * Close ouput port ... ")
    (format port "}")
    (close-output-port port)
    
    (format #t "Done!~%")
    (let* ((cost-clock (- (real-time-clock) start-clock))
           (nodes-count (length sequence)))
      (format #t " ---------------------------------------------------------~%")
      (format #t " Filename: ~A~%" filename)
      (format #t "   Vertex: ~A~%" nodes-count)
      (format #t "     Cost: ~A sec~%" (internal-time/ticks->seconds cost-clock))
      )
    ))

; DFA->DOT
;
;   Plot a DFA to a dot file. If a map-table is presented, the node of NFA
;   counterpart will be plot as node label instead.
(define (DFA->DOT D filename #!optional node-seq map-table)
   
  (define sequence
    (if (default-object? node-seq) (NFA/nodes->alist D) node-seq))

  (let* ((port (open-output-file filename))
         (start-time (get-universal-time))
         (start-clock (real-time-clock)))
    
    (format #t "** [DFA->DOT] **~%")
    (format #t "  * Started at ~A:~%" 
            (universal-time->local-time-string start-time))
    (format #t "  * Writing common header ... ")  
    (format port "digraph fsm {~%")
    (format port "d2toptions=\"-c -ftikz -tmath --autosize --edgeoptions=auto --tikzedgelabels\"; ~%")
    (format port "rankdir=LR; ~%")
    
    (format #t "Done! ~%  * Writing accept vertexs ... ")
    (format port "node [shape = doublecircle]; ~%")
    (forall accept-node in (DFA/get-accepts D)
      (format port
            "d~A [label=\"d{~A}\"]; ~%"
            (cdr (assq accept-node sequence))
            (cdr (assq accept-node sequence))
            ;(serials->string (cdr (assq accept-node map-table)))
            ))
    
    (format #t "Done! ~%  * Writing vertexs and edges (this may take a while) ... ")
    (format port "node [shape = circle];~%")
    (forall pair in sequence
      (let ((from (cdr pair)))
        (if (default-object? map-table)
            (format port "d~A [label=\"d_{~A}\"];~%" from from)
            (format port "d~A [label=~S];~%" from "asdf") ; FIXME
            )
        (forall edge in (then pair -> car -> get-edges)
          (let ((dest (cdr (assq (get-dest edge) sequence))))
            (format port "d~A -> d~A [label=\"~A\"];~%" from dest
                    (stringfy (get-char-set edge)))))))
    
    (format #t "Done! ~%  * Close ouput port ... ")
    (format port "}")
    (close-output-port port)
    
    (format #t "Done!~%")
    (let* ((cost-clock (- (real-time-clock) start-clock))
           (nodes-count (length sequence)))
      (format #t " ---------------------------------------------------------~%")
      (format #t " Filename: ~A~%" filename)
      (format #t "   Vertex: ~A~%" nodes-count)
      (format #t "     Cost: ~A sec~%" (internal-time/ticks->seconds cost-clock))
      )
    ))

(define (stringfy c)
  (cond ((eq? 'eps c) "\\\\epsilon")
        ((char-set? c)
         (format #f "\\\\texttt{~A}"
                 (join "," (scalar-values->strings (char-set->scalar-values c))))) 
        (else (error "type error!"))))

(define latex-escape
  (let ((escape-table '((#\$ . "\\\\$") (#\\ . "\\\\textbackslash")
                        (#\% . "\\\\%") (#\# . "\\\\#")
                        (#\_ . "\\\\_") (#\& . "\\\\&")
                        (#\{ . "\\\\{") (#\} . "\\\\}")
                        (#\~ . "\\\\textasciitilde")
                        (#\^ . "\\\\textasciicircum"))))
    (lambda (c)
      (let ((entry (assq (integer->char c) escape-table)))
        (if entry
            (cdr entry)
            (integer->char c))))))

(define scalar-values->strings
  (let* ((e latex-escape)
         (f (lambda (interval)
              (cond ((number? interval)
                     (format #f "~A" (e interval)))
                    (else
                     (format #f "[~A-~A]" (e (car interval)) (e (- (cdr interval) 1))))))))
    (lambda (cs) (map f cs))))

