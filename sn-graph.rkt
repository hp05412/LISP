;; Done
(module sn-graph racket
  (provide sn-consistent
           sn-empty
           sn-add-user
           sn-users
           sn-add-frndshp
           )

  ;; required libraries. (imported above)
  (require racket/dict)
  (require racket/set)

  
  ; Hard
  (define (sn-consistent p) #t)

  ;; graph
  ;; -> [a]
  ;; Easy (+0.5)

;; Already empty given but also can be done as "(define sn-empty '()) representing an empty list.  
  (define sn-empty
   empty)                      

  ;; Easy

 ;; Returns "users" from the graph.
  ;; [(k,v)] -> [u]
  (define (sn-users graph)
    (dict-keys graph))        

  
  ;; Hard
  ;; [(k,v)] u -> [(k,v)] | (u,{})
  (define (sn-add-user graph user)
    (if (dict-ref graph user #f)        ;; condition if the "user" found in graph returns false also, terminate the loop  
        graph
        (dict-set graph user '())))     ;; Else add the "user" to the graph or dict
    ;;(/ 1 0))

  ;; Hard
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;;  [(k,v)] | (u1,f1+{f2}) | (u2,f2+{f1})
  (define (sn-add-frndshp graph u1 u2)
    (map (lambda (lst)
           (cond
            [(equal? (first lst) u1) (not (member u2 lst)) (append lst (list u2))]    ;; Checking the user1 and user2 have same friends or not.
            [(equal? (first lst) u2) (not (member u1 lst)) (append lst (list u1))]    ;; If they do not have same friends new dictionary will be created and returns that with the new friends.
            [else lst]                                                                ;; Else returns the graph wihout having any modification.
            ))graph))
    ;;(/ 1 0))


  )