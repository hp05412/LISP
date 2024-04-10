(module sn-social-network racket

  (provide 
   sn-ff-for
   sn-cmn-frnds-btwn
   sn-cmn-frnds
   sn-frnd-cnt
   sn-frndlst-user
   sn-unfrndlst-user  )

  (require "sn-graph.rkt")
  (require "sn-utils.rkt")
  
  ;; social-network.
  ;; Easy
  ;; [(k,v)]| (u,vu) -> vu
 ;; (sn-ff-for) has been defined in a way which takes 2 aruguments one is "graph" which is dictionary and second one is "u1" which is keys.
 ;; and returns friends list or the values of the keys which has been given as an arguments for here its "f4" which is given at testing sections.

  (define (sn-ff-for graph u1)     
    (dict-ref graph u1 '()))     
    ;;(/ 1 0))


  ;; Medium
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;; f2 & f3
;; filter and lambda functions are used here to check and as for loop as it as side effect free program it does not contains "for each loop"
;; Find common friends from list1 and list 2 by comparing each others values via filter and lambda functions.
;; If list1 is a value or members of list2  or vise-versa then it returns common friends. If no common friends found returns an empty list.
 ;; As both have same value and it's a dictionary it create a list with same values with duplication.However,we have use "remove function" to remove duplicates.

  (define (my-intersect lst1 lst2)
  (filter (lambda (x) (member x lst2)) lst1)) 
  (define (sn-cmn-frnds-btwn graph u1 u2)      
    (let* ([ff1 (sn-ff-for graph u1)]          
           [ff2 (sn-ff-for graph u2)]
           [ffs (my-intersect ff1 ff2)])      
      (remove-duplicates ffs)))               
    ;;(/ 1 0) )

  
  ;; Hard
  ;; Takes social network file "graph" as an input.
  ;; Function "count-friends" has been defined which takes "user" as an input.And Find the length of the "user" from "sn-ff-for" function which is predefined
  ;; Cons functions return a pair of list.
  ;; Map function applies to count-friends in result it returns a list of pair consist of user freinds and number of friend they have/ count.

  (define (sn-frnd-cnt graph)            
    (define (count-friends user)          
      (cons user (length (sn-ff-for graph user)))) 
    (map count-friends (sn-users graph)))       
    ;;( / 1 0))

  ;; pre: length > 0
  ;; Takes social network file "graph" as an input.
  ;;  "friend-counts" is a function which takes an input from the "sn-frnd-cnt" which has been pre defined above function.
  ;; takes the friends count the maxmimum number of friend does the user have
  ;; Compares each and every numbers and find the maximum numbers of friends from the list
  ;; Returns maximum number of friends with the user in a pair, using "Cons Function."
  
  (define (sn-frndlst-user graph)       
    (let* ([friend-counts (sn-frnd-cnt graph)]     
           [max-friends (apply max (map cdr friend-counts))]  
           [max-friends-users (map car (filter (lambda (x) (= (cdr x) max-friends)) friend-counts))])  
      (cons (car max-friends-users) max-friends)))    
    
    ;;(/ 1 0))
              
          
  ;; pre: length > 0
  ;; Takes social network file "graph" as an input.
  ;;  "friend-counts" is a function which takes an input from the "sn-frnd-cnt" which has been pre defined above function.
  ;; takes the friends count the minimum number of friend does the user have.
  ;; Returns minimum number of friends with the user in a pair, using "Cons Function."
  
  (define (sn-unfrndlst-user graph)        
    (define min-friends (apply min (map cdr (sn-frnd-cnt graph))))     
    (let ((unfriends (filter (lambda (x) (= (cdr x) min-friends)) (sn-frnd-cnt graph))))  
      (cons (caar unfriends) min-friends)))   
    
    ;;(/ 1 0))

  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds-ff graph u)
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-btwn graph u key))
                keys)]
       
         )
      (sn-dict-ks-vs keys vals)))


  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds graph )
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-ff graph key))
                keys)]
         )
      (sn-dict-ks-vs keys vals)))

  )

