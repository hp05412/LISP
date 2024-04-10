;; Done

(module sn-utils racket
   (require racket/string)

  (provide sn-dict-ks-vs
           sn-line->entry
           sn-list->dict
           )

 
  ;; utils
  ;; [k] [v] -> [(k,v)]
  ;; Easy
   
;; map function has been used to apply on (ks and vs) elements cons functions as there to join it as (ks.vs)
;;or It takes two list apply cons functions and return list of cons element.
   (define (sn-dict-ks-vs ks vs)
    (map cons ks vs))

  ;; Medium
  ;; str -> (a,[a])
  (define (sn-line->entry values)
    (let ((lst (string-split values))) ;;lst has been used here as variable it takes string and split into a a list of stings
      (cons (string->symbol (car lst)) ;; It takes the first element from the list/cells and returns with cons list  
       (map string->symbol (cdr lst))))) ;; At last it converts all strings into a symbols 

  ;; [(a,b)] -> [(a,b)] 
  ;; Easy
  (define (sn-list->dict es)
   (map (lambda (entry)                    ;; Takes a list 
          (cons (car entry) (cdr entry))) es))  ;; Create a dictionary of the list and representing/ printing a keys and values or a list.

)
  
 

  
  