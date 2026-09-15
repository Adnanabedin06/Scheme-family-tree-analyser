#lang scheme
;;Your full name: Paula Peters
;;Student ID: 001303256-9
;;Date of birth (day/month/year): 12/10/2004

;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.

;;Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
((Mary Jones) (() ())((12 5 1967) (19 5 2024)))
((Tom Blake) (() ()) ((17 1 1964) ()))
((Ada West) (() ()) ((22 8 1973) ()))
((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
((Ned Bloom) (() ()) ((23 04 2001)()))
((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;,Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021)))
((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
((John Doe) (() ()) ((18 2 1899)(7 7 1970)))
((Lisa Brown) (() ())((30 6 1904) (6 3 1980)))
((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1- List the Maternal Branch
(define (lst-mb mb)
 (if(null? mb)
    '()
    (cons(caar mb)(lst-mb(cdr mb)))))
 
;; C2-List Paternal Branch  
(define (lst-pb pb)
 (if(null? pb)
    '()
    (cons(caar pb) (lst-pb(cdr pb)))))
 
;; C3- Append list and list all family members
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))

(define (lst-all mb pb)
  (append-lst(lst-mb mb)(lst-pb pb))) ;; Corrected the use function parameters mb and pb

;; Execute to disolay alll family members
(lst-all Mb Pb)

;; PARTNER A - PAULA PETERS

;; A1
(define (parents lst) ;; This function uses map to apply cadr to each element in the list, effectively extracting the parents' information
  (map (lambda (person) (cadr person)) lst))

;; Teast
;; (parents Mb)
;; (parents Pb)
;; (parents  (lst-all Mb Pb))
 
;; A2
(define (living-members lst) ;; Function filters the list to include only those who are alive
  (define (is-alive? person)
    (and (>= (length (caddr person )) 2) (null? (cadr (caddr person)))))

  (define (extract-living lst)
    (if (null? lst) ;;It checks if the deathe date is an empty list
        '()
        (let ((person (car lst))
              (rest (cdr lst)))
          (if (is-alive? person)
              (cons (car person) (extract-living rest))
              (extract-living rest)))))
  (extract-living lst))


;; Test
;; (living-members Pb)
;; (living-members Mb)
;; (living-members(lst-all Mb Pb))
;; A3
(define (current-age lst);; Calculate age based on the birth date and the current year
  (define (calculate-age birth-date)
    (if (and (pair? birth-date) (>= (length birth-date) 3))
        (let* ((current-year 2025)  ;; Update this if needed
               (birth-year (caddr birth-date))
               (birth-month (cadr birth-date))
               (birth-day (car birth-date))
               (age (- current-year birth-year)))
          (if (or (> birth-month 3)  ;; Assuming March is the current month
                  (and (= birth-month 3) (> birth-day 3)))
              (- age 1)
              age))
        #f))  ;; Return #f if birth-date is invalid
 
  (define (extract-ages lst)
    (if (null? lst)
        '()
        (let* ((person (car lst))
               (rest (cdr lst))
               (birth-info (car (caddr person))))
          (if (and (null? (cadr (caddr person))) (pair? birth-info))  ;; Check if alive and birth-date is valid
              (let ((age (calculate-age birth-info)))
                (if age
                    (cons (list (car person) age) (extract-ages rest))
                    (extract-ages rest))) ;; Recursive call if the person is alive
              (extract-ages rest))))) ;; Recursive call if the person is deceased
 
  (extract-ages lst))

;; Test
;; (current-age Mb)
;; (current-age Pb)


;; A4 Filters meembers born in a specified month by checking the month partg of the birth date
(define (same-birthday-month lst month)
  (define(extract-same-month lst month)
    (if (null? lst)
        '()
        (let* ((person(car lst)) ;; Use of recursive case, first family member in the list car lst
               (rest (cdr lst))
               (birth-date (car (caddr person))));; This assumes the birth date is stored as the first element of the third sublist in the family members data
          (if (and (pair? birth-date) (>= (length birth-date)
                                          3)) ;; Condition ensures the birth date is valid
              ;; (pair? birth-date): Checks if birth-date is a non-empty list.
              ;; (>= (length birth-date) 3): Ensures the birth date has at least three elements (day, month, year).
              (let((birth-month (cadr birth-date)))
                (if (= birth-month month)
                    (cons (car person) (extract-same-month rest month))
                    (extract-same-month rest month)))
              (extract-same-month rest month)))))
 

(extract-same-month lst month))
;; Test
;;  (same-birthday-month Mb 10)
;;  (same-birthday-month Pb 10)
 
;; A5 sort the family members by their last name. it uses a helper function to extract the last name and comparison function to sort the list
(define (sort-by-last lst)
  ;; Function to extract the last name from a full name
  (define (last-name name)
   (if (null? (cdr name))
        (car name) ;; If there's only one name, return it
        (last-name (cdr name)))) ;; Recursively get the last name
 
  ;; Function to compare 2 names by their last names
  (define (compare-last-names person1 person2)
    (string<? (symbol->string (last-name (car person1)))
              (symbol->string (last-name (car person2)))))
 
  ;; Higher-level function to sort the list of members by their last names
    (sort lst compare-last-names))

 ;; Test function
;;(sort-by-last Mb)
;; (sort-by-last Pb)


;; A6
;; Function to change the name of any member with a specific old name to a new name
(define (change-name-to-Juan lst old-name new-name)
  (define (change-name name)
    (if (and (not (null? name)) (equal? (car name) old-name))
        (cons new-name (cdr name))  ;; Change the old name to the new name
        name))  ;; Leave other names unchanged
 
;; Changes the name of a specified family member to a new name. It processes each members and updates the name if it matches the old name
 
  (define (process-member member)
    (let ((name (car member))  ;; Extract the name
          (parents (cadr member))  ;; Extract the parents
          (dates (caddr member)))  ;; Extract the birth and death dates
      (list (change-name name)  ;; Change the name if necessary
            parents  ;; Keep the parents unchanged
            dates)))  ;; Keep the dates unchanged
 
  (map process-member lst))

 ;; Test the function
;;(change-name-to-Juan Pb 'John 'Juan)
;; (change-name-to-Juan Mb 'John 'Juan)