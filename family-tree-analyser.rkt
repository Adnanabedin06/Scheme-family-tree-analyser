#lang scheme

;; Your full name: Adnan Abedin
;; Student ID: 001391972
;; Date of birth (day/month/year): 13/08/2006
;; Define maternal family tree

(define MaternalTree
  '(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
    ((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
    ((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
    ((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
    ((Mary Jones) (() ()) ((12 5 1967) (19 5 2024)))
    ((Tom Blake) (() ()) ((17 1 1964) ()))
    ((Ada West) (() ()) ((22 8 1973) ()))
    ((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
    ((Ned Bloom) (() ()) ((23 4 2001) ()))
    ((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;; Define paternal family tree
(define PaternalTree
  '(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021)))
    ((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
    ((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
    ((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
    ((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
    ((John Doe) (() ()) ((18 2 1899) (7 7 1970)))
    ((Lisa Brown) (() ()) ((31 6 1904) (6 3 1980)))
    ((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
    ((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
    ((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;; Function to list maternal family members (C1)
(define (list-maternal tree)
  (if (null? tree)
      '()
      (cons (caar tree) (list-maternal (cdr tree)))))

;; Function to list paternal family members (C2)
(define (list-paternal tree)
  (if (null? tree)
      '()
      (cons (caar tree) (list-paternal (cdr tree)))))

;; Combine both lists into one (c3)
(define (combine-lists list1 list2) ;joining the two lists together to output the combined family tree
  (if (null? list1)
      list2
      (cons (car list1) (combine-lists (cdr list1) list2))))

(define (list-all tree1 tree2)
  (combine-lists (list-maternal tree1) (list-paternal tree2)))

;; Display maternal, paternal, and all family members 
(list-maternal MaternalTree) ;when this is input it will print out all members from the maternal branch
(list-paternal PaternalTree)
(list-all MaternalTree PaternalTree)

;; Function to list children in a family tree (B1)
(define (find-children tree)      ;Finds children in the family trees who have both parents
  (define (has-parents? person)
    (let ((parents (cadr person)))
      (and (not (null? (car parents)))
           (not (null? (cadr parents))))))

  (define (get-name person)
    (string-join (map symbol->string (car person)) " "))
  
; Here the sumbol is converting to a string
  
  (if (null? tree)
      '()
      (let ((current (car tree))
            (rest (cdr tree)))
        (if (has-parents? current)
            (cons (get-name current) (find-children rest))
            (find-children rest)))))

;; Display children from both trees 
(find-children MaternalTree)
(find-children PaternalTree)


;; Function to find the oldest living member (B2)
; This will determine and print out the oldest member that is still alive from both family trees
(define (oldest-living tree)
  (define (alive? person)   ;Here it checks if the death date is null, which will indicate if a person is alive or deceased
    (null? (cadr (caddr person))))

  (define (get-birth-year person)  ;Gets the birth year from peoples birth date (tuple)
    (caddr (car (caddr person))))

  (define (compare-age oldest current)
    (if (and (alive? current)
             (or (null? oldest)
                 (> (get-birth-year oldest) (get-birth-year current))))
        current
        oldest))

  (let loop ((remaining tree) (oldest null))
    (if (null? remaining)
        (if oldest
            (list (caar oldest) (caddr (car (caddr oldest))))
            '("No living members"))
        (loop (cdr remaining) (compare-age oldest (car remaining))))))

;; Display oldest living members
(oldest-living MaternalTree)
(oldest-living PaternalTree)


;; Function to calculate average age at death (B3)

; This calculates average age of death for all deceased members in both branches
(define (average-death-age tree)
  (define (get-age person)
    (let ((birth (car (caddr person)))
          (death (cadr (caddr person))))
      (if (and (not (null? death))
               (not (null? birth)))
          (- (caddr death) (caddr birth))
          #f)))

  (let ((ages (filter number? (map get-age tree))))
    (if (null? ages)
        0
        (/ (apply + ages) (length ages)))))

;; Display average age at death
(average-death-age MaternalTree)
(average-death-age PaternalTree)


;; Function to find members with birthdays in a specific month(B4)
; Prints out members who are born on August same as me to be specific...
(define (birthday-month tree month)
  (define (get-month person)
    (cadr (car (caddr person))))

  (define (same-month? person)   ; comparison between the extracted month to the give month of August as it must match my birth month
    (= (get-month person) month))

  (define (get-name person)
    (string-join (map symbol->string (car person)) " "))

  (map get-name (filter same-month? tree)))

;; Display members with birthdays in August (8)
(birthday-month MaternalTree 8)
(birthday-month PaternalTree 8)

;Sorts in chronological order by First names similar to a register system
;; Function to sort members by first name (B5)
(define (sort-by-first-name tree)
  (define (first-name person)
    ;Gets the first name as a string from a persons symbol converted
    (symbol->string (car (car person))))

  (define (compare person1 person2)
    (string<? (first-name person1) (first-name person2)))

  (define (get-name person)
    (string-join (map symbol->string (car person)) " "))

  (map get-name (sort tree compare)))

;; Display sorted names
(sort-by-first-name MaternalTree)
(sort-by-first-name PaternalTree)


;; Function to rename "Mary" to "Maria" (B6)
(define (rename-mary tree)
  (define (rename person)
    (let ((name (car person)))
      (if (equal? (car name) 'Mary)
          (cons 'Maria (cdr name))
          name)))

  (map rename tree))

;; Display renamed names
(rename-mary MaternalTree)
(rename-mary PaternalTree)
