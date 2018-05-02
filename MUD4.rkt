#lang racket

;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)

   #|------------Racket MUD---------------|#
   #|                                     |#
   #| Author: SZABOLCS NAGY               |#
   #|                                     |#
   #| Contact: 21293260@student.uwl.ac.uk |#
   #|                                     |#
   #|-------------------------------------|#

      #|-------------------|#
      #|  Data Structures  |#
      #|-------------------|#

;; Object descriptions
(define objects '((1 "the holy grail")
                  (2 "N")(11 "N")(13 "N")(15 "N")(16 "N")
                  (5 "a metal sword")
                  (3 "a holy cross")
                  (4 "common gavel")
                  (12 "a 24 inch gauge")
                  (14 "an interdimensional communicator")
                  (6 "a chisel")
                  (7 "a square")
                  (8 "compasses")
                  (9 "a skull and crossbones")
                  (10 "a blue and white apron")))

;;Life available in the rooms to be picked up by the user
(define lives-in-the-room '((1 " 1 ") (2 "z") (3 "1") (4 "z") (5 "z") (6 "z")
                            (7 "1") (8 "z") (9 "") (10 "z") (11 "z") (12 "z")
                            (13 "z") (14 "z") (15 "z") (16 "z")))
                            

;;Monsters present in the rooms
(define monsters-in-the-room '((1 "S")
                               (2 "M")
                               (3 "M")
                               (4 "M")
                               (5 "S")
                               (6 "M")
                               (7 "M")
                               (8 "S")
                               (9 "S")
                               (10 "M")
                               (11 "S")
                               (12 "M")
                               (13 "S")
                               (14 "S")
                               (15 "M")
                               (16 "S")))


;; Rooms description
(define descriptions '((1 "\n   - You are in a lodge. -")
                       (2 "\n   - You are in King Solomon's temple. -")
                       (3 "\n   - You are in a temple. -")
                       (4 "\n   - You are in a graveyard. -")
                       (5 "\n   - You are in a mystic room. -")
                       (6 "\n   - You are in the ancient ruins entrance. -")
                       (7 "\n   - You are in the old great hall. -")
                       (8 "\n   - You are in Jerusalem. -")
                       (9 "\n   - You are in a coffin. -")
                       (10 "\n   - You are on the top of a mountain. -")
                       (11 "\n   - You are in a castle. -")
                       (12 "\n   - You are in a river. -")
                       (13 "\n   - You are on the north pole. -")
                       (14 "\n   - You are in a 2nd World War bunker. -")
                       (15 "\n   - You are in a Buckingham Palace. -")
                       (16 "\n   - You are in a on the Moon. -")))





;; Initialise the life database
(define lifedb (make-hash))

;; Initialise the monster database
(define monsterdb (make-hash))

;; Initializes the object database
(define objectdb (make-hash))

;; Initializes the inventory database
(define inventorydb (make-hash))

;; Lists of pairs. First we have the user's entry and second we have what our software should understand with that entry

(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define add '(((add) add) ((health) add)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define lives '(((lives) lives) ((life) lives)))
(define help '(((help) help)))
(define show '(((show) show)))
(define kill '(((kill) kill) ((fight) kill) ((hit) kill) ((kick) kill)))
(define monsters '(((monsters) monsters) ((monster) monsters)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

;; Lists using unquote-splicing to dynamically reference all the other lists
(define actions `(,@look ,@pick ,@drop ,@inventory ,@help ,@quit ,@show ,@lives ,@add ,@monsters ,@kill  ))
(define decisiontable `((1 ((north) 2) ((east) 13) ((west) 11) ((north west) 10) ((north east) 12) ,@actions)
                        (2 ((south) 1) ((north) 3) ((north west) 10) ((south west) 11) ((east) 12) ((north east) 7) ((south east) 13) ,@actions)
                        (3 ((west) 4) ((south) 2) ((north east) 5) ((east) 7) ((north) 6 ) ((north east) 5) ((south east) 12) ((south west) 10) ((north west) 9) ,@actions)
                        (4 ((south east) 2) ((east) 3) ((north east) 6) ((north) 9) ((south) 10) ,@actions)
                        (5 ((south west) 3) ((west) 6) ((south) 7) ((south east) 8) ((east) 14) ,@actions)
                        (6 ((south) 3) ((east) 5) ((south east) 7) ((south west) 4) ((west) 9) ,@actions)
                        (7 ((west) 3) ((north) 5) ((north west) 6) ((east) 8) ((south east) 15) ((north east) 14) ((south) 12) ,@actions)
                        (8 ((west) 7) ((north) 14) ((north west) 5) ((south west) 12) ((south) 15) ,@actions)
                        (9 ((east) 6) ((south) 4) ((south east) 3) ,@actions)
                        (10 ((east) 2) ((south) 11) ((north) 4) ((south east) 1) ((north east) 3) ,@actions)
                        (11 ((north) 10) ((east) 1) ((north east) 2) ,@actions)
                        (12 ((west) 2) ((east) 15) ((south) 13) ((north) 7) ((south west) 1) ((north west) 3) ((south) 13) ((north east) 8) ((south east) 16) ,@actions)
                        (13 ((north) 12) ((east) 16) ((west) 1) ((north east) 15) ,@actions)
                        (14 ((west) 5) ((south) 8) ((south west) 7) ,@actions)
                        (15 ((west) 12) ((south) 16) ((south west) 13) ((north west) 7) ((north) 8) ,@actions)
                        (16 ((west) 13) ((north) 15) ((north west) 12) ,@actions)))
    

      #|-------------------|#
      #|     Functions     |#
      #|-------------------|#

;;The following function is used to identify the name of the location, this is done by passing the location id. This will also show what objents we have on the ground in the current room
(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; The following describes the objects that are present in the room
  (display-objects objectdb id)
  ;; The following describes if there is life in the room that the user can pick up
  (display-objects lifedb id)
  ;; The following describes if there are monsters present in the room
  (display-objects monsterdb id)
  ;; The following prints greater then sign, the role of this is to prompt the user for an entry
  (printf " > "))

;;Initialise the health of the user, set it to null
(define health null)
;;  The following function displays the life that is available to be collected in the room and in the inventory
(define (display-objects db id)
  ;;The following checks if key (id) has something stored in db, if so, then proceed
  (when (hash-has-key? db id)
    ;;Then following records the content of the key id inside the db hash table, gets previous items assigned to a room or bag
    (let* ((record (hash-ref db id))
           
           ;;The following formats the output, creates a list of the items in the room
           (output (string-join record "  ")))
      
      ;;The following shows lives and on the ground. Adds treatment to cases where the room is empty
      (cond
        ((and (or(equal? output "")(equal? output "S")) (number? id)) (printf "\n   - The area appears to be safe, no Monsters around! -\n\n\n"))
        ((and (equal? output "M") (number? id)) (printf "\n   - BEWARE! - THERE'S A MONSTER RIGHT IN FRONT OF YOU! -\n
                                                                (remember: you can only fight the Monster if you have a weapon! 
                                                                If you don't fight the Monster and you leave the area you will 
                                                                loose a life from your health as the Monster will hit you!)\n\n"))
        ((and (or(null? output)(equal? output "NO") )  (number? id)) (printf "\n   - No health availabe here -\n"))
        ((and (eq? id 'lives) (not(null? output)) ) (printf "\n   - YOUR HEALTH: ~a \n" output))
        ((and (equal? output "E") (eq? id 'bag)) (printf "   - Your bag is empty -\n"))
        ((and (not(equal? output "E")) (eq? id 'bag)) (printf "   - Your bag is empty -\n"))
        ((and (equal? output "N") (number? id)) (printf "\n   - There are no weapon on the floor in this area -\n"))
        ((and (not (equal? output "")) (eq? id 'bag)) (printf "\n   - You are carrying ~a.\n" output))
        ((and(equal? output "1") (eq? db lifedb)) (printf "\n   - You see ~a life that you can pick up! -\n" output))
        ((and (not(equal?  output "E")) (eq? db objectdb)) (printf "\n   - You see ~a on the floor, \n     you can pick it up and add it to you bag\n     for later, as you might need it to fight Monsters. -\n" output))
        ))))


;;The following function retrieves the cdr of the first pair in assqlist where the car is equal the to id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))



;;The following function identifies the items related to the room
(define (lookup id tokens)
  ;;The following assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         ;;The following assigns to keylist a list with the valid keywords for the game
         (keylist (get-keywords id))
         ;;The following calls list-of-lengths, By doing this creates a list of lengths with the most probable commands and then decide which one is the most probable using index-of-largest-number
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    ;;The following checks if there is an index, this is to prevent errors, and retrieves the command that is present in that index inside the list record, which contains the valid actions for the current room. Otherwise returns false
    (if index 
      (cadr (list-ref record index))
      #f)))

;;The following function does the same as assq-ref but uses eqv? for comparison instead of eq?
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;;The following function retrieves the valid keywords for the game
(define (get-keywords id)
  ;;The following assigns to keys, a list with the possible actions for the current room
  (let ((keys (assq-ref decisiontable id)))
    ;;The following returns the accepted keywords, but not their actions
    (map (lambda (key) (car key)) keys)))

;;The following function returns a list of lengths that shows the most probable commands given by the user. e.g. (0 0 0 3 0 0 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     ;;The following returns the intersection between the tokens list(command given) and the keyword
     (let ((set (lset-intersection eq? tokens x)))
       ;;The following checks if there is an intersection between the lists, if there is, the length of set will not be zero, and thus, the result will have some weight
       (* (/ (length set) (length x)) (length set))))
   keylist))

;;The following function returns the most probable input command
(define (index-of-largest-number list-of-numbers)
  ;;The following sorts the list of lengths in descending order and gets the first element(greatest)
  (let ((n (car (sort list-of-numbers >))))
    ;;The following checks if the list is not empty, returns #f if the greatest element is 0
    (if (zero? n)
      #f
      ;;The following returns the index of the entry with the greatest weight, so it can be matched with the list of keywords later
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;;The following function retrieve what directions you see from the room you are
(define (get-directions id)
  ;;The following describes objects that are present in the room
  (display-objects objectdb id)
  ;;The following describes the life  that is present in the room, if any
  (display-objects lifedb id)
  ;;The following describes if there are monsters present in the room, if any
  (display-objects monsterdb id)
  ;;The following finds the pair that has car equals to id in the decisiontable list, and assign it to record
  (let ((record (assq id decisiontable)))
    ;;The following filters through record and if the second value of it is a number, then that is a room, and it is assigned to result. Also gets the length of n, the rooms you can go to
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;;The following is a conditional case used to finally check the directions
      (cond ((= 0 n)
             ;;The following informs that in the current room no directions were retrieved
             (printf "   - You appear to have entered a room with no exits -\n\n\n"))
            ((= 1 n)
             ;;The following extracts the directions from result using our slist->string function
             (printf "   - You can see an exit to the ~a -\n\n\n" (slist->string (caar result))))
            (else
             ;;In the following the first line(losym) in let* remove the indexes(numbers) from the directions. The second line (lostr) transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;;Thee following takes the atoms from lostr and transform them into a string separated by " and "
               (printf "   - You can see exits to the ~a -\n\n\n" (string-join lostr " and "))))))))

;;The following function maps the parameter to a list of atoms and then joins it in a string with separator " " 
(define (slist->string l)
  (string-join (map symbol->string l)))

;;The following function is for picking up objects
(define (pick-item id input)
  ;;The following removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
   (and (remove-E inventorydb )
(remove-object-from-room objectdb id item))))

;;The following function removes object from the room and add to your bag
(define (remove-m id)
   ;;The following checks if key (id) has something stored in db, if it does then proceed
  (when (hash-has-key? monsterdb id)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room)
    (let* ((record (hash-ref monsterdb id))
            ;;The following removes the occurrence of the item, based on the sufix, which is the most probable user input e.g. dagger, from the room
            (result (remove(lambda (x) (string-suffix-ci? "M" x))  record))
            ;;The following returns the items that record have and result don't
            (item (lset-difference equal? record result)))
          (cond
            ((equal? item '("M"))
                     (hash-set! monsterdb id result)
                     (printf "   - Well done Sir!\n     You defeated the Monster!\n\n\n")
                     )
            (else
             (printf "This area is safe Sir! There are no Monsters here!\n\n\n")
            )))))
;;The following function removes object from the room and add to your bag
(define (remove-object-from-room db id str)
   ;;The following checks if key (id) has something stored in db, if it does then proceed
  (when (hash-has-key? db id)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room)
    (let* ((record (hash-ref db id))
            ;;The following removes the occurrence of the item, based on the sufix, which is the most probable user input e.g. dagger, from the room
            (result (remove (lambda (x) (string-suffix-ci? str x)) record))
            ;;The following returns the items that record have and result don't
            (item (lset-difference equal? record result)))
          (cond
            ((equal? item '("N"))
                    (printf "   - There aren't any collectable weapons in this area Sir! -\n\n\n"))
            ( (null? item)
                ;; If item is null(item is not in the room), reports error
             (printf "   - There's no more available in this in this Sir, you picked them all up! -\n\n\n"))
            ((and(equal? item '("M")) (eq? db monsterdb)) 
                                                          (hash-set! db id result)
                     )
             ((and(and(eq? db objectdb)(not(null? item)))
                  (not(equal? item '("N"))))
              (printf "   - Added ~a to your bag. -\n\n\n" (first item))
              ;; Adds item to inventorydb
              (add-object inventorydb 'bag (first item))
              
              ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
              (if (eq? (first item) "an interdimensional communicator")
                (begin
                  ;; Shows message and exits game
                  (printf "   - Something strange is happening...\nYOU HAVE FOUND THE WAY TO FREEDOM! -\n")
                  (exit))
                ;; Removes item from the ground  
                (hash-set! db id result)))))))

   (define c 0)
(+ c 1)


;;The following function is for picking up life 
(define (add-health id input)
  ;;The following removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input )))))
    (remove-life-from-room lifedb id item) (add-life lifedb 'lives item)))


 ;;The following function is called when user goes from one room to the other. What it does is it checks if there is a Monster in the room that the user is about to exit,
 ;;meaning that the user leaves the room without killing the Monster, if so, then calls the remove-life function
(define (monster-hit  id input)  
  ;;The following checks if key (id) has something stored in db, if so, then proceed
  (when (hash-has-key? monsterdb id)
    ;;Then following records the content of the key id inside the db hash table, gets previous items assigned to a room or bag
    (let* ((record (hash-ref monsterdb id))
            ;;The following formats the output, creates a list of the items in the room
            (output (string-join record )))
      ;;The following checks if there is a Monster in the room when we are leaving the room
      (cond
        ((and (equal? record '("M")) (number? id))
         (begin
         
 (let ((heath(string-join (cdr (string-split input))))
    ;;The foillowing checks if key (id) has something stored in db, if it has then proceed
  (when (hash-has-key? lifedb 'lives) ))
   ;;The following line assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref lifedb 'lives)))

              ;; The following notifies the user of life loss
              (printf "   - The Monster hit you and you lost  life! -\n"  )
        ;; Sets the new result as 'lives
              (hash-set! lifedb 'lives "1")
             (cond
                ((null? 'lives)
                      (begin
                  ;;The following shows message and exits game
                  (printf "   - OH NO! THE MONSTER KILLED YOU! -\n")
                  (exit))))))))))))
       
 
 


                
;;The following function removes life from the room and adds it to health ('lives)
(define (remove-life-from-room db id str)
  ;;The following checks if key (id) has something stored in db, if it does then proceed
  (when (hash-has-key? db 'lives)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room)
    (let* ((record (hash-ref db 'lives))
         
            ;;The following removes the occurrence of the item, based on the sufix, which is the most probable user input e.g. dagger, from the room
            (result (remove (lambda (x) (string-suffix-ci? str x)) record))
            ;;The following returns the items that record have and result don't
            (item (lset-difference equal? record result))
            (st(string-join item "")))
      (cond
        (  (equal? st "z"  )  
             ;;The following checks if item is null, if it is null, meaning that the item is not in the room, it reports error
             (printf "   - There is no life to pick up! -\n"))
            ((equal? st " 1 " )
             (printf "You added 1 life to your health\n")
             (add-life lifedb 'lives " 1 "))
             
              ))))
              


;;The following function checks if the user has a weapon in bag to kill the monster. Error handling for when the user has not got a weapon in bag
(define (weapon id input)
  ;;The following checks if key (id) has something stored in db, if so, then proceed
  (when (hash-has-key? inventorydb 'bag)
    ;;Then following records the content of the key id inside the db hash table, gets previous items assigned to a room or bag
    (let* ((record (hash-ref inventorydb 'bag))
            ;;The following formats the output, creates a list of the items in the room
          ( output (string-join record ))  )
      ;;The following checks if the user ha s any weapon in the bag. Adds treatment to cases where the bag is empty
      (cond
        ;;If bag is empty or has the letter "E" in it
        ((or (equal? output "E" )  (null? output))
  ;;The following message gets printed to notify the user of not having weapon in the bag
  ( printf "   - I'm afraid you don't have a weapon Sir! -\n"))
   ;; The following calls the remove-m function, giving parameters: monsterdb, id and input     
  (else (and(printf "\n   - You are using your weapon to fight the monster -\n\n     ***************)))))|||||||(((((*****************\n\n" )
        ;;The following line calls the remove-life function if there is a Monster in the room when we leave the room
         (remove-m id) (remove-w output)))))))

(define (remove-w input)
;;The following function checks if key (id) has something stored in db, if it has then proceed
  (when (hash-has-key? inventorydb 'bag)
   ;;The following line assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref inventorydb 'bag))
           ;;The following line removes the occurrence of the item, based on the sufix, which is the most probable user input
             (result (remove (lambda (x) (string-suffix-ci? input x)) record))
             ;;The following line returns the items that record have and result don't
             (item (lset-difference equal? record result)))
      (cond ((equal? item '())
               )
             ((or(not(equal? item '())) (not(equal? item '("E")))
              (printf "   - You used ~a to defeat the Monster! -\n" (first item)) 
              (hash-set! inventorydb 'bag result)))))))


(define (remove-item db id str)
;;The following function checks if key (id) has something stored in db, if it has then proceed
  (when (hash-has-key? db 'bag)
   ;;The following line assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref db 'bag))
           ;;The following line removes the occurrence of the item, based on the sufix, which is the most probable user input
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             ;;The following line returns the items that record have and result don't
             (item (lset-difference equal? record result)))
      (cond ((equal? item '())
              (printf "   - You don't have any more weapons Sir! -\n"))
             ((or(not(equal? item '())) (not(equal? item '("E")))
              (printf "   - You used ~a to defeat the Monster! -\n" (first item)) 
              (hash-set! db 'bag result)))))))

;The following function is for dropping objects
(define (drop-item id input)
  ;;The following removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item))) 



;The following function is for dropping objects
(define (drop-life id input)
  ;;The following removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-life lifedb 'lives "1"))) 




;;The following function removes object from database and add it to the room
(define (remove-E db)
  ;;The foillowing checks if key (id) has something stored in db, if it has then proceed
  (when (hash-has-key? inventorydb 'bag)
   ;;The following line assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref inventorydb 'bag))
            ;;The following line removes the occurrence of the item, based on the sufix, which is the most probable user input
             (result (remove (lambda (x) (string-suffix-ci? "E" x)) record))
             ;;The following line returns the items that record have and result don't
             (item (lset-difference equal? record result)))
      (hash-set! inventorydb 'bag result))))



;;The following function removes object from database and add it to the room
(define (remove-life db id str)
  ;;The foillowing checks if key (id) has something stored in db, if it has then proceed
  (when (hash-has-key? db id) 
   ;;The following line assigns to record the content of the key id inside the db hash table
    (let* ((record (hash-ref db id))

            ;;The following line removes the occurrence of the item, based on the sufix, which is the most probable user input
             (result (remove (lambda (x) (string-suffix-ci? str x)) record)))
             ;;The following line returns the items that record have and result don't
           
            
              ;; The following notifies the user of life loss
              (printf "   - The Monster hit you and you lost  life! -\n"  )
        ;; Sets the new result as 'lives
              (hash-set! db 'lives result)
             (cond
                ((null? 'lives)
                      (begin
                  ;;The following shows message and exits game
                  (printf "   - OH NO! THE MONSTER KILLED YOU! -\n")
                  (exit)))
      )
      )))
              
;; Remove object from your bag and add it to the room
(define (remove-object-from-inventory db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             (item (lset-difference equal? record result)))
      (cond ((null? item)
              (printf "You are not carrying that item!\n"))
             (else
              (printf "Removed ~a from your bag.\n" (first item))
              (add-object objectdb id (first item))
              (hash-set! db 'bag result))))))

;;The following function displays the content of users bag in the inventory
(define (display-inventory)
  (display-objects inventorydb 'bag))

;; The following function displays the lives the user has got. Each life is displayed with the number 1 eg. 1 1 means 2 lifes; 1 1 1 means 3 lifes
(define (display-lives) 
  (display-objects lifedb 'lives ))

;; The next function shows the map of the maze to the user
(define (display-show)
  (printf "
                                                                                            +------+------+------+------+
                                                                                            |      |  M   |      |      |
                                                                                            | 9    | 6    | 5    | 14   |
                                                                                            +------+------+------+------+
                                                                                            |   M  |  M   |   M  |   M  |
                                                                                            | 4    | 3    | 7    | 8    |
                                                                                            +------+------+------+------+
                                                                                            |    M |  M   |   M  |   M  |
                                                                                            | 10   | 2    | 12   | 15   |
                                                                                            +------+------+------+------+
                                                                                            |      |      |      |      |
                                                                                            | 11   | 1    | 13   | 16   |
                                                                                            +------+------+------+------+
                                                                                                     

                                                                                                   START POINT: 1
                                                                                                M: Monster in the room
                                     
                                                                                            


"))
           
;;The following function prints the help text on the screen
(define (display-help)
  (printf "\n HELP \n
     
               |||---       M.       U.      D.     ---|||

                    - Multi User Dungeon in racket -\n
          * GAME OBJECTIVE * \n
          The game objective is to activate the nether portal to escape the maze. 
          To be able to open the portal, you must find the Interdimensional Communicator in one of the maze rooms.
          You start off with 1 life, but cou can collect and add more lifes to your health from some of the rooms where there is collectable life available.
          While you are trying to find the Interdimensional Communicator and through it your freedom, you will meet monsters in the various locations you vill visit
          You can also collect objects in your bag from rooms where are collectable objects available, and you can use these objects to kill the Monsters.
          But be careful! You can only defeat a Monster if you have a weapon/object in your bag, if you don't have any weapon in your bag and you enter 
          a location where is a Monster, then the Monster will take a life from your health when you are leanving the room.
          As soon as you loose the last life from your health, you will see a message informing you that the Monster killed you and the game will terminate. \n
         * VALID COMMANDS *\n
          - (look | directions | examine room): Retrieve information about the current room.\n
          - (pick | get | pickup) <item-name> : Your character pick up the item correspondent to <item-name>. If no <item-name> is supplied, it picks up the first item on the rooms list.\n
          - (drop | put | place | remove) <item-name> : Your character throws the item correspondent to <item-name> in your bag on the ground. If no <item-name> is supplied, it drops the first item on your inventory. \n
          - (inventory | bag) : Shows a list composed by the items present in your inventory at the time.\n
          - (kill) : Kill the Monster, if there is in the area\n
          - (add) : picks up the life from the room, if there is, and adds it to health\n
          - (lives | life) : Retrieves information about the nomber of lifes in health. Each health is represented by the number '1', eg. 1 1 1 means 3 lifes.\n
          - (help) : Shows the help file for Racket MUD.\n
          - (quit | exit | quit game | exit game) : Quit the application.\n

                     HAVE FUN!\n
          "))

;;The following function adds a given object to a database (inventory or object dbs)
(define (add-object db id object)
  ;;The follwing returns true if id is stored in the database, otherwise it return false 
  (if (hash-has-key? db id)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room or bag)
    (let ((record (hash-ref db id)))
      ;;The following assigns to the table key (id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons object record)))
    ;;The following assigns the object (consed with '() to become a list) to a key (id) in the hash table
    (hash-set! db id (cons object empty))))

;;The following function pregame population of rooms with objects
(define (add-objects db)
  (for-each
    (lambda (r)
      ;; The following adds description (second r) to room id (first r)
      (add-object db (first r) (second r))) objects))

           
;;The following function adds the monsters to the database 
(define (add-monster db id monster)
  ;;The follwing returns true if id is stored in the database, otherwise it return false 
  (if (hash-has-key? db id)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room or bag)
    (let ((record (hash-ref db id)))
      ;;The following assigns to the table key (id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons monster record)))
    ;;The following assigns the object (consed with '() to become a list) to a key (id) in the hash table
    (hash-set! db id (cons monster empty))))

;;The following function pregame population of rooms with monsters
(define (add-monsters db)
  (for-each
    (lambda (r)
      ;; The following adds description (second r) to room id (first r)
      (add-monster monsterdb (first r) (second r))) monsters-in-the-room))

;;The following function picks life up from the room, if there is
(define (add-life db id life)
  ;;The following returns true if id is stored in the database, otherwise it return false 
  (if (hash-has-key? db id)
    ;;The following assigns to record the content of the key id inside the db hash table (gets previous items assigned to a room or bag)
    (let ((record (hash-ref db id)))
      ;;The following assigns to the table key (id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons life record)))
    ;;The following assigns the object (consed with '() to become a list) to a key (id) in the hash table
    (hash-set! db id (cons life empty))))
    

;;(hash-set! db 'lives (cons life empty))


;;The following function pregame population of rooms with lifes
(define (add-lives db)
  (for-each
    (lambda (r)
      ;; The following adds description (second r) to room id (first r)
      (add-object db (first r) (second r))) lives-in-the-room))



      #|-------------------|#
      #|     Game Loop     |#
      #|-------------------|#

;; The following loop is the main game loop. The id refers to the room in which the player is.
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t) (lives-in-the-room #t) (monsters-in-the-room #t))
    (if description
        ;;The following shows description on the screen if there is one available 
        (get-location id)
    (if lives-in-the-room
        ;;The following shows life on the screen if there is one available 
        (get-location id)
    (if monsters-in-the-room
        ;;The following shows Monsters on the screen if there is one
        (get-location id)
        ;; Next one is an else statement. Doesn't show location (because there isn't any description). Just shows the greater than symbol to incite user to type in text field
        (printf " > "))))
 
    
    ;; The next one reads input from the keyboard
    (let* ((input (read-line))
           ;; contained in the srfi/13 library, tokenize the input into substrings where a space character is found
           (string-tokens (string-tokenize input))
           ;; Creates a list of symbols (not strings) of the input. This is needed to compare the entry with our predefined lists
           (tokens (map string->symbol string-tokens)))
      ;; Next one decides which action response corresponds to. One of the most important calls in the code 
      (let ((response (lookup id tokens)))
        ;; (printf "Input: ~a\nTokens: ~a\nResponse: ~a\n" input tokens response)
        (cond ((number? response)
               (monster-hit id input)
               (loop response #t #t #t))
              ;; If response couldn't be found after the lookup function, shows this error message
              ((eq? #f response)
               (format #t "   - Pardon? I don't understand that! -\n")
               (loop id #f #f #f))


              ;; Add health
              ((eq? response 'add)
               ;; Add a life to your health
               (add-health id input)
               (loop id #f #f #f))

              ;; Response action is kill the monster
              ((eq? response 'kill)
               ;; Eliminate the monster
               (weapon id input)
               (loop id #f #f #f))
              ;; Response action is look around the room for directions
              ((eq? response 'look)
               ;; Retrieve possible directions
               (get-directions id)
               (loop id #f #f #f))
              ;; Response action is to pick an item
              ((eq? response 'pick)
               ;; Pick up item
               (pick-item id input)
               (loop id #f #f #f))
              ;; Response action is to drop an item
              ((eq? response 'drop)
               ;; Drop item
               (drop-item id input)
               (loop id #f #f #f))
              ;; Response action is to show inventory
              ((eq? response 'inventory)
               ;; Display the inventory
               (display-inventory)
               (loop id #f #f #f))
              ;; Response action is to show lives
              ((eq? response 'lives)
               ;; Display the lives
               (display-lives)
               (loop id #f #f #f))
              ;; Response action is to display the help file
              ((eq? response 'help)
                ;; Displays Help text on the screen
                (display-help)
                (loop id #f #f #f))
              ;; Response action is to display the map of the maze
              ((eq? response 'show)
                ;; Displays Map on the screen
                (display-show)
                (loop id #f #f #f))
              ;; Exit game command
              ((eq? response 'quit)
               ;; Exit the application
               (format #t "Hasta la vista, baby!\n")
               (exit)))))))

;; The following adds the objects to the database before the game starts
(add-objects objectdb)
;; The following adds the monsters to the database before the game starts
(add-monsters monsterdb)

;; The following adds "E" to bag in the inventory database before the game starts
(add-object inventorydb 'bag "E")
;; The following adds 1 f to 'lives (health of the user) in life database before the game starts
(add-life lifedb 'lives " 1 ")
;; The following adds the lifes in the rooms to the life database before the game starts
(add-lives lifedb)

;; Starts the game in the first room
(startgame 1)

