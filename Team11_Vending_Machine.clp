;;;################
;;;  Deftemplate
;;;################

(deftemplate current-value
	(slot value))
(deftemplate required-value
	(slot value))
	
;;;#########################
;;;  Function for asking
;;;#########################

(deffunction ask-question (?question $?allowed-values)
   (print ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (if (or (eq ?answer nickel) (eq ?answer Nickel))
			then (bind ?answer 5)))
   (if (lexemep ?answer) 
       then (if (or (eq ?answer quarter) (eq ?answer Quarter))
			then (bind ?answer 25)))
   (while (not (member$ ?answer ?allowed-values)) do
	  (println crlf "'" ?answer "' is not allowed. Please try again:" crlf)
      (print ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
	
;;;################
;;;    Deffacts
;;;################

; Fact to maintain the global state of the FSM
(deffacts vending
   (current-value (value 0))
   (required-value (value 0)))
   
;;;####################
;;;	     STARTUP
;;;####################

(defrule title ""
	(declare (salience 10))
	=>
	(println crlf "Soft Drink Vending Machine - Costs 55c" crlf))
	
;;;################
;;;  Query Rules
;;;################

(defrule ask-for-coin
	?f1 <- (current-value (value ?x))
	(test (< ?x 55))
	=>
	(bind ?sum (+ ?x (ask-question "Enter amount (quarter/nickel/25/5) :" quarter nickel 5 25 q n)) )
	(modify  ?f1 (value ?sum))
	(println crlf "The current amount is : " ?sum "c " crlf)
)

(defrule ask-item
	?f2 <- (required-value (value ?x))
	(test (eq ?x 0))
=>
	(printout t "Please select an item to purchase:	" crlf "(cola R8.50)" crlf "(orange R10.00)" crlf "(sweets R12.50)" crlf "(chocolate R15.00)" crlf)
	(bind ?item (read))
	(if (or (eq ?item cola) (eq ?item 8.50))
		then (bind ?item 8.50))
    (if (or (eq ?item orange) (eq ?item 10.00))
		then (bind ?item 10.00))
	(if (or (eq ?item sweets) (eq ?item 12.50))
		then (bind ?item 12.50))
	(if (or (eq ?item chocolate) (eq ?item 15.00))
		then (bind ?item 15.00))
	(modify ?f2 (value ?item)))


;;;################
;;;  Check Rules
;;;################	

(defrule checkA-money_counter 
	(current-value (value ?x))
	(required-value (value ?y))
	(test (eq ?x ?y ) )
=>	
	(println crlf "You have just enough money to buy a soft drink!!" crlf)


(defrule checkB-money_counter 
	(current-value (value ?x))
	(required-value (value ?y))
	(test (> ?x ?y) )
=>	
	(println crlf "You have enough money to buy a soft drink!!" crlf)
)	

   
