(defmodule MAIN (export ?ALL))

(deftemplate item (slot id) (slot name) (slot instructor) 
				  (slot duration) (multislot groups))
(deftemplate instructor (slot name) (multislot intervals))
(deftemplate schedule-entry (slot id) (slot name) (slot instructor) 
	(slot duration) (multislot groups) (slot room) (slot day)
	(slot shour))

(deffacts facts
	(max) (thehour) (theday) (randroom) (rooms) (items) (maxfitness 0)
	(randhour) (randday) (mutated 1) (ind1 1) (ind2 1)
	(teachAcc) (testedGroups) (ind3 -1) (days Mon Tue Wed Thu Fri))

;Variabila in care retinem fitnessul calculat in timpul unei iteratii
(defglobal MAIN ?*fit* = 0)


; Functie ce returneaza indicele fiecarei zile din saptamana.
; Se va folosi pentru comparatie zilelor dintr-o saptamana.
(deffunction getDayNo (?d)
	(if (eq ?d Mon) then 1
	else (if (eq ?d Tue) then 2
	else (if (eq ?d Wed) then 3
	else (if (eq ?d Thu) then 4
	else (if (eq ?d Fri) then 5))))))

; Returneaza in schimbul unui numar de la 0 la 5 
; ziua corespunzatoare din saptamana.
(deffunction getDayByNo (?n)
	(if (= ?n 1) then Mon
	else (if (= ?n 2) then Tue
	else (if (= ?n 3) then Wed
	else (if (= ?n 4) then Thu
	else (if (= ?n 0) then Fri))))))
	
; Functie ce determina daca exista conflicte de ora (intervale)
; ce se suprapun pentru parametri dati. ?s1 si ?s2 reprezinta 
; orele de start luate din schedule-entry si ?d1 ?d2 reprezinta 
; durata itemului respectiv. Va returna TRUE daca nu exista 
; conflicte si FALSE in cazul in care exista conflicte.
(deffunction noHourConflicts(?s1 ?d1 ?s2 ?d2)
	(or (and (< ?s1 ?s2) (<= (+ ?s1 ?d1) ?s2))
		(and (>= ?s1 (+ ?s2 ?d2)) (> (+ ?s1 ?d1) (+ ?s2 ?d2)))))
		


; Se incarca faptele din fisier
(defrule MAIN::init
	(not (loaded $?))
=>
	(focus MAXIM GENERATE)
	(load-facts "input.txt")
	(assert (loaded 1)))
	
; Se realizeaza un orar in care fiecarui item ii sunt atribuite 
; camere aleatorii si intervale orare aleatorii
(defrule MAIN::initial-schedule
	(iterations ?it)
	(initial ?init)
	(test (= ?it ?init))
	?f <- (item (id ?id) (name ?name) (instructor ?instructor) 
		  (duration ?duration) (groups $?groups))
	?g <- (randhour ?hour)
	?h <- (randday ?d)
	?i <- (randroom ?r)
=> 
	(retract ?f)
	(retract ?g)
	(retract ?h)
	(retract ?i)
	(assert (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
			(duration ?duration) (groups $?groups) (room ?r) (day ?d)
			(shour ?hour)))
	(assert (schedule-entry (id (- 0 ?id)) (name ?name) (instructor ?instructor)
			(duration ?duration) (groups $?groups) (room ?r) (day ?d)
			(shour ?hour))))

; Calculeaza fitnessul. Se acorda punctaj doar pentru cazurile in care 
; nu se suprapun itemi cu acelasi profesor sau in aceeasi sala.
(defrule MAIN::overlappingTeachersandRooms
	(schedule-entry (id ?id1) (name ?name1) (instructor ?instructor1) 
		  (duration ?duration1) (groups $?groups1) (room ?r1) (day ?d1)
		  (shour ?hour1))
	(schedule-entry (id ?id2) (name ?name2) (instructor ?instructor2)
		  (duration ?duration2) (groups $?groups2) (room ?r2) (day ?d2)
		  (shour ?hour2))
	(test (and (> ?id1 0) (> ?id2 0)))
	(max ?m) 
	?f <- (ind1 ?i) 
	?g <- (ind2 ?j)
	(test (= ?i ?id1))
	(test (= ?j ?id2))
=>
	; Setam indicii astfel incat sa se faca comparatii intre 
	; toti itemii.
	(if (= ?j (- ?m 1)) then 
			(retract ?g)
			(assert	(ind2 1))
			(retract ?f)
			(assert (ind1 (+ ?i 1)))
		else 
			(retract ?g)
			(assert (ind2 (+ ?j 1))))
	; Mai intai tratam cazul valid in care nu exista conflicte
	; pentru profesori.
	(if (not (and (eq ?instructor1 ?instructor2) 
		(not (noHourConflicts ?hour1 ?duration1 ?hour2 ?duration2))
		(eq ?d1 ?d2))) then 
			(bind ?*fit* (+ ?*fit* 5))
		; Daca exista conflicte retinem un fapt cu idul respectiv
		else (if (neq ?i ?j) then 
			  (assert (tConf ?id1))))
	; Tratam cazul in care nu exista conflicte pentru camere in 
	; acelasi interval orar.
	(if (not (and (eq ?r1 r2) 
		(not (noHourConflicts ?hour1 ?duration1 ?hour2 ?duration2))
		(eq ?d1 ?d2))) then 
			(bind ?*fit* (+ ?*fit* 5))
		; Daca exista conflicte retinem un fapt cu idul respectiv
		else (if (neq ?i ?j) then 
			  (assert (rConf ?id1))))
	; adaugam puncte din oficiu pentru toate grupele din toti itemi, 
	; urmand ca apoi aplicand alt fapt sa scadem din scorul de fitness 
	; pentru grupele unde exista conflicte de timp.
	(if (neq ?i ?j) then
		(bind ?*fit* (+ ?*fit* 10))))
		
; Creste fitnesul daca exista schedule-entry in care sunt respectate 
; doleantele instructorilor
(defrule MAIN::teacherPreferences 
	(instructor (name ?name) (intervals $? [ ?day ?start ?end ] $?))
	(schedule-entry (id ?id) (name ?namem) (instructor ?name) 
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))
	?f <- (teachAcc $?pref)
	(test (eq (member ?name (create$ $?pref)) FALSE))
	(test (and (>= ?hour ?start) (<= (+ ?hour ?duration) ?end)))
=>
	(retract ?f) 
	(assert (teachAcc $?pref ?name))
	(bind ?*fit* (+ ?*fit* 1)))
		
; Scade din fitnessul actual punctajul pentru acei itemi care contin 
; grupe in acelasi interval orar. Se scade pentru fiecare coincidenta
; dintre 2 itemi
(defrule MAIN::overlappingGroups
	(schedule-entry (id ?id1) (name ?name1) (instructor ?instructor1) 
		  (duration ?duration1) (groups $? ?group $?) (room ?r1) (day ?d)
		  (shour ?hour1))
	(schedule-entry (id ?id2) (name ?name2) (instructor ?instructor2)
		  (duration ?duration2) (groups $? ?group $?) (room ?r2) (day ?d)
		  (shour ?hour2))
	(test (and (> ?id1 0) (> ?id2 0)))
	(test (neq ?id1 ?id2))
	(not (gComp ?id1 ?id2))
=>
	(if (not (noHourConflicts ?hour1 ?duration1 ?hour2 ?duration2))
		then (bind ?*fit* (- ?*fit* 10))
			 ;(printout t "Conflicts: " ?hour1 " " ?duration1 " " ?hour2 " " ?duration2 crlf)
			 ; Daca exista conflicte retinem un fapt cu idul respectiv
			 (assert (gConf ?id1))
			 (assert (gComp ?id1 ?id2))))

(defrule MAIN::sanitize 
	(declare (salience -3))
	?f <- (replaced ?)
=> 
	(retract ?f))

; Va face focus pe NEXT_STEP unde se decide daca orarul curent
; este mai bun decat ce avem pana la momentul actual.
(defrule MAIN::isBetter
	(declare (salience -5))
	?f <- (mutated ?)
	(iterations ?it) 
	(test (> ?it 0))
=> 
	(focus NEXT_STEP)
	(retract ?f)
	;(printout t "Fitness" ?*fit* crlf)
	(assert (fitness  ?*fit*)))

; === Partea de afisare ===

;Adunam toti itemii intr-un fapt pe care ulterior il vom 
;folosi pentru a sorta dupa zi si ora orarul.
(defrule MAIN::grabItems
	(iterations ?it) 
	(test (= ?it 0)) ;<- o sa punem 0 
	(schedule-entry (id ?id) (day ?day) (shour ?hour))
	?f <- (ind3 ?i)
	(test (= ?i ?id))
	?g <- (items $?rest)
=> 
	(retract ?f)
	(retract ?g)
	(assert (items $?rest [ ?id ?day ?hour ]))
	(assert (ind3 (- ?i 1))))

;Sorteaza itemi dupa zi si ora
(defrule MAIN::sort
	(iterations ?it)
	(test (= ?it 0))
	?f <- (items $?h [ ?id1 ?day1 ?hour1 ] $?m [ ?id2 ?day2 ?hour2 ] $?t)
	(test (or (< (getDayNo ?day2) (getDayNo ?day1))
	(and (= (getDayNo ?day2) (getDayNo ?day1)) (< ?hour2 ?hour1))))
=>
	(retract ?f) 
	(assert (items $?h [ ?id2 ?day2 ?hour2 ] $?m [ ?id1 ?day1 ?hour1 ] $?t)))


(defrule MAIN::openfile
	(iterations ?it) 
	(test (= ?it 0)) 
	(not (opened ?))
=> 
	(open "out.txt" output "w")
	(printout output "| id | day | interval |  room  |  name  | instructor |   groups   |" crlf) 
	(assert (opened 1)))

(defrule MAIN::writeData 
	?f <- (opened ?o)
	?g <- (items [ ?id ?day ?hour ] $?rest)
	?h <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))
	
=> 
	(retract ?f)
	(retract ?g)
	(retract ?h)
	(assert (items $?rest))
	(assert (opened (+ ?o 1)))
	(assert (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour)))
	(printout output "  " (- 0 ?id) "   " ?day "    [" ?hour " " (+ ?hour ?duration) "]    " 
			  ?r "   " ?name "   " ?instructor "    " $?groups crlf))

(defrule MAIN::closeFile
	(max ?m)
	(opened ?m)
=> 
	(close))

; === Partea de generare orar aleator ===	
(defmodule GENERATE (import MAIN ?ALL))

(defrule GENERATE::randHour
	(declare (auto-focus TRUE))
	(not (randhour ?))
=> 
	(assert (randhour (random 8 17))))
	
; Urmatoarele 2 functii determina o zi aleatoare din saptamana.
; Mai intai se defineste un fapt cu indicele zilei aleatoare, 
; apoi se cauta in lista de zile acea zi si se aserteaza un fapt 
; cu ziua respectiva.
(defrule GENERATE::randomd-number
	(declare (auto-focus TRUE))
	(not (randday ?))
=>
	(assert (randdayn (random 0 4))))
	
(defrule GENERATE::randomDay
	(declare (auto-focus TRUE))
	(days $?head ?x $?)
	?f <- (randdayn ?randdayn)
	(test (= ?randdayn (length $?head)))
=> 
	(retract ?f)
	(assert (randday ?x)))
		

; Urmatoarele 2 functii determina o camera aleatoare din
; faptul rooms. Pentru a refolosi functiile se va retrage 
; faptul rooms si se aserta din nou cu vechea valoare
(defrule GENERATE::randomr-number
	(declare (auto-focus TRUE))
	(rooms $?all)
	(not (randroom ?))
	(test (not (= (length $?all) 0)))
=> 
	(assert (randroomn (random 0 (- (length $?all) 1)))))
 
(defrule GENERATE::random-room
	(declare (auto-focus TRUE))
	(rooms $?head ?x $?)
	?f <- (randroomn ?randroomn)
	(test (= ?randroomn (length $?head)))
=>
	(retract ?f)
	(assert (randroom ?x)))

; Se declanseaza la inceput pentru a retine numarul
; initial de iteratii
(defrule GENERATE::intialIterations
	(declare (auto-focus TRUE))
	(iterations ?it)
	(not (initial ?))
=>
	(assert (initial ?it)))

; Afla numarul maxim de itemuri din baza de fapte.
(defmodule MAXIM (import MAIN ?ALL))

(defrule MAXIM::initMax
	(not (max ?))
=>
	(assert (max 1)))

(defrule MAXIM::detMaxim
	(item (id ?id) (name ?name) (instructor ?instructor) 
		  (duration ?duration) (groups $?groups))
	?f <- (max ?m)
	(test (= ?id ?m))
=> 
	(retract ?f)
	(assert (max (+ ?m 1))))

; === Partea ce face pregatirile pentru urmatoare iteratie ===
(defmodule NEXT_STEP (import MAIN ?ALL))
 
(deffunction randomID(?id)
	(random 1 (- ?id 1)))

; Se sterge orarul anterior cel mai bun, 
; in cazul in care s-a gasit unul si mai bun.
(defrule NEXT_STEP::deleteOld
	(declare (salience 10))
	(not (mutated ?))
	(maxfitness ?m)
	(fitness ?fit)
	(test (> ?fit ?m))
	?f <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))
	(test (< ?id 0))
=>
	(assert (deleted 1))
	(retract ?f))

; Se inlocuieste fitnessul maxim.
(defrule NEXT_STEP::replaceFitness
	(declare (salience 7))
	?f <- (maxfitness ?m) 
	(fitness ?fit)
	(test (> ?fit ?m))
=> 
	(retract ?f)
	(assert (old-fitness ?fit))
	(assert (maxfitness ?fit)))

; Creeaza un nou cel mai bun orar.
(defrule NEXT_STEP::createNew
	(declare (salience 5))
	(not (mutated ?))
	(deleted ?)
	?f <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))
	(test (> ?id 0))
=>
	(assert (schedule-entry (id (- 0 ?id)) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))))

; Daca nu s-a sters nimic inainte vom prefera sa folosim cea mai buna mutatie 
; pentru a genera alte mutatii si nu mutatii "degenerate".
(defrule NEXT_STEP::retractOld
	(declare (salience 3))
	(not (deleted ?))
	(not (mutated ?))
	(not (replaced ?))
	?f <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
		  (shour ?hour))
	(test (> ?id 0))
=>
	(retract ?f))

; Se construieste baza pentru urmatoarea mutatie
(defrule NEXT_STEP::assertNew
	(declare (salience 2))
	(not (deleted ?))
	(not (mutated ?))
	?f <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
	  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
	  (shour ?hour))
	(test (< ?id 0))
=> 
	(assert (schedule-entry (id (* ?id -1)) (name ?name) (instructor ?instructor)
	  (duration ?duration) (groups $?groups) (room ?r) (day ?day)
	  (shour ?hour)))
	(assert (replaced 1)))
	
	

	
; Face o mutatie in cazul in care exista grupe care au ore 
; in acelasi interval orar.
(defrule NEXT_STEP::mutateGroupConflicts
	;(not (mutated ?))
	?f <- (gConf ?r)
	?g <- (schedule-entry (id ?r) (name ?name1) (instructor ?instructor1)
		(duration ?duration1) (groups $?groups1) (room ?r1) (day ?day1)
		(shour ?hour1))
=>
	(retract ?f)
	(retract ?g)
	(assert (mutated 1))
	; Se genereaza o noua zi si o noua ora aleatorie
	(assert (schedule-entry (id ?r) (name ?name1) (instructor ?instructor1)
		(duration ?duration1) (groups $?groups1) (room ?r1) (day (getDayByNo (random 0 4)))
		(shour (random 8 17)))))

; Face o mutatie aleatoare in cazul in care nu s-a facut nici o 
; mutatie in aceasta iteratie
(defrule NEXT_STEP::randomMutation
	(not (mutated ?))
	?g <- (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		(duration ?duration) (groups $?groups) (room ?r) (day ?day)
		(shour ?hour))
	(max ?m) 
	(test (= ?id (random 1 (- ?m 1))))
=> 
	(assert (mutated 1))
	(retract ?g)
	(assert (schedule-entry (id ?id) (name ?name) (instructor ?instructor)
		(duration ?duration) (groups $?groups) (room ?r) (day (getDayByNo (random 0 4)))
		(shour (random 8 17)))))

; Face pregatirile pentru pasul urmator. Sterge faptele 
; care "restrictioneaza" inceperea unei noi interatii.
(defrule NEXT_STEP::goAhead
	?f <- (ind1 ?i) 
	?g <- (ind2 ?j)
	?h <- (iterations ?it)
	?k <- (fitness ?fit)
	?l <- (teachAcc $?)
=> 
	(retract ?f) (retract ?g) (retract ?h)
	(retract ?k) (retract ?l) 
	(assert (teachAcc))
	(assert (ind1 1)) (assert (ind2 1))
    (assert (iterations (- ?it 1)))
	(bind ?*fit*))

(defrule NEXT_STEP::clean1
	?f <- (deleted ?)
=> 
	(retract ?f))
	
(defrule NEXT_STEP::clean2
	?f <- (gComp ? ?)
=> 
	(retract ?f))

	




