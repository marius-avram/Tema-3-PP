AVRAM Marius 324 CB
Tema 3 PP - CLIPS 

Initial am generat un orar aleator. Pentru fiecare item am creat un schedule-entry (care are toate sloturile unui item plus cateva sloturi aditionale) in care am pus numele camerei, ziua si ora. Dupa generearea acestui orar am luat fiecare componenta a orarului si am calculat cu diverse reguli fitnessul tinand cont de folosirea unei camere in acelasi interval orar, atribuirea unui ore in acelasi interval orar pentru un instructor, atribuirea unei ore in acealasi interval pentru aceasi grupa si am tinut cont de preferintele instructorului.

Dupa calcularea fitnessului verificam daca nu cumva este mai mare decat fitnesul maxim. Daca da atunci refacem faptele care retin orarul cel mai bun. Mutatiile se fac pe baza conflictelor de ore intre diverse grupe(acei itemi sunt cei modificati). Daca nu exista astfel de conflicte se face o mutatie aleatoare. Mutatia consta in generarea unei noi zile si ore. Am considerat-o ca fiind o optiune mult mai buna pentru ca interschimbarea de valori nu aducea schimbari importante la orar (din punct de vedere al conflictelor).

Mutatiile se fac intotdeauna din orarul cel mai bun. Pentru ca altfel orarul s-ar degenera prea mult si din nou dupa un numar foarte mare de iteratii nu s-ar ajunge nicaieri.

Afisarea se face la sfarsit, cand nr. iteratii = 0.

Tema merge oarecum bine cu 100 de iteratii. Insa si atunci mai apar mici conflicte intre grupe.