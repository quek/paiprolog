(ql:quickload :paiprolog)

(defpackage :test-papiprolog
  (:use :cl :paiprolog))

(in-package :test-papiprolog)

(<-- (member ?item (?item . ?rest)))
(<-  (member ?item (?x . ?rest)) (member ?item ?rest))

(<-- (length () 0))
(<-  (length (?x . ?y) (1+ ?n)) (length ?y ?n))

(<-- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<-  (nextto ?x ?y ?list) (iright ?y ?x ?list))
(<-- (iright ?left ?right (?left ?right . ?rest)))
(<-  (iright ?left ?right (?x . ?rest))
     (iright ?left ?right ?rest))

(<-- (zebra ?h ?w ?z)
     (= ?h ((house norwegian ? ? ? ?)
            ?
            (house ? ? ? milk ?) ? ?))
     (member (house englishman ? ? ? red) ?h)
     (member (house spaniard dog ? ? ?) ?h)
     (member (house ? ? ? coffee green) ?h)
     (member (house ukrainian ? ? tea ?) ?h)
     (iright (house ? ? ? ? ivory)
             (house ? ? ? ? green) ?h)
     (member (house ? snails winston ? ?) ?h)
     (member (house ? ? kools ? yellow) ?h)
     (nextto (house ? ? chesterfield ? ?)
             (house ? fox ? ? ?) ?h)
     (nextto (house ? ? kools ? ?)
             (house ? horse ? ? ?) ?h)
     (member (house ? ? luckystrike oj ?) ?h)
     (member (house japanese ? parliaments ? ?) ?h)
     (nextto (house norwegian ? ? ? ?)
             (house ? ? ? ? blue) ?h)
     (member (house ?w ? ? water ?) ?h)
     (member (house ?z zebra ? ? ?) ?h))

(assert (equal '((((HOUSE NORWEGIAN FOX KOOLS WATER YELLOW)
                   (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE)
                   (HOUSE ENGLISHMAN SNAILS WINSTON MILK RED)
                   (HOUSE SPANIARD DOG LUCKYSTRIKE OJ IVORY)
                   (HOUSE JAPANESE ZEBRA PARLIAMENTS COFFEE GREEN))
                  NORWEGIAN JAPANESE))
               (prolog-collect (?h ?w ?z)
                 (zebra ?h ?w ?z))))



(assert (equal '(10)
               (prolog-collect (?x)
                 (is 10 ?x))))

(assert (equal '(10)
               (prolog-collect (?x)
                 (is ?x 10))))






