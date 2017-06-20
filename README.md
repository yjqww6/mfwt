# mfwt
map &amp; filter without temporary lists

## Usage
---
For example,

    (require mfwt)
    (mfwt (map add1 (filter odd? '(1 2 3))))

will be expanded to forms like

    (let ((p1 add1) (l3 '(1 2 3)) (p7 odd?))
      (let all-loop ((s4 l3))
        (let failer6 ((s4 s4))
          (if (null? s4)
              '()
              (let ((me5 (car s4)))
                (if (p7 me5) 
                    (cons (p1 me5) (all-loop (cdr s4)))
                    (failer6 (cdr s4))))))))

which eliminates the temporary lists.


Currently, the start points supported are 

    map filter foldl foldr for-each andmap ormap

but their arguments can only be arbitrary combinations of *map*, *filter* and *range* forms, and other forms will be treated as normal lists and not be converted.

See more examples on tests.

## Notes
* The converted forms will not check whether all input lists have same lengths. It will stop recursion when encountering any end of inputs.
* If the procedures passed to *map* & *filter* have **side effects**, it may not behave the same as the original.