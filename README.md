# mfwt
map &amp; filter without temporary lists

## Usage
---
For example,

    (require mfwt)
    (mfwt (map add1 (filter odd? '(1 2 3))))

will be expanded to forms like

    (let ((p1775 add1) (l1779 '(1 2 3)) (p1778 odd?))
      (let all-loop ((s1780 l1779))
        (let failer1777 ((s1780 s1780))
          (if (null? s1780)
              '()
              (let ((me1781 (car s1780)))
                (if (or (null? s1780))
                    '()
                    (if (p1778 me1781)
                        (cons (p1775 me1781) (all-loop (cdr s1780)))
                        (failer1777 (cdr s1780)))))))))

which eliminates the temporary lists.


Currently, the start points supported are 

    map filter foldl foldr for-each andmap ormap

but their arguments can only be arbitrary combinations of *map*, *filter* and *range* forms, and other forms will be treated as normal lists and not be converted.

See more examples on tests.

## Notes
* The converted forms will not check whether all input lists have same lengths. It will stop recursion when encountering any end of inputs.
* If the procedures passed to *map* & *filter* have **side effects**, it may not behave the same as the original.