# mfwt
map &amp; filter without temporary lists

## Usage
---
For example,

    (require mfwt)
    (mfwt (map add1 (filter odd? '(1 2 3))))

will be expanded to forms like

    (let ((p1909 add1) (l1913 '(1 2 3)) (p1912 odd?))
      (let all-loop ((s1914 l1913))
        (let failer1911 ((s1914 s1914))
          (if (null? s1914)
              '()
              (let ((me1915 (car s1914)))
                (if (p1912 me1915)
                    (cons (p1909 me1915) (all-loop (cdr s1914)))
                    (failer1911 (cdr s1914))))))))

which eliminates the temporary lists.


Currently, the start points supported are 

    map filter foldl foldr for-each andmap ormap

but their arguments can only be arbitrary combinations of *map*, *filter* and *range* forms, and other forms will be treated as normal lists and not be converted.

See more examples on tests.

## Notes
* The converted forms will not check whether all input lists have same lengths. It will stop recursion when encountering any end of inputs.
* If the procedures passed to *map* & *filter* have **side effects**, it may not behave the same as the original.