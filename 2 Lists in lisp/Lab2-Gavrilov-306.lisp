(defun count-int (a lst)
    (+ (if (consp (first lst)) ;проверка первого элемента
            (count-int a (first lst)) ;если дерево ветвится дальше, переход на уровень ниже
            (if (= (first lst) a);если на первом месте элемент, его проверка
                1 
                0))
        (if (consp (rest lst));поиск в хвосте списка
            (count-int a (rest lst)) 
            0)))