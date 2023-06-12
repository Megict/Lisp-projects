(defun russian-upper-case-p (char)
  (position char "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))

(defun russian-char-downcase (char)
  (let ((i (russian-upper-case-p char)))
    (if i 
      (char "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" i)
      (char-downcase char)))); латиница

(defun russian-string-downcase (string)
  ;; Преобразовать и латинские, и русские буквы строки в строчные
  (map 'string #'russian-char-downcase string))

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  (loop with len = (length string)
    for left = 0 then (1+ right)
    for right = (or (position-if #'whitespace-char-p string :start left) len)
    unless (= right left)	; исключить пустые слова
      collect (subseq string left right)
      while (< right len)))


(defun remove-punctuation (list-text)
  ;;удаление пунктуации из списка слов текста, разбитого по предложениям
  ;;и приведение его слов к нижнему регистру
  (loop 
    for sentence in list-text
    collect (loop 
      for word in sentence
        collect (string (string-right-trim ",.;:?!" word)))))

(defun str-eql (lhs rhs) 
  (equal (russian-string-downcase lhs) (russian-string-downcase rhs)))

(defun rm (elm list)
  ;;удаление элемента elm из списка
  (cond
    ((null list ) nil)
    ((str-eql elm (first list)) (rm elm (rest list)))
    (t (cons (first list) (rm elm (rest list))))))

 (defun rm-rpt (list)
  ;;удаление поторяющихся элементов из списка
  (if  (null list) 
    nil
    (cons (first list) (rm-rpt (rm (first list) (rest list))))))

(defun unite (list-text) 
  ;;объединение списка списков в один список
  (if (null list-text) 
    nil
    (append (first list-text) (unite (rest list-text)))))


(defun collect-words-with-char (ch text)
  ;;перевод текста в список неповторяющихся слов в нижнем регистре
  ;;затем для каждого слова из списка проверяется соответствие первой или последней буквы заданному ch
  (loop 
    for word in (rm-rpt (unite  (remove-punctuation (mapcar #'(lambda (x) (word-list x)) text)) ))
    when (or (eql (russian-char-downcase (char word 0)) (russian-char-downcase ch)) (eql  (russian-char-downcase (char (reverse word) 0)) (russian-char-downcase ch)))
    collect word))