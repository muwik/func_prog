(defvar input_text '(Lorem Ipsum is simply dummy text of the printing in lorem industry. Lorem Ipsum has been industry's standard text ever since text)) ; Текст, що надається програмі

(defun len (l) ; Перевіряємо, що слово не є пустим (за довжиною)
  (if l
    (1+ (len (cdr l)))
    0))

(defun filter (s c) ; Фільтруємо слова, які повторюються, видаємо пари (слово - частота повторення)
    (cond ((equal c (car s)) (cons (car s) (filter (cdr s) c)))
          ((null s) nil)
          (t (filter (remove (car s) s) c))))

(defun counter (s) ; Рахуємо слова, що повторюються
    (cond ((null s) nil)
          (t (cons (cons (car s)
                         (cons (len (filter s (car s))) nil))
                   (counter (remove (car s) s))))))

(defun repeatedr (lst result) ; Виводимо три слова, що повторюються найбільше
  (if (null lst) ; Перевірка на пустий список
    result
    (if (member (first lst) (rest lst)) ; Підрахунок з використанням рекурсії
      (repeatedr (rest lst) (adjoin (first lst) result))
      (repeatedr (rest lst) result))))

(defun repeated (lst) ; Повертаємо список трьох найчастіших слів
  (repeatedr lst '()))

(print (counter input_text)) ; Викликаємо підрахунок частоти слів, що повторюються

(print (repeated input_text)) ; Викликаємо вивід трьох слів, що повторюються більше за всіх