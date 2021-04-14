(defun isspace (c) (if (char-position c " \t\n\r\f") #t #f))
