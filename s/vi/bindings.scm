(bind-motion cleft #\h)
(bind-motion cdown #\j)
(bind-motion cup #\k)
(bind-motion cright #\l)
(bind-motion eol #\$)
(bind-motion bol #\0)
(bind-motion bof #\g)
(bind-motion eof #\G)
(bind-motion word-forward #\w)
(bind-motion word-back #\b)

(bind-mutation delforward #\x)
(bind-mutation delbackward #\X)
(bind-mutation insert-mode #\i)

(bind-insertion motion cleft 'left)
(bind-insertion motion cright 'right)
(bind-insertion motion cup 'up)
(bind-insertion motion cdown 'down)

(bind-insertion mutation delforward 'delete)
(bind-insertion mutation delbackward 'backspace)
(LOW-create-binding 'insert 'tab "\t")
(bind-insertion mutation normal-mode 'escape)

(bind-higher-order-function til #\t)
(bind-higher-order-function find #\f)
(bind-higher-order-function til-back #\T)
(bind-higher-order-function find-back #\F)
(bind-higher-order-function delete #\d)
(bind-higher-order-function change #\c)
