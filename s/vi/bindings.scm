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

(bind-insertion motion cleft 'left)
(bind-insertion motion cright 'right)
(bind-insertion motion cup 'up)
(bind-insertion motion cdown 'down)

(bind-insertion mutation delforward 'delete)
(bind-insertion mutation delbackward 'backspace)

(bind-higher-order-function til #\t)
(bind-higher-order-function find #\f)
