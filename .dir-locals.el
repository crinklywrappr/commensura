;;; Directory Local Variables — see (info "(emacs) Directory Variables").
;;;
;;; Make `cider-jack-in` use the :repl alias, so the convenience `user` namespace (repl/user.clj —
;;; all of commensura with short aliases + core :refer :all) auto-loads with no prefix-arg.
;;; Emacs asks once to mark `cider-clojure-cli-aliases` as a safe local variable — answer yes.

((nil . ((cider-clojure-cli-aliases . ":repl"))))
