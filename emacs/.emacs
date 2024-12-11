(setq tetris-score-file "/tmp/tetris-scores")

(defun tetris-bot (moves)
	(tetris)
	(sit-for 1)
	(with-current-buffer (get-buffer "*Tetris*")
		(while t
			(dolist (move moves)
				(dotimes (+ 1 (random 10))
					(execute-kbd-macro (kbd move)))
				(sit-for (/ (+ .1 (random 10)) 10))
				(when (get-buffer "tetris-scores")
					(if (not window-system)
							(kill-emacs)
						(message "%s" "Tetris bot complete")))))))

(defun build-tetris-moves (buffer)
	(let ((moves '()))
		(with-current-buffer buffer
			(dotimes (i 100)
				(if (= 0 (mod i 10))
						(push "SPC" moves)
					(push
					 (char-code-to-tetris-move
						(or (char-after (+ 350 (random 2200))) 32))
					 moves)))
			)
		moves))

(defun char-code-to-tetris-move (char)
	(let ((mod-value (mod char 8)))
		(cond
		 ((= mod-value 0) "<down>")
		 ((< mod-value 3) "<up>")
		 ((< mod-value 6) "<right>")
		 (t "<left>"))))

(defun run-tetris-bot ()
	"Generate random moves and play tetris with them"
	(interactive)
	(let ((run-tetris-fn (lambda ()
												 (if-let* ((random-text-buffer (get-buffer "*eww*"))
																	 (moves (build-tetris-moves random-text-buffer)))
														 (progn 
															 (message "Random text buffer: %s" (get-buffer "*eww*"))
															 (sit-for 3)
															 (message "Moves: %s" moves)
															 (sit-for 5)
															 (tetris-bot moves))
													 (message "%s" "Random text buffer not found"))
												 (remove-hook 'eww-after-render-hook run-tetris-fn)
												 (message "%s" "Hook removed")
												 )))
		(add-hook 'eww-after-render-hook run-tetris-fn)
		(eww "https://randomtextgenerator.com")))

(run-tetris-bot)
