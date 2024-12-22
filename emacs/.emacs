;;; -*- lexical-binding: t -*-

(setq tetris-score-file "/tmp/tetris-scores")

(defun tetris-bot (moves)
	(tetris)
	(sit-for 1)
	(with-current-buffer (get-buffer "*Tetris*")
		(while t
			(dolist (move moves)
				(dotimes (+ 1 (random 20))
					(execute-kbd-macro (kbd move))
					(sit-for (/ (+ .1 (random 3)) 10)))
				(when (get-buffer "tetris-scores")
					(if (not window-system)
							(kill-emacs)
						(message "%s" "Tetris bot complete")))))))

(defun build-tetris-moves (buffer)
	(let ((moves '()))
		(with-current-buffer buffer
			(dotimes (i 100)
				(push
				 (char-code-to-tetris-move
					(or (char-after (+ 1 (random (- (point-max) 1))))
							(progn (message "%s" "No char found") 32)))
				 moves))
			)
		moves))

(defun generate-random-text-buffer (callback)
	(let ((build-text-fn nil))
		(setq build-text-fn (lambda ()
													(with-current-buffer (get-buffer "*eww*")
														(if-let ((start (progn
																							(goto-char (point-min))
																							(search-forward "dummy text in English")))
																		 (end (progn
																						(goto-char (point-max))
																						(search-backward "What does the Lorem Ipsum text mean"))))
																(progn
																	(copy-to-buffer (get-buffer-create "*tetris-text*") start end)
																	(funcall callback (get-buffer "*tetris-text*")))
															(message "%s" "Random text buffer not created")))
													(remove-hook 'eww-after-render-hook build-text-fn)))
		(add-hook 'eww-after-render-hook build-text-fn)
		(eww "https://randomtextgenerator.com")))

(defun char-code-to-tetris-move (char)
	(let ((mod-value (mod char 8)))
		(cond
		 ((= mod-value 0) "<down>")
		 ((= mod-value 1) "<up>")
		 ((< mod-value 5) "<right>")
		 (t "<left>"))))

(defun run-tetris-bot ()
	"Generate random moves and play tetris with them"
	(interactive)
	(generate-random-text-buffer (lambda (random-text-buffer)
																 (tetris-bot (build-tetris-moves random-text-buffer)))))

(run-tetris-bot)
