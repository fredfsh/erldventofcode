;;;  -*- lexical-binding: t -*-

;;; aoc.el -- Emacs utility for solving Advent of Code in Erlang.

;;; Commentary:
;;;   aoc-solve: load solution from file or create Erlang mod from template
;;;   aoc-compile: save and compile current solution
;;;   aoc-run: run last compiled solution

;;; Code:

(defvar aoc-root "~/AdventOfCode")

(defvar aoc-template "\
-module(template).

-export([start/0]).

start() ->
    Out = run(),
    io:format(\"~p~n\", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        N ->
            run_impl(Acc + do(N))
    end.

input() ->
    case io:fread(\"\", \"~d\") of
        eof ->
            eof;
        {ok, [N]} ->
            N
    end.

do(N) ->
    N.")

(defun max-year ()
  "Calculate latest year of AoC competition."
  (let* ((tail (cddddr (decode-time (current-time) "UTC-5")))
         (month (car tail))
         (year (cadr tail)))
    (if (eq month 12) year (1- year))))

(defvar aoc-ymin 2015)
(defvar aoc-ymax (max-year))
(defvar aoc-dmin 1)
(defvar aoc-dmax 25)

(defun validate (year day part)
  "Validate input on problem on DAY in YEAR, Part PART."
  (if (or (< year aoc-ymin) (> year aoc-ymax))
      (error "Year must be between %d and %d" aoc-ymin aoc-ymax)
    (if (or (< day aoc-dmin) (> day aoc-dmax))
        (error "Day must be between %d and %d" aoc-dmin aoc-dmax)
      (unless (or (eq part 1) (eq part 2))
        (error "Part must be either 1 or 2")))))

(defun from-first-part (filepath)
  "Create Erlang module content from Part 1 located at FILEPATH."
  (replace-regexp-in-string
   "-module(aoc_[[:digit:]]+_[[:digit:]]+_\\(1\\))[[:ascii:][:nonascii:]]*\\'"
   "2"
   (with-temp-buffer
     (insert-file-contents filepath)
     (buffer-string))
   t
   nil
   1))

(defun from-template (module)
  "Create Erlang module MODULE content from template."
  (replace-regexp-in-string "template" module aoc-template))

(defun aoc-solve (year day part)
  "Load solution or create an Erlang solution module from template.  \
YEAR and DAY are date of problem.  PART is either 1 or 2."
  (interactive "nYear: \nnDay: \nnPart: \n")
  (validate year day part)
  (let* ((module (format "aoc_%d_%d_%d" year day part))
         (filename (concat module ".erl"))
         (dir (expand-file-name (number-to-string year) aoc-root))
         (filepath (expand-file-name filename dir)))
    (mkdir dir t)
    (find-file filepath)
    (when (eq (buffer-size) 0)
      (let ((filepath1 (replace-regexp-in-string "2\\.erl" "1.erl" filepath))
            (case-fold-search nil))
        (if (and (eq part 2) (file-readable-p filepath1))
            (insert (from-first-part filepath1))
          (insert (from-template module)))))))

(defun aoc-compile ()
  "Save and compile current solution."
  (interactive)
  (save-buffer)
  (compile (concat "erlc -o /tmp " (buffer-file-name))))

(defun aoc-run ()
  "Run last compiled solution with input INPUT."
  (interactive)
  (let ((mod (file-name-base (buffer-file-name))))
    (switch-to-buffer (generate-new-buffer "input"))
    (message "Paste input and call save-buffer to run")
    (add-hook 'write-contents-functions
              (lambda()
                (shell-command-on-region
                 (point-min)
                 (point-max)
                 (format "erl -pa /tmp -noshell -run %s -run init stop" mod)))
              nil
              t)))

(provide 'aoc)
;;; aoc.el ends here
