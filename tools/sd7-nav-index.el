;;; tools/sd7-nav-index.el --- Seed7 navigation entity index reporter  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 12:07:11 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7-MODE package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Scans every Seed7 file in SEED7_DIR/prg/ (*.sd7) and SEED7_DIR/lib/ (*.s7i)
;; — the same spec list as `sd7-perf.el' — activates `seed7-mode' in each
;; buffer, calls `seed7-buffer-entities' to collect all named entities
;; (functions, procedures, structures, enumerations, arrays, sets), and writes
;; an RST report with one section per file.
;;
;; Each section contains a fixed-width RST grid table of entities sorted by
;; line number.  Fixed column widths keep table framing identical across runs,
;; so diffing two reports reveals only rows that changed.
;;
;; Typical workflow
;; ----------------
;;
;;   # baseline — before any change
;;   emacs -Q --batch --load tools/sd7-nav-index.el \
;;         -- ~/src/seed7 reports before-opt
;;
;;   # make a change to seed7-mode.el …
;;
;;   # after the change
;;   emacs -Q --batch --load tools/sd7-nav-index.el \
;;         -- ~/src/seed7 reports after-opt
;;
;;   # compare
;;   diff reports/seed7mode-nav-index-before-opt.rst \
;;        reports/seed7mode-nav-index-after-opt.rst
;;
;; Usage
;; -----
;;
;; 1. Interactive (preferred):
;;
;;      M-x sd7-nav-index-run
;;
;;    Prompts for SEED7_DIR, REPORT_DIR, and ID.
;;    Previous values are offered as defaults and remembered within the session.
;;
;; 2. Batch (command-line):
;;
;;      emacs -Q --batch --load tools/sd7-nav-index.el \
;;            -- SEED7_DIR REPORT_DIR ID
;;
;; 3. Makefile:
;;
;;      $(EMACS) -Q --batch --load tools/sd7-nav-index.el \
;;               -- $(SEED7_DIR) $(REPORT_DIR) $(ID)
;;
;; Arguments:
;;   SEED7_DIR  : path to the Seed7 source tree (must contain prg/ and lib/)
;;   REPORT_DIR : directory where the report file is written (created if absent)
;;   ID         : free-form report identifier (e.g. "01", "before-opt")
;;
;; Output:
;;   REPORT_DIR/seed7mode-nav-index-ID.rst

;;; --------------------------------------------------------------------------
;;; Load-path bootstrap
;;
;; Runs at both load time and byte-compile time.

(eval-and-compile
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (let* ((tools-dir (file-name-directory (expand-file-name this-file)))
             (root-dir  (expand-file-name ".." tools-dir)))
        (dolist (dir (list root-dir tools-dir))
          (unless (member dir load-path)
            (push dir load-path)))))))

(require 'seed7-mode)
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Repository root

(defvar sd7-nav-index--repo-root
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (expand-file-name
       ".."
       (file-name-directory (expand-file-name this-file)))))
  "Absolute path to the seed7-mode repository root.
Derived from the location of this file at load or byte-compile time.")

;;; --------------------------------------------------------------------------
;;; Remembered defaults (persist across invocations within a session)

(defvar sd7-nav-index--last-seed7-dir
  (expand-file-name "~/src/seed7")
  "Last Seed7 source directory used.
Offered as default in `sd7-nav-index-run'.")

(defvar sd7-nav-index--last-report-dir
  (when sd7-nav-index--repo-root
    (expand-file-name "reports" sd7-nav-index--repo-root))
  "Last report output directory.  Defaults to reports/ in the repo root.
Offered as default in `sd7-nav-index-run'.")

(defvar sd7-nav-index--last-id "01"
  "Last report ID used.  Offered as default in `sd7-nav-index-run'.")

;;; --------------------------------------------------------------------------
;;; Progress helpers

(defvar sd7-nav-index--current-file nil
  "File currently being indexed, or nil.")

(defun sd7-nav-index--message (fmt &rest args)
  "Emit a timestamped progress message."
  (let ((text (apply #'format fmt args)))
    (message "[%s] %s"
             (format-time-string "%H:%M:%S")
             text)))

;;; --------------------------------------------------------------------------
;;; Validation

(defun sd7-nav-index--validate-seed7-dir (seed7-dir)
  "Signal `user-error' if SEED7-DIR does not contain prg/ and lib/."
  (unless (file-directory-p seed7-dir)
    (user-error "Not a directory: %s" seed7-dir))
  (dolist (sub '("prg" "lib"))
    (unless (file-directory-p (expand-file-name sub seed7-dir))
      (user-error "Expected sub-directory '%s/' not found under: %s"
                  sub seed7-dir))))

;;; --------------------------------------------------------------------------
;;; File-name abbreviation  (same convention as sd7-perf.el)

(defun sd7-nav-index--abbrev (file-name)
  "Strip the leading path prefix, keeping from the Seed7 dir name onward.

Example:
  /home/user/src/seed7/prg/chkint.sd7  →  seed7/prg/chkint.sd7
  /opt/seed7/lib/string.s7i             →  seed7/lib/string.s7i"
  (replace-regexp-in-string
   "^.*/\\([^/]+/\\(?:prg\\|lib\\)/\\)" "\\1" file-name))

;;; --------------------------------------------------------------------------
;;; Fixed RST table column widths
;;
;; Keeping widths fixed across runs ensures that table framing is byte-for-byte
;; identical; `diff' then shows only changed data rows, not reflowed framing.

(defconst sd7-nav-index--col-line  6  "Width of the Line column (digits).")
(defconst sd7-nav-index--col-type 11  "Width of the Type column ('enumeration' = 11).")
(defconst sd7-nav-index--col-name 50  "Width of the Name column.")

;;; --------------------------------------------------------------------------
;;; RST simple-table helpers
;;
;; Produces the compact RST "simple table" format:
;;
;;   ======== ============= ====================================================
;;    Line     Type          Name
;;   ======== ============= ====================================================
;;   18        procedure     checkInt
;;   42        function      readInteger
;;   ======== ============= ====================================================
;;
;; All three rule lines are identical (all `='), so table framing is byte-for-byte
;; the same across runs — `diff' shows only changed data rows.

(defun sd7-nav-index--rst-rule ()
  "Return the RST simple-table rule line (used for top, header-sep, and bottom)."
  (format "%s %s %s"
          (make-string sd7-nav-index--col-line ?=)
          (make-string sd7-nav-index--col-type ?=)
          (make-string sd7-nav-index--col-name ?=)))

(defun sd7-nav-index--rst-header-row ()
  "Return the simple-table header row string."
  (format (format " %%-%ds %%-%ds %%-%ds"
                  sd7-nav-index--col-line
                  sd7-nav-index--col-type
                  sd7-nav-index--col-name)
          "Line" "Type" "Name"))

(defun sd7-nav-index--rst-data-row (line-no type name)
  "Return one RST simple-table data row string."
  (format (format "%%-%dd %%-%ds %%-%ds"
                  sd7-nav-index--col-line
                  sd7-nav-index--col-type
                  sd7-nav-index--col-name)
          line-no type (format "``%s``" name)))

(defun sd7-nav-index--rst-table (entities)
  "Return an RST simple table string for ENTITIES ((TYPE NAME LINE) ...).
Returns the string \"(none)\" when ENTITIES is nil."
  (if (null entities)
      "(none)\n"
    (let* ((rule (sd7-nav-index--rst-rule))
           (rows (list rule
                       (sd7-nav-index--rst-header-row)
                       rule)))
      (dolist (e entities)
        (push (sd7-nav-index--rst-data-row
               (nth 2 e) (symbol-name (nth 0 e)) (nth 1 e))
              rows))
      (push rule rows)
      (mapconcat #'identity (nreverse rows) "\n"))))

;;; --------------------------------------------------------------------------
;;; Per-file processing

(defun sd7-nav-index--process-file (file-path seed7-dir)
  "Load FILE-PATH, activate `seed7-mode', collect entities.

Returns a plist:
  :abbrev     abbreviated file name (relative to the Seed7 dir name)
  :rel        path relative to SEED7-DIR
  :lines      line count
  :entities   list from `seed7-buffer-entities'
  :error      error string, or nil on success"
  (let* ((abbrev  (sd7-nav-index--abbrev file-path))
         (rel     (file-relative-name file-path seed7-dir))
         line-count entities err-msg)
    (condition-case err
        (let ((buf (find-file-noselect file-path t)))
          (with-current-buffer buf
            (unless (eq major-mode 'seed7-mode)
              (seed7-mode))
            (setq line-count (line-number-at-pos (point-max))
                  entities   (seed7-buffer-entities)))
          (kill-buffer buf))
      (error
       (setq err-msg (format "%s" (cadr err)))))
    (list :abbrev   abbrev
          :rel      rel
          :lines    (or line-count 0)
          :entities (or entities '())
          :error    err-msg)))

;;; --------------------------------------------------------------------------
;;; Core runner

(defun sd7-nav-index-run-core (seed7-dir report-dir id)
  "Index all Seed7 entities in SEED7-DIR and write an RST report.

SEED7-DIR  : Seed7 source tree root (must contain prg/ and lib/).
REPORT-DIR : directory where the RST report is written (created if absent).
ID         : free-form identifier used verbatim in the output filename.

Files are processed in spec-list order:
  prg/*.sd7  (alphabetical), then  lib/*.s7i  (alphabetical)
— the same order as sd7-perf.el.

Returns the absolute path of the written report file."
  (setq seed7-dir  (expand-file-name seed7-dir)
        report-dir (if (file-name-absolute-p report-dir)
                       (expand-file-name report-dir)
                     (expand-file-name report-dir
                                       (or sd7-nav-index--repo-root
                                           default-directory))))
  (sd7-nav-index--validate-seed7-dir seed7-dir)
  (unless (and (stringp id) (not (string-empty-p id)))
    (user-error "ID must be a non-empty string, got: %S" id))

  (let* ((dir-specs        (list (list (expand-file-name "prg" seed7-dir) "sd7")
                                 (list (expand-file-name "lib" seed7-dir) "s7i")))
         (out-file         (expand-file-name
                            (format "seed7mode-nav-index-%s.rst" id)
                            report-dir))
         (timestamp        (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (total-start      (float-time))
         (file-count       0)
         (error-count      0)
         ;; results-by-spec : list of (SPEC-NAME . FILE-RESULTS-LIST)
         ;; Built up per spec-group so the RST writer can emit group headings.
         results-by-spec
         ;; results : flat list of all file plists (for statistics/errors).
         results)

    (make-directory report-dir t)

    (sd7-nav-index--message "Seed7 Navigation Entity Index")
    (sd7-nav-index--message "seed7-mode  : %s" seed7-mode-version-timestamp)
    (sd7-nav-index--message "Seed7 dir   : %s" seed7-dir)
    (sd7-nav-index--message "Report dir  : %s" report-dir)
    (sd7-nav-index--message "Report ID   : %s" id)

    ;; ── Collect entities per spec-group ────────────────────────────────────
    (dolist (spec dir-specs)
      (let* ((dir       (car spec))
             (ext       (cadr spec))
             (spec-name (file-name-nondirectory (directory-file-name dir)))
             (pat       (concat "\\." (regexp-quote ext) "\\'"))
             spec-results)
        (dolist (f (directory-files dir t pat))
          (setq sd7-nav-index--current-file f)
          (cl-incf file-count)
          (sd7-nav-index--message "[%d] %s" file-count
                                   (sd7-nav-index--abbrev f))
          (let ((r (sd7-nav-index--process-file f seed7-dir)))
            (when (plist-get r :error)
              (cl-incf error-count)
              (sd7-nav-index--message "  ERROR: %s" (plist-get r :error)))
            (push r spec-results)))
        (push (cons spec-name (nreverse spec-results)) results-by-spec)))

    (setq results-by-spec (nreverse results-by-spec))
    ;; Flat list derived from the grouped results — used for stats/errors below.
    (setq results (apply #'append (mapcar #'cdr results-by-spec)))

    (let ((total-elapsed (- (float-time) total-start)))
      (sd7-nav-index--message
       "Indexed %d file(s) in %.1f s (%d error(s))"
       file-count total-elapsed error-count)

      ;; ── Write RST report ──────────────────────────────────────────────────
      (with-temp-file out-file

        ;; ---- Document title ----
        (let* ((title "Seed7 Navigation Entity Index")
               (rule  (make-string (length title) ?=)))
          (insert rule "\n" title "\n" rule "\n\n"))

        ;; ---- Metadata ----
        (insert (format "- Running with: seed7-mode %s\n"
                        seed7-mode-version-timestamp))
        (insert (format "- Seed7 dir: %s\n"
                        (file-name-as-directory seed7-dir)))
        (insert (format "- Generated on: %s\n" timestamp))
        (insert (format "- Files indexed: %d\n" file-count))
        (when (> error-count 0)
          (insert (format "- Files errored: %d\n" error-count)))
        (insert "\n")
        (insert ".. sectnum::\n")
        (insert "   :depth: 2\n\n")
        (insert ".. contents:: Table of Contents\n")
        (insert "   :depth: 2\n\n")

        (insert ".. ---------------------------------------------------------------------------\n\n")

        ;; ---- One RST section per spec-group (prg / lib),
        ;;      then one subsection per file (- underline) ----
        (dolist (spec-group results-by-spec)
          (let* ((spec-name      (car spec-group))
                 (spec-underline (make-string (length spec-name) ?=)))
            ;; Group heading: "prg\n===" or "lib\n==="
            (insert spec-name "\n" spec-underline "\n\n"))
          (dolist (r (cdr spec-group))
            (let* ((rel       (plist-get r :rel))
                   (lines     (plist-get r :lines))
                   (entities  (plist-get r :entities))
                   (err       (plist-get r :error))
                   (underline (make-string (length rel) ?-)))
              ;; File subsection heading (one level below group with `-')
              (insert rel "\n" underline "\n\n")
              (if err
                  (progn
                    (insert (format ":Lines:    %d\n" lines))
                    (insert (format ":Error:    %s\n\n" err)))
                (insert (format ":Lines:    %d\n" lines))
                (insert (format ":Entities: %d\n\n" (length entities)))
                (insert (sd7-nav-index--rst-table entities))
                (insert "\n\n")))))

        ;; ---- Files with errors (only emitted when at least one file failed) ----
        (let ((errored (cl-remove-if-not
                        (lambda (r) (plist-get r :error))
                        results)))
          (when errored
            (insert "Files with Errors\n")
            (insert (make-string (length "Files with Errors") ?=) "\n\n")
            (dolist (r errored)
              (insert (format "- ``%s``: %s\n"
                              (plist-get r :rel)
                              (plist-get r :error))))
            (insert "\n")))

        ;; ---- Document terminator ----
        (insert "\n.. ---------------------------------------------------------------------------\n"))

      (sd7-nav-index--message "Report written to: %s" out-file)
      out-file)))

;;; --------------------------------------------------------------------------
;;; Interactive entry point

;;;###autoload
(defun sd7-nav-index-run (seed7-dir report-dir id)
  "Interactively index Seed7 entities and write an RST report.

Prompts for SEED7-DIR, REPORT-DIR, and ID.  Previous values are offered as
defaults and remembered within the session.

Report written to: REPORT-DIR/seed7mode-nav-index-ID.rst"
  (interactive
   (list
    (read-directory-name "Seed7 source directory: "
                         sd7-nav-index--last-seed7-dir nil nil)
    (read-directory-name "Report output directory: "
                         sd7-nav-index--last-report-dir nil nil)
    (read-string (format "Report ID (default %S): " sd7-nav-index--last-id)
                 nil nil sd7-nav-index--last-id)))
  (setq sd7-nav-index--last-seed7-dir  (directory-file-name
                                         (expand-file-name seed7-dir))
        sd7-nav-index--last-report-dir (directory-file-name
                                         (expand-file-name report-dir))
        sd7-nav-index--last-id         id)
  (sd7-nav-index-run-core seed7-dir report-dir id))

;;; --------------------------------------------------------------------------
;;; CLI entry point

(defun sd7-nav-index--cli-help-and-exit (exit-code)
  "Print usage to *Messages* and exit with EXIT-CODE."
  (message "\
Usage (batch):
  emacs -Q --batch --load tools/sd7-nav-index.el \\
        -- SEED7_DIR REPORT_DIR ID

  SEED7_DIR   root of the Seed7 source tree (must contain prg/ and lib/)
  REPORT_DIR  directory where the RST report is written
  ID          report identifier string (e.g.  01  or  before-opt)

Output: REPORT_DIR/seed7mode-nav-index-ID.rst

Diff workflow:
  # baseline
  emacs -Q --batch --load tools/sd7-nav-index.el \\
        -- ~/src/seed7 reports before-opt
  # after a change to seed7-mode.el
  emacs -Q --batch --load tools/sd7-nav-index.el \\
        -- ~/src/seed7 reports after-opt
  # compare
  diff reports/seed7mode-nav-index-before-opt.rst \\
       reports/seed7mode-nav-index-after-opt.rst")
  (kill-emacs exit-code))

(defun sd7-nav-index-main ()
  "Parse `command-line-args-left' and call `sd7-nav-index-run-core'.
CLI / Makefile entry point.  Exits Emacs with 0 on success or 1 on error."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (when (equal (car args) "--")
      (setq args (cdr args)))
    (when (< (length args) 3)
      (sd7-nav-index--cli-help-and-exit 1))
    (let* ((seed7-dir  (nth 0 args))
           (report-dir (nth 1 args))
           (id         (nth 2 args)))
      (when (string-empty-p id)
        (message "ERROR: ID must be a non-empty string")
        (kill-emacs 1))
      (condition-case err
          (progn
            (sd7-nav-index-run-core seed7-dir report-dir id)
            (kill-emacs 0))
        (user-error
         (message "ERROR: %s" (cadr err))
         (kill-emacs 1))))))

;;; --------------------------------------------------------------------------
;;; Auto-dispatch (mirrors sd7-perf.el: batch fires automatically)

(when noninteractive
  (sd7-nav-index-main))

;;; --------------------------------------------------------------------------
(provide 'sd7-nav-index)

;;; sd7-nav-index.el ends here
