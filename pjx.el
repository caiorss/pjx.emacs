;;; pjx.el --- Project management tool for Emacs.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2016  Caio Rodrigues Soares Silva

;; Filename: pjx.el 
;; Author: Caio Rodrigues       <caiorss DOT rodrigues AT gmail DOT com>
;; Maintainer: Caio Rordrigues  <caiorss DOT rodrigues AT gmail DOT com>
;; Keywords: project, automation, search, tool, management
;; Version: 0
;; URL: https://www.github.com/caiorss/pjx
;; Package-Requires: ((helm-core "2.0") (cl-lib "0.5"))
;; Description:  Project management tool for Emacs.


;;; Commentary

;; pjx - Provides commands to manipulate and manage software projects.
;; Features:
;;
;;  - Open a project in a new frame and assign the frame to the project name.
;;
;;  - C-x right (M-x next-buffer) and C-x left (M-x previous-buffer)
;;    switches only between project buffers. 
;;
;;  - Close project by killing all buffers that belongs to the project. 
;;
;;  - Run compilation command at project root directory.
;;  
;;  - Find files in projects
;;
;;  - Switch between project files
;;

;;; Licence

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code


(eval-when-compile (require 'cl))

;; (require 'cl)     ;; Provides common lisp emulation library.

(require 'subr-x) ;; Provides string-remove-suffix and other string functions.

(require 'dired-x) ;; Provides dired-omit-mode

;;; Refactor pjx project

;;; pjx Root directory
;;;
(setq pjx-root-directory "~/Documents/projects")

(setq pjx-current-project nil)


(setq pjx-ignore-suffix-list
      '( ".so" ".o" ".dll" ".exe" ".bin"
         ".jar" ".class" ".war" ".tar" ".tgz" ".pdf" ".zip" ".nupkg" ".pyc" ".elc"
         ".gif" ".png" ".jpg" ".jpeg" "~"
        ))

(setq pjx-ignore-prefix-list
      '("target/" "project/" "images/" "dist/" "bin/" "build/" "packages/" "obj/" "tmp/" "#"
        "sandbox/elpa"
        ))


;;; ============== Internal functions and helpers ========== ;;

(defun pjx--path-in-dir-p (root path)
  "Check if path is in root directory."
  (string-prefix-p (expand-file-name root) (expand-file-name path)))

(defun pjx--project-list ()
  "Returns all projects directories. See full doc."
  (mapcar (lambda (p) (cons (file-name-nondirectory p) p))
          (cdr (cdr (directory-files pjx-root-directory t)))))


(defun pjx--project-path (project-name)
  "Returns a path from a given project."
  (concat (file-name-as-directory pjx-root-directory) project-name))

(defun pjx--buffer-in-project-p (project-name buf)
  "Test if a buffer belongs to a project."
  (pjx--path-in-dir-p
   (pjx--project-path project-name)
   (with-current-buffer buf
                        (or (buffer-file-name)
                            default-directory))))



;; Select a project and call the functions callback
;; as (callback <project-path>) like (callback "~/Documents/projects/test-cpp")
;;
(defun pjx--project-open-callback (callback)
  ""
  (helm
   :prompt "Project: "
   :sources  `((
                (name       . "Pjx: ")
                (candidates . ,(pjx--project-list))
                (action     . (lambda (proj) (setq pjx-current-project proj)
                                             (funcall callback proj)))
                ))))

(defun pjx--get-buffers ()
  "Return all buffers which file name or default directory is in `pjx-root-directory`"
  (cl-remove-if-not (lambda (buf)
                      (pjx--path-in-dir-p pjx-root-directory
                                        (with-current-buffer buf
                                          (or (buffer-file-name)
                                              default-directory))))
                    (buffer-list)))


(defun pjx--get-opened-projects ()
  "Return a list with all opened projects."
  (mapcar (lambda (proj) (cons proj
                               (concat (file-name-as-directory pjx-root-directory)
                                       proj)))
          (remove-if-not (lambda (p) (and p (not (string-match-p "/" p))))
           (delete "."
              (delete-dups
               (mapcar (lambda (buf)
                         (string-remove-suffix
                          "/"
                          (file-name-directory
                           (file-relative-name (with-current-buffer buf
                                                 (or (buffer-file-name)
                                                     default-directory))
                                               pjx-root-directory)))

                         )
                       (pjx--get-buffers)))))))


(defun pjx--project-select-callback (callback)
  "Select a project with helm and pass its path to the callback function."
  (helm
   :prompt "Project: "
   :sources  `((
                (name       . "Pjx: ")
                (candidates . ,(pjx--get-opened-projects))
                (action     .  callback)
                ))))


(defun pjx--get-project-of-buffer ()
  "Get the project the current buffer is associated with."
  (car (remove-if-not (lambda (proj) (pjx--buffer-in-project-p (car proj) (current-buffer)))
                      (pjx--get-opened-projects))))


(defun pjx--get-project-buffers (project-name)
  "Returns all buffers that belongs to a project."
  (remove-if-not (lambda (buf) (pjx--buffer-in-project-p project-name buf))
                 (buffer-list)))

(defun pjx--filter-project-buffers (project-name predicate selector)
  "Filter all buffers that belongs to a project."
  (mapcar (lambda (buf) (cons (funcall selector buf) buf))
          (remove-if-not (lambda (buf)
                              (and (pjx--buffer-in-project-p project-name buf)
                                   (funcall predicate buf)))
                            (buffer-list))))

(defun pjx--get-project-buffers-files (project-name)
  "Returns all buffers that belongs to a project."
  (mapcar (lambda (buf)
            (cons (file-relative-name (buffer-file-name buf)
                                      (pjx--project-path project-name))
                  buf))
          (pjx--filter-project-buffers
           (lambda (buf) (and (buffer-file-name buf)
                              (pjx--buffer-in-project-p project-name buf))))))
          
(defun pjx--project-close (proj-name)
  "Close/kill all buffers belonging to a project."
  (mapc (lambda (buf)
             (with-current-buffer buf
                   ;; (save-buffer)
                   (kill-this-buffer)
                   ))
        (pjx--get-project-buffers proj-name)))


(defun pjx--find-files-by-regex (proj-name regex ignore-prefix-list ignore-suffix-list )
  (let ((path (pjx--project-path proj-name)))
    (remove-if (lambda (cell)
               (let ((s (car cell)))
                (or  (string-match-p "\\.git" s)
                    (some  (lambda (ext) (string-suffix-p ext s))
                           ignore-suffix-list)
                    (some  (lambda (pre) (string-prefix-p pre s))
                           ignore-prefix-list))))

               (mapcar (lambda (file) (cons (file-relative-name file path)
                                            file))
                       (directory-files-recursively path regex)))))

(defun pjx--find-project-files (proj ignore-prefix-list ignore-suffix-list)
  (pjx--find-files-by-regex proj
                            "."
                            ignore-prefix-list
                            ignore-suffix-list))


;;; ====================  User Commands ======================== ;;;

;;; =====> Help Commands

(defun pjx/help ()
  "Show pjx commands help."
  (interactive)
  (command-apropos "pjx/"))


;;; =====> Commands to Open Project

(defun pjx/root-dired ()
  "Open root project directory."
  (interactive)
  (dired pjx-root-directory)
  (dired-omit-mode)
  (dired-hide-details-mode))

(defun pjx/root-dired-frame ()
  "Open root project directory in a new frame."
  (interactive)
  (dired-other-frame pjx-root-directory))

(defun pjx/open ()
  "Select project directory and open it in dired-mode."
  (interactive)
  (pjx--project-open-callback (lambda (path)
                                (dired path)
                                (dired-omit-mode)
                                (dired-hide-details-mode)
                                )))


(defun pjx/frame-proj-files ()
  "Set the keybindings C-x right and C-x left switch between project buffers with files."
  (interactive)
  (set-frame-parameter
        nil
        'buffer-predicate
        (lambda (buf)
               ;; Test if buffer is associated to file or major mode is dired mode.
          (and (or (buffer-file-name buf) (equal 'dired-mode (buffer-local-value 'major-mode buf)))
               ;; Test if buffer is in project.
               (pjx--buffer-in-project-p (frame-parameter nil 'frame-project) buf)))))

(defun pjx/open-frame ()
  "Open project in a new frame."
  (interactive)
  (pjx--project-open-callback
   (lambda (path)
     (let ((proj (file-name-nondirectory path)))
       (dired-other-frame path)
       (dired-omit-mode)
       (dired-hide-details-mode)
       (set-frame-name (concat "Proj: "  proj))


        ;;; Set current project as frame project
       (set-frame-parameter nil 'frame-project proj)

       ;; C-x left or C-x right switches only between
       ;; buffer files.
       (pjx/frame-proj-files)
       ))))

;;; ****** Commands to close a project ********************** ;;


(defun pjx/project-close ()
  "Kill all buffers associated with a selected project."
  (interactive)
  (pjx--project-select-callback
   (lambda (path)
     (pjx--project-close
      (file-name-nondirectory path)))))

(defun pjx/close ()
  "Kill all buffers associated with a current project."
  (interactive)
  (pjx--project-close (car (pjx--get-project-of-buffer))))

(defun pjx/close-frame ()
  "Kill all buffers related to current project and close current project frame."
  (interactive)
  (pjx/close)
  (delete-frame))

;; **** Commands to switch between project directories ****** ;;


(defun pjx/project-switch-dir ()
  "Switch to project directory"
  (interactive)
  (pjx--project-select-callback #'dired))

(defun pjx/project-switch-dir-window ()
  "Switch to project directory in other window."
  (interactive)
  (pjx--project-select-callback #'dired-other-window))

(defun pjx/project-switch-dir-frame ()
  "Switch to project directory in other window."
  (interactive)
  (pjx--project-select-callback #'dired-other-frame))

;;; *** Commands for project navigation and file selection ***** ;; 


;;; Go to current project root directory
(defun pjx/top ()
  "Go to current project root directory."
  (interactive)
  (dired (cdr (pjx--get-project-of-buffer))))

(defun pjx/switch ()
  "Switch between buffers belonging to current project."
  (interactive)
  (helm
   :prompt "Project File: "
   :sources  `((
                (name       . "Proj:")
		
                (candidates . ,(mapcar (lambda (p) (cons (buffer-name p) p))
                                       (pjx--get-project-buffers
                                        (car (pjx--get-project-of-buffer)))))

                (action     . switch-to-buffer)
                ))))


(defun pjx/switch-file ()
  "Switch between buffers associated to files belonging to current project."
  (interactive)
  (helm
   :prompt "Project File: "
   :sources  `((
                (name       . "Proj:")
		
                (candidates . ,(pjx--filter-project-buffers (car (pjx--get-project-of-buffer))
                                                            #'buffer-file-name
                                                            (lambda (buf)
                                                              (file-relative-name (buffer-file-name buf)
                                                                                  (cdr (pjx--get-project-of-buffer))))))

                (action     . switch-to-buffer)
                ))))


(defun pjx/switch-dir ()
  "Switch between dired buffers within the current project."
  (interactive)
  (helm
   :prompt "Project File: "
   :sources  `((
                (name       . "Proj:")

                (candidates . ,(pjx--filter-project-buffers (car (pjx--get-project-of-buffer))
                                                            (lambda (buf) (equal 'dired-mode
                                                                                 (buffer-local-value 'major-mode buf)))
                                                            (lambda (buf) (file-relative-name
                                                                           (buffer-local-value 'default-directory buf)
                                                                           (cdr (pjx--get-project-of-buffer))
                                                                           ))))

                (action     . switch-to-buffer)
                ))))


(defun pjx/find-file ()
  "Find all project files recursively."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                (candidates . ,(pjx--find-project-files (car (pjx--get-project-of-buffer))
                                                        pjx-ignore-prefix-list
                                                        pjx-ignore-suffix-list
                                                        ))
                (action     .  find-file)
                ))))


(defun pjx/find-file-regex ()
  "Find all project files recursively."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                                
                (candidates . ,(pjx--find-files-by-regex (car (pjx--get-project-of-buffer))
                                                         (read-regexp "Pattern: ")
                                                         pjx-ignore-prefix-list
                                                         pjx-ignore-suffix-list))
                (action     .  find-file)
                ))))


;;; **** Commands to Build Project / Compile *******

(defun pjx/compile ()
  "Run compilation command at current project directory."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
    (call-interactively #'compile)))

(defun pjx/make ()
  "Run $ make at project root directory and execute Makefile main rule."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
    (compile "make ")))

(defun pjx/make-cmd ()
  "Run $ make at project root directory asking the user the make command to run."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
    (call-interactively #'compile "make ")))

(defun pjx/make-clean ()
  "Run '$ make clean' at project root directory and execute Makefile clean rule."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
    (compile "make clean")))

(defun pjx/make-again ()
  "Run '$ make clean && make' at project root directory and execute Makefile main rule."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
    (compile "make clean && make")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'pjx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF - END OF FILE 
