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

;; ********************  D E P E N D E N C I E S *********************** ;;

(eval-when-compile (require 'cl))

;; (require 'cl)     ;; Provides common lisp emulation library.

(require 'subr-x) ;; Provides string-remove-suffix and other string functions.

(require 'dired-x) ;; Provides dired-omit-mode

;;; Refactor pjx project

;; ******************** C U S T O M I Z A T I O N S *********************** ;;


(defgroup pjx nil
  "Pjx - Project Management Tool Settings"
  :group 'tools
  )

(defcustom pjx-root-directory "~/Documents/projects"
  "Pjx directory where all projects are stored.
Default value '~/Documents/projects'."
  :type  'directory
  :group 'pjx
  )

(setq pjx-ignore-suffix-list
      '( ".so" ".o" ".dll" ".exe" ".bin"
         ".jar" ".class" ".war" ".tar" ".tgz" ".pdf" ".zip" ".nupkg" ".pyc" ".elc"
         ".gif" ".png" ".jpg" ".jpeg" "~"
        ))

(setq pjx-ignore-prefix-list
      '("target/" "project/" "images/" "dist/" "bin/" "build/" "packages/" "obj/" "tmp/" "#"
        "sandbox/elpa"
        ))

(setq pjx-files-exclude-list
      '("*.git*"  "#*.*#"  "*.class" "*.png" "*.gif" "*.jpg" "*.jpeg"
        "images/*" "*project/target/*" "*/.stack-work/*" "*.so.*" "*.so"
        "*.o" "*.dll" "*.exe" "*/dist/*" "*.bin" "*.tar" "*.jar" "*.tar.gz" "*.tgz" "*.tar.xz"
        "*.hi"
        ))


;;; ============== Internal functions and helpers ========== ;;
;; ******************** I N T E R N A L  -  F U N C T I O N S  ********************** ;;

(defun pjx--path-in-dir-p (root path)
  "Check if path is in root directory."
  (string-prefix-p (expand-file-name root) (expand-file-name path)))

(defun pjx--project-list ()
  "Returns all projects directories. See full doc."
  (mapcar (lambda (p) (cons (file-name-nondirectory p) p))
          (cdr (cdr (directory-files pjx-root-directory t)))))

;;
;;
;; ELISP> (pjx--project-path "zhserver.haskell")
;; "~/Documents/projects/zhserver.haskell"
;;
;; ELISP> (pjx--project-path "pjx.emacs")
;; "~/Documents/projects/pjx.emacs"
;; ELISP>
;;
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
;; Example:
;;
;; ELISP> (pjx--get-project-of-buffer)
;; ("pjx.emacs" . "~/Documents/projects/pjx.emacs")
;;
(defun pjx--get-project-of-buffer ()
  "Get the project that the current buffer is associated with."
  (car (remove-if-not (lambda (proj)
                        (pjx--buffer-in-project-p (car proj)
                                                  (current-buffer)))
                      (pjx--get-opened-projects))))


(defun pjx--get-project-name ()
  "Get the name of current project"
  (car (pjx--get-project-of-buffer)))

(defun pjx--get-project-path ()
  "Get current project absolute path."
  (cdr (pjx--get-project-of-buffer)))
  

;;
;; ELISP> (pjx--get-project-buffers "pjx.emacs")
;; (#<buffer *ielm*> #<buffer pjx.el> #<buffer  *Minibuf-1*> #<buffer pjx-backup.el> ....)
;;
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


(defun pjx--find-files-subdirs (directory exclude)
  "Find files using $ find command in subdirectories."
  (mapcar (lambda (file) (cons (file-relative-name file directory) file))
          (split-string (shell-command-to-string
                         (concat  (format "find '%s' -type f " (expand-file-name directory))
                                  (mapconcat  (lambda (p) (format "! -path '%s'" p))
                                              exclude
                                              " "
                                              )))
                        "\n"
                        t
                        )))


(defun pjx--find-files-subdirs-ext (directory extensions exclude)
  "Find all files in current directory and subdirectories with a given set of extensions."
  (remove-if-not (lambda (cell)
                   (some (lambda (ext) (string-suffix-p ext (cdr cell)))
                                      extensions))
                 (pjx--find-files-subdirs directory exclude)))


(defun pjx--find-files-by-regex (proj-name regex ignore-prefix-list ignore-suffix-list )
  (let ((path (pjx--project-path proj-name)))
    (remove-if (lambda (cell)
                 (let ((s (car cell)))
                   (or   ;; Exclude common directories and binary files
                        (string-match-p "\\.git"  s)         ;; Exclude any file in .git/ directory
                        (string-match-p "\\.so.*" s)         ;; Exclude libm.so.2, libm.so  ...
                        (string-match-p "\\.dll" s)
                        (string-match-p "\\.exe" s)
                        (string-match-p "\\.class" s)
                        (string-match-p "\\.war" s)
                        (string-match-p "\\.jar" s)
                        (string-match-p "\\.bin" s)
                        (string-match-p "\\.sqlite" s)
                        (string-match-p "\\.hi" s)
                        (string-match-p "\\.o" s)

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


(defun pjx--open-frame (proj)
  "Open project in a new frame and make it exclusive to the project root directory."
  (let* ((path (pjx--project-path proj)))

    (dired-other-frame path)
    (dired-omit-mode)
    (dired-hide-details-mode)
    (set-frame-name (concat "Proj: "  proj))
       ;;; Set current project as frame project
    (set-frame-parameter nil 'frame-project proj)
    ;; C-x left or C-x right switches only between
    ;; buffer files.
    (pjx/frame-proj-files)
    ))

;;; ====================  User Commands ======================== ;;;

;;; =====> Help Commands

(defun pjx/help ()
  "Show pjx commands help."
  (interactive)
  (command-apropos "pjx/"))

(defun pjx/customize ()
  "Open customization panel for pjx tool."
  (interactive)
  (customize-group "pjx"))

(defun pjx/new-project (&optional project-namep)
  "Create a new project directory at `pjx-root-directory` and open it."
  (interactive)
  (let* ((default-directory pjx-root-directory)
         (project-name      (if project-namep
                                project-namep
                                (read-string "Project name: "))))
    (unless (file-exists-p project-name)
      (make-directory project-name))
    (dired project-name)))

(defun pjx/new-project-frame ()
  "Create a new project directory at `pjx-root-directory` and open it in a new frame."
  (interactive)
  (let* ((default-directory pjx-root-directory)
         (project-name      (read-string "Project name: ")))
    (unless (file-exists-p project-name)
      (make-directory project-name))
    (pjx--open-frame project-name)))


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
   (lambda (path) (pjx--open-frame (file-name-nondirectory path)))))

;;; ****** Commands to close a project ********************** ;;


(defun pjx/project-close ()
  "Kill all buffers associated with a selected project."
  (interactive)
  (pjx--project-select-callback
   (lambda (path)
     (pjx--project-close
      (file-name-nondirectory path)))))



(defun pjx/close-files ()
  "Close all current project buffers without close current project."
  (interactive)
  (mapc (lambda (buf)
          (if (buffer-file-name buf)
              (with-current-buffer buf
                   (save-buffer)
                   (kill-this-buffer)
                   )))                
       (pjx--get-project-buffers  (pjx--get-project-name))))

(defun pjx/close ()
  "Kill all buffers associated with a current project."
  (interactive)
  (pjx--project-close (pjx--get-project-name)))

(defun pjx/close-frame ()
  "Kill all buffers related to current project and close current project frame."
  (interactive)
  (pjx/close)
  (delete-frame))

(defun pjx/close-all ()
  "Close all projects"
  (interactive)
  (mapc #'pjx--project-close
        (mapcar #'car (pjx--get-opened-projects))))


;; **** Commands to switch between project directories ****** ;;


(defun pjx/project-switch ()
  "Switch to project directory"
  (interactive)
  (pjx--project-select-callback #'dired))

(defun pjx/project-switch-window ()
  "Switch to project directory in other window."
  (interactive)
  (pjx--project-select-callback #'dired-other-window))

(defun pjx/project-switch-frame ()
  "Switch to project directory in other window."
  (interactive)
  (pjx--project-select-callback #'dired-other-frame))

;;; *** Commands for project navigation and file selection ***** ;; 


;;; Go to current project root directory
(defun pjx/top ()
  "Go to current project root directory."
  (interactive)
  (dired (pjx--get-project-path)))

(defun pjx/switch ()
  "Switch between buffers belonging to current project."
  (interactive)
  (helm
   :prompt "Project File: "
   :sources  `((
                (name       . "Proj:")
		
                (candidates . ,(mapcar (lambda (p) (cons (buffer-name p) p))
                                       (pjx--get-project-buffers
                                        (pjx--get-project-name))))

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
                                                              (file-relative-name
                                                               (buffer-file-name buf)
                                                               (pjx--get-project-path)))))

                (action     . switch-to-buffer)
                ))))


(defun pjx/switch-file-frame ()
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
                                                                                  (pjx--get-project-path)))))

                (action     . (lambda (buf) (with-selected-frame (make-frame)
                                               (switch-to-buffer buf))))
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
                                                                           (pjx--get-project-path)
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
                (candidates . ,(pjx--find-files-subdirs
                                 (pjx--get-project-path)
                                 pjx-files-exclude-list))
                (action     .  find-file)
                ))))

(defun pjx/find-file-frame ()
  "Find file in project subdirectories and opens it in a new frame."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                (candidates . ,(pjx--find-files-subdirs
                                (pjx--get-project-path)
                                 pjx-files-exclude-list))
                
                (action     .  (lambda (file) (with-selected-frame (make-frame)
                                                (find-file file))))
                
                ))))


(defun pjx/find-file-regex ()
  "Find all project files recursively matching a regex."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                                
                (candidates . ,(pjx--find-files-by-regex (pjx--get-project-name)
                                                         (read-regexp "Pattern: ")
                                                         pjx-ignore-prefix-list
                                                         pjx-ignore-suffix-list))
                (action     .  find-file)
                ))))

(defun pjx/find-file-ext ()
  "Find all project files recursively by extensions.
Examples:
- M-x pjx/find-file-ext fs fsx  -> Will find all projects with .fs and .fsx extension."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")

                (candidates . ,(pjx--find-files-subdirs-ext (cdr (pjx--get-project-of-buffer))
                                                            (split-string (read-string "Extensions: ") " " t)
                                                            pjx-files-exclude-list
                                                            ))
                (action     .  find-file)
                ))))


(defun pjx/find-file-here ()
  "Find files in subdirectories of current directory."
  (interactive)  
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                (candidates . ,(pjx--find-files-subdirs default-directory
                                                        pjx-files-exclude-list))
                (action     .  find-file)
                ))))

(defun pjx/find-file-dir ()
  "Find files at current directory."
  (interactive)
  (let ((pwd default-directory))
   (helm
   :prompt "Open file: "
   :sources  `((
                (name       . "File:")
                (candidates . ,(directory-files pwd))
                (action     .  find-file)
                )))))


(defun pjx/find-files-open ()
  "Open all files with given extensions in the project and open it.
Use M-x pjx/switch-file to switch between the files opened."
  (interactive)
  (mapc (lambda (cell) (find-file-noselect (cdr cell)))
        (pjx--find-files-subdirs-ext  (cdr (pjx--get-project-of-buffer))
                                      (split-string (read-string "Extensions: ") " " t)
                                      pjx-files-exclude-list)))


(defun pjx/find-dired ()
  "Run M-x find-dired at root project directory."
  (interactive)
  (find-dired (cdr (pjx--get-project-of-buffer))
              (mapconcat #'identity
                         '(
                           ;; Directories to exclude
                           "-not -path '*/.git*'"         ;; Exclude .git Directory
                           "-not -path '*/bin*'"
                           "-not -path '*/lib*'"
                           "-not -path '*/target*'"

                           ;; Temporary files
                           "-and -not -name '.#*'"        ;; Exclude temporary files starting with #
                           "-and -not -name '#*'"
                           "-and -not -name '*#'"
                           "-and -not -name '*~' "        ;; Exclude ending with ~ (tilde)

                           ;; Java Binary files
                           "-and -not -name '*.jar'"
                           "-and -not -name '*.war'"
                           "-and -not -name '*.class'"

                           ;; .NET binary files
                           "-and -not -name '*.exe'"
                           "-and -not -name '*.dll'"
                           "-and -not -name '*.mdb'"

                           ;; Haskell binary files
                           "-and -not -name '*.hi'"

                           ;; C/C++ Binary files
                           "-and -not -name '*.o'"
                           "-and -not -name '*.bin'"

                           ;; Archives
                           "-and -not -name '*.tar'"
                           "-and -not -name '*.tgz'"
                           "-and -not -name '*.zip'"
                           "-and -not -name '*.gz'"
                           )
                         " ")))


(defun pjx/find-dired-here ()
  "Run the command M-x find-dired at current directory."
  (interactive)
  (find-dired default-directory
              (mapconcat #'identity
                         '(
                           ;; Directories to exclude
                           "-not -path '*/.git*'"         ;; Exclude .git Directory
                           "-not -path '*/bin*'"
                           "-not -path '*/lib*'"
                           "-not -path '*/target*'"

                           ;; Temporary files
                           "-and -not -name '.#*'"        ;; Exclude temporary files starting with #
                           "-and -not -name '#*'"
                           "-and -not -name '*#'"
                           "-and -not -name '*~' "        ;; Exclude ending with ~ (tilde)

                           ;; Java Binary files
                           "-and -not -name '*.jar'"
                           "-and -not -name '*.war'"
                           "-and -not -name '*.class'"

                           ;; .NET binary files
                           "-and -not -name '*.exe'"
                           "-and -not -name '*.dll'"
                           "-and -not -name '*.mdb'"

                           ;; Haskell binary files
                           "-and -not -name '*.hi'"

                           ;; C/C++ Binary files
                           "-and -not -name '*.o'"
                           "-and -not -name '*.bin'"

                           ;; Archives
                           "-and -not -name '*.tar'"
                           "-and -not -name '*.tgz'"
                           "-and -not -name '*.zip'"
                           "-and -not -name '*.gz'"
                           )
                         " ")))


(defun pjx/open-files-regex ()
  "Open all project files matching regex recursively."
  (interactive)
  (mapc  (lambda (cell) (find-file-noselect (cdr cell)))
         (pjx--find-files-by-regex (pjx--get-project-name)
                             (read-regexp "Pattern: ")
                             pjx-ignore-prefix-list
                             pjx-ignore-suffix-list)))

(defun pjx/open-files-ext ()
  "Open all project files with given extensions."
  (interactive)
  (mapc  (lambda (cell) (find-file-noselect (cdr cell)))
   (pjx--find-files-by-regex
   (car (pjx--get-project-of-buffer))
   (mapconcat 'identity
              (mapcar (lambda (ext) (format "\\.%s$" ext))
                      (split-string (read-string "Extension:  ")))
              "\\\|")
   pjx-ignore-prefix-list
   pjx-ignore-suffix-list)))

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

;;; Commands to insert project file relative path to current buffer

(defun pjx/insert-path ()
  "Find a project file and insert its relative path to current buffer at point."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                (candidates . ,(mapcar (lambda (cell)
                                         (file-relative-name (cdr cell) default-directory))
                                       (pjx--find-project-files (car (pjx--get-project-of-buffer))
                                                                pjx-ignore-prefix-list
                                                                pjx-ignore-suffix-list
                                                                )))
                (action     .  (lambda (p) (save-excursion (insert p))))
                ))))


(defun pjx/insert-path-abs ()
  "Find a project file and insert its absolute path at point."
  (interactive)
  (helm
   :prompt "Project Files: "
   :sources  `((
                (name       . "File: ")
                (candidates . ,(pjx--find-project-files (car (pjx--get-project-of-buffer))
                                                                pjx-ignore-prefix-list
                                                                pjx-ignore-suffix-list
                                                                ))
                (action     .  (lambda (p) (save-excursion (insert p))))
                ))))

(defun pjx/insert-path-sys ()
  "Insert path at point to system file or directory."
  (interactive)
  (insert (read-file-name "path >" "~")))


(defun pjx/org-image-insert ()
  "Insert path to image in documentation  REAME.org file."
  (interactive)
  (let* ((proj-dir (cdr (pjx--get-project-of-buffer)))
         (file     (file-relative-name  (read-file-name "path >" "./images"  proj-dir)
                                        proj-dir
                                        )))
    (insert (org-make-link-string  (concat "file:" file )
                                   (concat "file:" file )))))

;;; Commands to run shell command at project root directory

(defun pjx/shell ()
  "Run a synchronous shell command at project root directory."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
       (call-interactively #'shell-command)))

(defun pjx/shell-async ()
  "Run an asynchronous shell command at project root directory."
  (interactive)
  (let ((default-directory (cdr (pjx--get-project-of-buffer))))
       (call-interactively #'async-shell-command)))


(defun pjx/scratch ()
  "Open scratch directory"
  (interactive)
  (pjx/new-project "scratch"))

(defun pjx/scratch-frame ()
  "Open scratch project in a new frame."
  (interactive)
  (with-selected-frame (make-frame)
    (pjx/scratch)))

(defun pjx/copy-file-path ()
  "Copy buffer absolute file name to clipboard."
  (interactive)
  (let ((fname (buffer-file-name)))
    (with-temp-buffer
      (insert fname)
      (clipboard-kill-region (point-min) (point-max)))
    (message (format "Buffer file name: %s copied to clipboard."
                     fname))))

(defun pjx/copy-project-path ()
  "Copy current project root directory to clipboard."
  (interactive)
  (let ((proj-path (cdr (pjx--get-project-of-buffer))))
   (with-temp-buffer
     (insert proj-path)
     (clipboard-kill-region (point-min) (point-max)))
   (message (format "Copied to clipboard: %s" proj-path))))



(defun pjx/copy-dir-path ()
  "Copy absolute path to buffer current directory."
  (interactive)
  (let ((fname default-directory))
    (with-temp-buffer
      (insert fname)
      (clipboard-kill-region (point-min) (point-max)))
    (message (format "Buffer file name: %s copied to clipboard."
                     fname))))


;;;;;;;;; Github ;;;;;;;;;;;;;;;;;;;;;;;;


(defun pjx/github ()
  "Open project in Github. 
It supposes that the remote origins is set to Github https url."
  (interactive)
  (browse-url (shell-command-to-string "git config --get remote.origin.url")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'pjx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF - END OF FILE 
