
(require 'cl)
(require 'helm)

(defgroup pjx nil
  "Project Tool for Eamcs"
  :group 'tools
  )

(defcustom pjx/root
  
  (concat (file-name-as-directory user-emacs-directory)
          "root")
  "Pjx root directory:"
  
  :type 'string 
  :group 'pjx
  )

(defcustom pjx/project-root "~/Documents/projects"
  "path to Nuget executable file"
  :type  'directory
  :group 'pjx 
  )

(defcustom pjx/terminal "xfce4-terminal"
  "Path to default terminal emulator. Default value xterm."
  :type 'file
  :group 'pjx 
  )


(defvar pjx/project-file-blacklist
  '(".o" ".bin" ".git" ".svn" )

  "Project file extensions which will be 
   excluded from the helm selection.
  ")

(defvar pjx/current-project nil "Holds the current project name")


(defun pjx/file-in-directory-p (directory filename)
  (equal (file-name-as-directory (expand-file-name (file-name-directory filename)))
         (file-name-as-directory (expand-file-name directory))))


(defun pjx/directory-files (directory)
  "List a directory not showing files starting with (.), (~), (#) "
  (remove-if-not
   
   (lambda (s)

     (let (
           (b (file-name-base s))
           )

       (not (or
           
             (equal ".." b)
             (equal "." b)
             (string-prefix-p ".#" b)
             (string-suffix-p "~"  b )
             (string-prefix-p "#" b)
             (string-suffix-p "#" b)
           ))))

   (directory-files directory t)))

(defun pjx/project-list ()
  "Returns all projects directories. See full doc."
  (mapcar (lambda (p) (cons (file-name-nondirectory p) p))
          (pjx/directory-files pjx/project-root)))


(defun pjx/project-get-files ()
  "Returns all files in project directory"
  (mapcar (lambda (p) (cons (file-name-nondirectory p) p))          
          (pjx/directory-files pjx/current-project))
  )

(defun pjx/project-helm-fn (callback)
  ""
  (helm
   :prompt "Project: "
   :sources  `((
                (name       . "Pjx: ")
                (candidates . ,(pjx/project-list))
                (action     . callback)
                ))))


(defun pjx/project-files-helm-fn (callback)
  (helm
   :prompt "Project File: " 
   :sources  `((
                (name       . "Dir: ")
                (candidates . ,(pjx/project-get-files))
                (action     . callback)
                ))))


;;;;============== User Commands ================ ;;
;;  @SECTION: User commands 
;;

(defun pjx/helm ()
  "Select project directory and open it in Helm."
  (interactive)
  (pjx/project-helm-fn (lambda (p)                         
                         (setq pjx/current-project p)
                         ;;(dired p)

                         (if pjx/current-project
                             (progn  (pjx/close)
                                     (setq pjx))

                           (setq pjx/current-project p)
                           )
                         )))


(defun pjx/dir ()
  "Open current project set with pjx/helm.
   
  It opens the directory stored in variable:  
  `pjx/current-project`
  "
  (interactive)
  (dired pjx/current-project))

(defun pjx/file-open  ()
  "Select a file of current project and open it."
  (interactive)
  (pjx/project-files-helm-fn #'find-file))


(defun pjx/panel ()
  "Opens a vertical panel containing the project files."
  (interactive)
  (split-window-horizontally)  
  (with-current-buffer  (dired-other-window pjx/current-project)      
    (dired-omit-mode)
    (dired-hide-details-mode)
    ))

(defun pjx/close ()
  "Close all files belonging to the current project."
  (interactive)
  
  (mapc (lambda (b)
          (let
              (
               (file-path (buffer-file-name b))
               )
            
            (if (and file-path (pjx/file-in-directory-p pjx/current-project file-path))

                (with-current-buffer b
                  (save-buffer)
                  (kill-buffer b)

                  ))))


        (buffer-list))

  (message "Project files closed")
  
  ) ;; End of pxj/close


(defun pjx/term ()
  "Launch terminal Emulator at project directory."

  (interactive)
  
  (let ((default-directory  (file-name-as-directory  pjx/current-project)))

    (start-process "term"  ;; Process name 
                   nil     ;; buffer name
                   pjx/terminal
                   )))

