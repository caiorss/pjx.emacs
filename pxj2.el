;;; Refactor pjx project

;;; pjx Root directory
;;;
(setq pjx-root-directory "~/Documents/projects")

(setq pjx-current-project nil)


;;; ============== Internal functions and helpers ========== ;;

(defun pjx--path-in-dir-p (root path)
  "Check if path is in root directory."
  (string-prefix-p (expand-file-name root) (expand-file-name path)))

(defun pjx--project-list ()
  "Returns all projects directories. See full doc."
  (mapcar (lambda (p) (cons (file-name-nondirectory p) p))
          (pjx--directory-files pjx/project-root)))

;; Select a project and call the functions callback
;; as (callback <project-path>) like (callback "~/Documents/projects/test-cpp")
;;
(defun pjx--select-callback (callback)
  ""
  (helm
   :prompt "Project: "
   :sources  `((
                (name       . "Pjx: ")
                (candidates . ,(pjx--project-list))
                (action     . callback)
                ))))




(defun pjx--get-buffers ()
  "Return all buffers which file name or default directory is in `pjx-root-directory`"
  (cl-remove-if-not (lambda (buf)
                      (pxj--path-in-dir-p pjx-root-directory
                                        (with-current-buffer buf
                                          (or (buffer-file-name)
                                              default-directory))))
                    (buffer-list)))


(defun pjx--get-opened-projects ()
  "Return a list with all opened projects."
  (mapcar (lambda (proj) (cons proj
                               (concat (file-name-as-directory pjx-root-directory)
                                       proj)))
   (delete "."
          (delete-dups
           (mapcar (lambda (buf)
                   (string-remove-suffix
                    "/"
                    (file-name-directory
                     (file-relative-name (with-current-buffer buf
                                           (or (buffer-file-name)
                                               default-directory))
                                         pjx-root-directory))))
                   (pjx--get-buffers))))))

(defun pjx--project-select-callback (callback)
  "Select a project with helm and pass its path to the callback function."
  (helm
   :prompt "Project: "
   :sources  `((
                (name       . "Pjx: ")
                (candidates . ,(pjx--get-opened-projects))
                (action     .  callback)
                ))))


;;; ============  User Commands ====================== ;;;


(defun pjx/dired ()
  "Open root project directory."
  (interactive)
  (dired pjx-root-directory))

(defun pjx/dired-frame ()
  "Open root project directory in a new frame."
  (interactive)
  (dired-other-frame pjx-root-directory))

(defun pjx/project-open ()
  "Select project directory and open it in dired-mode."
  (interactive)
  (pjx--select-callback #'dired))

(defun pjx/project-open-frame ()
  "Open project in a new frame."
  (interactive)
  (pjx--select-callback #'dired-other-frame))

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


