
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

(defcustom pjx/project-root '("~/Documents/projects")
  "path to Nuget executable file"
  :type  '(repeat directory)
  :group 'nuget 
  )




