;;; gptel-custom --- gptel customization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'gptel-custom)

;;; Code:

(require 'gptel)
(setq gptel-default-mode 'org-mode)

(defun gptel-custom-org-rewrite-directive ()
  (when (eq major-mode 'org-mode)
    "You are an org-mode editor.
     Follow my instructions and improve or rewrite the text I
     provide.  Generate ONLY the replacement text, without any
     explanation or markdown code fences. Don't change latex
     environments. Specifically leave equation align environments
     unchanged."))

(add-hook 'gptel-rewrite-directives-hook 'gptel-custom-org-rewrite-directive)

(defun generate-image-with-gptel (prompt)
  "Generate an image based on PROMPT using an image generation API."
  (interactive "sEnter your prompt: ")
  (let ((api-url "https://api.openai.com/v1/images/generations")
        (api-key (gptel-api-key-from-auth-source))) ; Replace with your actual API key
    (url-retrieve
     api-url
     (lambda (status)
       (message "%s" (buffer-string))
       (goto-char (point-min))
       (re-search-forward "^$") ; Go to the end of headers
       (let ((json-response (json-read)))
         (let ((image-url (alist-get 'image_url json-response))) ; Adjust based on API response
           (switch-to-buffer "*Generated Image*")
           (insert-image (create-image image-url))))))
    nil nil
    `(,api-key ,prompt)))

(defun gptel-custom-dalle-generate-image (prompt)
  "Generate an image based on PROMPT using an image generation API."
  (interactive "sEnter your prompt: ")
  (request
    "https://api.openai.com/v1/images/generations"
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " (gptel-api-key-from-auth-source))))
    :data (json-encode
           `(("prompt" . ,prompt)
             ("n" . 1)
             ("model" . "dall-e-3")
             ("size" . "1024x1024")))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Image URL: %s" (cdr (assoc 'url (elt (cdr (assoc 'data data)) 0))))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys&rest _)
              (message "Error: %S" error-thrown)))))

(defun gptel-custom-chatgpt-generate-image (prompt)
  "Generate an image based on PROMPT using an image generation API."
  (interactive "sEnter your prompt: ")
  (request
    "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " (gptel-api-key-from-auth-source))))
    :data (json-encode
           `((model . "gpt-4o-mini")
             (messages . [((role . "user")
                           (content . "Create an image of a futuristic city floating in clouds, in the style of a digital painting."))])
             (functions . [((name . "generate_image")
                            (parameters . ((type . "object")
                                           (properties . ((prompt . ((type . "string")))
                                                          (n . ((type . "integer")))
                                                          (size . ((type . "string")))))
                                           (required . ["prompt"]))))])
             (function_call . "auto")))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Response: %S" data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys&rest _)
              (message "Error: %S" error-thrown)))))

 
(provide 'gptel-custom)

;;; gptel-custom.el ends here
