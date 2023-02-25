;;; openai.el -- a Elisp wrapper for use OpenAI's API -*- lexical-binding:t -*-

;; Copyright (C) 2023 Li Wujun

;; Author: Li Wujun <li.wujun@wujun.li>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a library for interact with OpenAI's API.

;;; Code:

(require 'url)
(require 'mm-url)
(require 'mml)

(require 'json)

;;; Library

;; Parameters

(defcustom openai-api-key nil
  "String used for authentication.
Visit URL `https://platform.openai.com/account/api-keys' to retrieve it."
  :type 'string)

(defcustom openai-organization nil
  "String used for specify organization whose subscription quota will be count against."
  :type 'string)

;; Functions

;; Internal

(defun openai--make-request (uri &optional data content-type method)
  "Interact with api.openai.com through HTTP requests.
Return the response which decoded by `json-read'."
  (let ((url-request-method (or method (if data "POST" "GET")))
	(url-request-extra-headers
	 (append
	  `(("Authorization" . ,(encode-coding-string
				 (concat "Bearer " openai-api-key)
				 'utf-8)))
	  (if openai-organization
	      `(("OpenAI-Organization" . ,openai-organization)))
	  (if data
	      `(("Content-Type" . ,(or content-type "application/json"))))))
	(url-request-data (if data
			      (if (string-match-p "^multipart/form-data;"
						  (or content-type ""))
				  (mm-url-encode-multipart-form-data
				   data
				   (substring content-type 30))
				(encode-coding-string
				 (json-encode data)
				 'utf-8)))))
    (with-current-buffer (url-retrieve-synchronously
			  (concat "https://api.openai.com" uri))
      (json-read-from-string
       (decode-coding-region (1+ (progn (goto-char (point-min))
				       (search-forward-regexp "^$")))
			     (point-max) 'utf-8 t)))))

(defun openai--preprocess-request-data (&optional args keywords content-type)
  "Preprocess request body data."
  (let* ((k (car keywords))
	 (v (plist-get args k)))
    (if (and k v)
	(cons
	 (if (and (string-match-p "^multipart/form-data;"
				  (or content-type "application/json"))
		  (and (stringp v)
		       (string-match-p "^@" v)))
	     `("file" . (("name" . ,(substring (symbol-name k) 1))
			 ("filename" . ,(file-name-nondirectory v))
			 ("content-type" . "image/png")
			 ("filedata" . ,(with-temp-buffer
					  (insert-file-contents-literally
					   (substring v 1))
					  (buffer-string)))))
	   `(,(substring (symbol-name k) 1) . ,v))
	 (openai--preprocess-request-data args (cdr keywords) content-type))
      (if k
	  (openai--preprocess-request-data args (cdr keywords) content-type)))))

(defun openai--gen-docstring (do &optional keywords required-keywords doc_url)
  "A helper function for generate docstring"
  (concat do "\n"
	  "Return the response which decoded by `json-read'.\n"
	  (if keywords (format
			"\nARGS is a plist whose property name should be in:\n \\='%S\n"
			keywords))
	  (if required-keywords (format
				 "and \\='%S is required while others are optional.\n"
				 required-keywords))
	  (if doc_url (format "\nVisit URL `%s' for details.\n" doc_url))))

(defmacro openai--define-api (name arglist docstring
				   uri &optional keywords content-type method)
  "A helper macro for define OpenAI API function."
  `(prog1 (defun ,(intern (concat "openai-" name)) ,arglist
	    (openai--make-request ,uri
				  (funcall #'openai--preprocess-request-data
					   ,(cadr arglist) ,keywords ,content-type)
				  ,content-type
				  ,method))
     (put ',(intern (concat "openai-" name))
	  'function-documentation
	  ,docstring)))

;; Models

(openai--define-api "list-models" ()
		    (openai--gen-docstring
		     "List models which currently available with its basic infomation.")
		    "/v1/models")

(openai--define-api "retrieve-model" (model)
		    (openai--gen-docstring
		     "Retrieve a model instance with its basic infomation by its ID.")
		    (concat "/v1/models/" model))

;; Completions

(let* ((keywords '(:model
		   :prompt :suffix :max_tokens
		   :temperature :top_p :n
		   :stream :logprobs :stop))
       (docstring (openai--gen-docstring
		   "Create a completion."
		   keywords
		   '(:model)
		   "https://platform.openai.com/docs/api-reference/completions/create")))
  (openai--define-api "create-completion" (&rest args)
		      docstring
		      "/v1/completions"
		      keywords))

;; Edits

(let* ((keywords '(:model
		   :input :instruction :n
		   :temperature :top_p))
       (docstring (openai--gen-docstring
		   "Create an edited version of the input."
		   keywords
		   '(:model :instruction)
		   "https://platform.openai.com/docs/api-reference/edits/create")))
  (openai--define-api "create-edit" (&rest args)
		      docstring
		      "/v1/edits"
		      keywords))

;; Images

(let* ((keywords '(:prompt
		   :n :size :response_format :user))
       (docstring (openai--gen-docstring
		   "Create an image."
		   keywords
		   '(:prompt)
		   "https://platform.openai.com/docs/api-reference/images/create")))
  (openai--define-api "create-image" (&rest args)
		      docstring
		      "/v1/images/generations"
		      keywords))

(let* ((keywords '(:image
		   :mask :prompt :n :size
		   :response_format :user))
       (docstring (openai--gen-docstring
		   "Create an edited image."
		   keywords
		   '(:image :prompt)
		   "https://platform.openai.com/docs/api-reference/images/create-edit")))
  (openai--define-api "create-image-edit" (&rest args)
		      docstring
		      "/v1/images/edits"
		      keywords
		      (concat "multipart/form-data; boundary="
			      (mml-compute-boundary nil))))

(let* ((keywords '(:image
		   :n :size :response_format :user))
       (docstring (openai--gen-docstring
		   "Create a variation of original image."
		   keywords
		   '(:image)
		   "https://platform.openai.com/docs/api-reference/images/create-variation")))
  (openai--define-api "create-image-variation" (&rest args)
		      docstring
		      "/v1/images/variations"
		      keywords
		      (concat "multipart/form-data; boundary="
			      (mml-compute-boundary nil))))

;; Embeddings

(let* ((keywords '(:model :input :user))
       (docstring (openai--gen-docstring
		   "Create an embedding vector representing the input."
		   keywords
		   '(:model :input)
		   "https://platform.openai.com/docs/api-reference/embeddings/create")))
  (openai--define-api "create-embedding" (&rest args)
		      docstring
		      "/v1/embeddings"
		      keywords))

;; Files

(openai--define-api "list-files" ()
		    (openai--gen-docstring
		     "List files belonging to the user's organization.")
		    "/v1/files")

(let* ((keywords '(:file :purpose))
       (docstring (openai--gen-docstring
		   "Upload a file."
		   keywords
		   keywords
		   "https://platform.openai.com/docs/api-reference/files/upload")))
  (openai--define-api "create-file" (&rest args)
		      docstring
		      "/v1/files"
		      keywords
		      (concat "multipart/form-data; boundary="
			      (mml-compute-boundary nil))))

(openai--define-api "delete-file" (file_id)
		    (openai--gen-docstring
		     "Delete a file.")
		    (concat "/v1/files/" file_id)
		    nil nil
		    "DELETE")

(openai--define-api "retrieve-file" (file_id)
		    (openai--gen-docstring
		     "Retrieve a file's infomation.")
		    (concat "/v1/files/" file_id))

(openai--define-api "download-file" (file_id)
		    (openai--gen-docstring
		     "Get the file's content.")
		    (concat "/v1/files/" file_id "/content"))

;; Fine-tunes

(let* ((keywords '(:training_file
		   :validation_file :model :n_epochs :batch_size
		   :learning_rate_multiplier :prompt_loss_weight
		   :compute_classification_metrics :classification_n_classes
		   :classification_positive_class :classification_betas
		   :suffix))
       (docstring (openai--gen-docstring
		   "Create a fine-tuning job."
		   keywords
		   '(:training_file)
		   "https://platform.openai.com/docs/api-reference/fine-tunes/create")))
  (openai--define-api "create-fine-tune" (&rest args)
		      docstring
		      "/v1/fine-tunes"
		      keywords))

(openai--define-api "list-fine-tunes" ()
		    (openai--gen-docstring
		     "List fine-tuning jobs.")
		    "/v1/fine-tunes")

(openai--define-api "retrieve-fine-tune" (fine-tune-id)
		    (openai--gen-docstring
		     "Retrieve the fine-tuning job's infomation.")
		    (concat "/v1/fine-tunes/" fine_tune_id))

(openai--define-api "cancel-fine-tune" (fine-tune-id)
		    (openai--gen-docstring
		     "Cancel a fine-tuning job.")
		    (concat "/v1/fine-tunes/" fine-tune-id "/cancel"))

(openai--define-api "list-fine-tune-events" (fine_tune_id)
		    (openai--gen-docstring
		     "List the fine-tuning job's status.")
		    (concat "/v1/fine-tunes/" fine-tune-id "/events"))

(openai--define-api "delete-model" (model)
		    (openai--gen-docstring
		     "Delete a fine-tuned model.")
		    (concat "/v1/models/" model)
		    nil nil
		    "DELETE")

;; Moderations

(let* ((keywords '(:input :model))
       (docstring (openai--gen-docstring
		   "Predicte if the input violates OpenAI's content policy."
		   keywords
		   '(:input)
		   "https://platform.openai.com/docs/api-reference/moderations/create")))
  (openai--define-api "create-moderation" (&rest args)
		      docstring
		      "/v1/moderations"
		      keywords))

;;; Commands

;; General functions

(defun openai--region-string ()
  "Return string in region."
  (if (use-region-p) (buffer-substring-no-properties
		      (use-region-beginning)
		      (use-region-end))))

(defun openai--region-string-or-sentence-at-point ()
  "Return string in region if use region, or sentence at point."
  (or (openai--region-string)
      (sentence-at-point t)))

(defun openai--nth-sentence-after-point (&optional nth)
  "Return NTH sentence after point, say 0 means sentence at point."
  (let ((position (point)))
    (backward-char)
    (forward-sentence (+ (or nth 0) 1))
    (if (= (point) (point-max)) (backward-char))
    (prog1 (sentence-at-point t)
      (goto-char position))))

;; Text completion

(defcustom openai-complete-text-default-args
  '(:model "text-davinci-003"
    :prompt nil :suffix nil :max_tokens 125
    :temperature nil :top_p nil :n nil
    :stream nil :logprobs nil :stop nil)
  "Default args for complete text"
  :type '(plist))

(defun openai-complete-text (prompt &optional max_tokens
				      buffer-or-name position
				      &rest args)
  "Complete text for given PROMPT (and other ARGS), and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of PROMPT is read from region or sentence at point,
and use numeric prefix argument to specify MAX_TOKENS.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, and use current buffer while PROMPT is default, it will be inserted properly.

ARGS will override `openai-complete-text-default-args', see `openai-create-completion' for details."
  (interactive (list (read-string
		      (format "Input the prompt (%s): "
			      (openai--region-string-or-sentence-at-point))
		      nil nil
		      (openai--region-string-or-sentence-at-point))
		     current-prefix-arg))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (not position)
	     (not buffer-or-name)
	     (string= prompt (openai--region-string-or-sentence-at-point)))
	(if (use-region-p) (goto-char (use-region-end))
	  (backward-char)
	  (forward-sentence)))
    (if position (goto-char position))
    (let ((response (apply #'openai-create-completion
			   (append `(:prompt ,prompt)
				   (if (numberp max_tokens)
				       `(:max_tokens ,max_tokens))
				   args
				   openai-complete-text-default-args))))
      (prog1 response
	(insert (alist-get 'text (seq-elt (alist-get 'choices response) 0)))))))

(defcustom openai-complete-text-cat-default-args
  '(:model "text-davinci-003"
    :prompt nil :suffix nil :max_tokens 256
    :temperature nil :top_p nil :n nil
    :stream nil :logprobs nil :stop nil)
  "Default args for complete text (concatenation)."
  :type '(plist))

(defun openai-complete-text-cat (prompt suffix &optional max_tokens
					buffer-or-name position
					&rest args)
  "Complete text (concatenation) for given PROMPT (and other ARGS), and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of PROMPT is read from region or sentence at point,
default value of SUFFIX is read from region or next sentence after point,
and use numeric prefix argument to specify MAX_TOKENS.

If PROMPT SUFFIX is equal, split with \"\\n\", first set to PROMPT, and rest set to SUFFIX.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, and use current buffer while PROMPT is default, it will be inserted properly.

ARGS will override `openai-complete-text-cat-default-args', see `openai-create-completion' for details."
  (interactive (list (read-string
		      (format "Input the prefix prompt (%s): "
			      (openai--region-string-or-sentence-at-point))
		      nil nil
		      (openai--region-string-or-sentence-at-point))
		     (read-string
		      (format "Input the suffix prompt (%s): "
			      (if (use-region-p)
				  (openai--region-string-or-sentence-at-point)
				(openai--nth-sentence-after-point 1)))
		      nil nil
		      (if (use-region-p)
			  (openai--region-string-or-sentence-at-point)
			(openai--nth-sentence-after-point 1)))
		     current-prefix-arg))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (string= prompt suffix)
	(let ((index (string-match "\n" prompt)))
	  (setq prompt (substring prompt 0 index)
		suffix (substring suffix (+ index 1)))))
    (if (and (not position)
	     (not buffer-or-name))
	(if (string= (concat prompt "\n" suffix)
		     (openai--region-string))
	    (goto-char (+ (use-region-beginning) (length prompt)))
	  (if (and (string= prompt (sentence-at-point))
		   (string= suffix (openai--nth-sentence-after-point 1)))
	      (progn (backward-char) (forward-sentence)))))
    (if position (goto-char position))
    (apply #'openai-complete-text (append (list prompt max_tokens
						(current-buffer) (point))
					  `(:suffix ,suffix)
					  args
					  openai-complete-text-cat-default-args))))

;; Text edit

(defcustom openai-edit-text-default-args
  '(:model "text-davinci-edit-001"
	   :input nil :instruction nil :n nil
	   :temperature nil :top_p nil)
  "Default args for edit text."
  :type '(plist))

(defun openai-edit-text (input instruction
			       &optional buffer-or-name position
			       &rest args)
  "Edit text for given INPUT, INSTRUCTION (and other ARGS), and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of INPUT is read from region or sentence at point.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, and use current buffer while INPUT is default, it will be inserted properly.

ARGS will override `openai-edit-text-default-args', see `openai-create-edit' for details."
  (interactive (list (read-string (format "Input the original text (%s): "
					  (openai--region-string-or-sentence-at-point))
				  nil nil
				  (openai--region-string-or-sentence-at-point))
		     (read-string (format "Input the instruction: "))))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (not position)
	     (not buffer-or-name))
	(if (use-region-p) (goto-char (use-region-end))
	  (backward-char)
	  (forward-sentence)))
      (if position (goto-char position))
      (let ((response (apply #'openai-create-edit
			     (append `(:input ,input :instruction ,instruction)
				     args
				     openai-edit-text-default-args))))
	(prog1 response
	  (insert (alist-get 'text (seq-elt (alist-get 'choices response) 0)))))))

(provide 'openai)

;;; openai.el ends here
