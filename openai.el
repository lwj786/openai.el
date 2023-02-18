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
Return the response which decoded by `json-read'"
  (let ((url-request-method (or method (if data "POST" "GET")))
	(url-request-extra-headers
	 (append
	  `(("Authorization" . ,(concat "Bearer " openai-api-key)))
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
      (goto-char url-http-end-of-headers)
      (json-read))))

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

(defun openai--gen-docstring (do keywords required-keywords doc_url)
  "A helper function for generate docstring"
  (format "%s
ARGS is a plist whose property name should be in:
 \\='%S
and \\='%S is required while others are optional.
Visit URL `%s' for details."
	   do keywords required-keywords doc_url))

(defmacro openai--define-api (name arglist docstring
				   uri &optional keywords content-type method)
  "A helper macro for define OpenAI API function."
  `(prog1 (defun ,(intern (concat "openai-" name)) ,arglist
	    ,docstring
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
		    "List models which currently available with its basic infomation."
		    "/v1/models")

(openai--define-api "retrieve-model" (model)
		    "Retrieve a model instance with its basic infomation by its ID"
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
		      "v1/edits"
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
		    "List files belonging to the user's organization."
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
		    "Delete a file."
		    (concat "/v1/files/" file_id)
		    nil nil
		    "DELETE")

(openai--define-api "retrieve-file" (file_id)
		    "Retrieve a file's infomation."
		    (concat "/v1/files/" file_id))

(openai--define-api "download-file" (file_id)
		    "Get the file's content."
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
		    "List fine-tuning jobs."
		    "/v1/fine-tunes")

(openai--define-api "retrieve-fine-tune" (fine-tune-id)
		    "Retrieve the fine-tuning job's infomation."
		    (concat "/v1/fine-tunes/" fine_tune_id))

(openai--define-api "cancel-fine-tune" (fine-tune-id)
		    "cancel a fine-tuning job."
		    (concat "/v1/fine-tunes/" fine-tune-id "/cancel"))

(openai--define-api "list-fine-tune-events" (fine_tune_id)
		    "List the fine-tuning job's status."
		    (concat "/v1/fine-tunes/" fine-tune-id "/events"))

(openai--define-api "delete-model" (model)
		    "Delete a fine-tuned model."
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

(provide 'openai)

;;; openai.el ends here
