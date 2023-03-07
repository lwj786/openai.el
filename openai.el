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
(require 'mailcap)

(require 'json)

(require 'thingatpt)

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
                         ("content-type" . ,(mailcap-file-name-to-mime-type v))
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
                        "\nARGS is a plist whose property name should be in:\n \\='%S\n
If there is a argument which for specify file path, the path must be prefixed with '@'.\n"
                        keywords))
          (if required-keywords (format
                                 "And arguments: \\='%S is required while others are optional.\n"
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
                   :stream :logprobs :stop :user))
       (docstring (openai--gen-docstring
                   "Create a completion."
                   keywords
                   '(:model)
                   "https://platform.openai.com/docs/api-reference/completions/create")))
  (openai--define-api "create-completion" (&rest args)
                      docstring
                      "/v1/completions"
                      keywords))

;; Chat

(let* ((keywords '(:model :messages
                          :temperature :top_p :n
                          :stream :stop :max_tokens
                          :presence_penalty :frequency_penalty
                          :logit_bias :user))
       (docstring (openai--gen-docstring
                   "Create a cat completion for given chat conversation."
                   keywords
                   '(:model :messages)
                   "https://platform.openai.com/docs/api-reference/chat/create")))
  (openai--define-api "create-chat-completion" (&rest args)
                      docstring
                      "/v1/chat/completions"
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

;; Audio

(let* ((keywords '(:file :model
                         :prompt :response_format
                         :temperature :language))
       (docstring (openai--gen-docstring
                   "Transcribes audio into text."
                   keywords
                   '(:file :model)
                   "https://platform.openai.com/docs/api-reference/audio/create")))
  (openai--define-api "create-transcription" (&rest args)
                      docstring
                      "/v1/audio/transcriptions"
                      keywords
                      (concat "multipart/form-data; boundary="
                              (mml-compute-boundary nil))))

(let* ((keywords '(:file :model
                         :prompt :response_format :temperature))
       (docstring (openai--gen-docstring
                   "Translate audio into text in English."
                   keywords
                   '(:file :model)
                   "https://platform.openai.com/docs/api-reference/audio/create")))
  (openai--define-api "create-translation" (&rest args)
                      docstring
                      "/v1/audio/translations"
                      keywords
                      (concat "multipart/form-data; boundary="
                              (mml-compute-boundary nil))))

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

(defun openai--create-image-descriptor (response &optional response_format)
  "Return image descriptor for given RESPONSE, RESPONSE_FORMAT."
  (let* ((response_format (or response_format "url"))
         (url-or-b64 (alist-get (intern response_format) (seq-elt (alist-get 'data response) 0)))
         (image-data  (if (string= "b64_json" response_format)
                          (base64-decode-string url-or-b64)
                        (with-current-buffer (url-retrieve-synchronously url-or-b64)
                          (encode-coding-string
                           (buffer-substring (1+ (progn (goto-char (point-min))
                                                        (search-forward-regexp "^$")))
                                             (point-max))
                           'raw-text)))))
    (create-image image-data 'png t)))

(defun openai-save-image-at-point (&optional file)
  "Save image at point, and return FILE which expanded by `expand-file-name'.
In an interactive call or FILE is nil, use sha1 as file name and save to `user-emacs-directory'/images/."
  (interactive)
  (if (image-at-point-p)
      (let ((image-data (plist-get (cdr (image--get-image))
                                   :data)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert image-data)
          (prog1 (setq file (expand-file-name
                             (or file
                                 (concat user-emacs-directory
                                         "images/"
                                         (sha1 (current-buffer))
                                         ".png"))))
            (or (file-exists-p (file-name-directory file))
                (mkdir (file-name-directory file)))
            (write-region (point-min) (point-max) file))))))

;; Text completion

(defcustom openai-complete-text-default-args
  '(:model "text-davinci-003"
           :prompt nil :suffix nil :max_tokens 128
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
If POSITION is not specified, use current buffer and PROMPT is default, it will be inserted properly.

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
    (let* ((args (append `(:prompt ,prompt)
                         (if (numberp max_tokens)
                             `(:max_tokens ,max_tokens))
                         args
                         openai-complete-text-default-args))
           (response (apply #'openai-create-completion args))
           (text (alist-get 'text (seq-elt (alist-get 'choices response) 0)))
           (text-property '(face (:background "#d2f4d3"))))
      (prog1 response
        (insert (apply #'propertize text text-property)
                (unless (plist-get args :suffix) "\n"))))))

(defcustom openai-complete-text-cat-default-args
  '(:model "text-davinci-003"
           :prompt nil :suffix nil :max_tokens 256
           :temperature nil :top_p nil :n nil
           :stream nil :logprobs nil :stop nil)
  "Default args for complete text (concatenation)."
  :type '(plist))

(defcustom openai-complete-text-cat-separator
  "\n"
  "Separator for split prompt."
  :type 'string)

(defun openai-complete-text-cat (prompt suffix &optional max_tokens
                                        buffer-or-name position
                                        &rest args)
  "Complete text (concatenation) for given PROMPT, SUFFIX (and other ARGS),
and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of PROMPT is read from region or sentence at point,
default value of SUFFIX is read from region or next sentence after point,
and use numeric prefix argument to specify MAX_TOKENS.

If PROMPT SUFFIX is equal, split with `openai-complete-text-cat-separator',
first is set to PROMPT, and rest is set to SUFFIX.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, use current buffer and PROMPT, SUFFIX is default,
separator will be deleted and text will be inserted properly.

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
        (let ((index (string-match openai-complete-text-cat-separator prompt)))
          (setq prompt (substring prompt 0 index)
                suffix (substring suffix (+ index (length openai-complete-text-cat-separator))))))
    (if (and (not position)
             (not buffer-or-name))
        (if (string= (concat prompt openai-complete-text-cat-separator suffix)
                     (openai--region-string))
            (progn
              (goto-char (+ (use-region-beginning) (length prompt)))
              (delete-char (length openai-complete-text-cat-separator)))
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
  "Edit text for given INPUT, INSTRUCTION (and other ARGS),
and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of INPUT is read from region or sentence at point.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, use current buffer and INPUT is default, it will be inserted properly.

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
    (let* ((args (append `(:input ,input :instruction ,instruction)
                         args
                         openai-edit-text-default-args))
           (response (apply #'openai-create-edit args))
           (text (alist-get 'text (seq-elt (alist-get 'choices response) 0)))
           (text-property '(face (:background "#d2f4d3"))))
      (prog1 response
        (insert (apply #'propertize text text-property) "\n")))))

;; Code completion

(defcustom openai-complete-code-default-args
  '(:model "code-davinci-002"
           :prompt nil :suffix nil :max_tokens 128
           :temperature nil :top_p nil :n nil
           :stream nil :logprobs nil :stop nil)
  "Default args for complete code"
  :type '(plist))

(defun openai-complete-code (&rest args)
  "Same as `openai-complete-text' but for code,
by set `openai-complete-text-default-args' to `openai-complete-code-default-args'.

ARGS is same as arguments of `openai-complete-text'.
In an non-interactive call, required arguments must be specified."
  (interactive)
  (let ((openai-complete-text-default-args openai-complete-code-default-args))
    (if args
        (apply #'openai-complete-text args)
      (call-interactively #'openai-complete-text))))

(defcustom openai-complete-code-cat-default-args
  '(:model "code-davinci-002"
           :prompt nil :suffix nil :max_tokens 256
           :temperature nil :top_p nil :n nil
           :stream nil :logprobs nil :stop nil)
  "Default args for complete code (concatenation)."
  :type '(plist))

(defcustom openai-complete-code-cat-separator
  "\n\n\n"
  "Separator for split prompt (code)."
  :type 'string)

(defun openai-complete-code-cat (&rest args)
  "Same as `openai-complete-text-cat' but for code,
by set `openai-complete-text-cat-default-args' to `openai-complete-code-cat-default-args',
`openai-complete-text-cat-separator' to `openai-complete-code-cat-separator'.

ARGS is same as argument of `openai-complete-text-cat'.
In an non-interactive call, required argument must be specified."
  (interactive)
  (let ((openai-complete-text-cat-default-args openai-complete-code-cat-default-args)
        (openai-complete-text-cat-separator openai-complete-code-cat-separator))
    (if args
        (apply #'openai-complete-text-cat args)
      (call-interactively #'openai-complete-text-cat))))

;; Code edit

(defcustom openai-edit-code-default-args
  '(:model "code-davinci-edit-001"
           :input nil :instruction nil :n nil
           :temperature nil :top_p nil)
  "Default args for edit code."
  :type '(plist))

(defun openai-edit-code (&rest args)
  "Same as `openai-edit-text' but for code,
by set `openai-edit-text-default-args' to `openai-edit-code-default-args'.

ARGS is same as argument of `openai-edit-text',
and will be passed to `openai-edit-text' in an non-interactive call."
  (interactive)
  (let ((openai-edit-text-default-args openai-edit-code-default-args))
    (if args
        (apply #'openai-edit-text args)
      (call-interactively #'openai-edit-text))))

;; Chat completion

(defcustom openai-complete-chat-default-args
  '(:model "gpt-3.5-turbo" :messages nil
           :temperature nil :top_p nil :n nil
           :stream nil :stop nil :max_tokens nil
           :presence_penalty nil :frequency_penalty nil
           :logit_bias nil :user nil)
  "Default args fro complete chat."
  :type '(plist))

(defun openai-complete-chat (messages &optional max_tokens
                                      buffer-or-name position text-property
                                      &rest args)
  "Complete chat for given messages (and other ARGS), and insert it in BUFFER-OR-NAME at POSITION with TEXT-PROPERTY.
Return the response which decoded by `json-read'.

MESSAGES should be a vector of \\='((role . \"...\") (content . \"...\")).

In an interactive call, default value of MESSAGES is read from region or sentence at point
which will be converted to \\='[((role . \"user\") (content . \"...\"))],
and use numeric prefix argument to specify MAX_TOKENS.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, use current buffer and PROMPT is default, it will be inserted properly.

ARGS will override `openai-complete-chat-default-args', see `openai-create-chat-completion' for details."
  (interactive (list `[((role . "user")
                        (content . ,(read-string
                                     (format "I (%s): "
                                             (openai--region-string-or-sentence-at-point))
                                     nil nil
                                     (openai--region-string-or-sentence-at-point))))]
                     current-prefix-arg))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (not position)
             (not buffer-or-name)
             (string= (alist-get 'content (seq-elt messages 0))
                      (openai--region-string-or-sentence-at-point)))
        (if (use-region-p) (goto-char (use-region-end))
          (backward-char)
          (forward-sentence)))
    (if position (goto-char position))
    (let* ((args (append `(:messages ,messages)
                         (if (numberp max_tokens)
                             `(:max_tokens ,max_tokens))
                         args
                         openai-complete-chat-default-args))
           (response (apply #'openai-create-chat-completion args))
           (text (alist-get 'content
                            (alist-get 'message
                                       (seq-elt (alist-get 'choices response) 0))))
           (text-property (or text-property
                              '(face (:background "#d2f4d3")))))
      (prog1 response
        (insert (apply #'propertize text text-property) "\n")))))

;; Image generation

(defcustom openai-generate-image-default-args
  '(:prompt nil
            :n nil :size nil
            :response_format nil :user nil)
  "Default args for generate image"
  :type '(plist))

(defun openai-generate-image (prompt
                              &optional buffer-or-name position
                              &rest args)
  "Generate image for given PROMPT and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of PROMPT is read from region or sentence at point.

If BUFFER-OR-NAME not specified, use current buffer.
If POSITION is not specified, use current buffer and PROMPT is default, it will be inserted properly.

ARGS will override `openai-generate-image-default-args', see `openai-create-image' for details."
  (interactive (list (read-string
                      (format "Input the prompt (%s): "
                              (openai--region-string-or-sentence-at-point))
                      nil nil
                      (openai--region-string-or-sentence-at-point))))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (not position)
             (not buffer-or-name)
             (string= prompt (openai--region-string-or-sentence-at-point)))
        (if (use-region-p) (goto-char (use-region-end))
          (backward-char)
          (forward-sentence)))
    (if position (goto-char position))
    (let* ((args (append `(:prompt ,prompt)
                         args
                         openai-generate-image-default-args))
           (response (apply #'openai-create-image args))
           (image (openai--create-image-descriptor
                   response
                   (plist-get args :response_format))))
      (prog1 response
        (insert-image image prompt)))))

(defcustom openai-edit-image-default-args
  '(:image nil
           :mask nil :prompt nil :n nil
           :size nil :response_format nil :user nil)
  "Default args for edit image."
  :type '(plist))

(defun openai-edit-image (image prompt &optional mask
                                buffer-or-name position
                                &rest args)
  "Create edit for given IMAGE, PROMPT (and optional MASK), insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of IMAGE will use image at point or read a file name.

If MASK not specified, IMAGE must have transparency area.
If BUFFER-OR-NAME and POSITION not specified, use current buffer and position.

ARGS will override `openai-edit-image-default-args', see `openai-create-image-edit' for details."
  (interactive (list (or (openai-save-image-at-point)
                         (expand-file-name (read-file-name
                                            "Select an image (png type and less than 4 MB):")))
                     (read-string "Input the prompt: ")))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if position (goto-char position))
    (let* ((args (append `(:image ,(concat "@" image)
                                  :prompt ,prompt)
                         (if mask `(:mask ,(concat "@" mask)))
                         args
                         openai-edit-image-default-args))
           (response (apply #'openai-create-edit args))
           (image-edit (openai--create-image-descriptor
                        response
                        (plist-get args :response_format))))
      (prog1 response
        (insert-image image-edit
                      (concat "Edit of " image))))))

(defcustom openai-generate-image-variation-default-args
  '(:image nil
           :n nil :size nil
           :response_format nil :user nil)
  "Default args for generate image variation."
  :type '(plist))

(defun openai-generate-image-variation (image
                                        &optional buffer-or-name position
                                        &rest args)
  "Generate variation for given IMAGE and insert it in BUFFER-OR-NAME at POSITION.
Return the response which decoded by `json-read'.

In an interactive call, default value of IMAGE will use image at point or read a file name.

If BUFFER-OR-NAME and POSITION not specified, use current buffer and position.

ARGS will override `openai-generate-image-variation-default-args', see `openai-create-image-variation' for details."
  (interactive (list (or (openai-save-image-at-point)
                         (expand-file-name (read-file-name
                                            "Select an image (png type and less than 4 MB):")))))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if position (goto-char position))
    (let* ((args (append `(:image ,(concat "@" image))
                         args
                         openai-generate-image-variation-default-args))
           (response (apply #'openai-create-image-variation args))
           (image-variation (openai--create-image-descriptor
                             response
                             (plist-get args :response_format))))
      (prog1 response
        (insert-image image-variation
                      (concat "Variation of " image))))))

;; Mode

(defcustom openai-chat-default-args
  '(:model "gpt-3.5-turbo" :messages nil
           :temperature nil :top_p nil :n nil
           :stream nil :stop nil :max_tokens nil
           :presence_penalty nil :frequency_penalty nil
           :logit_bias nil :user nil)
  "Default arguments for OpenAI Chat API."
  :type '(plist))

(defcustom openai-chat-initial-system-content
  nil
  "Default initial system content."
  :type 'string)

(defcustom openai-chat-user-input-prompt
  (propertize "曰：" 'face '(:foreground "red"))
  "Prompt for user input."
  :type 'string)

(defcustom openai-chat-assistant-output-prompt
  (concat (propertize "人♡" 'face '(:background "#d2f4d3")) "\n"
          (propertize "∆λI" 'face '(:background "#d2f4d3")) ": ")
  "Prompt for assistant output."
  :type 'string)

(defcustom openai-chat-dir
  (concat user-emacs-directory
          "openai/chat/")
  "Default directory for save chat."
  :type 'string)

(defvar-keymap openai-chat-mode-map
  "C-j" #'openai-chat-send
  "C-x C-s" #'openai-chat-save
  "C-c s a" #'openai-chat-save-as
  "C-c s s" #'openai-chat-system-say
  "C-c r u i" #'openai-chat-reset-user-input)

(define-derived-mode openai-chat-mode fundamental-mode "OpenAI/Chat"
  "OpenAI chat mode."
  (setq-local openai-chat-messages '[])
  (setq-local openai-chat-file nil)
  (make-local-variable 'openai-chat-default-args)
  (make-local-variable 'openai-chat-user-input-beggining)

  (openai-chat-system-say openai-chat-initial-system-content)
  (openai-chat-set-io-prompt))

(defun openai-chat-set-io-prompt (&optional non-user)
  "Set prompt for user or assistant."
  (let ((prompt (if non-user
                    openai-chat-assistant-output-prompt
                  openai-chat-user-input-prompt)))
    (or (= (pos-bol) (point))
        (insert "\n"))
    (insert (propertize prompt
                        'read-only t
                        'rear-nonsticky '(read-only))))
  (or non-user
      (setq openai-chat-user-input-beggining (point))))

(defun openai-chat-put-messages (&rest args)
  "ARGS should be a message or two string: role and content."
  (setq openai-chat-messages
        (vconcat openai-chat-messages
                 (if (= (length args) 1)
                     args
                   `[((role . ,(car args))
                      (content . ,(cadr args)))]))))

(defun openai-chat-system-say (content)
  "Put CONTENT as system's content to `openai-chat-messages'."
  (interactive "sAs system say: ")
  (if (> (length content) 0)
      (openai-chat-put-messages "system" content)))

(defun openai-chat-reset-user-input ()
  "Reset user input.
Can be used when failed to recvice response."
  (interactive)
  (if (derived-mode-p 'openai-chat-mode)
      (openai-chat-set-io-prompt)))

(defun openai-chat-send (&optional resend)
  "Send user's input, recvice response and insert it, put them to `openai-chat-messages'.
Return the response which decoded by `json-read'.

If user's input is empty, may will not send.
If RESEND is non nil, always send and not to put user's input to `openai-chat-messages',
can be used when failed to recvice response.

In an interactive call, use prefix argument to specify RESEND."
  (interactive "P")
  (if (derived-mode-p 'openai-chat-mode)
      (let ((user-input (buffer-substring-no-properties
                         openai-chat-user-input-beggining
                         (point-max))))
        (if (or (length user-input)
                resend)
            (progn
              (goto-char (point-max))
              (unless resend
                (openai-chat-put-messages "user" user-input))
              (openai-chat-set-io-prompt t)
              (let* ((args (append
                            `(,openai-chat-messages
                              ,(current-buffer) nil nil
                              (read-only t rear-nonsticky (read-only)))
                            openai-chat-default-args))
                     (response (apply #'openai-complete-chat args)))
                (openai-chat-put-messages (alist-get
                                           'message
                                           (seq-elt (alist-get 'choices response) 0)))
                (openai-chat-set-io-prompt)
                response))
          (openai-chat-set-io-prompt) nil))))

(defun openai-chat-save-as (file)
  "Save the current chat as FILE and return the FILE.

Default directory is set by `openai-chat-dir'.
In an interactive call, prompt for FILE, and default value is in \"chat-%Y%m%d%H%M%S.json\" format."
  (interactive (list (let ((filename (concat openai-chat-dir
                                             (format-time-string
                                              "chat-%Y%m%d%H%M%S.json"))))
                       (read-file-name (format "Save the chat (%s): "
                                               filename)
                                       openai-chat-dir
                                       filename))))
  (if (derived-mode-p 'openai-chat-mode)
      (let ((chat (openai--preprocess-request-data
                   (plist-put openai-chat-default-args
                              :messages
                              openai-chat-messages)
                   '(:model :messages
                            :temperature :top_p :n
                            :stream :stop :max_tokens
                            :presence_penalty :frequency_penalty
                            :logit_bias :user))))
        (with-temp-buffer
          (insert (json-encode chat))
          (write-file file)
          file))))

(defun openai-chat-save ()
  "Save current chat to `openai-chat-file', return the saved file.
If `openai-chat-file' is nil, call `openai-chat-save-as' interactively."
  (interactive)
  (if openai-chat-file
      (openai-chat-save-as openai-chat-file)
    (call-interactively #'openai-chat-save-as)))

(defun openai-chat-continue (file &optional n)
  "Create or switch to chat buffer, load chat file, return the buffer.
If `openai-chat-messages' is empty or only contain a system content, chat file will be load.
FILE is a json file saved by `openai-chat-save' or `openai-chat-save-as'.
As for N, check `openai-chat' for details."
  (interactive (list (read-file-name "Load the chat: "
                                     openai-chat-dir)
                     current-prefix-arg))
  (with-current-buffer (openai-chat n)
    (when (and (file-exists-p file)
               (or (= (length openai-chat-messages) 0)
                   (and (= (length openai-chat-messages) 1)
                        (string= (alist-get 'role
                                            (seq-elt openai-chat-messages 0))
                                 "system"))))
      (let ((data (json-read-file file)))
        (setq openai-chat-file file
              openai-chat-messages (alist-get 'messages data))
        (dolist (elt (mapcar #'car data))
          (setq openai-chat-default-args
                (plist-put openai-chat-default-args
                           (intern (concat ":" (symbol-name elt)))
                           (alist-get elt data)))))
      (let ((inhibit-read-only t)) (erase-buffer))
      (let ((i 0)
            (l (length openai-chat-messages)))
        (while (< i l)
          (let* ((m (seq-elt openai-chat-messages i))
                 (r (alist-get 'role m))
                 (c (alist-get 'content m)))
            (cond ((string= r "user")
                   (openai-chat-set-io-prompt)
                   (insert (propertize c
                                       'read-only t
                                       'rear-nonsticky '(read-only))))
                  ((string= r "assistant")
                   (openai-chat-set-io-prompt t)
                   (insert (propertize c
                                       'read-only t
                                       'rear-nonsticky '(read-only))))))
          (setq i (1+ i))))
      (openai-chat-set-io-prompt)
      (current-buffer))))

(defun openai-chat (&optional n)
  "Create or switch to OpenAI chat buffer, and return the buffer.
N specify the buffer with that number (create if not exist).

In an interactive call, use numeric prefix arg N to create or switch specified buffer."
  (interactive "P")
  (let ((buf (get-buffer-create (if n
                                    (format "%s<%d>"
                                            "OpenAI/Chat"
                                            n)
                                  "OpenAI/Chat"))))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (or (derived-mode-p 'openai-chat-mode)
          (openai-chat-mode)))
    buf))


(provide 'openai)

;;; openai.el ends here
