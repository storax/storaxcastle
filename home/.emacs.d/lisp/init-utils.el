;;; init-utils --- Useful stuff

;;; Commentary:

;;; Code:
(require 'tramp)

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Handier way to access items
;;----------------------------------------------------------------------------
(unless (fboundp 'alist-get) ;;25.1
  (defun alist-get (key alist &optional default remove)
    "Get the value associated to KEY in ALIST.

DEFAULT is the value to return if KEY is not found in ALIST.
REMOVE, if non-nil, means that when setting this element, we should
remove the entry if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))

;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;----------------------------------------------------------------------------
;; Edit the dotemacs file
;;----------------------------------------------------------------------------
(defun edit-dotemacs ()
  "Edit .emacs."
  (interactive)
  (find-file-existing user-init-file)
  (widen))

;; FIX 300 status code for disambiguation result to set success to t
;; Sometimes it works, sometimes url-retrieve simply does not call the callback.
(defun url-http-parse-headers ()
 "Parse and handle HTTP specific headers.
Return t if and only if the current buffer is still active and
should be shown to the user."
  ;; The comments after each status code handled are taken from RFC
  ;; 2616 (HTTP/1.1)
  (url-http-mark-connection-as-free (url-host url-current-object)
				    (url-port url-current-object)
				    url-http-process)

  (if (or (not (boundp 'url-http-end-of-headers))
	  (not url-http-end-of-headers))
      (error "Trying to parse headers in odd buffer: %s" (buffer-name)))
  (goto-char (point-min))
  (url-http-debug "url-http-parse-headers called in (%s)" (buffer-name))
  (url-http-parse-response)
  (mail-narrow-to-head)
  ;;(narrow-to-region (point-min) url-http-end-of-headers)
  (let ((connection (mail-fetch-field "Connection")))
    ;; In HTTP 1.0, keep the connection only if there is a
    ;; "Connection: keep-alive" header.
    ;; In HTTP 1.1 (and greater), keep the connection unless there is a
    ;; "Connection: close" header
    (cond
     ((string= url-http-response-version "1.0")
      (unless (and connection
		   (string= (downcase connection) "keep-alive"))
	(delete-process url-http-process)))
     (t
      (when (and connection
		 (string= (downcase connection) "close"))
	(delete-process url-http-process)))))
  (let* ((buffer (current-buffer))
	 (class (/ url-http-response-status 100))
	 (success nil)
	 ;; other status symbols: jewelry and luxury cars
	 (status-symbol (cadr (assq url-http-response-status url-http-codes))))
    (url-http-debug "Parsed HTTP headers: class=%d status=%d"
		    class url-http-response-status)
    (when (url-use-cookies url-http-target-url)
      (url-http-handle-cookies))

    (pcase class
      ;; Classes of response codes
      ;;
      ;; 5xx = Server Error
      ;; 4xx = Client Error
      ;; 3xx = Redirection
      ;; 2xx = Successful
      ;; 1xx = Informational
      (1				; Information messages
       ;; 100 = Continue with request
       ;; 101 = Switching protocols
       ;; 102 = Processing (Added by DAV)
       (url-mark-buffer-as-dead buffer)
       (error "HTTP responses in class 1xx not supported (%d)"
	      url-http-response-status))
      (2				; Success
       ;; 200 Ok
       ;; 201 Created
       ;; 202 Accepted
       ;; 203 Non-authoritative information
       ;; 204 No content
       ;; 205 Reset content
       ;; 206 Partial content
       ;; 207 Multi-status (Added by DAV)
       (pcase status-symbol
	 ((or `no-content `reset-content)
	  ;; No new data, just stay at the same document
	  (url-mark-buffer-as-dead buffer))
	 (_
	  ;; Generic success for all others.  Store in the cache, and
	  ;; mark it as successful.
	  (widen)
	  (if (and url-automatic-caching (equal url-http-method "GET"))
	      (url-store-in-cache buffer))))
       (setq success t))
      (3				; Redirection
       ;; 300 Multiple choices
       ;; 301 Moved permanently
       ;; 302 Found
       ;; 303 See other
       ;; 304 Not modified
       ;; 305 Use proxy
       ;; 307 Temporary redirect
       (let ((redirect-uri (or (mail-fetch-field "Location")
			       (mail-fetch-field "URI"))))
	 (pcase status-symbol
	   (`multiple-choices	    ; 300
	    ;; Quoth the spec (section 10.3.1)
	    ;; -------------------------------
	    ;; The requested resource corresponds to any one of a set of
	    ;; representations, each with its own specific location and
	    ;; agent-driven negotiation information is being provided so
	    ;; that the user can select a preferred representation and
	    ;; redirect its request to that location.
	    ;; [...]
	    ;; If the server has a preferred choice of representation, it
	    ;; SHOULD include the specific URI for that representation in
	    ;; the Location field; user agents MAY use the Location field
	    ;; value for automatic redirection.
	    ;; -------------------------------
	    ;; We do not support agent-driven negotiation, so we just
	    ;; redirect to the preferred URI if one is provided.
	    (setq success t))
	   ((or `moved-permanently `found `temporary-redirect) ; 301 302 307
	    ;; If the 301|302 status code is received in response to a
	    ;; request other than GET or HEAD, the user agent MUST NOT
	    ;; automatically redirect the request unless it can be
	    ;; confirmed by the user, since this might change the
	    ;; conditions under which the request was issued.
	    (unless (member url-http-method '("HEAD" "GET"))
	      (setq redirect-uri nil)))
	   (`see-other			; 303
	    ;; The response to the request can be found under a different
	    ;; URI and SHOULD be retrieved using a GET method on that
	    ;; resource.
	    (setq url-http-method "GET"
		  url-http-data nil))
	   (`not-modified		; 304
	    ;; The 304 response MUST NOT contain a message-body.
	    (url-http-debug "Extracting document from cache... (%s)"
			    (url-cache-create-filename (url-view-url t)))
	    (url-cache-extract (url-cache-create-filename (url-view-url t)))
	    (setq redirect-uri nil
		  success t))
	   (`use-proxy			; 305
	    ;; The requested resource MUST be accessed through the
	    ;; proxy given by the Location field.  The Location field
	    ;; gives the URI of the proxy.  The recipient is expected
	    ;; to repeat this single request via the proxy.  305
	    ;; responses MUST only be generated by origin servers.
	    (error "Redirection thru a proxy server not supported: %s"
		   redirect-uri))
	   (_
	    ;; Treat everything like '300'
	    nil))
	 (when redirect-uri
	   ;; Clean off any whitespace and/or <...> cruft.
	   (if (string-match "\\([^ \t]+\\)[ \t]" redirect-uri)
	       (setq redirect-uri (match-string 1 redirect-uri)))
	   (if (string-match "^<\\(.*\\)>$" redirect-uri)
	       (setq redirect-uri (match-string 1 redirect-uri)))

	   ;; Some stupid sites (like sourceforge) send a
	   ;; non-fully-qualified URL (ie: /), which royally confuses
	   ;; the URL library.
	   (if (not (string-match url-nonrelative-link redirect-uri))
	       ;; Be careful to use the real target URL, otherwise we may
	       ;; compute the redirection relative to the URL of the proxy.
	       (setq redirect-uri
		     (url-expand-file-name redirect-uri url-http-target-url)))
	   (let ((url-request-method url-http-method)
		 (url-request-data url-http-data)
		 (url-request-extra-headers url-http-extra-headers))
	     ;; Check existing number of redirects
	     (if (or (< url-max-redirections 0)
		     (and (> url-max-redirections 0)
			  (let ((events (car url-callback-arguments))
				(old-redirects 0))
			    (while events
			      (if (eq (car events) :redirect)
				  (setq old-redirects (1+ old-redirects)))
			      (and (setq events (cdr events))
				   (setq events (cdr events))))
			    (< old-redirects url-max-redirections))))
		 ;; url-max-redirections hasn't been reached, so go
		 ;; ahead and redirect.
		 (progn
		   ;; Remember that the request was redirected.
		   (setf (car url-callback-arguments)
			 (nconc (list :redirect redirect-uri)
				(car url-callback-arguments)))
		   ;; Put in the current buffer a forwarding pointer to the new
		   ;; destination buffer.
		   ;; FIXME: This is a hack to fix url-retrieve-synchronously
		   ;; without changing the API.  Instead url-retrieve should
		   ;; either simply not return the "destination" buffer, or it
		   ;; should take an optional `dest-buf' argument.
		   (set (make-local-variable 'url-redirect-buffer)
			(url-retrieve-internal
			 redirect-uri url-callback-function
			 url-callback-arguments
			 (url-silent url-current-object)
			 (not (url-use-cookies url-current-object))))
		   (url-mark-buffer-as-dead buffer))
	       ;; We hit url-max-redirections, so issue an error and
	       ;; stop redirecting.
	       (url-http-debug "Maximum redirections reached")
	       (setf (car url-callback-arguments)
		     (nconc (list :error (list 'error 'http-redirect-limit
					       redirect-uri))
			    (car url-callback-arguments)))
	       (setq success t))))))
      (4				; Client error
       ;; 400 Bad Request
       ;; 401 Unauthorized
       ;; 402 Payment required
       ;; 403 Forbidden
       ;; 404 Not found
       ;; 405 Method not allowed
       ;; 406 Not acceptable
       ;; 407 Proxy authentication required
       ;; 408 Request time-out
       ;; 409 Conflict
       ;; 410 Gone
       ;; 411 Length required
       ;; 412 Precondition failed
       ;; 413 Request entity too large
       ;; 414 Request-URI too large
       ;; 415 Unsupported media type
       ;; 416 Requested range not satisfiable
       ;; 417 Expectation failed
       ;; 422 Unprocessable Entity (Added by DAV)
       ;; 423 Locked
       ;; 424 Failed Dependency
       (setq success
	     (pcase status-symbol
	       (`unauthorized			; 401
		;; The request requires user authentication.  The response
		;; MUST include a WWW-Authenticate header field containing a
		;; challenge applicable to the requested resource.  The
		;; client MAY repeat the request with a suitable
		;; Authorization header field.
		(url-http-handle-authentication nil))
	       (`payment-required              ; 402
		;; This code is reserved for future use
		(url-mark-buffer-as-dead buffer)
		(error "Somebody wants you to give them money"))
	       (`forbidden			; 403
		;; The server understood the request, but is refusing to
		;; fulfill it.  Authorization will not help and the request
		;; SHOULD NOT be repeated.
		t)
	       (`not-found			; 404
		;; Not found
		t)
	       (`method-not-allowed		; 405
		;; The method specified in the Request-Line is not allowed
		;; for the resource identified by the Request-URI.  The
		;; response MUST include an Allow header containing a list of
		;; valid methods for the requested resource.
		t)
	       (`not-acceptable		; 406
		;; The resource identified by the request is only capable of
		;; generating response entities which have content
		;; characteristics not acceptable according to the accept
		;; headers sent in the request.
		t)
	       (`proxy-authentication-required ; 407
		;; This code is similar to 401 (Unauthorized), but indicates
		;; that the client must first authenticate itself with the
		;; proxy.  The proxy MUST return a Proxy-Authenticate header
		;; field containing a challenge applicable to the proxy for
		;; the requested resource.
		(url-http-handle-authentication t))
	       (`request-timeout		; 408
		;; The client did not produce a request within the time that
		;; the server was prepared to wait.  The client MAY repeat
		;; the request without modifications at any later time.
		t)
	       (`conflict			; 409
		;; The request could not be completed due to a conflict with
		;; the current state of the resource.  This code is only
		;; allowed in situations where it is expected that the user
		;; might be able to resolve the conflict and resubmit the
		;; request.  The response body SHOULD include enough
		;; information for the user to recognize the source of the
		;; conflict.
		t)
	       (`gone                          ; 410
		;; The requested resource is no longer available at the
		;; server and no forwarding address is known.
		t)
	       (`length-required		; 411
		;; The server refuses to accept the request without a defined
		;; Content-Length.  The client MAY repeat the request if it
		;; adds a valid Content-Length header field containing the
		;; length of the message-body in the request message.
		;;
		;; NOTE - this will never happen because
		;; `url-http-create-request' automatically calculates the
		;; content-length.
		t)
	       (`precondition-failed		; 412
		;; The precondition given in one or more of the
		;; request-header fields evaluated to false when it was
		;; tested on the server.
		t)
	       ((or `request-entity-too-large `request-uri-too-large) ; 413 414
		;; The server is refusing to process a request because the
		;; request entity|URI is larger than the server is willing or
		;; able to process.
		t)
	       (`unsupported-media-type	; 415
		;; The server is refusing to service the request because the
		;; entity of the request is in a format not supported by the
		;; requested resource for the requested method.
		t)
	       (`requested-range-not-satisfiable ; 416
		;; A server SHOULD return a response with this status code if
		;; a request included a Range request-header field, and none
		;; of the range-specifier values in this field overlap the
		;; current extent of the selected resource, and the request
		;; did not include an If-Range request-header field.
		t)
	       (`expectation-failed		; 417
		;; The expectation given in an Expect request-header field
		;; could not be met by this server, or, if the server is a
		;; proxy, the server has unambiguous evidence that the
		;; request could not be met by the next-hop server.
		t)
	       (_
		;; The request could not be understood by the server due to
		;; malformed syntax.  The client SHOULD NOT repeat the
		;; request without modifications.
		t)))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
	 (setf (car url-callback-arguments)
	       (nconc (list :error (list 'error 'http url-http-response-status))
		      (car url-callback-arguments)))))
      (5
       ;; 500 Internal server error
       ;; 501 Not implemented
       ;; 502 Bad gateway
       ;; 503 Service unavailable
       ;; 504 Gateway time-out
       ;; 505 HTTP version not supported
       ;; 507 Insufficient storage
       (setq success t)
       (pcase url-http-response-status
	 (`not-implemented		; 501
	  ;; The server does not support the functionality required to
	  ;; fulfill the request.
	  nil)
	 (`bad-gateway			; 502
	  ;; The server, while acting as a gateway or proxy, received
	  ;; an invalid response from the upstream server it accessed
	  ;; in attempting to fulfill the request.
	  nil)
	 (`service-unavailable		; 503
	  ;; The server is currently unable to handle the request due
	  ;; to a temporary overloading or maintenance of the server.
	  ;; The implication is that this is a temporary condition
	  ;; which will be alleviated after some delay.  If known, the
	  ;; length of the delay MAY be indicated in a Retry-After
	  ;; header.  If no Retry-After is given, the client SHOULD
	  ;; handle the response as it would for a 500 response.
	  nil)
	 (`gateway-timeout		; 504
	  ;; The server, while acting as a gateway or proxy, did not
	  ;; receive a timely response from the upstream server
	  ;; specified by the URI (e.g. HTTP, FTP, LDAP) or some other
	  ;; auxiliary server (e.g. DNS) it needed to access in
	  ;; attempting to complete the request.
	  nil)
	 (`http-version-not-supported	; 505
	  ;; The server does not support, or refuses to support, the
	  ;; HTTP protocol version that was used in the request
	  ;; message.
	  nil)
	 (`insufficient-storage		; 507 (DAV)
	  ;; The method could not be performed on the resource
	  ;; because the server is unable to store the representation
	  ;; needed to successfully complete the request.  This
	  ;; condition is considered to be temporary.  If the request
	  ;; which received this status code was the result of a user
	  ;; action, the request MUST NOT be repeated until it is
	  ;; requested by a separate user action.
	  nil))
       ;; Tell the callback that an error occurred, and what the
       ;; status code was.
       (when success
	 (setf (car url-callback-arguments)
	       (nconc (list :error (list 'error 'http url-http-response-status))
		      (car url-callback-arguments)))))
      (_
       (error "Unknown class of HTTP response code: %d (%d)"
	      class url-http-response-status)))
    (if (not success)
	(url-mark-buffer-as-dead buffer)
      (url-handle-content-transfer-encoding))
    (url-http-debug "Finished parsing HTTP headers: %S" success)
    (widen)
    (goto-char (point-min))
    success))


(provide 'init-utils)
;;; init-utils ends here
