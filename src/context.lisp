;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; ID generation
(defvar *context-id-counter* 0)

(defun generate-id ()
  "Generate unique ID for context items."
  (format nil "ctx-~D" (incf *context-id-counter*)))

;;; Context item class

(defclass context-item ()
  ((id :initarg :id
       :accessor context-item-id
       :initform (generate-id))
   (item-type :initarg :type
              :accessor context-item-type
              :type (member :code :text :file :repl-history :error :custom)
              :documentation "Type of context item")
   (content :initarg :content
            :accessor context-item-content
            :type string
            :documentation "The actual content")
   (metadata :initarg :metadata
             :accessor context-item-metadata
             :initform nil
             :documentation "Plist of metadata: :filename, :start-line, :end-line, :package, etc.")
   (timestamp :initarg :timestamp
              :accessor context-item-timestamp
              :initform (get-universal-time))))

(defun make-context-item (content &key (type :code) metadata)
  "Create a new context item."
  (make-instance 'context-item
                 :content content
                 :type type
                 :metadata metadata))

;;; Context manager class

(defclass context-manager ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor context-items)
   (max-items :initarg :max-items
              :initform 50
              :accessor context-max-items
              :documentation "Sliding window size")))

;;; Context methods

(defgeneric add-context (manager item-or-content &key type metadata)
  (:documentation "Add context item to manager. Can pass context-item or raw content."))

(defmethod add-context ((manager context-manager) (item context-item) &key type metadata)
  "Add existing context-item to manager."
  (declare (ignore type metadata))
  (let ((items (context-items manager)))
    ;; Check if we've exceeded max
    (when (>= (length items) (context-max-items manager))
      ;; Remove oldest (shift array)
      (setf items (subseq items 1))
      (setf (context-items manager)
            (make-array (length items)
                       :adjustable t
                       :fill-pointer (length items)
                       :initial-contents items)))
    ;; Add new item
    (vector-push-extend item (context-items manager))))

(defmethod add-context ((manager context-manager) (content string) &key (type :code) metadata)
  "Add content as new context item."
  (let ((item (make-context-item content :type type :metadata metadata)))
    (add-context manager item)))

(defgeneric clear-context (manager)
  (:documentation "Clear all context items."))

(defmethod clear-context ((manager context-manager))
  "Clear all context items."
  (setf (context-items manager)
        (make-array 0 :adjustable t :fill-pointer 0)))

(defgeneric get-context (manager &key types limit)
  (:documentation "Get context items, optionally filtered by types and limited in count."))

(defmethod get-context ((manager context-manager) &key types limit)
  "Get context items."
  (let* ((items (coerce (context-items manager) 'list))
         (filtered (if types
                      (remove-if-not (lambda (item)
                                      (member (context-item-type item) types))
                                    items)
                      items))
         (limited (if limit
                     (subseq filtered (max 0 (- (length filtered) limit)))
                     filtered)))
    limited))

(defgeneric context-to-string (manager)
  (:documentation "Format context for inclusion in LLM prompt."))

(defmethod context-to-string ((manager context-manager))
  "Format all context items as markdown for LLM."
  (with-output-to-string (s)
    (format s "## Context~%~%")
    (loop for item across (context-items manager)
          for type = (context-item-type item)
          for content = (context-item-content item)
          for metadata = (context-item-metadata item)
          do
          (format s "### ~A"
                  (string-capitalize (symbol-name type)))
          ;; Add metadata to header if present
          (when metadata
            (let ((filename (getf metadata :filename))
                  (start-line (getf metadata :start-line))
                  (end-line (getf metadata :end-line)))
              (when filename
                (format s " (from ~A" filename)
                (when (and start-line end-line)
                  (format s ":~D-~D" start-line end-line))
                (format s ")"))))
          (format s "~%```lisp~%~A~%```~%~%" content))))
