;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Message class

(defclass message ()
  ((role :initarg :role
         :accessor message-role
         :type (member :system :user :assistant :debug))
   (content :initarg :content
            :accessor message-content
            :type string)
   (timestamp :initarg :timestamp
              :accessor message-timestamp
              :initform (get-universal-time))))

(defun make-message (role content)
  "Create a new message."
  (make-instance 'message :role role :content content))

;;; Conversation class

(defvar *conversation-id-counter* 0)

(defclass conversation ()
  ((id :initarg :id
       :accessor conversation-id
       :initform (format nil "conv-~D" (incf *conversation-id-counter*)))
   (messages :initform nil
             :accessor conversation-messages
             :documentation "List of messages (append to end)")
   (context-manager :initarg :context
                    :accessor conversation-context
                    :initform (make-instance 'context-manager))
   (created-at :initform (get-universal-time)
               :accessor conversation-created-at)
   (project :initarg :project
            :accessor conversation-project
            :initform nil
            :documentation "Project identifier for persistence")))

;;; Conversation methods

(defgeneric add-message (conversation role content)
  (:documentation "Add a message to the conversation."))

(defmethod add-message ((conv conversation) role content)
  "Add message with role and content."
  (let ((msg (make-message role content)))
    (setf (conversation-messages conv)
          (append (conversation-messages conv) (list msg)))
    msg))

(defgeneric get-messages (conversation &key limit)
  (:documentation "Get messages, optionally limited to last N."))

(defmethod get-messages ((conv conversation) &key limit)
  "Get messages from conversation."
  (let ((msgs (conversation-messages conv)))
    (if limit
        (subseq msgs (max 0 (- (length msgs) limit)))
        msgs)))

(defgeneric clear-conversation (conversation)
  (:documentation "Clear messages but preserve context manager."))

(defmethod clear-conversation ((conv conversation))
  "Clear conversation messages."
  (setf (conversation-messages conv) nil))

(defun new-conversation (&key project)
  "Create a new conversation."
  (make-instance 'conversation :project project))
