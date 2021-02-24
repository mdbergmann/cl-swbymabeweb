(defpackage :dungeon
  (:use :cl :arrows))

(in-package :dungeon)

(defparameter *difficulties* '(not-difficult-at-all
                               very-difficult))

(defstruct monster
  (type nil :type symbol)
  (creepy-factor 0 :type integer))

(defparameter *monsters* (list
                          (make-monster :type 'jade-warrior :creepy-factor 2)
                          (make-monster :type 'mercenary :creepy-factor 4)
                          (make-monster :type 'black-knight :creepy-factor 4)
                          (make-monster :type 'huge-spider :creepy-factor 9)))

(defparameter *special-items* '(ring-bronze
                                longsword-bronze
                                longbow-bronze
                                ring-silver
                                longsword-silver
                                longbow-silver
                                harkins-flute))

(defclass dungeon ()
  ((difficulty :initform 'not-difficult-at-all)
   (monsters :initform nil)
   (special-items :initform nil)))
(defclass castle-dungeon (dungeon) ())
(defclass cellar-dungeon (dungeon) ())

(defun make-dungeon (&key type)
  (make-instance (ecase type
                   (castle 'castle-dungeon)
                   (cellar 'cellar-dungeon))))

;; builder protocol
(defgeneric set-difficulty (dungeon difficulty))
(defgeneric add-monsters (dungeon amount))
(defgeneric add-special-items (dungeon amount))

(defmethod set-difficulty ((obj dungeon) diff)
  (with-slots (difficulty) obj
    (setf difficulty diff))
  obj)

(defmethod add-monsters ((obj dungeon) amount)
  (with-slots (monsters) obj
    (setf monsters
          (loop :repeat amount
                :collect (nth (random (length *monsters*)) *monsters*))))
  obj)

(defmethod add-special-items ((obj dungeon) amount)
  (with-slots (special-items) obj
    (setf special-items
          (loop :repeat amount
                :collect (nth (random (length *special-items*)) *special-items*))))
  obj)

;; specialized for 'castle-dungeon
(defmethod add-monsters ((obj castle-dungeon) amount)
  (with-slots (monsters) obj
    ;; set a bunch of nice looking monsters
    (setf monsters
          (filter-monsters-by-creepy-factor 5 #'< amount *monsters*)))
  obj)

;; specialized for 'cellar-dungeon
(defmethod add-monsters ((obj cellar-dungeon) amount)
  (with-slots (monsters) obj
    ;; set a bunch of creepy monsters
    (setf monsters
          (filter-monsters-by-creepy-factor 5 #'>= amount *monsters*)))
  obj)

(defun filter-monsters-by-creepy-factor (creepy-factor pred amount monsters)
  (loop :repeat amount
        :with filtered-monsters = (remove-if
                                   (lambda (monster)
                                     (funcall pred (monster-creepy-factor monster)
                                              creepy-factor))
                                   monsters)
        :with filtered-length = (length filtered-monsters)
        :collect (nth (random filtered-length) filtered-monsters)))

(let ((dungeon (make-dungeon :type 'castle)))
  (set-difficulty dungeon 'very-difficult)
  (add-monsters dungeon 15)
  (add-special-items dungeon 5)
  ;; do something with dungeon
  (format t "Created dungeon: ~a~%" dungeon)
  dungeon)

(let ((dungeon (-> (make-dungeon :type 'cellar)
                   (set-difficulty 'very-difficult)
                   (add-monsters 15)
                   (add-special-items 5))))
  ;; do something with dungeon
  (format t "Created dungeon: ~a~%" dungeon)
  dungeon)
