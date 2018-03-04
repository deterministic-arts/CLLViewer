
(in-package "CLL-CLIM-UTILITIES")

(defparameter *hierarchy-line-style*
  (make-line-style :thickness 1 :dashes '(2 1)))

(defun format-hierarchy-from-roots (roots list-children 
                                    &key
                                      (printer #'princ) (stream *standard-output*)
                                      (indentation-step 32) (line-style *hierarchy-line-style*)
                                      (line-ink +gray75+) (y-spacing 0)
                                      (move-cursor t))
  (multiple-value-bind (initial-x initial-y) (stream-cursor-position stream)
    (let ((root-record
           (labels
               ((continuation (stream root-record)
                  (with-output-recording-options (stream :draw nil :record t)
                    (let ((root-nodes (mapcar #'materialize-subtree roots))
                          (first t)
                          (cy initial-y))
                      (dolist (node root-nodes)
                        (if first (setf first nil) (incf cy y-spacing))
                        (setf cy (layout-subtree root-record initial-x cy node))
                        (connect-subtree node)))))
                (materialize-subtree (node)
                  (cons (with-output-to-output-record (stream)
                          (funcall printer node stream))
                        (mapcar #'materialize-subtree
                                (funcall list-children node))))
                (layout-subtree (root-record cx cy node)
                  (let ((head (car node)))
                    (setf (output-record-position head) (values cx cy))
                    (add-output-record head root-record)
                    (with-bounding-rectangle* (x1 y1 x2 y2) head
                      (declare (ignore x1 x2))
                      (let ((height (- y2 y1)))
                        (incf cy height)
                        (incf cx indentation-step)
                        (dolist (child (cdr node))
                          (incf cy y-spacing)
                          (setf cy (layout-subtree root-record cx cy child)))
                        cy))))
                (connect-subtree (node)
                  (when (cdr node)
                    (with-bounding-rectangle* (px1 py1 px2 py2) (car node)
                      (declare (ignore py1 px2))
                      (let ((last-child (caar (last (cdr node)))))
                        (with-bounding-rectangle* (cx1 cy1 cx2 cy2) last-child
                          (declare (ignore cx2))
                          (let ((mx (/ (+ px1 cx1) 2))
                                (my (/ (+ cy1 cy2) 2)))
                            (draw-line* stream mx py2 mx my :ink line-ink :line-style line-style)
                            (dolist (child (cdr node))
                              (with-bounding-rectangle* (cx1 cy1 cx2 cy2) (car child)
                                (declare (ignore cx2))
                                (let ((vy (/ (+ cy1 cy2) 2)))
                                  (draw-line* stream mx vy cx1 vy :ink line-ink :line-style line-style)))
                              (connect-subtree child)))))))))
                            
             (invoke-with-new-output-record stream #'continuation
                                            'standard-sequence-output-record))))
      (setf (output-record-position root-record) (values initial-x initial-y))
      (when (and (stream-drawing-p stream)
                 (climi::output-record-ancestor-p (stream-output-history stream) root-record))
        (with-output-recording-options (stream :draw t :record nil)
          (replay root-record stream)))
      (when move-cursor
        (setf (stream-cursor-position stream)
              (values (bounding-rectangle-max-x root-record)
                      (bounding-rectangle-max-y root-record))))
      root-record)))
