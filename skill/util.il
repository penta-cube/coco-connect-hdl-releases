;;; util.il - shared helpers for coco-connect-hdl bridge

(defun _coco_clone_string (str)
  (let ((result "")
        (i 1)
        (len 0))
    (setq len (strlen str))
    (while (leqp i len)
      (setq result (strcat result (substring str i 1)))
      (setq i (add1 i)))
    result))

(defun safeStr (value)
  (let ((result nil))
    (cond
      ((not value) "")
      ((stringp value) (_coco_clone_string value))
      (t
        (setq result (errset (sprintf nil "%L" value) nil))
        (if (and result (stringp (car result)))
          (_coco_clone_string (car result))
          "<unprintable>")))))

(defun _coco_to_string (value)
  (safeStr value))

(defun safeGetProp (prop propName)
  (let ((requested (safeStr propName))
        (name "")
        (value "")
        (result nil))
    (when prop
      (setq result (errset (cnGetPropName prop) nil))
      (when result
        (setq name (safeStr (car result))))
      (setq result (errset (cnGetPropValue prop) nil))
      (when result
        (setq value (safeStr (car result)))))
    (cond
      ((or (equal requested "") (equal requested "value")) value)
      ((equal requested "name") name)
      ((equal requested name) value)
      (t ""))))

(defun _coco_safe_format (fmt args)
  (let ((str (safeStr fmt))
        (result "")
        (rest args)
        (i 1)
        (len 0)
        (ch "")
        (spec ""))
    (setq len (strlen str))
    (while (leqp i len)
      (setq ch (substring str i 1))
      (if (and (equal ch "%") (lessp i len))
        (progn
          (setq i (add1 i))
          (setq spec (substring str i 1))
          (cond
            ((equal spec "%")
              (setq result (strcat result "%")))
            ((or (equal spec "s") (equal spec "L") (equal spec "d"))
              (if rest
                (progn
                  (setq result (strcat result (safeStr (car rest))))
                  (setq rest (cdr rest)))
                (setq result (strcat result "%" spec))))
            (t
              (setq result (strcat result "%" spec)))))
        (setq result (strcat result ch)))
      (setq i (add1 i)))
    (foreach arg rest
      (setq result (strcat result " " (safeStr arg))))
    result))

(defun safePrintf (fmt @rest args)
  (let ((result nil))
    (setq result (errset (printf "%s" (_coco_safe_format fmt args)) nil))
    (if result t nil)))

(defun _coco_log_error (context detail)
  (safePrintf "[coco-connect-hdl] ERROR %s: %s\n" (safeStr context) (safeStr detail)))

(defun _coco_trim_newline (str)
  (let ((len (strlen str))
        (result str))
    (when (and (greaterp len 0)
               (equal (substring result len len) "\n"))
      (setq result (substring result 1 (sub1 len)))
      (setq len (sub1 len)))
    (when (and (greaterp len 0)
               (equal (substring result len len) "\r"))
      (setq result (substring result 1 (sub1 len))))
    result))

(defun _coco_join_list (lst sep)
  (let ((result "")
        (first t))
    (foreach item lst
      (if first
        (setq result (_coco_to_string item))
        (setq result (strcat result sep (_coco_to_string item))))
      (setq first nil))
    result))

(defun _coco_member_string (value lst)
  (let ((found nil))
    (foreach item lst
      (when (equal item value)
        (setq found t)))
    found))

(defun _coco_merge_unique (base extra)
  (let ((result base))
    (foreach item extra
      (when (and (not (equal item ""))
                 (not (_coco_member_string item result)))
        (setq result (append result (list item)))))
    result))

(defun _coco_json_string (value)
  (let ((str (_coco_to_string value))
        (result "\"")
        (i 1)
        (len 0)
        (ch ""))
    (setq len (strlen str))
    (while (leqp i len)
      (setq ch (substring str i 1))
      (cond
        ((equal ch "\\") (setq result (strcat result "\\\\")))
        ((equal ch "\"") (setq result (strcat result "\\\"")))
        ((equal ch "\n") (setq result (strcat result "\\n")))
        ((equal ch "\r") (setq result (strcat result "\\r")))
        (t (setq result (strcat result ch))))
      (setq i (add1 i)))
    (strcat result "\"")))
