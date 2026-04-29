;;; bridge.il - coco-connect-hdl SKILL bridge (file-based IPC)
;;;
;;; SKILL infile/outfile cannot open Windows named pipes, so this bridge
;;; uses plain files in %TEMP% for request/response exchange.
;;;
;;; File layout:
;;;   <base>-rdy.txt   written by coco-connect-hdl on startup
;;;   <base>-req.txt   written by coco-connect-hdl, read by SKILL
;;;   <base>-res.txt   written by SKILL, read by coco-connect-hdl
;;;   <base>-lock.txt  written by SKILL while one bridge instance is active
;;;
;;; Start from HDL CommandConsole:
;;;   system("start /b cnskill.exe -i -nongraph \"C:\\path\\to\\bridge.il\"")

;;; Configuration

(unless (boundp '_coco_pipe_base)
  (setq _coco_pipe_base "coco-hdl"))
(unless (boundp '_coco_instance_id)
  (setq _coco_instance_id ""))
(unless (boundp '_coco_token)
  (setq _coco_token ""))
(unless (boundp '_coco_work_dir)
  (setq _coco_work_dir "C:\\Temp"))

(setq _coco_bridge_version "2026-04-22")
(setq _coco_run_opts (cnSetRunOptions ?result cnOUT_VERBOSE))

(unless (isDir _coco_work_dir)
  (createDir _coco_work_dir))

;;; File paths

(defun _coco_req_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-req.txt"))

(defun _coco_res_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-res.txt"))

(defun _coco_ready_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-rdy.txt"))

(defun _coco_lock_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-lock.txt"))

;;; String helpers

(defun _coco_clone_string (str)
  (let ((result "")
        (i 1)
        (len 0))
    (setq len (strlen str))
    (while (leqp i len)
      (setq result (strcat result (substring str i 1)))
      (setq i (add1 i)))
    result))

(defun _coco_to_string (value)
  (cond
    ((not value) "")
    ((stringp value) (_coco_clone_string value))
    (t (_coco_clone_string (sprintf nil "%L" value)))))

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

;;; Protocol

(defun _coco_read_line (fp)
  (let ((line ""))
    (errset (gets line fp) t)
    line))

(defun _coco_read_request (fp)
  (list
    (_coco_trim_newline (_coco_read_line fp))
    (_coco_trim_newline (_coco_read_line fp))
    (_coco_trim_newline (_coco_read_line fp))
    (_coco_trim_newline (_coco_read_line fp))))

(defun _coco_send_response (id status data)
  (errset (deleteFile (_coco_res_file)) t)
  (let ((fp (outfile (_coco_res_file))))
    (when fp
      (fprintf fp "%s\t%s\t%s\n" id status (_coco_to_string data))
      (close fp))))

(defun _coco_send_ok (id data)
  (_coco_send_response id "ok" data))

(defun _coco_send_error (id code msg)
  (_coco_send_response id code msg))

;;; Bridge ownership

(defun _coco_acquire_lock (path)
  (if (isFile path)
    nil
    (let ((fp (outfile path)))
      (when fp
        (fprintf fp "bridge=%s\n" _coco_bridge_version)
        (close fp))
      (isFile path))))

(defun _coco_release_lock (path)
  (when (isFile path)
    (errset (deleteFile path) t)))

;;; HDL helpers

(defun _coco_send_cmd (handle cmd)
  (cnSendCommand handle cmd ?runOptions _coco_run_opts))

(defun _coco_import_status ()
  (let ((r (errset (cnmpsImport) t)))
    (cond
      ((not r) (list nil "cnmpsImport raised an error"))
      ((car r) (list t ""))
      (t (list nil (sprintf nil "cnmpsImport returned %L" (car r)))))))

(defun _coco_import_handle ()
  (let ((status (_coco_import_status)))
    (if (car status)
      (car (errset (cnmpsImport) t))
      (error "HDL_NOT_EXPORTED" (cadr status)))))

(defun _coco_collect_prop_values (props allowed_names)
  (let ((values nil)
        (name "")
        (value ""))
    (foreach prop props
      (setq name (_coco_to_string (cnGetPropName prop)))
      (when (_coco_member_string name allowed_names)
        (setq value (_coco_to_string (cnGetPropValue prop)))
        (when (and (not (equal value ""))
                   (not (_coco_member_string value values)))
          (setq values (cons value values)))))
    (reverse values)))

(defun _coco_collect_values_from_find (handle find_cmd allowed_names)
  (let ((find_result nil)
        (group nil)
        (group_obj nil)
        (props nil)
        (values nil))
    (setq find_result (_coco_send_cmd handle find_cmd))
    (setq group (cnGetGroupName find_result))
    (when group
      (setq group_obj (cnGetGroupedObjectSet find_result))
      (when group_obj
        (setq props (cnGetProps group_obj))
        (setq values (_coco_collect_prop_values props allowed_names)))
      (errset (_coco_send_cmd handle (sprintf nil "EXCLUDE %s %s" group group)) t))
    values))

;;; Operations

(defun _coco_op_ping ()
  "pong")

(defun _coco_op_status ()
  (let ((status (_coco_import_status)))
    (strcat
      (if (car status) "1" "0")
      "|"
      (_coco_to_string (cadr status)))))

(defun _coco_op_list_nets ()
  (let ((handle (_coco_import_handle))
        (values nil))
    (setq values (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
    (_coco_join_list values "|")))

(defun _coco_op_list_parts ()
  (let ((handle (_coco_import_handle))
        (values nil)
        (extra_values nil))
    (setq values (_coco_collect_values_from_find handle "find cds_location=*" (list "CDS_LOCATION")))
    (setq extra_values (_coco_collect_values_from_find handle "find $location=*" (list "$LOCATION")))
    (setq values (_coco_merge_unique values extra_values))
    (_coco_join_list values "|")))

(defun _coco_dispatch (id token op arg)
  (if (and (not (equal _coco_token ""))
           (not (equal token _coco_token)))
    (_coco_send_error id "AUTH_FAILED" "token mismatch")
    (let ((result
            (errset
              (cond
                ((equal op "ping") (_coco_op_ping))
                ((equal op "status") (_coco_op_status))
                ((equal op "list_nets") (_coco_op_list_nets))
                ((equal op "list_parts") (_coco_op_list_parts))
                (t (error "UNKNOWN_OP" (_coco_to_string op))))
              t)))
      (if result
        (_coco_send_ok id (car result))
        (_coco_send_error id "SKILL_EVAL_FAILED" "SKILL evaluation failed")))))

;;; Main loop

(defun _coco_run ()
  (let ((ready_file (_coco_ready_file))
        (req_file (_coco_req_file))
        (lock_file (_coco_lock_file))
        (running t)
        (last_id "")
        (fp nil)
        (parsed nil)
        (id "")
        (token "")
        (op "")
        (arg ""))
    (if (not (_coco_acquire_lock lock_file))
      (printf "Another bridge instance already owns %s\n" lock_file)
      (progn
        (printf "coco-connect-hdl bridge %s\n" _coco_bridge_version)
        (printf "work_dir: %s\n" _coco_work_dir)
        (printf "ready:    %s\n" ready_file)
        (printf "req:      %s\n" req_file)
        (printf "res:      %s\n" (_coco_res_file))
        (printf "lock:     %s\n" lock_file)
        (printf "Waiting for ready file...\n")

        (while (not (isFile ready_file)) nil)
        (printf "Connected. Starting bridge loop...\n")

        (while running
          (if (isFile req_file)
            (progn
              (setq fp (infile req_file))
              (when fp
                (setq parsed (_coco_read_request fp))
                (close fp)
                (setq id (car parsed))
                (setq token (cadr parsed))
                (setq op (caddr parsed))
                (setq arg (cadddr parsed))
                (unless (equal id last_id)
                  (setq last_id id)
                  (if (equal op "quit")
                    (progn
                      (_coco_send_ok id "bye")
                      (setq running nil))
                    (_coco_dispatch id token op arg)))))
            (setq last_id "")))

        (_coco_release_lock lock_file)))))

(_coco_run)
(printf "Bridge exiting.\n")
