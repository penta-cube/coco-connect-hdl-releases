;;; bridge.il - coco-connect-hdl SKILL bridge entrypoint (file-based IPC)
;;;
;;; Start from HDL CommandConsole:
;;;   system("start /b cnskill.exe -i -nongraph \"C:\\path\\to\\bridge.il\"")
;;;
;;; If bridge.il is loaded from another wrapper, set _coco_bridge_dir to the
;;; directory containing this file so the module files can be loaded.

;;; Configuration

(unless (boundp '_coco_pipe_base)
  (setq _coco_pipe_base "coco-hdl"))
(unless (boundp '_coco_instance_id)
  (setq _coco_instance_id ""))
(unless (boundp '_coco_token)
  (setq _coco_token ""))
(unless (boundp '_coco_work_dir)
  (setq _coco_work_dir "C:\\Temp"))
(unless (boundp '_coco_bridge_dir)
  (setq _coco_bridge_dir "."))

(setq _coco_bridge_version "2026-04-22")

(unless (isDir _coco_work_dir)
  (createDir _coco_work_dir))

;;; Module loading

(defun _coco_module_file (name)
  (strcat _coco_bridge_dir "\\" name))

(load (_coco_module_file "util.il"))
(load (_coco_module_file "hdl.il"))
(load (_coco_module_file "read_model.il"))
(load (_coco_module_file "component_report.il"))
(load (_coco_module_file "schematic_validation.il"))
(load (_coco_module_file "component_detail.il"))
(load (_coco_module_file "page_summary.il"))

;;; File paths

(defun _coco_req_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-req.txt"))

(defun _coco_res_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-res.txt"))

(defun _coco_ready_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-rdy.txt"))

(defun _coco_lock_file ()
  (strcat _coco_work_dir "\\" _coco_pipe_base "-lock.txt"))

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

;;; Operations

(defun _coco_op_ping ()
  "pong")

(defun _coco_op_status ()
  (let ((status (_coco_import_status)))
    (strcat
      (if (car status) "1" "0")
      "|"
      (_coco_to_string (cadr status)))))

(defun _coco_dispatch (id token op arg)
  (if (and (not (equal _coco_token ""))
           (not (equal token _coco_token)))
    (_coco_send_error id "AUTH_FAILED" "token mismatch")
    (let ((result
            (errset
              (cond
                ((equal op "ping") (_coco_op_ping))
                ((equal op "status") (_coco_op_status))
                ((equal op "read_model") (_coco_op_read_model))
                ((equal op "component_report") (_coco_op_component_report))
                ((equal op "schematic_validation") (_coco_op_schematic_validation))
                ((equal op "component_detail") (_coco_op_component_detail arg))
                ((equal op "page_summary") (_coco_op_page_summary))
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
        (printf "bridge_dir: %s\n" _coco_bridge_dir)
        (printf "work_dir:   %s\n" _coco_work_dir)
        (printf "ready:      %s\n" ready_file)
        (printf "req:        %s\n" req_file)
        (printf "res:        %s\n" (_coco_res_file))
        (printf "lock:       %s\n" lock_file)
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
