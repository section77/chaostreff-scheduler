#!/bin/sh
#|
#
# **chaostreff-scheduler** schedules Chaostreff and Tech-Events in our calendar.
#
# Usage:
#
#   ./chaostreff-scheduler.scm <GIT_URL> [<WORK_DIR>]
#
#   <GIT_URL> is the URL to the 'website' src repository.
#     Write access is necessary to push the changes.
#     If it's on GitHub, use somethink like:
#       https://<TOKEN>:@github.com/section77/website
#
#   <WORK_DIR> is the directory where the website
#     checkout lives and the files are generated.
#     DEFAULT: '/website'
exec csi -s "$0" "$@"
|#
(use utils posix files srfi-1 matchable)
(include "timestamp.scm")
(import timestamp)

                                        ;
;;
;; record to hold an event
;;
(define-record event title date tmpl-file)
(define-record-printer (event e out)
  (fprintf out "#,(event ~s ~s ~s)"
	   (event-title e) (seconds->string (event-date e)) (event-tmpl-file e)))



;;
;; returns a list with chaostreff-events for the given month-offset
;;
(define (chaostreff-events month-offset)
  (define filter-odds
    (match-lambda
     [(y _ . ys) (cons y (filter-odds ys))]
     [ys         ys]))

  (let* ([timestamps   (month-timestamps month-offset)]
	 [tuesdays     (filter tuesday? timestamps)]
	 [event-dates  (map (lambda(ts) (set-time ts 20 0 0)) (filter-odds tuesdays))])
    (map (lambda(d) (make-event "Chaostreff" d "contents-chaostreff.tmpl")) event-dates)))



;;
;; returns the tech-event for the given month offset
;;
(define (tech-event month-offset)
  (let* ([timestamps (month-timestamps month-offset)]
	 [saturdays  (filter saturday? timestamps)]
	 [event-date (set-time (car saturdays) 14 0 0)])
    (make-event "Tech-Event" event-date "contents-tech-event.tmpl")))



;;
;; create an calendar entry from the given event
;;
(define (create-calendar-entry-for-event event dest)

  (define template-path (sprintf "~A/templates/chaostreff-scheduler/~A"
                                 dest
                                 (event-tmpl-file event)))

  (define contents-path (sprintf "~A/content/kalender/~A/~A/~A"
                                 dest
                                 (ts-fmt (event-date event) "%Y")
                                 (ts-fmt (event-date event) "%Y-%m-%d")
                                 "contents.lr"))

  (define content (string-translate* (read-all template-path)
                                 `(("{{title}}" . ,(event-title event))
                                   ("{{date}}"  . ,(ts-fmt (event-date event) "%Y-%m-%d %H:%M")))))

  (or (file-exists? contents-path)
      (begin
        (print "    create event: " (event-title event) " on: " (seconds->string (event-date event)))
        (create-directory (pathname-directory contents-path) #t)
        (with-output-to-file contents-path (lambda() (print content))))))






;;
;; action!
;;
;;   * parse command line arguments
;;   * pull website
;;   * create missing events
;;   * push website
;;
(let-values ([(website-vc website-work-dir) (match (command-line-arguments)
                                              [(website-vc website-work-dir) (values website-vc website-work-dir)]
                                              [(website-vc)                  (values website-vc "/website")]
                                              [else                          (begin
                                                                               (print "ABORT: usage: <GIT_URL> [<WORK_DIR>]")
                                                                               (exit 1))])])
  ;;
  ;; clone or update website src code
  ;;
  (let-values ([(_ _ exit-code) (process-wait (process-run "git"
                                                           (if (file-exists? (make-pathname website-work-dir ".git"))
                                                               (begin
                                                                 (print "update website repository")
                                                                 `("-C" ,website-work-dir "pull" ,website-vc))
                                                               (begin
                                                                 (print "clone website repository")
                                                                 `("clone" ,website-vc ,website-work-dir)))))])
    (or (zero? exit-code) (error "git failed")))
  (change-directory website-work-dir)



  ;;
  ;; create all missing calendar entries
  ;;
  (print "create calendar entries")
  (do ([month-offset 0 (+ month-offset 1)]) [(>= month-offset 3)]
    (print "month-offset: " month-offset)
    (print "  schedule tech-event")
    (create-calendar-entry-for-event (tech-event month-offset) website-work-dir)

    (print "  schedule chaostreffs")
    (for-each (lambda(e) (create-calendar-entry-for-event e website-work-dir)) (chaostreff-events month-offset)))


  ;;
  ;; push the website code if somethink has changed
  ;;
  (or (string-null? (call-with-input-pipe (sprintf "git status -s ~A" website-work-dir) read-all))
      (begin
        (print "push changes")
        (print (call-with-input-pipe "git add -A && git commit -q -m 'chaostreff scheduled' && git push" read-all))))

  (print "done"))



;; Local Variables:
;; mode: scheme
;; End:
