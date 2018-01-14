;;
;; simple module to work with timestamps
;;
(module
 timestamp
 (month-timestamps ts-get ts-fmt set-time
  sunday? monday? tuesday? wednesday? thursday? friday? saturday?)

 (import scheme chicken)
 (use posix matchable)


 ;; returns a list of timestamps for a month
 (define (month-timestamps month-offset)
   (define (next-day ts)
     (+ ts (* 60 60 24)))

   (define month (+ month-offset (ts-get (current-seconds) 'month)))
   (define ts-start (ts-set (ts-set (current-seconds) 'day 1) 'month month))

   (let loop ([ts ts-start])
     (if (= (ts-get ts 'month) month)
	 (cons ts (loop (next-day ts)))
	 '())))


 (define (sunday? ts)
   (= (ts-get ts 'wday) 0))
 (define (monday? ts)
   (= (ts-get ts 'wday) 1))
 (define (tuesday? ts)
   (= (ts-get ts 'wday) 2))
 (define (wednesday? ts)
   (= (ts-get ts 'wday) 3))
 (define (thursday? ts)
   (= (ts-get ts 'wday) 4))
 (define (friday? ts)
   (= (ts-get ts 'wday) 5))
 (define (saturday? ts)
   (= (ts-get ts 'wday) 6))


 ;; format the given timestamp
 (define (ts-fmt ts fmt)
   (time->string (seconds->local-time ts) fmt))


 ;; get an part from the given timestamp
 (define (ts-get ts k)
   (vector-ref
    (seconds->local-time ts)
    (case k
      ['seconds  0]
      ['minutes  1]
      ['hours    2]
      ['day      3]
      ['month    4]
      ['year     5]
      ['wday     6]
      [else      (error "unexpeced key" (symbol->string k))])))


 ;; set the time-part from a given timestamp
 (define (set-time ts hh mm ss)
   (ts-set (ts-set (ts-set ts 'hours hh) 'minutes mm) 'seconds ss))

 ;; set an part from the given timestamp
 ;;
 ;; this is very very very dirty / not save!
 (define (ts-set ts k v)
   (let ([lt (seconds->local-time ts)])
     (vector-set!
      lt
      (case k
	['seconds  0]
	['minutes  1]
	['hours    2]
	['day      3]
	['month    4]
	['year     5]
	[else      (error "unsupported key" (symbol->string k))])
      v)
     (local-time->seconds lt)))
 )
