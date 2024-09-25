#lang scribble/manual

@(require (for-label (only-in gregor moment? now/moment)
                     racket
                     sentry
                     web-server/http/request-structs))

@title{Sentry SDK}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[sentry]

@section[#:tag "intro"]{Introduction}

@(define sentry "https://sentry.io")

This library provides an interface for capturing and sending errors to
either a managed or a self-hosted @hyperlink[sentry]{Sentry} instance.

@local-table-of-contents[]

@section[#:tag "quickstart"]{Quickstart}

Install the package from the package server with

@verbatim{$ raco pkg install sentry}

Call @racket[make-sentry] to create an instance of the sentry client.
Keep a reference to the client around for as long as your application
needs to run and you can start sending exceptions by calling
@racket[sentry-capture-exception!]:

@racketblock[
(require sentry)

(parameterize ([current-sentry (make-sentry "https://key@sentry.io/12")])
  (sentry-capture-exception! (make-exn:fail "an error" (current-continuation-marks))))
]

@section[#:tag "reference"]{Reference}
@subsection{Core API}

@defproc[(sentry? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a Sentry client.
}

@defparam[current-sentry client (or/c #f sentry?)]{
  Stores the current Sentry client for use with
  @racket[sentry-capture-exception!].
}

@defproc[(make-sentry [dsn string?]
                      [#:backlog backlog exact-positive-integer? 128]
                      [#:release release (or/c #f non-empty-string?) (getenv "SENTRY_RELEASE")]
                      [#:environment environment (or/c #f non-empty-string?) (getenv "SENTRY_ENVIRONMENT")])
                      sentry?]{

  Returns a Sentry client.

  The @racket[#:backlog] argument controls the size of the error queue.
  Events are dropped silently when the queue is full.

  When the @racket[#:release] argument is set, every event's
  @tt{release} field is tagged with the given value. Ditto for the
  @racket[#:environment] argument and the @tt{environment} field on
  events.

  The reutrned client logs messages to the @racket['sentry] topic.
}

@defproc[(sentry-capture-exception! [e exn?]
                                    [client (or/c #f sentry?) (current-sentry)]
                                    [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'error]
                                    [#:timestamp timestamp moment? (now/moment)]
                                    [#:server-name server-name (or/c #f non-empty-string?) #f]
                                    [#:environment environment (or/c #f non-empty-string?) #f]
                                    [#:release release (or/c #f non-empty-string?) #f]
                                    [#:request request (or/c #f request?) #f]
                                    [#:tags tags (hash/c non-empty-string? string?) (hash)]
                                    [#:user user sentry-user? (current-sentry-user)])
                                    (evt/c void?)]{

  Sends @racket[e] to Sentry. Returns a synchronizable event that is
  ready for synchronization when the event leaves the queue, which means
  either that it has been sent to the Sentry API or that it has been
  dropped due to rate limits. When @racket[client] is @racket[#f], all
  events are dropped.
}

@defproc[(sentry-stop [client sentry? (current-sentry)]) void?]{
  Waits for all pending events in @racket[client] to be sent and then
  shuts down its event loop.
}


@subsection{Users}

@defproc[(sentry-user? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] represents a Sentry user.
}

@defparam[current-sentry-user user (or/c #f sentry-user?)]{
  A parameter that keeps track of data for the current user.
}

@defproc[(make-sentry-user [#:id id non-empty-string?]
                           [#:username username (or/c #f non-empty-string?) #f]
                           [#:email email (or/c #f non-empty-string?) #f]
                           [#:ip-address ip-address (or/c #f non-empty-string?) #f]
                           [#:subscription subscription (or/c #f non-empty-string?) #f])
                           sentry-user?]{

  Creates an object that can store various bits of information about a
  user. These can then be passed to @racket[sentry-capture-exception!]
  to have the data be associated with an error.
}
