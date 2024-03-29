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
  A parameter that can store the current Sentry client for use with
  @racket[sentry-capture-exception!].
}

@defproc[(make-sentry [dsn string?]
                      [#:backlog backlog exact-positive-integer? 128]
                      [#:release release (or/c #f non-empty-string?) (getenv "SENTRY_RELEASE")]
                      [#:environment environment (or/c #f non-empty-string?) (getenv "SENTRY_ENVIRONMENT")]) sentry?]{
  Initialize a Sentry client and start the background thread that will
  send errors to the API.

  @racket[backlog] specifies the size of the error queue.  When the
  queue fills up, calls to @racket[sentry-capture-exception!] will
  start to silently drop events.

  @racket[release] can be set to tag each error with the current
  release (usually a GIT SHA).

  @racket[environment] can be set to tag each error with the current
  environment (eg. "production" or "staging").

  Sentry clients log messages to the @racket['sentry] topic.
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
                                    [#:user user sentry-user? (current-sentry-user)]) void?]{
  Asynchronously send an error to the Sentry API.

  Does nothing when @racket[client] is @racket[#f].
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

  @racket[sentry-capture-exception!] automatically picks these values
  up unless a different value is specified via its @racket[#:user]
  argument.
}

@defproc[(make-sentry-user [#:id id non-empty-string?]
                           [#:username username (or/c #f non-empty-string?) #f]
                           [#:email email (or/c #f non-empty-string?) #f]
                           [#:ip-address ip-address (or/c #f non-empty-string?) #f]
                           [#:subscription subscription (or/c #f non-empty-string?) #f]) sentry-user?]{
  Creates an object that can store various bits of information about a
  user.  These can then be passed to @racket[sentry-capture-exception!]
  to have the data be associated with an error.
}

@index-section[]
