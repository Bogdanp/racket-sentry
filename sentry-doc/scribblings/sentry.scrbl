#lang scribble/manual

@(require (for-label (only-in db connection?)
                     (only-in gregor moment? now/moment)
                     json
                     racket
                     sentry
                     sentry/tracing
                     web-server/http/request-structs))

@title{Sentry SDK}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

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

(define client (make-sentry "https://key@sentry.io/12"))
(parameterize ([current-sentry client])
  (sentry-capture-exception! (make-exn:fail "an error" (current-continuation-marks))))
(sentry-stop client)
]

@section[#:tag "reference"]{Reference}
@defmodule[sentry]
@subsection{Core API}

@defproc[(event? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an event captured by
  @racket[sentry-capture-exception!].

  @history[#:added "0.5"]
}

@defproc[(sentry? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a Sentry client.
}

@defparam[current-sentry client (or/c #f sentry?)]{
  Stores the current Sentry client for use with
  @racket[sentry-capture-exception!].
}

@defproc[(make-sentry [dsn string?]
                      [#:sampler sampler (-> (or/c event? transaction?) (real-in 0.0 1.0)) (lambda (_) 1.0)]
                      [#:backlog backlog exact-positive-integer? 128]
                      [#:release release (or/c #f non-empty-string?) (getenv "SENTRY_RELEASE")]
                      [#:environment environment (or/c #f non-empty-string?) (getenv "SENTRY_ENVIRONMENT")]
                      [#:connect-timeout-ms connect-timeout exact-positive-integer? 5000]
                      [#:send-timeout-ms send-timeout exact-positive-integer? 5000]
                      [#:max-breadcrumbs max-breadcrumbs exact-positive-integer? 50])
                      sentry?]{
  Returns a Sentry client.

  The @racket[#:backlog] argument controls the size of the error queue.
  Events are dropped when the queue is full.

  When the @racket[#:release] argument is set, every event is tagged
  with the given value. Ditto for the @racket[#:environment] argument.

  The reutrned client logs messages to the @racket['sentry] topic.

  The @racket[#:sampler] argument determines what chance an event has to
  be sent to the server. The default implementation samples 100% of all
  events.

  @history[#:changed "0.5" @elem{Added the @racket[#:sampler] argument.}]
}

@defproc[(sentry-capture-exception! [e exn?]
                                    [client (or/c #f sentry?) (current-sentry)]
                                    [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'error]
                                    [#:timestamp timestamp (or/c date*? moment?) (current-utc-date)]
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


@subsection{Tracing}
@defmodule[sentry/tracing]

A @deftech{transaction} tracks a set of @tech{spans} and delivers them
to Sentry when the transaction completes. A @deftech{span} measures and
records information about a block of code.

@defproc[(transaction? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{transaction}.
}

@defparam[current-transaction t (or/c #f transaction?)]{
  Holds the current transaction. The @racket[call-with-transaction]
  procedure automatically installs a transaction in this parameter.
}

@(define annot-url "https://develop.sentry.dev/sdk/event-payloads/transaction/#transaction-annotations")
@(define traces-url "https://develop.sentry.dev/sdk/telemetry/traces/")
@(define span-ops-url "https://develop.sentry.dev/sdk/telemetry/traces/span-operations/")

@defproc[(call-with-transaction [name string?]
                                [proc (-> transaction? any)]
                                [#:data data (or/c #f (hash/c symbol? jsexpr?)) #f]
                                [#:source source symbol? 'custom]
                                [#:trace-id trace-id (or/c #f string?) #f]
                                [#:parent-id parent-id (or/c #f string?) #f]
                                [#:operation operation symbol? 'function]
                                [#:description description (or/c #f string?) #f]) any]{

  Calls @racket[proc] in the context of a @tech{transaction} with the
  given name. When the call to @racket[proc] finishes executing, the
  transaction is sent to the @racket[current-sentry] client. If there is
  no current client, the transaction information is discarded at the end
  of the call.

  The value passed to the @racket[name] argument should follow the
  conventions defined for the value passed to the @racket[#:source]
  argument. The supported @racket[#:source] values can be found in
  Sentry's @hyperlink[annot-url]{Transaction Annotations} documentation.

  The @racket[#:trace-id] and @racket[#:parent-id] may be used to
  propagate distributed tracing ids to the new transaction. See Sentry's
  documentation on the @hyperlink[traces-url]{sentry-trace header}
  for details. If not provided, and the call is nested within another
  transaction, then the parent transaction's values are inherited.
  If there is no parent transaction, then new trace and span ids are
  generated automatically.

  The @racket[#:operation] should be one of the values listed in
  Sentry's @hyperlink[span-ops-url]{Span Operations} documentation.

  The @racket[#:description] may be an arbitrary string describing the
  transaction in detail.
}

@defproc[(span? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{span}.
}

@defparam[current-span s (or/c #f span?)]{
  Holds the current span. The @racket[call-with-span] procedure
  automatically installs a span in this parameter.
}

@defproc[(call-with-span [proc (-> span? any)]
                         [#:operation operation symbol? 'function]
                         [#:description description (or/c #f string?) #f]
                         [#:origin origin symbol? 'manual]
                         [#:data data (or/c #f (hash/c symbol? jsexpr?)) #f]) any]{

  Calls @racket[proc] in the context of a @tech{span} with the given
  @racket[#:operation] and @racket[#:description]. When the call to
  @racket[proc] finishes executing, the span is registered with the
  surrounding transaction. If there is no surrounding transaction, the
  span is discarded at the end of the call.

  The @racket[#:operation] should be one of the values listed in
  Sentry's @hyperlink[span-ops-url]{Span Operations} documentation.

  The @racket[#:description] may be an arbitrary string describing the
  operation in detail.
}

@defproc[(get-span-id [v (or/c span? transaction?)]) string?]{
  Returns the id of the given @tech{transaction} or @tech{span}.
}

@defproc[(get-trace-id [v (or/c span? transaction?)]) string?]{
  Returns the trace id of the given @tech{transaction} or @tech{span}.
}

@defproc[(span-set! [s (or/c span? transaction?)]
                    [k symbol?]
                    [v jsexpr?]) void?]{

  Sets @racket[k] to @racket[v] within @racket[s]'s data payload.
}

@subsubsection{Database Queries}

@defproc[(trace-connection [c connection?]) connection?]{
  Wraps all queries performed by @racket[c] in a @racket['db.query]
  @tech{span}, recording the executed statement.
}
