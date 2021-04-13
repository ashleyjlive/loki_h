loki_h
=====
# Purpose

To allow for logs in a given Erlang system to be exportable to Loki thus
allowing developers to aggregate, view and search logs via Loki.

# Usage

To use within another application simply include this repo as a rebar3 dep and 
include it in your *.app.src file as a required application.

## Build
-----

    $ rebar3 compile

## Test

An example configuration is defined in `config/sys.config` and can be enabled by 
uncommenting the `config` tuple in rebar.config (under shell).

Then, it is a matter of simply calling...

-----

    $ rebar3 shell

And writing some logs via Erlangs `logger` module...

```erlang
  logger:notice("This is a test").
```

And waiting for the default timeout (generally 5 seconds) for the logs to be 
sent to Loki.

## Configuration
You will need to set the configuration for `loki_h` prior to booting the OTP 
application.

This can be achieved via by using the `application:set_env/3` function.
```erlang
application:set_env(loki_h, start_args, Conf).
```
Where `Conf` is defined as the following type specification.
```erlang
-type cfg_t() :: #{config := loki_h:cfg_args_t(),
                   % The configuration associated with the Loki endpoint 
                   % (see below for definition).
                   formatter := loki_h:cfg_fmtr_t(),
                   % The log formatter as defined in Erlang OTP `logger` module 
                   % [1].
                   filters := loki_h:cfg_fltrs_t(),
                   % The log filters as defined in Erlang OTP `logger` module
                   % [1].
                   sys_ops := loki_h:cfg_sys_ops_t()}.
```
[1] - https://erlang.org/doc/man/logger.html
### Loki Configuration
```erlang
-type cfg_args_t() :: 
        #{job := nonempty_string() | binary(),
          % The unique name for this job. 
          % NB: For multiple nodes representing the same system the `job` can 
          %     be reused.
          target := loki_h:cfg_trgt_t(),
          % Where to send the logs to.
          failure_strategy => ignore | drop, 
          % If the endpoint is offline either tear down supervision tree (crash) 
          % or drop logs.
          interval => non_neg_integer(),
          % Time in milliseconds before sending the current collected logs. 
          max_count => non_neg_integer(),
          % Maximum number of log entries before uploading to Loki.
          max_bytes => non_neg_integer()}.
          % Maximum number of bytes before initiating an upload to Loki.

-type cfg_trgt_t() :: 
        #{scheme := http | https,
          % Defines the scheme to use when uploading to Loki.
          host := unicode:chardata(),
          % Defines the host to contact.
          auth => loki_h:auth_t(),
          % Defines any authentication (if any) that is required when 
          % connecting/interacting with the endpoint.
          path => unicode:chardata(),
          % Defines the subpath - useful if using a proxy HTTP server.
          port => non_neg_integer()} |
        #{auth => loki_h:auth_t(),
          % Defines the authentication as explained aboved.
          url := uri_string:uri_string()} | 
          % A full URI string for the endpoint.
        uri_string:uri_string().
        % A full URI string - assumes no authentication.
```