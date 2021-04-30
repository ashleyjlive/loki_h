-type cfg_trgt_t() :: #{scheme := http | https,
                        host := unicode:chardata(),
                        auth => loki_h:auth_t(),
                        path => unicode:chardata(),
                        host := unicode:chardata(),
                        port => non_neg_integer()} |
                      #{auth => loki_h:auth_t(),
                        url := uri_string:uri_string()} | 
                      uri_string:uri_string().

-type cfg_args_t() :: 
        #{job := nonempty_string() | binary(),
          target := cfg_trgt_t(),
          failure_strategy := crash | drop,
          interval := non_neg_integer(),
          max_count := non_neg_integer(),
          max_bytes := non_neg_integer()}.

-type cfg_t() :: #{config := cfg_args_t(),
                   formatter := cfg_fmtr_t(),
                   filters := cfg_fltrs_t(),
                   sys_ops := cfg_sys_ops_t()}.

-type cfg_fmtr_t() :: {module(), logger:formatter_config()}.
-type cfg_fltrs_t() :: [{logger:filter_id(), logger:filter()}].
-type cfg_sys_ops_t() :: [term()].

-type cfg_k_t() :: config | formatter | filters | sys_ops.