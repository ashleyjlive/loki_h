%%% @doc Sourced by Mozilla Web Docs - https://developer.mozilla.org/en-US/docs/Web/HTTP/Status

%%% -- Information responses ---
-define('100_Continue', 100).
-define('101_Switching_Protocol', 101).
-define('102_Processing', 102).
-define('103_Early_Hints', 103).
-define(is_code_info(Code),
    (Code >= 100 andalso Code =< 103)).

%%% -- Successful responses --- 
-define('200_OK', 200).
-define('201_Created', 201).
-define('202_Accepted', 202).
-define('203_Non-Authoritative_Information', 203).
-define('204_No_Content', 204).
-define('205_Reset_Content', 205).
-define('206_Partial_Content', 206).
-define('207_Multi-Status', 207).
-define('208_Already_Reported', 208).
-define('226_IM_Used', 226).
-define(is_code_success(Code),
    (Code >= 200 andalso  Code =< 226)).

%%% -- Redirection messages --- 
-define('300_Multiple_Choice', 300).
-define('301_Moved_Permanently', 301).
-define('302_Found', 302).
-define('303_See_Other', 303).
-define('304_Not_Modified', 304).
-define('305_Use_Proxy', 305).
-define('306_Unused', 306).
-define('307_Temporary_Redirect', 307).
-define('308_Permanent_Redirect', 308).
-define(is_code_redirection(Code),
    (Code >= 300 andalso Code =< 308)).

%%% -- Client error responses --- 
-define('400_Bad_Request', 400).
-define('401_Unauthorized', 401).
-define('402_Payment Required', 402).
-define('403_Forbidden', 403).
-define('404_Not_Found', 404).
-define('405_Method_Not_Allowed', 405).
-define('406_Not_Acceptable', 406).
-define('407_Proxy_Authentication_Required', 407).
-define('408_Request_Timeout', 408).
-define('409_Conflict', 409).
-define('410_Gone', 410).
-define('411_Length_Required', 411).
-define('412_Precondition_Failed', 412).
-define('413_Payload_Too_Large', 413).
-define('414_URI_Too_Long', 414).
-define('415_Unsupported_Media_Type', 415).
-define('416_Range_Not_Satisfiable', 416).
-define('417_Expectation_Failed', 417).
-define('418_Im_A_Teapot', 418).
-define('421_Misdirected_Request', 421).
-define('422_Unprocessable_Entity', 422).
-define('423_Locked', 423).
-define('424_Failed_Dependency', 424).
-define('425_Too_Early', 425).
-define('426_Upgrade_Required', 426).
-define('428_Precondition_Required', 428).
-define('429_Too_Many_Requests', 429).
-define('431_Request_Header_Fields_Too_Large', 431).
-define('451_Unavailable_For_Legal_Reasons', 451).
-define(is_code_client_error(Code),
    (Code >= 400 andalso Code =< 451)).

%%% -- Server error responses --- 
-define('500_Internal_Server_Error', 500).
-define('501_Not_Implemented', 501).
-define('502_Bad_Gateway', 502).
-define('503_Service_Unavailable', 503).
-define('504_Gateway_Timeout', 504).
-define('505_HTTP_Version_Not_Supported', 505).
-define('506_Variant_Also_Negotiates', 506).
-define('507_Insufficient_Storage', 507).
-define('508_Loop_Detected', 508).
-define('510_Not_Extended', 510).
-define('511_Network_Authentication_Required', 511).
-define(is_code_server_error(Code),
    (Code >= 500 andalso Code =< 511)).

%%% -- Generalisations ----
-define(is_status_code_good(Code),
    (?is_code_info(Code)    orelse 
     ?is_code_success(Code) orelse 
     ?is_code_redirection(Code))).

-define(is_status_code_bad(Code),
    (?is_code_client_error(Code) orelse 
     ?is_code_server_error(Code))).