module Language.MSH.RuntimeError where

_msh_rt_invalid_call_abstract :: String
_msh_rt_invalid_call_abstract = "Invalid call: the method is abstract."

_msh_rt_invalid_call_state :: String
_msh_rt_invalid_call_state = "Invalid call: not supported by this object state"

_msh_rt_invalid_call_internal :: String
_msh_rt_invalid_call_internal = "Invalid call: internal call not supported by this object state"
