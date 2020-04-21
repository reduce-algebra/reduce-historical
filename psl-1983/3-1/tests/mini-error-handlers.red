% MINI-ERROR-HANDLERS.RED - Error Handler stubs
on syslisp;

syslsp procedure FatalError s;
 <<ErrorHeader(); Prin2 " FATAL "; ErrorTrailer s>>;

syslsp procedure StdError m;
  Error m;

off syslisp;

end;
