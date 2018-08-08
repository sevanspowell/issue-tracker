{ mkDerivation, aeson, base, bcrypt, beam-core, beam-migrate
, beam-postgres, bytestring, http-client, lens, mtl
, postgresql-simple, resource-pool, safe-exceptions, servant
, servant-auth, servant-auth-server, servant-client, servant-server
, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "issue-tracker";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bcrypt beam-core beam-migrate beam-postgres bytestring
    http-client lens mtl postgresql-simple resource-pool
    safe-exceptions servant servant-auth servant-auth-server
    servant-client servant-server text time transformers wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
