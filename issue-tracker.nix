{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, http-client, lens, postgresql-simple, resource-pool
, servant, servant-auth, servant-auth-server, servant-client
, servant-server, stdenv, text, time, wai, warp
}:
mkDerivation {
  pname = "issue-tracker";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres bytestring
    http-client lens postgresql-simple resource-pool servant
    servant-auth servant-auth-server servant-client servant-server text
    time wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
