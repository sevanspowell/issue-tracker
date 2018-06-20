{ mkDerivation, base, beam-core, beam-migrate, beam-postgres, lens
, postgresql-simple, stdenv, text, time
}:
mkDerivation {
  pname = "issue-tracker";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base beam-core beam-migrate beam-postgres lens postgresql-simple
    text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
