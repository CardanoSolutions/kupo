{
  perSystem = { hsPkgs, ... }:
    let
      kupo = hsPkgs.kupo;
    in
    {
      packages = {
        default = kupo.components.exes.kupo;
        kupo = kupo.components.exes.kupo;
        kupo-lib = kupo.components.library;
        kupo-tests = kupo.components.tests.unit;
        kupo-tests-accept = kupo.components.tests.accept;
      };

      checks.kupo-unit = kupo.checks.unit;
    };
}
