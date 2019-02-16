@stack build --force-dirty --ghc-options "-optl-Wl,--subsystem,windows" --copy-bins
@if not errorlevel 1 if not "%1"=="" dualternativeh-exe