{ mdbook, runCommand, postamble ? "" }:

runCommand
  "binplz.dev-book"
  {
    POSTAMBLE = postamble;
  }
  ''
    mkdir -p $out
    cp -r ${./.}/* .
    chmod -R a+w src
    echo "''${POSTAMBLE}" >> src/binplz.dev.md
    ${mdbook}/bin/mdbook build -d $out
  ''
