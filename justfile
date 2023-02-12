

_default:
  @just --list

@showvers:
  grep '^\s*version' build.sbt
	  
# Create zip file and copy it to dropbox
package VERSION:
  #!/usr/bin/env bash
  set -euo pipefail
  PKG=coltwi-{{VERSION}}
  if [ -d target/$PKG ]; then
    find target/$PKG -name .DS_Store -exec rm {} \+
    rm -f target/${PKG}.zip
    (cd target; zip -rq ${PKG}.zip $PKG)
    cp target/${PKG}.zip /Users/curt/Library/CloudStorage/Dropbox/coltwi/
  else
    echo target/$PKG does not exist
    exit 1
  fi

# Run this after the package.sh script and after updating the README.md file.
# Careful! This will add, commit and tag all updated files.
commit_release VERSION:
  git add --update .
  git ci -m"Update version number to {{VERSION}}"
  git tag -m"Release v{{VERSION}}" v{{VERSION}}

@setvers VERSION:
  ruby -p -i -e 'gsub(/(version\s*:=\s*)("\d+\.\d+")/, "\\1\"{{VERSION}}\"")' build.sbt
  ruby -p -i -e 'gsub(/coltwi_2.13-(\d+\.\d+)\.jar/, "coltwi_2.13-{{VERSION}}.jar")' src/other/coltwi src/other/coltwi.cmd
  ruby -p -i -e 'gsub(/(val\s+SOFTWARE_VERSION\s*=\s*)("\d+\.\d+")/, "\\1\"{{VERSION}}\"")' src/main/scala/coltwi/ColonialTwilight.scala
  echo "Version set to {{VERSION}}"
  