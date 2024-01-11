#!/bin/bash

(
echo '## Play demos in JsBeeb'
prefix='https://bbc.godbolt.org/?\&disc1=https://nick-chapman.github.io/beeb'
ls docs/*.ssd | sed 's|docs/\(.*\).ssd|- [\1]('$prefix'/\1.ssd\&autoboot)|'
) > docs/README.md
