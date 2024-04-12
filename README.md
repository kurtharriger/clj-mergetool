# clj-mergetool

A git diff and merge tool for edn and clojure code. Semantic diffs for conflict free merges.

# Overview

A git diff and merge tool for edn and clojure code. Semantic diffs for conflict free merges.

For example given the following deps.edn

```clj
{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
        org.clojure/core.async {:mvn/version "1.3.610"}}}
```

One user adds a dependency:

```clj
{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
        org.clojure/core.async {:mvn/version "1.3.610"}
        clj-http {:mvn/version "3.11.0"}}}
```

Another user upgrades a dependency:

```clj
{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
<<<<<<< HEAD
        org.clojure/core.async {:mvn/version "1.3.612"}}}
||||||| add178e
        org.clojure/core.async {:mvn/version "1.3.610"}}}
=======
        org.clojure/core.async {:mvn/version "1.3.610"}
        clj-http {:mvn/version "3.11.0"}}}
>>>>>>> right
```

Git line diff does not recognize these as logically independant changes. Also manually merging this change requires extra attention to the closing brackets.

What does clj mergetool do differently?

clj mergetool diffs the data structures rather then the lines.
(currently using editscript here, but this is likely to change)

The left diff might be represented as a replacement of the value at a given path (eg assoc-in)

```clj
[[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"]]
```

The right diff adds a new entry

```clj
[[[:deps clj-http] :+ {:mvn/version "3.11.0"}]]
```

Combined:

```clj
[[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"] [[:deps clj-http] :+ {:mvn/version "3.11.0"}]]
```

The resulting patch is conflict free:

```clj
{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
        org.clojure/core.async {:mvn/version "1.3.612"}
        clj-http {:mvn/version "3.11.0"}}}
```

# Installation

```sh
git clone  --recurse-submodules --depth 1 https://github.com/kurtharriger/clj-mergetool.git clj-mergetool
cd clj-mergetool
clj -T:build install
```

Note:
If you have GraalVM installed (when `native-image` is in your PATH), install will attempt to build a native-image instead of an executable jar file which has much faster startup time than running jar file.

I use [sdkman](https://sdkman.io/) to manage multiple versions of java.

```bash
curl -s "https://get.sdkman.io" | bash
sdk install java 22-graal
sdk use java 22-graal
clj -T:build install
```

# Usage

When you encounter a git conflict you can invoke this tool to attempt to automatically resolve the conflicts.
This tool will fetch the ancestor current and other version from the git index and attempt to remerge them.
If files is not specified all unmerged files in the index will be remerged.

```sh
clj-mergetool remerge [files...]
```

# Git mergetool

Although not yet recommended, this tool can be automatically invoked by git merge.

The merge tool needs to be configured in both .git/config or ~/.gitconfig
and .gitattributes or ~/.gitattributes.

.gitattributes is typically added to source control however
.git/config unfortunatly cannot be included in source control

git will use default behavior without warning if mergetool is refrenced
in .gitattributes but not installed in .git/config

```

git config --local "merge.clj-mergetool.driver" "clj-mergetool mergetool %O %A %B"

cat <<END >> .gitattributes
*.clj merge=clj-mergetool
*.cljs merge=clj-mergetool
*.edn merge=clj-mergetool
END

```

See also https://github.com/Praqma/git-merge-driver/blob/master/.gitconfig

# Status

Initial funding for this project provided by [ClojuristsTogether](https://www.clojuriststogether.org/)!

# Share your merge conflicts

I would love to get any example merge conflicts you have encountered in practice on open source code that I can use in my unit tests during development.

Please [open an issue](https://github.com/kurtharriger/clj-mergetool/issues/new) with an example or any other feedback you may have.

# Known Issues

- cljc / reader-literals

  rewrite-clj currenlty emits reader literals as strings which are somewhat opaque to editscript often resulting in the entire node being replaced.

- Leading whitespace and/or comments on map keys not preserved

- Conflict detection

- Untested on Windows

  Probably works, but assumes linux conventions (eg installs to ~/.local/bin)
  Let me know if you have [issues](https://github.com/kurtharriger/clj-mergetool/issues/new).

# Roadmap

- fix whitespace/comments on map keys
- cljc support
- explore alternative diff representation
- improve conflict detection
- simplify installation
- improve diff visualization
- explore detection and respresentation of higher order refactorings, such as: rename symbol, align forms, sort keys

## License

Copyright © 2024 Kurt Harriger

Distributed under the Eclipse Public License version 1.0.
