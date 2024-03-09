# clj-mergetool

A git diff and merge tool for edn and clojure code.
Symantic diffs for conflict free merges.

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
        org.clojure/core.async {:mvn/version "1.3.610"}}}
```

Git's line based diff produces the following conflict:

```
    {:deps {org.clojure/clojure {:mvn/version "1.10.1"}
    <<<<<<< HEAD
            org.clojure/core.async {:mvn/version "1.3.612"}}}
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

# Usage

TBD.
Started March 2024

clone repo.
build uberjar: clj -M:uberdeps
add bin/clj-mergetool to path

todo: clj-mergetool install

NOTE:
the merge tool needs to be configured in both .git/config or ~/.gitconfig
and .gitattributes or ~/.gitattributes.

.gitattributes is typically added to source control however
.git/config unfortunatly cannot be included in source control

git will use default behavior without warning if mergetool is refrenced
in .gitattributes but not installed in .git/config

```

git config --local "merge.clj-mergetool.driver" "clj-mergetool merge %O %A %B"

cat <<END >> .gitattributes
*.clj merge=clj-mergetool
*.edn merge=clj-mergetool
END
```

https://github.com/Praqma/git-merge-driver/blob/master/.gitconfig

# Roadmap:

## March 2024

MVP using editscript and rewrite-clj.

Diff with editscript and apply patch with rewrite-clj to preserve whitespace. This will likely work in many cases where the composed diff is conflict free. However, editscript ignores whitespace and thus new code that is added will need to be reformatted post merge. I expect that actual conflicts to applied where last edit wins.

At this stage it is probably best to use clj-mergetool as fallback when standard git merge fails.

## April 2024

Fork editscript to work over rewrite-clj nodes to preserve whitespace.
Potentailly reconsider diff representation.
Update combine to identify actual conflicts.

## May 2024

Run under babashka for performance and cross platform support.
Usable as primary git mergetool.

## June 2024

Better visualization of diffs?
Even higher level symantic operations?

- Align forms. Attempt to identify when user aligns values to start at same column.
- Sorting namespace requires or map keys
- Replacing multiple let assignments with -> or ->>
- ... ?

## License

Copyright © 2024 Kurt Harriger

Distributed under the Eclipse Public License version 1.0.

```

```
