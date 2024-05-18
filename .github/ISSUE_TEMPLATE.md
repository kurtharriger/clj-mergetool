---
name: Example Merge Conflict
about: Use this template for providing an example merge conflict
labels: example merge conflict
---

*Note: This issue will be public, please do not attach any code to this issue that is not publicly available.*

clj-mergetool version: `$(clj-mergetool version)`

# Steps to reproduce:

*The REPOSITORY_URL must be public.*

### Given the following:
```
REPOSITORY_URL=
FEATURE_BRANCH_HASH=
MAIN_BRANCH_HASH=
```

### These steps:
```
git clone $REPOSITORY_URL
git checkout $FEATURE_BRANCH_HASH -b right
git checkout $MAIN_BRANCH_HASH -b left
git merge right
clj-mergetool remerge clj-mergetool remerge src/clj/fluree/db/ledger/json_ld.cljc
```

# Produce this result

...

# The result I expect is

...