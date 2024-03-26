---
name: Example Merge Conflict
about: Use this template for providing an example merge conflict
labels: example merge conflict
---

If you would like to share a git conflict, please ONLY share code that is publicly available and free to copy into my unit tests.

- [ ] The attached code is publicly available and can be used in unit tests.

Github/gitlab/bitbucket url:

- [ ] I have attached the **three** unmerged source files from the git index
- [ ] I have also attached the manually merged file with the expected results

# Pull unmerged source files from git index

Please note that this tool does a 3-way diff merge which also requires three source files: ancestor, current, other.

If diff3 configuration option was enabled prior to merge you can attach the conflict file instead (see next section).

```
git show :1:filename.clj > filename.base.clj
git show :2:filename.clj > filename.current.clj
git show :3:filename.clj > filename.other.clj
```

Resolve conflicts in original file and then zip or tar these files to attach to the issue.

```
tar cJvf filename.tar.xz filename*
```

# Conflict file

The conflict file is not needed if the original three source files are attached. However if you had diff3 enabled prior to merge the three source versions can be extracted from the conflict file itself. Unfortunatly, diff3 is NOT enabled by default but can be enabled using git config as follows:

```
git config --global merge.conflictStyle diff3
```

When enabled you will see an additional section bookmarked with `|||||||` - `=======`. eg:

```
||||||| add178e
        org.clojure/core.async {:mvn/version "1.3.610"}}}
=======
```

If the conflict file does not include `|||||||` then you will need to pull the three source files from the index as documented in previous section or abort and retry merge to regenerate the conflict file.

Thanks!
