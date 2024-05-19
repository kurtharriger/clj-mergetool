# Change Log

## 0.6.0 - [Unreleased]

Implement CICD pipeline

## [0.5.0] - 2024-05-18

Fix [#2](https://github.com/kurtharriger/clj-mergetool/issues/2) error querying git index where unmerged file is deleted

Revert change in 0.3.0 causing NullPointerException


## [0.4.3] - 2024-04-14

Fix uneval nodes need additional encoding decoding

## [0.4.2] - 2024-04-14

Fix anonymous functions need additional encoding decoding

## [0.4.1] - 2024-04-14

Fix decode-reader-macro

## [0.4.0] - 2024-04-14

Change conflict handling to still merge and print warning rather than abort
Remove identitcal edits to avoid conflict warning

## [0.3.1] - 2024-04-14

Fix only forms sexpr representation resulting in replace of ns form only
Fix missing else resulting in nil replacement
Endoce/decode reader-macros to support cljc

## [0.3.0] - 2024-04-13

Copy leading whitespace on map keys

## [0.2.2] - 2024-04-13

Add conflict message
Print conflict to stderr
Fix broken symlink on dev install `clj -T:build install :link true`

## [0.2.1] - 2024-04-12

Update readme instructions

## [0.2.0] - 2024-04-12

Add install tool
Update readme with installation and usage instructions

## [0.1.0] - 2024-04-12

Initial release

---

[Unreleased]: https://github.com/kurtharriger/clj-mergetool/compare/0.5.0...HEAD
[0.5.0]: https://github.com/kurtharriger/clj-mergetool/compare/0.4.3...0.5.0
[0.4.3]: https://github.com/kurtharriger/clj-mergetool/compare/0.4.2...0.4.3
[0.4.2]: https://github.com/kurtharriger/clj-mergetool/compare/0.4.1...0.4.2
[0.4.1]: https://github.com/kurtharriger/clj-mergetool/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/kurtharriger/clj-mergetool/compare/0.3.1...0.4.0
[0.3.1]: https://github.com/kurtharriger/clj-mergetool/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/kurtharriger/clj-mergetool/compare/0.2.2...0.3.0
[0.2.2]: https://github.com/kurtharriger/clj-mergetool/compare/0.2.1...0.2.2
[0.2.1]: https://github.com/kurtharriger/clj-mergetool/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/kurtharriger/clj-mergetool/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/kurtharriger/clj-mergetool/compare/0.0.0...0.1.0
