import Test.DocTest
main = doctest ["-XOverloadedStrings", "-XNoImplicitPrelude", "-isrc", "src/ChaostreffScheduler.hs"]
