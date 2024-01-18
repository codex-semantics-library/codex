# What does this tool do?

This tools takes one argument, that is the folder where it will look for all the files and its differences.

First executes two commands and it creates two lists where:
- listAll: Stores all files of the tests directory.
- listDiff: Stores only the files that have been modified inside of the tests directory.

After that, it stores all the elements of the listDiff into a hash table to optimize the search.

Finally, it iterates over all the elements of the list of all files, searching if they are in 
the hashTableDiff, printing the results in an XML file:
- If there are no differences, it prints the name of the test.
- If there are differences, it prints the name and inside a failure tag, the corresponding difference.

# Example of result.xml

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<testcase name="tests/folder1/file1" classname="Tests" />
<testcase name="tests/folder1/file2" classname="Tests" />
<failure message="Assertion error message" type="AssertionError">
@@ -3,4 +3,4 @@ world
 how
 are
 you
-agneig
+doing
</failure>
</testcase>
<testcase name="tests/ocaml_diff.ml" classname="Tests" />
```

Here we can see the following structure of the folder tests:

tests
├── folder1
│   ├── file1
│   └── file2
└── ocaml_diff.ml

Also, we can see that `tests/folder1/file1` and `tests/ocaml_diff.ml` have not been modified,
but that `tests/folder1/file2` has been modified, with the current diff inside the failure tag.