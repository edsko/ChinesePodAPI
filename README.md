# ChinesePodAPI

[ChinesePod](http://chinesepod.com) is an absolutely fantastic resource for
students of Chinese, I cannot recommend it highly enough. They recently also
started providing an [API](http://chinesepod.com/api). This package provides
Haskell bindings that provide a nice typed interface on top of the ChinesePod
API, which smooths over the irregularities in the underlying ChinesePod
database. It currently provides access to the following resources:

```
account/login
account/logout
account/get-user-info
lesson/get-lesson
library/get-latest-lessons
library/search-lessons
```

which is enough to list all available lessons and get all details for each of
them (description, vocabulary, expansion, grammar, etc.). I've tested the
bindings against the full ChinesePod database (as of Saturday Jan 16, 2016; all
3684 lessons).

**NOTE**: In addition to a (premium) subscription user account, you will also
need an API access code in order to be able to use these bindings. You will need
to contact ChinesePod to apply for one.
