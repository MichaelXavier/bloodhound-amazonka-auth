0.1.1.0
* Worked around seemingly a bug in V4 signatures with AWS ES service with paths that needed encoding. This comes up if you make authenticated requests with index patterns, e.g. /foo*/. They would previously fail to authenticate.

0.1.0.0
* Initial release
