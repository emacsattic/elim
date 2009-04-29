#ifndef _EMACSIM_SEXP_EXAMPLE_H_
#define _EMACSIM_SEXP_EXAMPLE_H_

#define EXAMPLE_SEXP_METHODCALL \
    "(methodCall ((abc . \"def\") (moose . \"elk\") (ghi . \"123\")) \"\n\
  \" (methodName nil \"foobar\") \"\n\
  \" (params nil \"\n\
    \" (param nil \"\n\
      \" (value nil \"\n\
        \" (struct nil \"\n\
          \" (member nil \"\n\
            \" (name nil \"str_float\") \"\n\
            \" (value nil \"\n\
              \" (string nil \"3.1415\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"false\") \"\n\
            \" (value nil \"\n\
              \" (boolean nil \"0\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"cast\") \"\n\
            \" (value nil \"\n\
              \" (string nil \"4321\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"b64\") \"\n\
            \" (value nil \"\n\
              \" (base64 nil \"AGJsZXJn\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"true\") \"\n\
            \" (value nil \"\n\
              \" (boolean nil \"1\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"string\") \"\n\
            \" (value nil \"\n\
              \" (string nil \"€uromøøse! <>\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"string2\") \"\n\
            \" (value nil \"\n\
              \" (string nil \"12345\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"array\") \"\n\
            \" (value nil \"\n\
              \" (array nil \"\n\
                \" (data nil \"\n\
                  \" (value nil \"\n\
                    \" (string nil \"badger\") \"\n\
                  \") \"\n\
                  \" (value nil \"\n\
                    \" (string nil \"badger\") \"\n\
                  \") \"\n\
                  \" (value nil \"\n\
                    \" (string nil \"badger\") \"\n\
                  \") \"\n\
                  \" (value nil \"\n\
                    \" (string nil \"mushroom\") \"\n\
                  \") \"\n\
                \") \"\n\
              \") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"float\") \"\n\
            \" (value nil \"\n\
              \" (double nil \"3.1415\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"integer\") \"\n\
            \" (value nil \"\n\
              \" (int nil \"1234\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_true\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"2009-02-16T00:32:55\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"bigneg\") \"\n\
            \" (value nil \"\n\
              \" (int nil \"1\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_str\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"19750603T013000\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_nil\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"0000-00-00T00:00:00\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_false\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"1970-01-01T00:00:00\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"hash\") \"\n\
            \" (value nil \"\n\
              \" (struct nil \"\n\
                \" (member nil \"\n\
                  \" (name nil \"e\") \"\n\
                  \" (value nil \"\n\
                    \" (string nil) \"\n\
                  \") \"\n\
                \") \"\n\
                \" (member nil \"\n\
                  \" (name nil \"c\") \"\n\
                  \" (value nil \"\n\
                    \" (int nil \"1\") \"\n\
                  \") \"\n\
                \") \"\n\
                \" (member nil \"\n\
                  \" (name nil \"a\") \"\n\
                  \" (value nil \"\n\
                    \" (string nil \"b\") \"\n\
                  \") \"\n\
                \") \"\n\
              \") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_now\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"2009-02-16T00:32:55\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"bigint\") \"\n\
            \" (value nil \"\n\
              \" (double nil \"4294967296\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"date_then\") \"\n\
            \" (value nil \"\n\
              \" (dateTime\\.iso8601 nil \"1970-01-01T00:00:01\") \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"thing\") \"\n\
            \" (value nil \"\n\
              \" (string nil) \"\n\
            \") \"\n\
          \") \"\n\
          \" (member nil \"\n\
            \" (name nil \"float_str\") \"\n\
            \" (value nil \"\n\
              \" (double nil \"3.1415\") \"\n\
            \") \"\n\
          \") \"\n\
        \") \"\n\
      \") \"\n\
    \") \"\n\
  \") \"\n\
\")"

#endif
