; corpus fixture: exercises both block comment and triple-quoted string paths

#| this is a block comment spanning
   multiple lines |#

(defun greeting ()
  """
  Hello, world!
  This line embeds a """nested""" triple-quote and a #| fake opener |# inside.
  """)
