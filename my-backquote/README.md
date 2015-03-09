my-backquote
============

A custom implementation of how backquote works. Entirely based on the implementation coded by Guy Steele Jr. and made available <a href="https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html">here</a>.

The only difference is that I have bounded the inner backquote to the real backquote character and the same goes for the inner comma. So all the regular Lisp code goes through that implementation of quasiquoting.

The initial goal was to remove the transformation work off the hand of the read process. Because by doing so, the read process would output an s-expression filled with tokens about a backquote being there, a comma here, and so on. That s-expression accurately represents the code that was read while initially, when the read process performs the quasiquoting by itself, it returned the result of that transformation. And it is not at all the code that was read.

Thus my editor would save codes in a different form that the initial one, even if the programmer would have made no change! By having the accurate s-expression, I can watch out for the tokens so as to output the very same code that was read.

Thank you so much, Mr Guy Steele Jr.
------------------------------------

I had another problem back at that time. I was able to parse s-expression but I have shied away from tackling how to parse backquoted forms containing commas and the like.

I suddenly discover, upon replacing the quasiquoting facility of my implementation, that my parser was now working on backquoted forms. I haven't spend time yet on that issue and I still need to sort things out (which actions are installed and which one are used). But it is functional and the work feels possible.

I have only felt amazed by the implications of what I was programming twice and this is the most tremendous one of the two, by far. Need I say both happened with Lisp?

This would have not be possible in a so quick and so painless manner if someone hadn't taken the trouble of implementing in Common Lisp the backquote facility. It happened Mr Guy Steele Jr. did and added that work as an annex to the book Common Lisp the Language.

I knew about it even before my sabbatical so when the idea struck my mind to have tokens about backquotes and commas, I knew where to look. And from there, it was a smooth road.

So, thank you very much, Mr Guy Steele Jr.!