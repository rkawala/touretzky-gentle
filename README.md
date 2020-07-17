# touretzky
### _Rick Kawala_

This is a bunch of Lisp code that I wrote while I read *A Gentle Introduction To Symbolic Computation* by David Touretzky. A PDF of the book is available for free online at https://www.cs.cmu.edu/~dst/LispBook/.

Rob Mee suggested the idea of learning something new by writing unit tests. It's worked out really well. You can read a book and think you understand something, but you don't know what you don't know until you actually try to express it as code and run it.

I also learned a lot by doing my best to DRY the tests. I wrote my first Lisp macro that way, among other things.

## Running the code

One of the hard things, as a person new to Lisp, was figuring out how to get that first test to run. I recommend you do the following:

1. Install Portacle from https://portacle.github.io/
2. Clone this repo into the portacle/projects directory.
2. Start Portacle.
3. Go to the Emacs buffer named \*slime-repl sbcl\*
4. Execute (ql:quickload :rove) to download the testing framework and its dependencies.
5. Execute (asdf:load-system :touretzky) to load this project.
6. To run the tests, do (progn (rove:run :touretzky) nil)

I wrote that list from memory, so feel free to add a GitHub issue if I got it wrong.

## License

This project is licensed under the MIT license. Use this code in any way you want, just don't sue me.

