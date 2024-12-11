# Overview
This project attempts to sort small arrays in an absurd way.  Efficiency and speed are frowned upon.

# Process
The current process involves the following steps:
1. A NodeJS script receives the input array via CLI argument.
2. A docker container containing emacs is built.
3. A container is launched for each element in the array.
	1. Emacs is started with a customized .emacs file.
	2. eww is used to load https://randomtextgenerator.com
	3. Random characters are selected from the eww buffer and each character is mapped to a move (up, down, left, right).
	4. Tetris is started (in emacs) and the moves are fed to the game with a random delay.
	5. When tetris halts emacs is stopped (along with the container) and the tetris score is read from a host file that was bound to the container.
4. When all tetris scores are obtained the input array is sorted using the scores.

# Installation
## Prerequisites
* NodeJS 18
* Docker
## Steps
* Clone this repository and `cd` into it
* Run `npm install`
* Make sure docker is running
* If necessary, run `chmod +x painful_sort`
* Confim the app can execute by running `./painful_sort --help`
	* It may be necessary to modify the shebang line at the top of the "painful_sort" file.

# Running
Don't.

But if you must:
`./painful_sort -i '["ItemA", "ItemB", "ItemC"]'`

The first run of the application will build the docker container, so for the purposes of performance monitoring this run should be ignored.

# Issues/Future plans
* Emacs output doesn't display properly.  The command line output (using the https://github.com/chjj/blessed) is a stream of the docker logs and is lacking something.
* Larger arrays could be better handled by chunking and running a set of containers on a chunk at a time.
* Tetris moves could be displayed better, probably by using a host file bound to the container.  This would also allow the moves to be examined afterwards.  The emacs script does display the moves in the "Messages" buffer but this isn't very helpful.
