const fs = require("fs");
const path = require("path");
const { randomUUID } = require("crypto");
const { PassThrough } = require("stream");
const blessed = require("blessed");

const BOX_WIDTH = 45;

/**
 * @returns {void}
 */
const cleanOutputDir = () => {
	const files = fs.readdirSync("./output");
	files
		.filter((fileName) => fileName !== ".gitkeep")
		.forEach((fileName) => fs.unlinkSync(path.join("./output", fileName)));
};

/**
 * @param {Docker} docker
 * @returns {Promise}
 */
const build = async (docker) => {
	console.log("Docker build starting");
	const buildStream = await docker.buildImage(
		{
			context: __dirname,
			src: ["Dockerfile", "emacs/.emacs"],
		},
		{ t: "emacs_container:latest" },
	);
	const results = await new Promise((resolve, reject) => {
		docker.modem.followProgress(buildStream, (err, result) =>
			err ? reject(err) : resolve(result),
		);
	});
	console.log(
		results
			.map((result) => result.stream)
			.filter((result) => result !== "\n")
			.join("\n"),
	);
};

/**
 * @param {Docker} docker
 * @param {blessed.screen} screen
 * @param {Integer} containerNumber
 * @returns {Promise}
 */
const run = async (docker, screen, containerNumber) => {
	const outputFilePath = path.resolve(`./output/${randomUUID()}.txt`);
	fs.closeSync(fs.openSync(outputFilePath, "a"));

	const containerStream = new PassThrough();

	const columnsPerRow = Math.floor(screen.width / BOX_WIDTH);
	const boxHeight = Math.floor(BOX_WIDTH * 0.6);
	const rowNumber = Math.floor(containerNumber / columnsPerRow);
	const columnNumber = containerNumber % columnsPerRow;

	const terminal = blessed.terminal({
		parent: screen,
		width: BOX_WIDTH,
		height: boxHeight,
		top: boxHeight * rowNumber,
		left: columnNumber * BOX_WIDTH,
		content: "Starting emacs...\n",
		scrollable: false,
		border: "line",
		style: {
			border: {
				fg: "green",
			},
		},
	});

	screen.render();

	containerStream.on("data", (chunk) => {
		terminal.write(chunk.toString());
		screen.render();
	});

	await docker.run(
		"emacs_container:latest",
		["bash", "-c", "emacs -nw"],
		containerStream,
		{
			Tty: true,
			HostConfig: {
				Binds: [`${outputFilePath}:/tmp/tetris-scores`],
			},
		},
	);

	const score = extractScore(fs.readFileSync(outputFilePath));
	terminal.write(`Tetris complete.  Score: ${score}`);
	return score;
};

/**
 * @param {Buffer} content
 * @return {Integer}
 */
const extractScore = (content) => {
	return parseInt(content.toString().split(" ")[0]);
};

module.exports = { cleanOutputDir, build, run };
