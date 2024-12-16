const fs = require("fs");
const path = require("path");
const { randomUUID } = require("crypto");
const { PassThrough } = require("stream");
const blessed = require("blessed");

const DISPLAY_COLUMNS = 5;

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
	const result = await new Promise((resolve, reject) => {
		docker.modem.followProgress(buildStream, (err, result) =>
			err ? reject(err) : resolve(result),
		);
	});
	console.log(result);
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

	const boxWidth = Math.floor(100 / DISPLAY_COLUMNS);
	const boxHeight = Math.floor(boxWidth * 2);
	const rowNumber = Math.floor(containerNumber / DISPLAY_COLUMNS);
	const columnNumber = containerNumber % DISPLAY_COLUMNS;

	const logBox = blessed.terminal({
		parent: screen,
		left: `${columnNumber * boxWidth}%`,
		width: `${boxWidth}%`,
		top: `${boxHeight * rowNumber}%`,
		height: `${boxHeight}%`,
		content: "Starting emacs...\n",
		scrollable: false,
		border: {
			type: "line",
		},
		style: {
			fg: "white",
			bg: "grey",
			border: {
				fg: "green",
			},
		},
	});

	screen.render();

	containerStream.on("data", (chunk) => {
		logBox.write(chunk.toString());
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
	logBox.write(`Tetris complete.  Score: ${score}`);
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
