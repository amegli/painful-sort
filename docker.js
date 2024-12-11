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
	files.forEach((fileName) => fs.unlinkSync(path.join("./output", fileName)));
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
 * @returns {Promise}
 */
const run = async (docker, screen, containerNumber, itemCount) => {
	const outputFilePath = path.resolve(`./output/${randomUUID()}.txt`);
	fs.closeSync(fs.openSync(outputFilePath, "a"));

	const containerStream = new PassThrough();

	const boxWidth = Math.floor(100 / DISPLAY_COLUMNS);
	const rowCount = Math.ceil(itemCount / DISPLAY_COLUMNS);
	const boxHeight = Math.floor(100 / rowCount);
	const rowNumber = Math.floor(containerNumber / DISPLAY_COLUMNS);
	const columnNumber = containerNumber % DISPLAY_COLUMNS;

	const logBox = blessed.box({
		left: `${columnNumber * boxWidth}%`,
		width: `${boxWidth}%`,
		top: `${boxHeight * rowNumber}%`,
		height: `${boxHeight}%`,
		content: "Starting emacs...\n",
		scrollable: true,
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

	screen.append(logBox);
	screen.render();

	containerStream.on("data", (chunk) => {
		logBox.setContent(logBox.getContent() + chunk.toString());
		logBox.scrollTo(logBox.getScrollHeight());
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
	logBox.setContent(`Tetris complete.  Score: ${score}`);
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
