const { spawn } = require("child_process");

function runCommand(event, callback, commandPath, commandArgs) {
  process.env["LD_LIBRARY_PATH"] = process.env["LAMBDA_TASK_ROOT"]
  process.env["PATH"] = process.env["PATH"] + ":" + process.env["LAMBDA_TASK_ROOT"];

  const main = spawn(commandPath, commandArgs, {
    stdio: ["pipe", process.stdout, process.stderr, "pipe"],
  });
  const stdin = main.stdin;
  const communication = main.stdio[3];

  // JSON-encode event object and send to stdin
  stdin.end(JSON.stringify(event) + "\n", "utf8");

  // Accumulate stdout
  let output = "";
  communication.on("data", chunk => output += chunk);

  let exited = false;

  main.on("exit", function (code) {
    if (!exited) {
      exited = true;
      if (code == 0) {
        try {
          const result = JSON.parse(output);
          callback(null, result);
        } catch (err) {
          const message = `child process output bad JSON: ${output}`;
          console.error(message);
          callback(message);
        }
      }
      else {
        const message = `child process exited with code ${code}`;
        console.error(message);
        callback(message);
      }
    }
  });

  main.on("error", function (err) {
    if (!exited) {
      exited = true;
      const message = `child process exited with error: ${err}`;
      console.error(message);
      callback(err, message);
    }
  });
}

exports.handler = (event, context, callback) => {
  runCommand(event, callback, "hlambda", []);
};
