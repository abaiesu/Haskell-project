// Import the child_process module
const { spawn } = require('child_process');

// Define the path to your .exe file
const exePath = '../Text.exe';

// Define the input data you want to pass to the .exe file
const inputData = '2 3';

// Create a child process to run the .exe file
const child = spawn(exePath, [inputData]);

// Capture the output of the .exe file
let outputData = '';

// Handle standard output
child.stdout.on('data', (data) => {
    outputData += data.toString();
});

// Handle errors, if any
child.stderr.on('data', (error) => {
    console.error(error.toString());
});

// Handle process exit
child.on('exit', (code) => {
    console.log(`Child process exited with code ${code}`);
    console.log(`Output: ${outputData}`);
});