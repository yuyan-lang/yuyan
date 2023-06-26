const express = require("express");
const { exec } = require("child_process");
const fs = require("fs");
const path = require("path");

const app = express();
const PORT = 3000;



app.use(express.json());


let isCompiling = false;

app.post("/compile", (req, res) => {
    if (isCompiling) {
        return res.status(403).json({ error: "有其他用户正在编译，请稍后再试" });
    }
    
    isCompiling = true;

  const { code } = req.body;


  // Generate a timestamp-based directory name
  const timestamp = Date.now();
  const userDirectory = `user-${timestamp}`;

  // Create the directory
  fs.mkdirSync(userDirectory);

  // Write the code to a file in the user directory
  const codeFilePath = path.join(userDirectory, "user-code.yuyan");
  fs.writeFile(codeFilePath, code, (err) => {
    if (err) {
      console.error("Error writing code to file:", err);
      isCompiling = false;
      return res.status(500).json({ error: "Error writing code to file" });
    }

    // Invoke the `make` command to compile the file
    exec(`make compile_user_code USER_CODE_DIR=${userDirectory}`, (error, stdout, stderr) => {
      if (error) {
        console.error("Compilation error:", error);
        console.error("stdout:", stdout);
        console.error("stderr:", stderr);
        isCompiling = false;

        return res.status(500).json({ error: stdout + stderr });
      }

      const compiledFilePath = path.join(".yybuild.nosync", userDirectory, "user-code-compiled.js");
      const wasmFilePath = path.join(".yybuild.nosync", userDirectory, "user-code-compiled.wasm");

      // Check if the compiled files exist
      if (fs.existsSync(compiledFilePath) && fs.existsSync(wasmFilePath)) {
        // Read the compiled files
        // const compiledFileContents = fs.readFileSync(compiledFilePath, "utf-8");
        // const wasmFileContents = fs.readFileSync(wasmFilePath);

        // Encode the file contents as base64 strings
        // const compiledFileBase64 = Buffer.from(compiledFileContents).toString("base64");
        // const wasmFileBase64 = wasmFileContents.toString("base64");

        // Send the compiled files as JSON response
        res.json({ compiledFileDirectory: userDirectory });
      } else {
        console.error("Compiled files not found");
        isCompiling = false;
        return res.status(500).json({ error: "Compiled files not found" });
      }
      isCompiling = false;
    });
  });
});

app.get('/online_ide.html', (req, res) => {
    res.sendFile(path.join(__dirname, 'online_ide.html'));
});

app.get("/compiled/:timestamp/:fileName", (req, res) => {
  const { timestamp, fileName } = req.params;

  const filePath = path.join(process.cwd(),".yybuild.nosync", timestamp, fileName);

  res.sendFile(filePath);
});

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});
