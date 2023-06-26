// pre.js

var Module = {
    print: function(text) {
      // Redirect stdout to stderrOutput element
      var stdoutOutput = document.getElementById("stdout-output");
      stdoutOutput.textContent += text + "\n";
    },
    printErr: function(text) {
      // Redirect stderr to stderrOutput element
      var stderrOutput = document.getElementById("stderr-output");
      stderrOutput.textContent += text + "\n";
    }
  };
