
import lldb

def breakpoint_callback(frame, bp_loc, dict):
    # Get the value of the variable you want to print
    value = frame.FindVariable('resultString')
    # Print the value
    print(value)
    # Continue execution
    frame.GetThread().Continue()

# Create a new debugger instance
debugger = lldb.SBDebugger.Create()

# Set up the target and executable
target = debugger.CreateTarget('yy_bs_bs')

# Set the arguments for the target
args = ['tests/syntax/empty-1.yuyan', '-vvv']  # Replace with your desired arguments
target.SetLaunchInfo(lldb.SBLaunchInfo(args))


# Set the breakpoint at the desired location
breakpoint = target.BreakpointCreateByLocation('strings.c', 328)

# Set the callback function for the breakpoint
breakpoint.SetScriptCallbackFunction('breakpoint_callback')

# Launch the debugger and run the program
debugger.SetAsync(False)
debugger.HandleCommand('run')
