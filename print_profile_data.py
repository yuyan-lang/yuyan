
import subprocess
from collections import defaultdict

def get_symbol_offset(executable_path, symbol_name):
    try:
        nm_output = subprocess.check_output(['nm', executable_path], universal_newlines=True)
        
        # Assuming the output format is like "address T symbol_name"
        for line in nm_output.splitlines():
            parts = line.split()
            if len(parts) >= 3 and parts[2] == symbol_name:
                offset_hex = parts[0]
                # Convert hex offset to an integer
                offset = int(offset_hex, 16)
                return offset

        return None
    except subprocess.CalledProcessError:
        return None

def get_symbol_name(executable, address):
    try:
        result = subprocess.check_output(['addr2line', '-e', executable, '-f', '-C', '-i', address],
                                         universal_newlines=True, stderr=subprocess.PIPE)
        return result.strip().splitlines()
    except subprocess.CalledProcessError as e:
        return ['Error: Unable to retrieve symbol name for address', address]

def process_profiling_data(file_path):
    # Read the executable name and addresses from the profiling data file
    with open(file_path, 'r') as file:
        lines = file.readlines()

    [executable, entryMainAddr] = lines[0].strip().split()
    offset = int(entryMainAddr, 16) - get_symbol_offset(executable, 'entryMain')
    address_lines = [address.strip().split() for address in lines[1:] if address.strip()]
    addresses = list(set([a for ads in address_lines for a in ads]))


    total_lines = len(lines) - 1
    # Analyze and print the results
    results = []
    for address in addresses:
        symbol_info = get_symbol_name(executable, hex(int(address, 16) - offset))
        symbol_name = symbol_info[0] if len(symbol_info) > 0 else 'Unknown Symbol'
        
        self_time = (len([0 for al in address_lines if al[0] == address]) / total_lines) * 100
        other_time = (len([0 for al in address_lines if al[0] != address and address in al]) / total_lines) * 100
        all_times = (len([0 for al in address_lines if address in al]) / total_lines) * 100

        results.append({
            'Address': address,
            'Symbol Name': symbol_name,
            'Self Time (%)': f'{self_time:.4f}',
            'Other Time (%)': f'{other_time:.4f}',
            'All Times (%)': f'{all_times:.4f}'
        })

    # Sort results by self time in descending order
    results.sort(key=lambda x: float(x['Self Time (%)']), reverse=True)

    # Print the results
    for result in results:
        print(result)

if __name__ == "__main__":
    profiling_data_file = "yy_profiledata.txt"  # Replace with your actual file path
    process_profiling_data(profiling_data_file)
