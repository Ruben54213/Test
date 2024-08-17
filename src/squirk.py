#!/usr/bin/env python

import sys
import os
import json
from datetime import datetime
from http.server import BaseHTTPRequestHandler, HTTPServer
import urllib.parse
import mimetypes

hostName = "localhost"
serverPort = 5000

PRESETS = {
    ".sqrk": "",
    ".py": "#!/usr/bin/env python\n\n\"\"\"Python Script\"\"\"\n\nif __name__ == '__main__':\n    pass\n",
    ".c": "#include <stdio.h>\n\nint main() {\n    // Your code here\n    return 0;\n}\n",
    ".js": "// JavaScript File\n\nfunction main() {\n    // Your code here\n}\n\nmain();\n",
    ".html": "<!DOCTYPE html>\n<html>\n<head>\n    <title>Title</title>\n</head>\n<body>\n    <!-- Your content here -->\n</body>\n</html>\n",
    ".css": "/* CSS File */\n\nbody {\n    font-family: Arial, sans-serif;\n}\n",
    ".cpp": "#include <iostream>\n\nint main() {\n    // Your code here\n    return 0;\n}\n",
    ".java": "public class Main {\n    public static void main(String[] args) {\n        // Your code here\n    }\n}\n",
    ".sh": "#!/bin/bash\n\n# Shell Script\n\n",
    ".rb": "# Ruby Script\n\nputs 'Hello, World!'\n",
    ".php": "<?php\n\n// PHP Script\n\necho 'Hello, World!';\n",
    ".swift": "import Foundation\n\nprint(\"Hello, World!\")\n",
    ".kt": "fun main() {\n    println(\"Hello, World!\")\n}\n",
    ".ts": "function main() {\n    console.log('Hello, World!');\n}\n\nmain();\n",
    ".go": "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}\n",
    ".json": "{\n    \"key\": \"value\"\n}\n",
    ".yaml": "# YAML file\n\nkey: value\n",
    ".xml": "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<root>\n    <element>Content</element>\n</root>\n",
    ".md": "# Markdown File\n\n## Title\n\nContent here.\n",
    ".r": "# R Script\n\nprint('Hello, World!')\n",
    ".pl": "#!/usr/bin/perl\n\n# Perl Script\n\nprint \"Hello, World!\\n\";\n",
    ".lua": "print(\"Hello, World!\")\n",
    ".bat": "@echo off\n\nREM Batch Script\n\necho Hello, World!\n",
    ".tsv": "# Tab-Separated Values\n\nkey\tvalue\n",
    ".sql": "-- SQL Script\n\nSELECT * FROM table;\n",
    ".vbs": "MsgBox \"Hello, World!\"\n",
    ".awk": "#!/usr/bin/awk -f\n\n# AWK Script\n\nBEGIN { print \"Hello, World!\" }\n",
    ".ps1": "# PowerShell Script\n\nWrite-Output \"Hello, World!\"\n",
    ".dockerfile": "# Dockerfile\n\nFROM python:3.8\n\nCOPY . /app\nWORKDIR /app\nRUN pip install -r requirements.txt\nCMD [\"python\", \"app.py\"]\n",
    ".h": "#ifndef HEADER_FILE_H\n#define HEADER_FILE_H\n\n// Header File Content\n\n#endif // HEADER_FILE_H\n",
    ".rmd": "---\ntitle: \"Document\"\noutput: html_document\n---\n\n## R Markdown\n\nHere is some R code:\n\n```{r}\nsummary(cars)\n```\n",
    ".scss": "// SCSS File\n\n$primary-color: #333;\n\nbody {\n    color: $primary-color;\n}\n",
    ".json5": "{\n    // This is a JSON5 file\n    key: \"value\",\n    // Comment\n    number: 123,\n    bool: true,\n    array: [1, 2, 3]\n}\n",
    ".clj": "(ns example.core)\n\n(defn -main\n  \"I don't do a whole lot.\"\n  [& args]\n  (println \"Hello, World!\"))\n",
    ".erl": "-module(hello).\n-export([world/0]).\n\nworld() ->\n    io:format(\"Hello, World!~n\").\n",
    ".plk": "#!/usr/bin/env perl\n\n# Perl Script\n\nuse strict;\nuse warnings;\n\nprint \"Hello, World!\\n\";\n",
    ".sml": "(* Standard ML Script *)\n\nfun main () =\n    print \"Hello, World!\\n\"\n",
    ".fsh": "open System\n\n[<EntryPoint>]\nlet main argv =\n    printfn \"Hello, World!\"\n    0\n",
    ".lisp": "(defun hello-world ()\n  (format t \"Hello, World!~%\"))\n\n(hello-world)\n",
    ".m": "% MATLAB Script\n\ndisp('Hello, World!')\n",
    ".tex": "\\documentclass{article}\n\n\\begin{document}\n\nHello, World!\n\n\\end{document}\n",
    ".coffee": "console.log 'Hello, World!'\n"
}

def show_help():
    print("SQUIRK Help")
    print("-- version                                                          Displays the current version.")
    print("--help                                                              Show this help message.")
    print("--presets                                                           List all available file presets.")
    print('--server start "<directory>"                                        Starts/Stops a local SQUIRK Server in the specified directory.')
    print('init <project_name> <author> "<description>" <info - yes/no>        Initialize a new SQUIRK project.')
    print('initf "<directory>" <type> <name> <preset>                          Create a new file in the specified folder with the given type.')
    print('run <file.sqrk>                                                     Run a .sqrk file.')

def show_presets():
    """Zeigt die verfügbaren Presets an."""
    print("Available Presets:")
    for ext in PRESETS:
        print(f"{ext}")

def create_instructions_file(project_path):
    """Erstellt die INSTRUCTIONS.txt Datei im angegebenen Projektpfad."""
    instructions_content = (
        "SQUIRK - PROJECT INSTRUCTIONS\n"
        "You have successfully created a SQUIRK project. You can now add as many files as you like. To create a new file, use the command:\n\n"
        "squirk initf <folder> <type> (e.g., .py, .c, .js) <name> [preset (optional)]\n\n"
        "This will create a file in the specified folder. It doesn't have to be a project folder of SQUIRK, but it offers many advantages.\n\n"
        "For more information, visit the repository: https://github.com/AcariusTV/SQUIRK"
    )

    instructions_path = os.path.join(project_path, "INSTRUCTIONS.txt")
    with open(instructions_path, "w") as file:
        file.write(instructions_content)

def create_fpindex_file(project_path, author=None, description=None):
    """Erstellt die fpindex.sqrk Datei im angegebenen Projektpfad."""
    fpindex = {
        "key": "fpindexp",
        "name": "fpindex.sqrk",
        "author": author if author else "Unknown",
        "date": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "description": description if description else ""
    }

    metadata_path = os.path.join(project_path, "fpindex.sqrk")
    with open(metadata_path, "w") as file:
        file.write('[\n')
        file.write(f'    "key": "{fpindex["key"]}",\n')
        file.write(f'    "name": "{fpindex["name"]}",\n')
        file.write(f'    "author": "{fpindex["author"]}",\n')
        file.write(f'    "date": "{fpindex["date"]}",\n')
        file.write(f'    "description": "{fpindex["description"]}"\n')
        file.write(']')
    print(f"fpindex.sqrk file created in '{project_path}'.")

def create_information_file(project_path, description):
    """Erstellt die INFORMATION.md Datei im angegebenen Projektpfad."""
    information_content = f"# Project Information\n\n{description}\n"

    information_path = os.path.join(project_path, "INFORMATION.md")
    with open(information_path, "w") as file:
        file.write(information_content)
    print(f"INFORMATION.md file created in '{project_path}'.")

def init_project(project_name, author=None, description=None, create_markdown=False):
    """Initialisiert ein neues SQUIRK-Projekt."""
    if not project_name:
        print("Error: No project name provided.")
        return
    
    projects_dir = os.path.join("C:\\", "SQUIRK", "Projects")
    
    if not os.path.exists(projects_dir):
        os.makedirs(projects_dir)
    
    project_path = os.path.join(projects_dir, project_name)
    
    if os.path.exists(project_path):
        print(f"Error: Project '{project_name}' already exists.")
        return
    
    os.makedirs(project_path)
    create_instructions_file(project_path)
    create_fpindex_file(project_path, author, description)
    
    if create_markdown:
        create_information_file(project_path, description)
    
    print(f"Project '{project_name}' initialized successfully in '{project_path}'.")

def create_file(directory, file_type, name, preset=None):
    """Erstellt eine neue Datei im angegebenen Verzeichnis."""
    if not os.path.exists(directory):
        print(f"Error: Directory '{directory}' does not exist.")
        return

    file_name = f"{name}{file_type}"
    file_path = os.path.join(directory, file_name)

    if os.path.exists(file_path):
        print(f"Error: File '{file_name}' already exists in directory '{directory}'.")
        return

    with open(file_path, "w") as file:
        content = PRESETS.get(preset, "") if preset else PRESETS.get(file_type, "")
        file.write(content)
    print(f"File '{file_name}' created successfully in '{directory}'.")

def run_squirk_file(file_path):
    """Führt eine .sqrk-Datei aus und gibt deren Inhalt aus, wenn sie dem erwarteten Format entspricht."""

    if not os.path.exists(file_path):
        print(f"Error: File '{file_path}' does not exist.")
        return

    if not file_path.endswith(".sqrk"):
        print(f"Error: File '{file_path}' is not a .sqrk file.")
        return

    try:
        with open(file_path, 'r') as file:
            content = file.read().strip()
        
        if not (content.startswith('[') and content.endswith(']')):
            print("Error: File content does not start and end with brackets.")
            return
        
        content = content[1:-1].strip()
        if not content:
            print("Error: File content is empty after removing brackets.")
            return
        
        lines = content.split('\n')
        
        data = {}
        for line in lines:
            line = line.strip()
            if line:
                if ':' not in line:
                    print(f"Error: Line does not contain ':' - {line}")
                    return
                
                key, value = line.split(':', 1)
                key = key.strip()
                value = value.strip()
                
                data[key] = value
        
        if 'key' not in data:
            print("Error: Missing required key 'key'")
            return
        
        if 'name' not in data:
            print("Error: Missing required key 'name'")
            return
        
        expected_name = os.path.splitext(os.path.basename(file_path))[0]
        
        if data['name'] != expected_name:
            print(f"Error: The 'name' value '{data['name']}' does not match the file name '{expected_name}'.")
            return
        
        print(f"key: {data['key']}")
        print(f"name: {data['name']}")
        
        additional_keys = [k for k in data if k not in ['key', 'name']]
        if additional_keys:
            for key in additional_keys:
                print(f"{key}: {data[key]}")
    
    except IOError as e:
        print(f"Error: Unable to read file '{file_path}'. {e}")

def run_server(directory):
    """Startet einen lokalen SQUIRK-Server."""
    os.chdir(directory)
    webServer = HTTPServer((hostName, serverPort), MyServer)
    print(f"Server started at http://{hostName}:{serverPort}")

    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
    print("Server stopped.")

class MyServer(BaseHTTPRequestHandler):
    def do_GET(self):
        parsed_path = urllib.parse.urlparse(self.path)
        file_path = parsed_path.path.strip("/")

        if not file_path:
            file_path = "index.html"

        if os.path.exists(file_path):
            self.send_response(200)
            self.send_header("Content-type", mimetypes.guess_type(file_path)[0])
            self.end_headers()

            with open(file_path, "rb") as file:
                self.wfile.write(file.read())
        else:
            self.send_response(404)
            self.end_headers()
            self.wfile.write(b"404 - File not found")

if __name__ == "__main__":
    args = sys.argv[1:]

    if not args:
        print("Error: No arguments provided. Use --help for more information.")
    elif "--help" in args:
        show_help()
    elif "--version" in args:
        print("Current version: v.0.2.1")
    elif "--presets" in args:
        show_presets()
    elif args[0] == "init" and len(args) == 5:
        init_project(args[1], args[2], args[3], args[4].lower() == "yes")
    elif args[0] == "initf" and len(args) >= 4:
        directory = args[1]
        file_type = args[2]
        name = args[3]
        preset = args[4] if len(args) == 5 else None
        create_file(directory, file_type, name, preset)
    elif args[0] == "server" and len(args) == 3:
        if args[1] == "start":
            run_server(args[2])
        else:
            print("Error: Invalid server command. Use 'start'.")
    elif args[0] == "run" and len(args) == 2:
        run_squirk_file(args[1])
    else:
        print("Error: Invalid command. Use --help for more information.")
