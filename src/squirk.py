#!/usr/bin/env python

import sys
import os
import ctypes
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
import time
from datetime import datetime
import urllib.parse
import mimetypes

hostName = "localhost"
serverPort = 5000

PRESETS = {
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
    print("--help                                                              Show this help message.")
    print("--presets                                                           List all available file presets.")
    print('--server start "<directory>"                                        Starts/Stops a local SQUIRK Server in the specified directory.')
    print('init <project_name> <author> "<description>" <info - yes/no>        Initialize a new SQUIRK project.')
    print('initf "<directory>" <type> <name> <preset>                          Create a new file in the specified folder with the given type.')

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

def create_metadata_file(project_path, author=None, description=None):
    """Erstellt die metadata.json Datei im angegebenen Projektpfad."""
    metadata = {
        "author": author if author else "Unknown",
        "date": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "description": description if description else ""
    }

    metadata_path = os.path.join(project_path, "metadata.json")
    with open(metadata_path, "w") as file:
        json.dump(metadata, file, indent=4)
    print(f"metadata.json file created in '{project_path}'.")

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
    
    if not os.path.exists(project_path):
        os.makedirs(project_path)
        create_instructions_file(project_path)
        create_metadata_file(project_path, author, description)
        if create_markdown:
            create_information_file(project_path, description)
        print(f"Project '{project_name}' created successfully in '{project_path}'.")
    else:
        print(f"Error: Project '{project_name}' already exists in '{project_path}'.")

def create_file(folder, file_type, name, preset=None):
    """Erstellt eine neue Datei im angegebenen Ordner mit dem angegebenen Typ und Namen."""
    if not os.path.exists(folder):
        print(f"Error: Folder '{folder}' does not exist.")
        return
    
    file_path = os.path.join(folder, f"{name}{file_type}")

    if os.path.exists(file_path):
        print(f"Error: File '{file_path}' already exists.")
        return
    
    with open(file_path, "w") as file:
        if preset and preset in PRESETS:
            file.write(PRESETS[preset])
        else:
            file.write(f"# {name} - {file_type}\n\n")

    print(f"File '{file_path}' created successfully.")

class MyServer(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        """Log to server.log file."""
        log_path = os.path.join(self.server.server_directory, "LOGGING", "server.log")
        with open(log_path, "a") as log_file:
            log_file.write(f"{self.log_date_time_string()} - {format % args}\n")

    def do_GET(self):
        """Serve a file from the server directory."""
        self.server.server_directory = os.path.abspath(self.server.server_directory)
        parsed_path = urllib.parse.urlparse(self.path)
        
        # Handle root path request
        if parsed_path.path == '/':
            parsed_path = urllib.parse.urlparse('/index.html')
        
        file_path = os.path.join(self.server.server_directory, parsed_path.path.strip("/"))

        if os.path.isfile(file_path):
            try:
                with open(file_path, 'rb') as file:
                    self.send_response(200)
                    self.send_header("Content-type", self.guess_type(file_path))
                    self.end_headers()
                    self.wfile.write(file.read())
            except IOError:
                self.send_error(404, "File not found")
        else:
            self.send_error(404, "File not found")

    def guess_type(self, path):
        """Guess the MIME type based on file extension."""
        return mimetypes.guess_type(path)[0] or "application/octet-stream"

def create_welcome_file(directory):
    """Erstellt die src/index.html Datei mit einem Willkommenstext."""
    index_path = os.path.join(directory, "index.html")
    welcome_content = (
        "<!DOCTYPE html>\n"
        "<html>\n"
        "<head>\n"
        "    <title>Welcome</title>\n"
        "</head>\n"
        "<body>\n"
        "    <h1>Welcome to SQUIRK Server</h1>\n"
        "    <p>Your SQUIRK server is up and running!</p>\n"
        "</body>\n"
        "</html>\n"
    )

    with open(index_path, "w") as file:
        file.write(welcome_content)
    print(f"Welcome file created at '{index_path}'.")

def start_server(directory):
    if not os.path.isdir(directory):
        print(f"Error: Directory '{directory}' does not exist.")
        return

    # Create the welcome file
    create_welcome_file(directory)

    log_dir = os.path.join(directory, "LOGGING")
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)
    
    log_path = os.path.join(log_dir, "server.log")
    if not os.path.isfile(log_path):
        with open(log_path, "w") as log_file:
            log_file.write("Server Log Initialized\n")

    server_address = (hostName, serverPort)
    httpd = HTTPServer(server_address, MyServer)
    httpd.server_directory = directory
    
    print(f"Server started at http://{hostName}:{serverPort} with directory '{directory}'")
    print(f"Logging to '{log_path}'")
    
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        httpd.server_close()
        print("Server stopped.")

def parse_description(description_args):
    """Parst die Beschreibung aus den Argumenten."""
    return ' '.join(description_args).strip('"')

def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == "--help":
            show_help()
        elif sys.argv[1] == "--version":
            print("Current version: Alpha v.0.1.0")
        elif sys.argv[1] == "--presets":
            show_presets()
        elif sys.argv[1] == "--server":
            if len(sys.argv) > 2 and sys.argv[2] == "start":
                if len(sys.argv) > 3:
                    directory = sys.argv[3]
                    start_server(directory)
                else:
                    print("Error: No directory specified.")
            else:
                print("Unknown command. Use --help for available commands.")
        elif sys.argv[1] == "init":
            if len(sys.argv) > 2:
                project_name = sys.argv[2]
                author = sys.argv[3] if len(sys.argv) > 3 else None
                description_args = []
                create_markdown = False
                if len(sys.argv) > 4:
                    if sys.argv[-1].lower() in ['yes', 'no']:
                        create_markdown = sys.argv[-1].lower() == 'yes'
                        description_args = sys.argv[4:-1]
                    else:
                        description_args = sys.argv[4:]
                description = parse_description(description_args)
                init_project(project_name, author, description, create_markdown)
            else:
                print("Error: No project name provided.")
        elif sys.argv[1] == "initf":
            if len(sys.argv) > 4:
                folder = sys.argv[2]
                file_type = sys.argv[3]
                name = sys.argv[4]
                preset = sys.argv[5] if len(sys.argv) > 5 else None
                create_file(folder, file_type, name, preset)
            else:
                print("Error: Insufficient arguments for 'initf'.")
        else:
            print("Unknown command. Use --help for available commands.")
    else:
        print("Unknown command. Use --help for available commands.")

if __name__ == "__main__":
    main()
