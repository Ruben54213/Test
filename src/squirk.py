#!/usr/bin/env python

import sys
import os
import ctypes
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
import time
from datetime import datetime

# Presets für verschiedene Dateitypen
PRESETS = {
    ".py": "#!/usr/bin/env python\n\n\"\"\"Python Script\"\"\"\n\nif __name__ == '__main__':\n    pass\n",
    ".c": "#include <stdio.h>\n\nint main() {\n    // Your code here\n    return 0;\n}\n",
    ".js": "// JavaScript File\n\nfunction main() {\n    // Your code here\n}\n\nmain();\n",
    ".html": "<!DOCTYPE html>\n<html>\n<head>\n    <title>Title</title>\n</head>\n<body>\n    <!-- Your content here -->\n</body>\n</html>\n",
    ".css": "/* CSS File */\n\nbody {\n    font-family: Arial, sans-serif;\n}\n",
    ".cpp": "#include <iostream>\n\nint main() {\n    // Your code here\n    return 0;\n}\n",
    ".java": "public class Main {\n    public static void main(String[] args) {\n        // Your code here\n    }\n}\n",
    ".sh": "#!/bin/bash\n\n# Shell Script\n\n",
}

def show_help():
    print("SQUIRK Help")
    print("--help                                                       Show this help message.")
    print("--presets                                                    List all available file presets.")
    print("--server start                                               Starts/Stops a local SQUIRK Server.")
    print('init <project_name> <author> "<description>" <info - yes/no>   Initialize a new SQUIRK project.')
    print('initf "<folder>" <type> <name> <preset>                      Create a new file in the specified folder with the given type.')

hostName = "localhost"
serverPort = 5000

class MyServer(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes("<html><head><title>SQUIRK Local Server</title></head>", "utf-8"))
        self.wfile.write(bytes("<body>", "utf-8"))
        self.wfile.write(bytes("<p>This is a SQUIRK Local Server.</p>", "utf-8"))
        self.wfile.write(bytes("</body></html>", "utf-8"))

def show_presets():
    """Zeigt die verfügbaren Presets an."""
    print("Available Presets:")
    for ext in PRESETS:
        print(f"{ext}")

def is_admin():
    """Überprüft, ob das Skript mit Administratorrechten ausgeführt wird."""
    try:
        return ctypes.windll.shell32.IsUserAnAdmin()
    except:
        return False

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
    
    # Überprüfen, ob das Skript mit Administratorrechten ausgeführt wird
    if not is_admin():
        print("Error: Initializing a project requires administrator privileges.")
        return
    
    # Verzeichnis für Projekte in Program Files
    projects_dir = os.path.join(os.environ["ProgramFiles"], "SQUIRK", "Projects")
    
    # Überprüfen, ob das Verzeichnis existiert, und ggf. erstellen
    if not os.path.exists(projects_dir):
        os.makedirs(projects_dir)
    
    project_path = os.path.join(projects_dir, project_name)
    
    if os.path.exists(project_path):
        print(f"Error: Project directory '{project_path}' already exists.")
    else:
        os.makedirs(project_path)
        print(f"Project '{project_name}' initialized successfully at '{project_path}'.")
        
        # INSTRUCTIONS.txt Datei erstellen
        create_instructions_file(project_path)
        # metadata.json Datei erstellen
        create_metadata_file(project_path, author, description)
        
        # INFORMATION.md Datei erstellen, wenn gewünscht
        if create_markdown:
            create_information_file(project_path, description)

def create_file(folder, file_type, name, preset=None):
    """Erstellt eine Datei im angegebenen Ordner mit dem spezifizierten Dateityp."""
    folder = folder.strip('"')  # Entferne potenzielle Anführungszeichen
    
    if not os.path.exists(folder):
        print(f"Error: Folder '{folder}' does not exist.")
        return
    
    file_name = f"{name}{file_type}"
    file_path = os.path.join(folder, file_name)
    
    if os.path.exists(file_path):
        print(f"Error: File '{file_path}' already exists.")
        return
    
    with open(file_path, "w") as file:
        if preset and preset in PRESETS:
            file.write(PRESETS[preset])
        else:
            file.write(f"# File created with SQUIRK\n")
        print(f"File '{file_path}' created successfully.")

def start_server():
    webServer = HTTPServer((hostName, serverPort), MyServer)
    print(f"Server started at http://{hostName}:{serverPort}")

    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
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
                start_server()
            else:
                print("Unknown command. Use --help for available commands.")
        elif sys.argv[1] == "init":
            if len(sys.argv) > 2:
                project_name = sys.argv[2]
                author = sys.argv[3] if len(sys.argv) > 3 else None
                # Parsing description and markdown creation flag
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
