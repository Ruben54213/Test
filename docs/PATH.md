# SQUIRK: HOW TO MODIFY PATH

The `PATH` environment variable tells the operating system where to look for executable files. Sometimes, you might need to add a new directory to the `PATH` so that the system can find and run programs located there.

## Finding the PATH Variable

### Using Command Prompt

1. **Open the Command Prompt**:
   - Press `Win + R`, type `cmd`, and press `Enter`.

2. **Display the PATH**:
   - Type the following command and press `Enter`:
     ```
     echo %PATH%
     ```

   - This command will display the current `PATH` variable.

## Adding to the PATH Variable

1. **Access Environment Variables**:
   - Right-click on `This PC` or `Computer` on your desktop or in File Explorer.
   - Select `Properties`, then click on `Advanced system settings`.
   - In the System Properties window, click on `Environment Variables`.

2. **Edit the PATH Variable**:
   - In the Environment Variables window, locate the `Path` variable in the `System variables` section and select it.
   - Click `Edit...`.

3. **Add a New Path**:
   - In the Edit Environment Variable window, click `New` and enter the directory you want to add to the `PATH`. Make sure the directory you add exists, as the system will look for executables here.

4. **Rearrange Paths (Optional)**:
   - You can use the `Move Up` and `Move Down` buttons to reorder the directories in the `PATH`. The system will search the directories in the order they appear.

5. **Save Changes**:
   - After adding the new path(s), click `OK` to close each window.

6. **Verify the Change**:
   - Open a new Command Prompt window and type `echo %PATH%` to ensure that your changes have been applied.

## Tips

- **Avoid Mistakes**: Be careful when editing the `PATH`. Incorrectly modifying it can cause system issues.
- **Back-Up**: Before making changes, consider copying the current `PATH` variable to a text file as a backup.

Now your system will recognize executables in the newly added directory without needing to specify the full path to them.
