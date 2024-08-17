# SQUIRKLANG
SQUIRKLANG is the config language of SQUIRK, used to create projects or save data for various purposes locally. It uses some json-like syntax to make it more accessible. On top of the easy data save, it works very good with many other projects and can be used without any problems in any project.

## Syntax
SQUIRKLANG uses easy json-like syntax to make it more accessible for all people. If you start your .sqrk file, you first have to place two brackets like this:

```
[

]
 ```
Now, you can add various values to it. But before you can add these values, you first have to add the two default values, named `key` and `name`. The `key` value can be anything, that fits the purpose of the file. The `name` value has to be the file name, but without the ending (main, not main.sqrk):

```
[
key: mykey
name: main
]
```
After that, you can add various values to your file. Make sure, you first type the `identifier`, the name of your data, then a `:`, and after that and a whitespace, you type in the value:

```
[
key: mykey
name: main
bananas: 20
]
```

## Usage
Now, after saving the file, you can (if you have the latest version of SQUIRK installed), navigate to the directory and type in:

```
squirk run <filename.sqrk>
```
Now you have succesfully created your first SQUIRKLANG file!
