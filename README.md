## Usage

- From the terminal, run `sbt "runCam -i <input_file>"`
- Inside sbt shell run `runCam -i <input_file>`

By default, the working directory for input and output files is `$CAM_ROOT/storage`; therefore, it is recommended to
define this variable where you are comfortable storing input and output files. If `$CAM_ROOT` does not specified,
`./storage` will be used as the default.

### Available options

```
Usage:  [options]

  -i, --input <value>      Input file name. The output file will be created in the same directory with a '_result' 
  suffix and a '.md' extension. If the file already exists, it will be overwritten.
  -v, --verbose            Enable verbose output: all output will be duplicated to the console, in addition to being 
  written to the output file.
  -d, --input-dir <value>  Specify directory for input file (default: $CAM_ROOT/storage)
```