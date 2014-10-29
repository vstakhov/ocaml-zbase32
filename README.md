## ocaml-zbase32

This is a conversion module from and to [zbase32](http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt)
encoding. 

The main purpose of zbase32 is to provide *human* readable encoding that is more efficient than `hex` encoding.
`zbase32` utilizes up to `len * 5 / 8` of space for encoded date and contains no padding (and hence no error control, like `base64`). However, it seems to be much readable for a human when an encoding does not contain padding.

This module is based on [ocaml-hex](https://github.com/mirage/ocaml-hex) module written by Thomas Gazagnaire.

```ocaml
#require "zbase32";;
# Zbase32.of_string ~pretty:false "TestTestTest";;
- : string = "wk3g84tkf5he8kicud7y"
# Zbase32.to_string "wk3g84tkf5he8kicud7y";;
- : string = "TestTestTest"
```
