# Coerce a chat response to a single character scalar

`ellmer` chat objects' `$chat()` method returns a character vector
(possibly with class attributes for ANSI). The
[`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
returns a plain string. This helper normalises both shapes.

## Usage

``` r
as_chat_text(x)
```
