import re
import sys


def _add_prelude(text: str) -> str:
    prelude = """
module Int with
  fun to_string x = string_of_int x
end

module String with
  fun to_int s = int_of_string s
end

module List with
  type 'a list =
    | Empty
    | Node of 'a * 'a list

  fun reverse l =
    fun aux l acc =
      match l with
      | Empty -> acc
      | Node (hd, tl) -> aux tl (Node (hd, acc))
    in
    aux l Empty

  fun make f n =
    fun aux n' acc =
      match n' with
      | 0 -> acc
      | _ -> 
        let elem = f n' in
        aux (n' - 1) (Node (elem, acc))
    in
    aux n Empty

  fun fold f acc l =
    match l with
    | Empty -> acc
    | Node (hd, tl) -> fold f (f acc hd) tl

  fun print f l =
    fun aux l acc =
      match l with
      | Empty -> acc ^ "]"
      | Node (hd, Empty) -> aux Empty (acc ^ (f hd))
      | Node (hd, tl) -> aux tl (acc ^ (f hd) ^ ",")
    in 
    print_endline (aux l "[")

  fun map f l =
    fun aux l acc =
      match l with
      | Empty -> reverse acc
      | Node (hd, tl) -> 
        let next = f hd in
        aux tl (Node (next, acc))
    in
    aux l Empty

  fun iter f l =
    match l with
    | Empty -> ()
    | Node (hd, tl) -> f hd; iter f tl

  fun iteri f l =
    match l with
    | Empty -> ()
    | Node (hd, tl) -> f hd; iter f tl
end

module Io with
  fun print = Printf.printf
end
"""
    return prelude + text


def _replace_fun(text: str) -> str:
    return re.sub(r"(?<!\()\bfun\b", "let rec", text)


def _replace_module(text: str) -> str:
    return re.sub(
        r"\bmodule\s+([A-Za-z_][A-Za-z0-9_]*'{0,})\s+with\b",
        r"module \g<1> = struct",
        text,
    )


def _replace_array(text: str) -> str:
    pattern = r"\{([^{}]*)\}"

    def brace_to_array(m):
        inside = m.group(1).strip()
        if not inside:
            return "[||]"
        inside = re.sub(r"\s*,\s*", "; ", inside)
        return f"[|{inside}|]"

    return re.sub(pattern, brace_to_array, text)


def _replace_array_access(text: str) -> str:
    return re.sub(
        r"\[\s+([0-9])\s+\]",
        r"\.\(\g<1>\)",
        text,
    )


def _replace_array(text: str) -> str:
    pattern = r"\{([^{}]*)\}"

    def brace_to_array(m):
        inside = m.group(1).strip()
        if not inside:
            return "[||]"
        inside = re.sub(r"\s*,\s*", "; ", inside)
        return f"[|{inside}|]"

    return re.sub(pattern, brace_to_array, text)


def _transpile(text: str) -> str:
    text = _add_prelude(text)
    text = _replace_fun(text)
    text = _replace_module(text)
    text = _replace_array(text)
    text = _replace_array_access(text)
    return text


if __name__ == "__main__":
    filepath = sys.argv[1]
    out = sys.argv[2]
    with open(filepath, "r") as file:
        text = file.read()
    text = _transpile(text)
    with open(out, "w") as file:
        file.write(text)
